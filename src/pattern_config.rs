#![forbid(unsafe_code)]

use crate::{find_pattern_instances, FancyRegex, Line, MatchInstance, PatternStep};
use serde::Deserialize;
use std::collections::{BTreeSet, HashMap};
use std::sync::Arc;

#[derive(Debug)]
pub struct PatternPack {
    pub pack: String,
    pub patterns: Vec<PatternDefinition>,
}

#[derive(Debug)]
pub struct PatternDefinition {
    pub name: String,
    pub enabled_by_default: bool,
    pub description: Option<String>,
    pub steps: Vec<PatternStep>,
}

#[derive(Debug)]
pub enum ConfigError {
    Yaml(serde_yaml::Error),
    MissingPackName,
    EmptyPatterns,
    DuplicatePatternName(String),
    UnknownBuiltin {
        pattern: String,
        builtin: String,
    },
    UnknownFunction(String),
    MissingSteps {
        pattern: String,
    },
    MissingCondition {
        pattern: String,
    },
    InvalidStepCondition {
        pattern: String,
    },
    Regex {
        pattern: String,
        error: fancy_regex::Error,
    },
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::Yaml(err) => write!(f, "{err}"),
            ConfigError::MissingPackName => write!(f, "missing pack name"),
            ConfigError::EmptyPatterns => write!(f, "no patterns defined"),
            ConfigError::DuplicatePatternName(name) => {
                write!(f, "duplicate pattern name: {name}")
            }
            ConfigError::UnknownBuiltin { pattern, builtin } => {
                write!(f, "unknown builtin '{builtin}' for pattern '{pattern}'")
            }
            ConfigError::UnknownFunction(func) => write!(f, "unknown function '{func}'"),
            ConfigError::MissingSteps { pattern } => {
                write!(f, "pattern '{pattern}' is missing steps")
            }
            ConfigError::MissingCondition { pattern } => {
                write!(f, "pattern '{pattern}' step is missing a condition")
            }
            ConfigError::InvalidStepCondition { pattern } => write!(
                f,
                "pattern '{pattern}' step must set exactly one of regex, equals, or function"
            ),
            ConfigError::Regex { pattern, error } => {
                write!(f, "invalid regex '{pattern}': {error}")
            }
        }
    }
}

impl std::error::Error for ConfigError {}

#[derive(Deserialize)]
struct PatternPackYaml {
    pack: Option<String>,
    #[serde(default)]
    patterns: Vec<PatternYaml>,
    #[serde(default)]
    builtins: HashMap<String, BuiltinYaml>,
}

#[derive(Deserialize)]
struct PatternYaml {
    name: String,
    builtin: Option<String>,
    #[serde(default = "default_enabled")]
    enabled_by_default: bool,
    description: Option<String>,
    steps: Option<Vec<StepYaml>>,
}

#[derive(Deserialize)]
struct BuiltinYaml {
    steps: Vec<StepYaml>,
    #[serde(default)]
    description: Option<String>,
}

#[derive(Clone, Deserialize)]
struct StepYaml {
    #[serde(default)]
    rewind: Option<usize>,
    regex: Option<String>,
    equals: Option<String>,
    function: Option<String>,
}

fn default_enabled() -> bool {
    true
}

pub fn load_pattern_pack_yaml(contents: &str) -> Result<PatternPack, ConfigError> {
    let parsed: PatternPackYaml = serde_yaml::from_str(contents).map_err(ConfigError::Yaml)?;
    let pack = parsed.pack.ok_or(ConfigError::MissingPackName)?;
    if parsed.patterns.is_empty() {
        return Err(ConfigError::EmptyPatterns);
    }

    let mut seen = BTreeSet::new();
    let mut definitions = Vec::with_capacity(parsed.patterns.len());

    let mut regex_cache: HashMap<String, Arc<FancyRegex>> = HashMap::new();
    let function_registry = crate::patterns::condition_registry();

    for pattern in parsed.patterns {
        if !seen.insert(pattern.name.clone()) {
            return Err(ConfigError::DuplicatePatternName(pattern.name));
        }

        let steps_source = if let Some(steps) = pattern.steps {
            steps
        } else if let Some(builtin) = pattern.builtin.as_ref() {
            parsed
                .builtins
                .get(builtin)
                .map(|b| b.steps.clone())
                .ok_or_else(|| ConfigError::UnknownBuiltin {
                    pattern: pattern.name.clone(),
                    builtin: builtin.clone(),
                })?
        } else {
            Vec::new()
        };

        if steps_source.is_empty() {
            return Err(ConfigError::MissingSteps {
                pattern: pattern.name,
            });
        }

        let steps = steps_source
            .into_iter()
            .map(|s| to_pattern_step(&pattern.name, s, &function_registry, &mut regex_cache))
            .collect::<Result<Vec<_>, _>>()?;

        definitions.push(PatternDefinition {
            name: pattern.name,
            enabled_by_default: pattern.enabled_by_default,
            description: pattern.description.or_else(|| {
                pattern
                    .builtin
                    .as_ref()
                    .and_then(|b| parsed.builtins.get(b).and_then(|b| b.description.clone()))
            }),
            steps,
        });
    }

    Ok(PatternPack {
        pack,
        patterns: definitions,
    })
}

fn to_pattern_step(
    pattern_name: &str,
    step: StepYaml,
    function_registry: &HashMap<&'static str, crate::ConditionFn>,
    regex_cache: &mut HashMap<String, Arc<FancyRegex>>,
) -> Result<PatternStep, ConfigError> {
    let mut chosen = 0usize;
    if step.regex.is_some() {
        chosen += 1;
    }
    if step.equals.is_some() {
        chosen += 1;
    }
    if step.function.is_some() {
        chosen += 1;
    }
    if chosen == 0 {
        return Err(ConfigError::MissingCondition {
            pattern: pattern_name.to_string(),
        });
    }
    if chosen > 1 {
        return Err(ConfigError::InvalidStepCondition {
            pattern: pattern_name.to_string(),
        });
    }

    let condition = if let Some(regex) = step.regex {
        let compiled = if let Some(existing) = regex_cache.get(&regex) {
            existing.clone()
        } else {
            let compiled = FancyRegex::new(&regex).map_err(|error| ConfigError::Regex {
                pattern: regex.clone(),
                error,
            })?;
            let arc = Arc::new(compiled);
            regex_cache.insert(regex.clone(), arc.clone());
            arc
        };
        crate::StepCondition::Regex(compiled)
    } else if let Some(eq) = step.equals {
        let eq = Arc::new(eq);
        crate::StepCondition::Fn(Arc::new(move |line, _| line.code == *eq))
    } else if let Some(func_name) = step.function {
        let func = function_registry
            .get(func_name.as_str())
            .ok_or_else(|| ConfigError::UnknownFunction(func_name.clone()))?
            .clone();
        crate::StepCondition::Fn(func)
    } else {
        return Err(ConfigError::MissingCondition {
            pattern: pattern_name.to_string(),
        });
    };

    Ok(PatternStep {
        rewind: step.rewind,
        condition,
    })
}

pub fn run_pack_on_lines(
    filename: &str,
    lines: &[Line],
    pack: &PatternPack,
) -> Vec<(String, Vec<MatchInstance>)> {
    let mut out = Vec::new();

    for pat in &pack.patterns {
        if !pat.enabled_by_default {
            continue;
        }
        let matches = find_pattern_instances(filename, lines, &pat.name, &pat.steps);
        if !matches.is_empty() {
            out.push((pat.name.clone(), matches));
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::preprocess_properties;

    #[test]
    fn loads_pack_from_yaml() {
        let yaml = r#"
pack: core
builtins:
  no_op_ld:
    steps:
      - regex: '^ld ([abcdehl]), \1$'
patterns:
  - name: "No-op ld"
    builtin: "no_op_ld"
"#;

        let pack = load_pattern_pack_yaml(yaml).unwrap();
        assert_eq!(pack.pack, "core");
        assert_eq!(pack.patterns.len(), 1);
        assert_eq!(pack.patterns[0].name, "No-op ld");
        assert!(pack.patterns[0].enabled_by_default);
    }

    #[test]
    fn runs_enabled_builtin_patterns() {
        let yaml = r#"
pack: core
builtins:
  no_op_ld:
    steps:
      - regex: '^ld ([abcdehl]), \1$'
patterns:
  - name: "No-op ld"
    builtin: "no_op_ld"
"#;
        let pack = load_pattern_pack_yaml(yaml).unwrap();

        let lines = preprocess_properties("Label:\n  ld b, b\n");
        let got = run_pack_on_lines("file.asm", &lines, &pack);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].0, "No-op ld");
        assert_eq!(got[0].1.len(), 1);
        assert_eq!(got[0].1[0].lines.len(), 1);
    }

    #[test]
    fn empty_pack_is_rejected() {
        let yaml = "pack: pret\n";
        assert!(matches!(
            load_pattern_pack_yaml(yaml),
            Err(ConfigError::EmptyPatterns)
        ));
    }
}
