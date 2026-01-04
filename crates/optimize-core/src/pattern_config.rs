#![forbid(unsafe_code)]

use crate::{
    canonicalize_rgbds_operand, find_pattern_instances, is_rgbds_zero_literal, parse_instruction,
    Line, MatchInstance, PatternStep,
};
use serde::Deserialize;
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PatternPack {
    pub pack: String,
    pub patterns: Vec<PatternDefinition>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PatternDefinition {
    pub name: String,
    pub builtin: String,
    pub enabled_by_default: bool,
    pub description: Option<String>,
}

#[derive(Debug)]
pub enum ConfigError {
    Toml(toml::de::Error),
    MissingPackName,
    EmptyPatterns,
    DuplicatePatternName(String),
    UnknownBuiltin { pattern: String, builtin: String },
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::Toml(err) => write!(f, "{err}"),
            ConfigError::MissingPackName => write!(f, "missing pack name"),
            ConfigError::EmptyPatterns => write!(f, "no patterns defined"),
            ConfigError::DuplicatePatternName(name) => {
                write!(f, "duplicate pattern name: {name}")
            }
            ConfigError::UnknownBuiltin { pattern, builtin } => {
                write!(f, "unknown builtin '{builtin}' for pattern '{pattern}'")
            }
        }
    }
}

impl std::error::Error for ConfigError {}

#[derive(Deserialize)]
struct PatternPackToml {
    pack: Option<String>,

    #[serde(default)]
    pattern: Vec<PatternToml>,
}

#[derive(Deserialize)]
struct PatternToml {
    name: String,
    builtin: String,

    #[serde(default = "default_enabled")]
    enabled_by_default: bool,

    description: Option<String>,
}

fn default_enabled() -> bool {
    true
}

pub fn load_pattern_pack_toml(contents: &str) -> Result<PatternPack, ConfigError> {
    let parsed: PatternPackToml = toml::from_str(contents).map_err(ConfigError::Toml)?;
    let pack = parsed.pack.ok_or(ConfigError::MissingPackName)?;
    if parsed.pattern.is_empty() {
        return Err(ConfigError::EmptyPatterns);
    }

    let mut seen = std::collections::BTreeSet::new();
    let mut patterns = Vec::with_capacity(parsed.pattern.len());
    for p in parsed.pattern {
        if !seen.insert(p.name.clone()) {
            return Err(ConfigError::DuplicatePatternName(p.name));
        }
        patterns.push(PatternDefinition {
            name: p.name,
            builtin: p.builtin,
            enabled_by_default: p.enabled_by_default,
            description: p.description,
        });
    }

    // Ensure enabled patterns are runnable (disabled patterns may be placeholders).
    for p in &patterns {
        if !p.enabled_by_default {
            continue;
        }
        if builtin_pattern_steps(&p.builtin).is_none() {
            return Err(ConfigError::UnknownBuiltin {
                pattern: p.name.clone(),
                builtin: p.builtin.clone(),
            });
        }
    }

    Ok(PatternPack { pack, patterns })
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
        if let Some(steps) = builtin_pattern_steps(&pat.builtin) {
            let matches = find_pattern_instances(filename, lines, &pat.name, steps);
            if !matches.is_empty() {
                out.push((pat.name.clone(), matches));
            }
        }
    }

    out
}

fn builtin_pattern_steps(builtin: &str) -> Option<&'static [PatternStep]> {
    static CACHE: LazyLock<Mutex<HashMap<String, &'static [PatternStep]>>> =
        LazyLock::new(|| Mutex::new(HashMap::new()));

    if let Ok(cache) = CACHE.lock() {
        if let Some(hit) = cache.get(builtin).copied() {
            return Some(hit);
        }
    }

    let steps: Vec<PatternStep> = match builtin {
        // Example: ld b, b (or other identical registers)
        "no_op_ld" => Some(vec![PatternStep::new(|line, _prev| {
            let Some(ins) = parse_instruction(&line.code) else {
                return false;
            };
            if ins.mnemonic != "ld" || ins.operands.len() != 2 {
                return false;
            }
            let left = ins.operands[0].to_ascii_lowercase();
            let right = ins.operands[1].to_ascii_lowercase();
            matches!(left.as_str(), "a" | "b" | "c" | "d" | "e" | "h" | "l") && left == right
        })]),

        // Example: add|sub 0 (including "add a, 0" forms in some code styles)
        "no_op_add_sub" => Some(vec![PatternStep::new(|line, _prev| {
            let Some(ins) = parse_instruction(&line.code) else {
                return false;
            };
            if ins.mnemonic != "add" && ins.mnemonic != "sub" {
                return false;
            }

            let imm = match ins.operands.as_slice() {
                [op] => op,
                [first, second] if first.eq_ignore_ascii_case("a") => second,
                _ => return false,
            };

            let imm = imm.trim();
            is_rgbds_zero_literal(imm)
        })]),

        // Example: [hl+] and [hli] should be treated equivalently in the future.
        // This is a tiny demonstration hook: match "ld a, [hl+]" or "ld a, [hli]".
        "ld_a_from_hli" => Some(vec![PatternStep::new(|line, _prev| {
            let Some(ins) = parse_instruction(&line.code) else {
                return false;
            };
            if ins.mnemonic != "ld" || ins.operands.len() != 2 {
                return false;
            }
            let dst = ins.operands[0].to_ascii_lowercase();
            let src = canonicalize_rgbds_operand(&ins.operands[1]);
            dst == "a" && src == "[hli]"
        })]),

        _ => None,
    }
    .or_else(|| crate::patterns::steps(builtin))?;

    let leaked: &'static [PatternStep] = Box::leak(steps.into_boxed_slice());
    if let Ok(mut cache) = CACHE.lock() {
        cache.insert(builtin.to_string(), leaked);
    }
    Some(leaked)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::preprocess_properties;

    #[test]
    fn loads_pack_from_toml() {
        let toml = r#"
pack = "core"

[[pattern]]
name = "No-op ld"
builtin = "no_op_ld"
"#;

        let pack = load_pattern_pack_toml(toml).unwrap();
        assert_eq!(pack.pack, "core");
        assert_eq!(pack.patterns.len(), 1);
        assert_eq!(pack.patterns[0].name, "No-op ld");
    }

    #[test]
    fn runs_enabled_builtin_patterns() {
        let toml = r#"
pack = "core"

[[pattern]]
name = "No-op ld"
builtin = "no_op_ld"
"#;
        let pack = load_pattern_pack_toml(toml).unwrap();

        let lines = preprocess_properties("Label:\n  ld b, b\n");
        let got = run_pack_on_lines("file.asm", &lines, &pack);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].0, "No-op ld");
        assert_eq!(got[0].1.len(), 1);
        assert_eq!(got[0].1[0].lines.len(), 1);
    }

    #[test]
    fn pret_pack_can_be_empty_placeholder_in_repo_configs() {
        // We intentionally allow projects to omit pret packs entirely.
        // This test exists to ensure our loader enforces non-empty patterns.
        let toml = "pack = \"pret\"\n";
        assert!(matches!(
            load_pattern_pack_toml(toml),
            Err(ConfigError::EmptyPatterns)
        ));
    }
}
