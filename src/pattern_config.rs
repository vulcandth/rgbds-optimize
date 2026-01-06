#![forbid(unsafe_code)]

use crate::{FancyRegex, Line, MatchInstance, PatternStep, StepCondition};
use serde::Deserialize;
use std::collections::{BTreeSet, HashMap};
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct PatternPack {
    pub pack: String,
    pub patterns: Vec<PatternDefinition>,
}

#[derive(Clone, Debug)]
pub struct PatternDefinition {
    pub name: String,
    pub enabled_by_default: bool,
    pub description: Option<String>,
    pub steps: Vec<PatternStep>,
}

#[derive(Debug)]
pub enum ConfigError {
    Yaml(serde_yaml::Error),
    UnknownPack(String),
    EmptyPatterns,
    DuplicatePatternName(String),
    UnknownPatternId { pack: String, id: String },
    UnknownRegex(String),
    InvalidRegex { name: String, err: fancy_regex::Error },
    InvalidOperand(String),
    UnknownCondition(String),
    ConditionCycle(String),
    InvalidStringExpr(String),
    UnknownBuiltin(String),
    InvalidBuiltin(String),
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigError::Yaml(err) => write!(f, "{err}"),
            ConfigError::UnknownPack(name) => write!(f, "unknown pack '{name}'"),
            ConfigError::EmptyPatterns => write!(f, "no patterns defined"),
            ConfigError::DuplicatePatternName(name) => write!(f, "duplicate pattern name: {name}"),
            ConfigError::UnknownPatternId { pack, id } => {
                write!(f, "unknown pattern id '{id}' in pack '{pack}'")
            }
            ConfigError::UnknownRegex(name) => write!(f, "unknown regex '{name}'"),
            ConfigError::InvalidRegex { name, err } => write!(f, "invalid regex '{name}': {err}"),
            ConfigError::InvalidOperand(msg) => write!(f, "invalid operand condition: {msg}"),
            ConfigError::UnknownCondition(name) => write!(f, "unknown condition '{name}'"),
            ConfigError::ConditionCycle(name) => {
                write!(f, "condition '{name}' forms a cycle")
            }
            ConfigError::InvalidStringExpr(msg) => write!(f, "invalid string expr: {msg}"),
            ConfigError::UnknownBuiltin(name) => write!(f, "unknown builtin '{name}'"),
            ConfigError::InvalidBuiltin(msg) => write!(f, "invalid builtin: {msg}"),
        }
    }
}

impl std::error::Error for ConfigError {}

#[derive(Deserialize)]
struct RootYaml {
    #[serde(default)]
    regexes: HashMap<String, String>,

    #[serde(default)]
    conditions: HashMap<String, ConditionYaml>,

    #[serde(default)]
    patterns: HashMap<String, PatternYaml>,

    #[serde(default)]
    packs: HashMap<String, PackYaml>,
}

#[derive(Deserialize)]
struct PackYaml {
    #[serde(default)]
    patterns: Vec<PackPatternYaml>,
}

#[derive(Deserialize)]
struct PackPatternYaml {
    id: String,

    #[serde(default = "default_enabled")]
    enabled_by_default: bool,
}

fn default_enabled() -> bool {
    true
}

#[derive(Clone, Deserialize)]
struct PatternYaml {
    name: String,
    description: Option<String>,

    #[serde(default)]
    steps: Vec<StepYaml>,
}

#[derive(Clone, Deserialize)]
struct StepYaml {
    rewind: Option<usize>,
    when: ConditionYaml,
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
enum ConditionYaml {
    Regex { regex: String },
    TextRegex { text_regex: String },
    StrEq { str_eq: Box<StrEqYaml> },
    CodeEq { code_eq: String },
    CodeNe { code_ne: String },
    CodeStartsWith { code_starts_with: String },
    CodeEndsWith { code_ends_with: String },
    CodeContains { code_contains: String },
    Cond { cond: String },
    Any { any: Vec<ConditionYaml> },
    All { all: Vec<ConditionYaml> },
    Not { not: Box<ConditionYaml> },
    Instruction { instruction: InstructionYaml },
    IncDecSameTargetAsPrevLd { inc_dec_same_target_as_prev_ld: usize },
    Builtin { builtin: BuiltinYaml },
}

#[derive(Clone, Deserialize)]
struct StrEqYaml {
    left: StringExprYaml,
    right: StringExprYaml,
}

#[derive(Clone, Deserialize)]
struct PrevFieldYaml {
    idx: usize,
    field: String,
}

#[derive(Clone, Deserialize)]
struct StringExprYaml {
    #[serde(default)]
    current: Option<String>,

    #[serde(default)]
    prev: Option<PrevFieldYaml>,

    #[serde(default)]
    r#const: Option<String>,

    #[serde(default)]
    strip_prefix: Option<String>,

    #[serde(default)]
    after_prefix: Option<String>,

    #[serde(default)]
    before_comma: Option<bool>,

    #[serde(default)]
    after_comma: Option<bool>,

    #[serde(default)]
    after_comma_raw: Option<bool>,

    #[serde(default)]
    pair_reg: Option<bool>,

    #[serde(default)]
    byte_at: Option<usize>,

    #[serde(default)]
    last_char: Option<bool>,

    #[serde(default)]
    last_token: Option<bool>,

    #[serde(default)]
    strip_trailing_colon: Option<bool>,

    #[serde(default)]
    symbol_like: Option<bool>,

    #[serde(default)]
    trim: Option<bool>,
}

impl StringExprYaml {
    fn into_runtime(self) -> Result<crate::StringExpr, ConfigError> {
        let base = match (self.current, self.prev, self.r#const) {
            (Some(field), None, None) => crate::StringBase::Current(parse_string_field(&field)?),
            (None, Some(prev), None) => crate::StringBase::Prev {
                idx: prev.idx,
                field: parse_string_field(&prev.field)?,
            },
            (None, None, Some(value)) => crate::StringBase::Const(value),
            _ => {
                return Err(ConfigError::InvalidStringExpr(
                    "expected exactly one of current, prev, or const".to_string(),
                ));
            }
        };

        let mut transforms = Vec::new();

        if let Some(prefix) = self.strip_prefix {
            transforms.push(crate::StringTransform::StripPrefix(prefix));
        }
        if let Some(prefix) = self.after_prefix {
            transforms.push(crate::StringTransform::AfterPrefix(prefix));
        }
        if self.before_comma.unwrap_or(false) {
            transforms.push(crate::StringTransform::BeforeComma);
        }
        if self.after_comma.unwrap_or(false) {
            transforms.push(crate::StringTransform::AfterComma);
        }
        if self.after_comma_raw.unwrap_or(false) {
            transforms.push(crate::StringTransform::AfterCommaRaw);
        }
        if self.pair_reg.unwrap_or(false) {
            transforms.push(crate::StringTransform::PairReg);
        }
        if let Some(idx) = self.byte_at {
            transforms.push(crate::StringTransform::ByteAt(idx));
        }
        if self.last_char.unwrap_or(false) {
            transforms.push(crate::StringTransform::LastChar);
        }
        if self.last_token.unwrap_or(false) {
            transforms.push(crate::StringTransform::LastToken);
        }
        if self.strip_trailing_colon.unwrap_or(false) {
            transforms.push(crate::StringTransform::StripTrailingColon);
        }

        if self.symbol_like.unwrap_or(false) {
            transforms.push(crate::StringTransform::SymbolLike);
        }
        if self.trim.unwrap_or(false) {
            transforms.push(crate::StringTransform::Trim);
        }

        Ok(crate::StringExpr { base, transforms })
    }
}

fn parse_string_field(s: &str) -> Result<crate::StringField, ConfigError> {
    match s {
        "code" => Ok(crate::StringField::Code),
        "text" => Ok(crate::StringField::Text),
        "context" => Ok(crate::StringField::Context),
        "comment" => Ok(crate::StringField::Comment),
        "comment_lower" => Ok(crate::StringField::CommentLower),
        other => Err(ConfigError::InvalidStringExpr(format!(
            "unknown field '{other}'"
        ))),
    }
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
enum BuiltinYaml {
    Name(String),
    Call {
        name: String,
        #[serde(default)]
        jump_idx: Option<usize>,
    },
}

#[derive(Clone, Deserialize)]
struct InstructionYaml {
    mnemonic: Option<String>,

    #[serde(default)]
    operands: Vec<OperandYaml>,
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
enum OperandYaml {
    Eq { eq: String },
    CanonEq { canon_eq: String },
    IsZeroLiteral { is_zero_literal: bool },
    Any { any: Vec<OperandYaml> },
}

impl OperandYaml {
    fn into_runtime(self) -> Result<crate::OperandCondition, ConfigError> {
        Ok(match self {
            OperandYaml::Eq { eq } => crate::OperandCondition::Eq(eq),
            OperandYaml::CanonEq { canon_eq } => crate::OperandCondition::CanonEq(canon_eq),
            OperandYaml::IsZeroLiteral { is_zero_literal } => {
                if !is_zero_literal {
                    return Err(ConfigError::InvalidOperand(
                        "is_zero_literal must be true".to_string(),
                    ));
                }
                crate::OperandCondition::IsZeroLiteral
            }
            OperandYaml::Any { any } => {
                crate::OperandCondition::Any(
                    any.into_iter()
                        .map(OperandYaml::into_runtime)
                        .collect::<Result<Vec<_>, _>>()?,
                )
            }
        })
    }
}

struct ConditionCompiler<'a> {
    regexes: &'a HashMap<String, Arc<FancyRegex>>,
    yaml_conditions: &'a HashMap<String, ConditionYaml>,
    compiled_conditions: HashMap<String, StepCondition>,
    visiting: BTreeSet<String>,
}

impl<'a> ConditionCompiler<'a> {
    fn new(
        regexes: &'a HashMap<String, Arc<FancyRegex>>,
        yaml_conditions: &'a HashMap<String, ConditionYaml>,
    ) -> Self {
        Self {
            regexes,
            yaml_conditions,
            compiled_conditions: HashMap::new(),
            visiting: BTreeSet::new(),
        }
    }

    fn compile_named(&mut self, name: &str) -> Result<StepCondition, ConfigError> {
        if let Some(cached) = self.compiled_conditions.get(name) {
            return Ok(cached.clone());
        }
        if self.visiting.contains(name) {
            return Err(ConfigError::ConditionCycle(name.to_string()));
        }
        let Some(yaml) = self.yaml_conditions.get(name).cloned() else {
            return Err(ConfigError::UnknownCondition(name.to_string()));
        };

        self.visiting.insert(name.to_string());
        let compiled = self.compile_inline(yaml)?;
        self.visiting.remove(name);
        self.compiled_conditions
            .insert(name.to_string(), compiled.clone());
        Ok(compiled)
    }

    fn compile_inline(&mut self, yaml: ConditionYaml) -> Result<StepCondition, ConfigError> {
        Ok(match yaml {
            ConditionYaml::Regex { regex } => {
                let Some(re) = self.regexes.get(&regex).cloned() else {
                    return Err(ConfigError::UnknownRegex(regex));
                };
                StepCondition::Regex(re)
            }

            ConditionYaml::TextRegex { text_regex } => {
                let Some(re) = self.regexes.get(&text_regex).cloned() else {
                    return Err(ConfigError::UnknownRegex(text_regex));
                };
                StepCondition::TextRegex(re)
            }

            ConditionYaml::StrEq { str_eq } => {
                let StrEqYaml { left, right } = *str_eq;
                StepCondition::StrEq {
                    left: left.into_runtime()?,
                    right: right.into_runtime()?,
                }
            }

            ConditionYaml::CodeEq { code_eq } => StepCondition::CodeEq(code_eq),
            ConditionYaml::CodeNe { code_ne } => StepCondition::CodeNe(code_ne),
            ConditionYaml::CodeStartsWith { code_starts_with } => {
                StepCondition::CodeStartsWith(code_starts_with)
            }
            ConditionYaml::CodeEndsWith { code_ends_with } => {
                StepCondition::CodeEndsWith(code_ends_with)
            }
            ConditionYaml::CodeContains { code_contains } => StepCondition::CodeContains(code_contains),

            ConditionYaml::Cond { cond } => self.compile_named(&cond)?,

            ConditionYaml::Any { any } => StepCondition::Any(
                any.into_iter()
                    .map(|c| self.compile_inline(c))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            ConditionYaml::All { all } => StepCondition::All(
                all.into_iter()
                    .map(|c| self.compile_inline(c))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            ConditionYaml::Not { not } => StepCondition::Not(Box::new(self.compile_inline(*not)?)),

            ConditionYaml::Instruction { instruction } => {
                StepCondition::Instruction(crate::InstructionCondition {
                    mnemonic: instruction.mnemonic,
                    operands: instruction
                        .operands
                        .into_iter()
                        .map(OperandYaml::into_runtime)
                        .collect::<Result<Vec<_>, _>>()?,
                })
            }

            ConditionYaml::IncDecSameTargetAsPrevLd {
                inc_dec_same_target_as_prev_ld,
            } => StepCondition::IncDecSameTargetAsPrevLd {
                prev_step: inc_dec_same_target_as_prev_ld,
            },

            ConditionYaml::Builtin { builtin } => {
                let (name, jump_idx) = match builtin {
                    BuiltinYaml::Name(name) => (name, None),
                    BuiltinYaml::Call { name, jump_idx } => (name, jump_idx),
                };
                StepCondition::Builtin(crate::BuiltinCondition::from_yaml(&name, jump_idx)?)
            }
        })
    }
}

pub fn load_pattern_pack_yaml(contents: &str, pack_name: &str) -> Result<PatternPack, ConfigError> {
    let root: RootYaml = serde_yaml::from_str(contents).map_err(ConfigError::Yaml)?;

    let mut compiled_regexes: HashMap<String, Arc<FancyRegex>> = HashMap::new();
    for (name, re) in root.regexes {
        let compiled = FancyRegex::new(&re).map_err(|err| ConfigError::InvalidRegex { name: name.clone(), err })?;
        compiled_regexes.insert(name, Arc::new(compiled));
    }

    let mut compiler = ConditionCompiler::new(&compiled_regexes, &root.conditions);
    let condition_names: Vec<String> = root.conditions.keys().cloned().collect();
    for name in condition_names {
        compiler.compile_named(&name)?;
    }

    let Some(pack) = root.packs.get(pack_name) else {
        return Err(ConfigError::UnknownPack(pack_name.to_string()));
    };
    if pack.patterns.is_empty() {
        return Err(ConfigError::EmptyPatterns);
    }

    let mut seen_names: BTreeSet<String> = BTreeSet::new();
    let mut out_patterns: Vec<PatternDefinition> = Vec::with_capacity(pack.patterns.len());

    for pp in &pack.patterns {
        let Some(pat) = root.patterns.get(&pp.id) else {
            return Err(ConfigError::UnknownPatternId {
                pack: pack_name.to_string(),
                id: pp.id.clone(),
            });
        };

        if !seen_names.insert(pat.name.clone()) {
            return Err(ConfigError::DuplicatePatternName(pat.name.clone()));
        }

        let mut steps: Vec<PatternStep> = Vec::with_capacity(pat.steps.len());
        for step in &pat.steps {
            let cond = compiler.compile_inline(step.when.clone())?;
            steps.push(PatternStep {
                rewind: step.rewind,
                condition: cond,
            });
        }

        out_patterns.push(PatternDefinition {
            name: pat.name.clone(),
            enabled_by_default: pp.enabled_by_default,
            description: pat.description.clone(),
            steps,
        });
    }

    Ok(PatternPack {
        pack: pack_name.to_string(),
        patterns: out_patterns,
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

        let matches = crate::find_pattern_instances(filename, lines, &pat.name, &pat.steps);
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
    fn loads_pack_from_yaml_and_compiles_regexes() {
        let yaml = r#"
regexes:
    NO_OP_LD: '^ld ([abcdehl]), \1$'

conditions:
    not_halt:
        not:
            code_eq: halt

patterns:
  no_op_ld:
    name: No-op ld
    steps:
            - when:
                    all:
                        - { cond: not_halt }
                        - { regex: NO_OP_LD }

packs:
  core:
    patterns:
      - id: no_op_ld
"#;

        let pack = load_pattern_pack_yaml(yaml, "core").unwrap();
        assert_eq!(pack.pack, "core");
        assert_eq!(pack.patterns.len(), 1);
        assert_eq!(pack.patterns[0].name, "No-op ld");

        let lines = preprocess_properties("Label:\n  ld b, b\n");
        let got = run_pack_on_lines("file.asm", &lines, &pack);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].0, "No-op ld");
        assert_eq!(got[0].1.len(), 1);
        assert_eq!(got[0].1[0].lines.len(), 1);
    }
}
