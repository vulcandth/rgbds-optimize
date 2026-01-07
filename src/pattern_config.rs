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
    UnknownPatternId {
        pack: String,
        id: String,
    },
    UnknownRegex(String),
    InvalidRegex {
        name: String,
        err: fancy_regex::Error,
    },
    InvalidOperand(String),
    InvalidInstruction(String),
    UnknownCondition(String),
    ConditionCycle(String),
    InvalidStringExpr(String),
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
            ConfigError::InvalidInstruction(msg) => {
                write!(f, "invalid instruction condition: {msg}")
            }
            ConfigError::UnknownCondition(name) => write!(f, "unknown condition '{name}'"),
            ConfigError::ConditionCycle(name) => {
                write!(f, "condition '{name}' forms a cycle")
            }
            ConfigError::InvalidStringExpr(msg) => write!(f, "invalid string expr: {msg}"),
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
#[serde(untagged)]
enum PackPatternYaml {
    Id(String),
    Full {
        id: String,

        #[serde(default = "default_enabled")]
        enabled_by_default: bool,
    },
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
#[serde(untagged)]
enum StepYaml {
    Full {
        rewind: Option<usize>,
        when: ConditionYaml,
    },
    When(ConditionYaml),
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
enum ConditionYaml {
    /// Shorthand for `{ cond: NAME }`.
    CondName(String),
    Regex {
        regex: String,
    },
    RegexIn {
        regex_in: Vec<String>,
    },
    TextRegex {
        text_regex: String,
    },
    StrEq {
        str_eq: Box<StrEqYaml>,
    },
    StrEqIn {
        str_eq_in: Box<StrEqInYaml>,
    },
    CodeEq {
        code_eq: String,
    },
    CodeIn {
        code_in: Vec<String>,
    },
    CodeNe {
        code_ne: String,
    },
    CodeStartsWith {
        code_starts_with: String,
    },
    CodeStartsWithAny {
        code_starts_with_any: Vec<String>,
    },
    CodeEndsWith {
        code_ends_with: String,
    },
    CodeEndsWithAny {
        code_ends_with_any: Vec<String>,
    },
    CodeContains {
        code_contains: String,
    },
    CodeContainsAny {
        code_contains_any: Vec<String>,
    },
    Cond {
        cond: String,
    },
    Any {
        any: Vec<ConditionYaml>,
    },
    All {
        all: Vec<ConditionYaml>,
    },
    Not {
        not: Box<ConditionYaml>,
    },

    /// Shorthand for:
    /// `instruction: { mnemonic: <string>, operands: [...] }`
    InstructionWithOperands {
        instruction: String,

        #[serde(default)]
        operands: Vec<OperandYaml>,
    },

    /// Shorthand for:
    /// `instruction_in: { mnemonics: [..], operands: [...] }`
    InstructionInWithOperands {
        instruction_in: Vec<String>,

        #[serde(default)]
        operands: Vec<OperandYaml>,
    },

    Instruction {
        instruction: InstructionYaml,
    },
    InstructionIn {
        instruction_in: InstructionInYaml,
    },
    IncDecSameTargetAsPrevLd {
        inc_dec_same_target_as_prev_ld: usize,
    },
}

#[derive(Clone, Deserialize)]
struct StrEqYaml {
    left: StringExprYaml,
    right: StringExprYaml,
}

#[derive(Clone, Deserialize)]
struct StrEqInYaml {
    left: StringExprYaml,
    rights: Vec<String>,
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

    #[serde(default)]
    lower: Option<bool>,
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

        if self.lower.unwrap_or(false) {
            transforms.push(crate::StringTransform::Lower);
        }

        Ok(crate::StringExpr { base, transforms })
    }
}

fn parse_string_field(s: &str) -> Result<crate::StringField, ConfigError> {
    let s = s.trim();

    if let Some((head, tail)) = s.split_once(',') {
        let head = head.trim();
        let tail = tail.trim();

        if head.eq_ignore_ascii_case("instruction") {
            let selector = tail.to_ascii_lowercase();

            if matches!(selector.as_str(), "mnemonic" | "op") {
                return Ok(crate::StringField::InstructionMnemonic);
            }

            if selector == "cc" {
                return Ok(crate::StringField::InstructionCc);
            }

            if matches!(selector.as_str(), "root" | "target") {
                return Ok(crate::StringField::InstructionRoot);
            }

            if let Some(num) = selector.strip_prefix("operand") {
                let n: usize = num.parse().map_err(|_| {
                    ConfigError::InvalidStringExpr(format!(
                        "invalid instruction field '{s}': operand must be a number"
                    ))
                })?;
                if n == 0 {
                    return Err(ConfigError::InvalidStringExpr(format!(
                        "invalid instruction field '{s}': operand is 1-based (operand1, operand2, ...)"
                    )));
                }

                return Ok(crate::StringField::InstructionOperand { idx: n - 1 });
            }

            return Err(ConfigError::InvalidStringExpr(format!(
                "unknown instruction field '{s}'"
            )));
        }
    }

    match s {
        "code" => Ok(crate::StringField::Code),
        "text" => Ok(crate::StringField::Text),
        "context" => Ok(crate::StringField::Context),
        "label" => Ok(crate::StringField::Label),
        "comment" => Ok(crate::StringField::Comment),
        "comment_lower" => Ok(crate::StringField::CommentLower),
        other => Err(ConfigError::InvalidStringExpr(format!(
            "unknown field '{other}'"
        ))),
    }
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
enum InstructionYaml {
    /// Shorthand for `instruction: { mnemonic: <string> }`.
    Mnemonic(String),
    Full(InstructionFullYaml),
}

#[derive(Clone, Deserialize)]
struct InstructionFullYaml {
    #[serde(default, alias = "op")]
    mnemonic: Option<String>,

    #[serde(default)]
    operands: Vec<OperandYaml>,
    #[serde(default, alias = "is_cc")]
    has_cc: Option<bool>,
    #[serde(default)]
    cc_eq: Option<String>,
    #[serde(default)]
    cc_ne: Option<String>,
    #[serde(default)]
    cc_in: Option<Vec<String>>,
}

#[derive(Clone, Default)]
struct InstructionMeta {
    has_cc: Option<bool>,
    cc_eq: Option<String>,
    cc_ne: Option<String>,
    cc_in: Option<Vec<String>>,
}

impl InstructionYaml {
    fn into_parts(self) -> (Option<String>, Vec<OperandYaml>, InstructionMeta) {
        match self {
            InstructionYaml::Mnemonic(mnemonic) => (Some(mnemonic), Vec::new(), InstructionMeta::default()),
            InstructionYaml::Full(full) => (
                full.mnemonic,
                full.operands,
                InstructionMeta {
                    has_cc: full.has_cc,
                    cc_eq: full.cc_eq,
                    cc_ne: full.cc_ne,
                    cc_in: full.cc_in,
                },
            ),
        }
    }
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
enum InstructionInYaml {
    /// Shorthand for `instruction_in: { mnemonics: [..] }`.
    Mnemonics(Vec<String>),
    Full(InstructionInFullYaml),
}

#[derive(Clone, Deserialize)]
struct InstructionInFullYaml {
    #[serde(alias = "in")]
    mnemonics: Vec<String>,

    #[serde(default)]
    operands: Vec<OperandYaml>,
    #[serde(default, alias = "is_cc")]
    has_cc: Option<bool>,
    #[serde(default)]
    cc_eq: Option<String>,
    #[serde(default)]
    cc_ne: Option<String>,
    #[serde(default)]
    cc_in: Option<Vec<String>>,
}

impl InstructionInYaml {
    fn into_parts(self) -> (Vec<String>, Vec<OperandYaml>, InstructionMeta) {
        match self {
            InstructionInYaml::Mnemonics(mnemonics) => (mnemonics, Vec::new(), InstructionMeta::default()),
            InstructionInYaml::Full(full) => (
                full.mnemonics,
                full.operands,
                InstructionMeta {
                    has_cc: full.has_cc,
                    cc_eq: full.cc_eq,
                    cc_ne: full.cc_ne,
                    cc_in: full.cc_in,
                },
            ),
        }
    }
}

#[derive(Clone, Deserialize, Default)]
#[serde(deny_unknown_fields)]
struct OperandYaml {
    #[serde(default)]
    eq: Option<String>,

    #[serde(default)]
    canon_eq: Option<String>,

    #[serde(default)]
    is_zero_literal: Option<bool>,

    #[serde(default)]
    lower: Option<bool>,

    #[serde(default)]
    is_one_literal: Option<bool>,

    #[serde(default)]
    is_reg8: Option<bool>,

    #[serde(default)]
    is_reg16: Option<bool>,

    #[serde(default)]
    is_reg16_stack: Option<bool>,

    #[serde(default)]
    is_cc: Option<bool>,

    #[serde(default)]
    is_mem: Option<bool>,

    #[serde(default)]
    is_mem_hl: Option<bool>,

    #[serde(default)]
    is_mem_hli: Option<bool>,

    #[serde(default)]
    is_mem_hld: Option<bool>,

    #[serde(default)]
    is_mem_r16: Option<bool>,

    #[serde(default)]
    is_mem_c: Option<bool>,

    #[serde(default)]
    is_imm: Option<bool>,

    #[serde(default)]
    is_number_literal: Option<bool>,

    #[serde(default)]
    number_eq: Option<NumberLiteralYaml>,

    #[serde(default)]
    number_ne: Option<NumberLiteralYaml>,

    #[serde(default)]
    number_lt: Option<NumberLiteralYaml>,

    #[serde(default)]
    number_le: Option<NumberLiteralYaml>,

    #[serde(default)]
    number_gt: Option<NumberLiteralYaml>,

    #[serde(default)]
    number_ge: Option<NumberLiteralYaml>,

    #[serde(default)]
    is_u3_literal: Option<bool>,

    #[serde(default)]
    is_rst_vec_literal: Option<bool>,

    #[serde(default)]
    any_operand: Option<bool>,

    #[serde(default)]
    any: Option<Vec<OperandYaml>>,
}

#[derive(Clone, Deserialize)]
#[serde(untagged)]
enum NumberLiteralYaml {
    Int(i64),
    Str(String),
}

impl NumberLiteralYaml {
    fn into_i64(self, key: &str) -> Result<i64, ConfigError> {
        match self {
            NumberLiteralYaml::Int(value) => Ok(value),
            NumberLiteralYaml::Str(raw) => crate::parse_rgbds_int(&raw).ok_or_else(|| {
                ConfigError::InvalidOperand(format!(
                    "{key} expects a number literal (e.g. 10, $ff, %1010), got '{raw}'"
                ))
            }),
        }
    }
}

impl OperandYaml {
    fn into_runtime(self) -> Result<crate::OperandCondition, ConfigError> {
        let OperandYaml {
            eq,
            canon_eq,
            is_zero_literal,
            lower: _lower,
            is_one_literal,
            is_reg8,
            is_reg16,
            is_reg16_stack,
            is_cc,
            is_mem,
            is_mem_hl,
            is_mem_hli,
            is_mem_hld,
            is_mem_r16,
            is_mem_c,
            is_imm,
            is_number_literal,
            number_eq,
            number_ne,
            number_lt,
            number_le,
            number_gt,
            number_ge,
            is_u3_literal,
            is_rst_vec_literal,
            any_operand,
            any,
        } = self;

        if let Some(options) = any {
            if eq.is_some()
                || canon_eq.is_some()
                || is_zero_literal.is_some()
                || is_one_literal.is_some()
                || is_reg8.is_some()
                || is_reg16.is_some()
                || is_reg16_stack.is_some()
                || is_cc.is_some()
                || is_mem.is_some()
                || is_mem_hl.is_some()
                || is_mem_hli.is_some()
                || is_mem_hld.is_some()
                || is_mem_r16.is_some()
                || is_mem_c.is_some()
                || is_imm.is_some()
                || is_number_literal.is_some()
                || number_eq.is_some()
                || number_ne.is_some()
                || number_lt.is_some()
                || number_le.is_some()
                || number_gt.is_some()
                || number_ge.is_some()
                || is_u3_literal.is_some()
                || is_rst_vec_literal.is_some()
                || any_operand.is_some()
            {
                return Err(ConfigError::InvalidOperand(
                    "operand: `any:` cannot be combined with other keys".to_string(),
                ));
            }

            return Ok(crate::OperandCondition::Any(
                options
                    .into_iter()
                    .map(OperandYaml::into_runtime)
                    .collect::<Result<Vec<_>, _>>()?,
            ));
        }

        if let Some(false) = any_operand {
            return Err(ConfigError::InvalidOperand(
                "any_operand must be true or omitted".to_string(),
            ));
        }

        let mut conds: Vec<crate::OperandCondition> = Vec::new();

        if let Some(eq) = eq {
            conds.push(crate::OperandCondition::Eq(eq));
        }
        if let Some(canon_eq) = canon_eq {
            conds.push(crate::OperandCondition::CanonEq(canon_eq));
        }

        fn push_bool(
            conds: &mut Vec<crate::OperandCondition>,
            value: Option<bool>,
            cond: crate::OperandCondition,
        ) {
            match value {
                None => {}
                Some(true) => conds.push(cond),
                Some(false) => conds.push(crate::OperandCondition::Not(Box::new(cond))),
            }
        }

        push_bool(
            &mut conds,
            is_zero_literal,
            crate::OperandCondition::IsZeroLiteral,
        );
        push_bool(
            &mut conds,
            is_one_literal,
            crate::OperandCondition::NumberLiteralEq(1),
        );
        push_bool(&mut conds, is_reg8, crate::OperandCondition::IsReg8);
        push_bool(&mut conds, is_reg16, crate::OperandCondition::IsReg16);
        push_bool(
            &mut conds,
            is_reg16_stack,
            crate::OperandCondition::IsReg16Stack,
        );
        push_bool(&mut conds, is_cc, crate::OperandCondition::IsCc);
        push_bool(&mut conds, is_mem, crate::OperandCondition::IsMem);
        push_bool(&mut conds, is_mem_hl, crate::OperandCondition::IsMemHl);
        push_bool(&mut conds, is_mem_hli, crate::OperandCondition::IsMemHli);
        push_bool(&mut conds, is_mem_hld, crate::OperandCondition::IsMemHld);
        push_bool(&mut conds, is_mem_r16, crate::OperandCondition::IsMemR16);
        push_bool(&mut conds, is_mem_c, crate::OperandCondition::IsMemC);
        push_bool(&mut conds, is_imm, crate::OperandCondition::IsImm);
        push_bool(
            &mut conds,
            is_number_literal,
            crate::OperandCondition::IsNumberLiteral,
        );

        if let Some(value) = number_eq {
            conds.push(crate::OperandCondition::NumberLiteralEq(
                value.into_i64("number_eq")?,
            ));
        }
        if let Some(value) = number_ne {
            conds.push(crate::OperandCondition::NumberLiteralNe(
                value.into_i64("number_ne")?,
            ));
        }
        if let Some(value) = number_lt {
            conds.push(crate::OperandCondition::NumberLiteralLt(
                value.into_i64("number_lt")?,
            ));
        }
        if let Some(value) = number_le {
            conds.push(crate::OperandCondition::NumberLiteralLe(
                value.into_i64("number_le")?,
            ));
        }
        if let Some(value) = number_gt {
            conds.push(crate::OperandCondition::NumberLiteralGt(
                value.into_i64("number_gt")?,
            ));
        }
        if let Some(value) = number_ge {
            conds.push(crate::OperandCondition::NumberLiteralGe(
                value.into_i64("number_ge")?,
            ));
        }

        push_bool(
            &mut conds,
            is_u3_literal,
            crate::OperandCondition::IsU3Literal,
        );
        push_bool(
            &mut conds,
            is_rst_vec_literal,
            crate::OperandCondition::IsRstVecLiteral,
        );

        if let Some(true) = any_operand {
            if !conds.is_empty() {
                return Err(ConfigError::InvalidOperand(
                    "any_operand cannot be combined with other keys".to_string(),
                ));
            }
            return Ok(crate::OperandCondition::AnyOperand);
        }

        match conds.len() {
            0 => Err(ConfigError::InvalidOperand(
                "operand map must not be empty".to_string(),
            )),
            1 => Ok(conds.pop().unwrap()),
            _ => Ok(crate::OperandCondition::All(conds)),
        }
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
            ConditionYaml::CondName(cond) => self.compile_named(&cond)?,

            ConditionYaml::Regex { regex } => {
                let Some(re) = self.regexes.get(&regex).cloned() else {
                    return Err(ConfigError::UnknownRegex(regex));
                };
                StepCondition::Regex(re)
            }

            ConditionYaml::RegexIn { regex_in } => {
                let mut out = Vec::with_capacity(regex_in.len());
                for name in regex_in {
                    let Some(re) = self.regexes.get(&name).cloned() else {
                        return Err(ConfigError::UnknownRegex(name));
                    };
                    out.push(StepCondition::Regex(re));
                }
                StepCondition::Any(out)
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

            ConditionYaml::StrEqIn { str_eq_in } => {
                let StrEqInYaml { left, rights } = *str_eq_in;
                let left = left.into_runtime()?;
                StepCondition::Any(
                    rights
                        .into_iter()
                        .map(|right| StepCondition::StrEq {
                            left: left.clone(),
                            right: crate::StringExpr {
                                base: crate::StringBase::Const(right),
                                transforms: Vec::new(),
                            },
                        })
                        .collect::<Vec<_>>(),
                )
            }

            ConditionYaml::CodeEq { code_eq } => StepCondition::CodeEq(code_eq),

            ConditionYaml::CodeIn { code_in } => StepCondition::Any(
                code_in
                    .into_iter()
                    .map(StepCondition::CodeEq)
                    .collect::<Vec<_>>(),
            ),

            ConditionYaml::CodeNe { code_ne } => StepCondition::CodeNe(code_ne),
            ConditionYaml::CodeStartsWith { code_starts_with } => {
                StepCondition::CodeStartsWith(code_starts_with)
            }

            ConditionYaml::CodeStartsWithAny {
                code_starts_with_any,
            } => StepCondition::Any(
                code_starts_with_any
                    .into_iter()
                    .map(StepCondition::CodeStartsWith)
                    .collect::<Vec<_>>(),
            ),

            ConditionYaml::CodeEndsWith { code_ends_with } => {
                StepCondition::CodeEndsWith(code_ends_with)
            }

            ConditionYaml::CodeEndsWithAny { code_ends_with_any } => StepCondition::Any(
                code_ends_with_any
                    .into_iter()
                    .map(StepCondition::CodeEndsWith)
                    .collect::<Vec<_>>(),
            ),

            ConditionYaml::CodeContains { code_contains } => {
                StepCondition::CodeContains(code_contains)
            }

            ConditionYaml::CodeContainsAny { code_contains_any } => StepCondition::Any(
                code_contains_any
                    .into_iter()
                    .map(StepCondition::CodeContains)
                    .collect::<Vec<_>>(),
            ),

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

            ConditionYaml::InstructionWithOperands {
                instruction,
                operands,
            } => StepCondition::Instruction(crate::InstructionCondition {
                mnemonic: Some(instruction),
                operands: operands
                    .into_iter()
                    .map(OperandYaml::into_runtime)
                    .collect::<Result<Vec<_>, _>>()?,
                has_cc: None,
                cc_eq: None,
                cc_ne: None,
                cc_in: None,
            }),

            ConditionYaml::InstructionInWithOperands {
                instruction_in,
                operands,
            } => {
                if instruction_in.is_empty() {
                    return Err(ConfigError::InvalidInstruction(
                        "instruction_in must include at least one mnemonic".to_string(),
                    ));
                }

                let operands = operands
                    .into_iter()
                    .map(OperandYaml::into_runtime)
                    .collect::<Result<Vec<_>, _>>()?;

                StepCondition::Any(
                    instruction_in
                        .into_iter()
                        .map(|mnemonic| {
                            StepCondition::Instruction(crate::InstructionCondition {
                                mnemonic: Some(mnemonic),
                                operands: operands.clone(),
                                has_cc: None,
                                cc_eq: None,
                                cc_ne: None,
                                cc_in: None,
                            })
                        })
                        .collect::<Vec<_>>(),
                )
            }

            ConditionYaml::Instruction { instruction } => {
                let (mnemonic, operands, meta) = instruction.into_parts();
                let icond = crate::InstructionCondition {
                    mnemonic,
                    operands: operands
                        .into_iter()
                        .map(OperandYaml::into_runtime)
                        .collect::<Result<Vec<_>, _>>()?,
                    has_cc: meta.has_cc,
                    cc_eq: meta.cc_eq,
                    cc_ne: meta.cc_ne,
                    cc_in: meta.cc_in,
                };
                StepCondition::Instruction(icond)
            }

            ConditionYaml::InstructionIn { instruction_in } => {
                let (mnemonics, operands, meta) = instruction_in.into_parts();
                if mnemonics.is_empty() {
                    return Err(ConfigError::InvalidInstruction(
                        "instruction_in must include at least one mnemonic".to_string(),
                    ));
                }

                let operands = operands
                    .into_iter()
                    .map(OperandYaml::into_runtime)
                    .collect::<Result<Vec<_>, _>>()?;

                StepCondition::Any(
                    mnemonics
                        .into_iter()
                        .map(|mnemonic| {
                            StepCondition::Instruction(crate::InstructionCondition {
                                mnemonic: Some(mnemonic),
                                operands: operands.clone(),
                                has_cc: meta.has_cc.clone(),
                                cc_eq: meta.cc_eq.clone(),
                                cc_ne: meta.cc_ne.clone(),
                                cc_in: meta.cc_in.clone(),
                            })
                        })
                        .collect::<Vec<_>>(),
                )
            }

            ConditionYaml::IncDecSameTargetAsPrevLd {
                inc_dec_same_target_as_prev_ld,
            } => StepCondition::IncDecSameTargetAsPrevLd {
                prev_step: inc_dec_same_target_as_prev_ld,
            },
        })
    }
}

pub fn load_pattern_pack_yaml(contents: &str, pack_name: &str) -> Result<PatternPack, ConfigError> {
    let root: RootYaml = serde_yaml::from_str(contents).map_err(ConfigError::Yaml)?;

    let mut compiled_regexes: HashMap<String, Arc<FancyRegex>> = HashMap::new();
    for (name, re) in root.regexes {
        let compiled = FancyRegex::new(&re).map_err(|err| ConfigError::InvalidRegex {
            name: name.clone(),
            err,
        })?;
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
        let (id, enabled_by_default) = match pp {
            PackPatternYaml::Id(id) => (id.as_str(), true),
            PackPatternYaml::Full {
                id,
                enabled_by_default,
            } => (id.as_str(), *enabled_by_default),
        };

        let Some(pat) = root.patterns.get(id) else {
            return Err(ConfigError::UnknownPatternId {
                pack: pack_name.to_string(),
                id: id.to_string(),
            });
        };

        if !seen_names.insert(pat.name.clone()) {
            return Err(ConfigError::DuplicatePatternName(pat.name.clone()));
        }

        let mut steps: Vec<PatternStep> = Vec::with_capacity(pat.steps.len());
        for step in &pat.steps {
            let (rewind, when) = match step {
                StepYaml::Full { rewind, when } => (*rewind, when.clone()),
                StepYaml::When(when) => (None, when.clone()),
            };

            let cond = compiler.compile_inline(when)?;
            steps.push(PatternStep {
                rewind,
                condition: cond,
            });
        }

        out_patterns.push(PatternDefinition {
            name: pat.name.clone(),
            enabled_by_default,
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
    CALL_OR_RET: '^(?:call|ret)'

conditions:
    op_is_one_of:
        code_in: ['xor a', 'xor a, a']
    starts_with_any:
        code_starts_with_any: ['ld ', 'ldh ']
    ends_with_any:
        code_ends_with_any: [', a', ', hl']
    contains_any:
        code_contains_any: ['[hl]', '[hli]']
    regex_one_of:
        regex_in: [NO_OP_LD, CALL_OR_RET]
    str_eq_one_of:
        str_eq_in:
            left: { current: code, byte_at: 0 }
            rights: ['l', 'c']
    not_halt:
        not:
            code_eq: halt

patterns:
    no_op_ld:
        name: No-op ld
        steps:
            - all:
                    - not_halt
                    - { regex: NO_OP_LD }
                    - regex_one_of
                    - str_eq_one_of

    add_any:
        name: Add any
        steps:
            - instruction: add

    add_or_adc_any:
        name: Add/adc any
        steps:
            - instruction_in: [add, adc]

    add_or_adc_with_sibling_operands:
        name: Add/adc sibling operands
        steps:
            - instruction_in: [add, adc]
              operands:
                  - { eq: a }
                  - { any_operand: true }

    add_to_a_redundant_arg:
        name: Add/adc redundant arg
        steps:
            - instruction_in:
                    in: [add, adc]
                    operands:
                        - { eq: a }
                        - { any_operand: true }

packs:
    core:
        patterns:
            - no_op_ld
            - add_any
            - add_or_adc_any
            - add_or_adc_with_sibling_operands
            - add_to_a_redundant_arg
"#;

        let pack = load_pattern_pack_yaml(yaml, "core").unwrap();
        assert_eq!(pack.pack, "core");
        assert_eq!(pack.patterns.len(), 5);
        assert_eq!(pack.patterns[0].name, "No-op ld");
        assert_eq!(pack.patterns[1].name, "Add any");
        assert_eq!(pack.patterns[2].name, "Add/adc any");
        assert_eq!(pack.patterns[3].name, "Add/adc sibling operands");
        assert_eq!(pack.patterns[4].name, "Add/adc redundant arg");

        let lines = preprocess_properties("Label:\n  ld b, b\n  add a, b\n  adc a, c\n");
        let got = run_pack_on_lines("file.asm", &lines, &pack);
        assert_eq!(got.len(), 5);
        assert_eq!(got[0].0, "No-op ld");
        assert_eq!(got[0].1.len(), 1);
        assert_eq!(got[0].1[0].lines.len(), 1);

        assert_eq!(got[1].0, "Add any");
        assert_eq!(got[1].1.len(), 1);
        assert_eq!(got[1].1[0].lines.len(), 1);

        assert_eq!(got[2].0, "Add/adc any");
        assert_eq!(got[2].1.len(), 2);

        assert_eq!(got[3].0, "Add/adc sibling operands");
        assert_eq!(got[3].1.len(), 2);

        assert_eq!(got[4].0, "Add/adc redundant arg");
        assert_eq!(got[4].1.len(), 2);
    }

    #[test]
    fn operand_maps_support_multiple_keys_and_false_negation() {
        let yaml = r#"
patterns:
    mem_but_not_mem_r16:
        name: Mem but not [bc]/[de]
        steps:
            - instruction:
                    mnemonic: ld
                    operands:
                        - { eq: a }
                        - { is_mem: true, is_mem_r16: false }

packs:
    core:
        patterns:
            - mem_but_not_mem_r16
"#;

        let pack = load_pattern_pack_yaml(yaml, "core").unwrap();
        let lines =
            preprocess_properties("  ld a, [bc]\n  ld a, [de]\n  ld a, [hl]\n  ld a, [hFoo]\n");
        let got = run_pack_on_lines("file.asm", &lines, &pack);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].0, "Mem but not [bc]/[de]");
        assert_eq!(got[0].1.len(), 1);
        assert_eq!(got[0].1[0].lines[0].code, "ld a, [hFoo]");
    }

    #[test]
    fn operand_maps_support_number_literal_comparisons() {
        let yaml = r#"
patterns:
    small_imm:
        name: Small immediate
        steps:
            - instruction:
                    mnemonic: ld
                    operands:
                        - { eq: a }
                        - { number_lt: 10 }

    not_ten:
        name: Not ten
        steps:
            - instruction:
                    mnemonic: ld
                    operands:
                        - { eq: a }
                        - { number_ne: 10 }

    between:
        name: Between 8 and 15
        steps:
            - instruction:
                    mnemonic: ld
                    operands:
                        - { eq: a }
                        - { number_ge: 8, number_le: 15 }

packs:
    core:
        patterns:
            - small_imm
            - not_ten
            - between
"#;

        let pack = load_pattern_pack_yaml(yaml, "core").unwrap();
        let lines = preprocess_properties(
            "  ld a, 7\n  ld a, 9\n  ld a, 10\n  ld a, $0f\n  ld a, Label\n  ld a, $30+8\n",
        );
        let got = run_pack_on_lines("file.asm", &lines, &pack);

        assert_eq!(got.len(), 3);

        assert_eq!(got[0].0, "Small immediate");
        assert_eq!(got[0].1.len(), 2);

        assert_eq!(got[1].0, "Not ten");
        assert_eq!(got[1].1.len(), 3);

        assert_eq!(got[2].0, "Between 8 and 15");
        assert_eq!(got[2].1.len(), 3);
    }
}
