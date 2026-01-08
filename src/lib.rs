#![forbid(unsafe_code)]

mod pattern_config;

pub use pattern_config::run_pack_on_lines;
pub use pattern_config::{PatternPack, load_pattern_pack_yaml};
use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug)]
pub struct FancyRegex(fancy_regex::Regex);

impl FancyRegex {
    pub fn new(re: &str) -> Result<Self, fancy_regex::Error> {
        fancy_regex::Regex::new(re).map(Self)
    }

    pub fn is_match(&self, text: &str) -> bool {
        match self.0.is_match(text) {
            Ok(matched) => matched,
            Err(err) => {
                eprintln!("Pattern matching error for text '{text}': {err}");
                false
            }
        }
    }
}

impl Deref for FancyRegex {
    type Target = fancy_regex::Regex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Line {
    pub num: usize,
    pub code: String,
    pub comment: String,
    pub comment_lower: String,
    pub text: String,
    pub context: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
    pub mnemonic: String,
    pub operands: Vec<String>,
}

impl Instruction {
    pub fn operand(&self, idx: usize) -> Option<&str> {
        self.operands.get(idx).map(|s| s.as_str())
    }
}

pub fn parse_instruction(code: &str) -> Option<Instruction> {
    let code = code.trim();
    if code.is_empty() {
        return None;
    }

    // `Line::code` is comment-stripped for `;` comments, but RGBASM also supports block
    // comments `/* ... */` anywhere in a line. Strip those before parsing.
    let code = strip_rgbasm_block_comments(code);
    let code = code.trim();
    if code.is_empty() {
        return None;
    }

    // RGBASM syntax allows optional leading labels (`Label:` or `.loop:`) before an
    // instruction on the same line.
    let code = strip_leading_label(code);
    let code = code.trim();
    if code.is_empty() {
        return None;
    }

    // RGBASM allows multiple instructions on one line separated by `::`.
    // For instruction matching we consider only the first instruction segment.
    let code = first_instruction_segment(code);
    let code = code.trim();
    if code.is_empty() {
        return None;
    }

    let (mnemonic_raw, rest) = split_mnemonic_and_rest(code);
    let mnemonic = mnemonic_raw.to_ascii_lowercase();

    // A leading `#` denotes a raw identifier (escaped keyword). That cannot be an
    // instruction mnemonic.
    if mnemonic.starts_with('#') {
        return None;
    }

    if !is_gbz80_mnemonic(&mnemonic) {
        return None;
    }

    let operands = if rest.is_empty() {
        Vec::new()
    } else {
        split_operands(rest)
            .into_iter()
            .map(|op| op.trim())
            .filter(|op| !op.is_empty())
            .map(str::to_string)
            .collect()
    };

    Some(Instruction { mnemonic, operands })
}

fn is_gbz80_mnemonic(mnemonic_lower: &str) -> bool {
    // Mnemonics per gbz80(7). This intentionally excludes directives and user macros.
    matches!(
        mnemonic_lower,
        "adc"
            | "add"
            | "and"
            | "bit"
            | "call"
            | "ccf"
            | "cp"
            | "cpl"
            | "daa"
            | "dec"
            | "di"
            | "ei"
            | "halt"
            | "inc"
            | "jp"
            | "jr"
            | "jmp"
            | "ld"
            | "ldh"
            | "nop"
            | "or"
            | "pop"
            | "push"
            | "res"
            | "ret"
            | "reti"
            | "rl"
            | "rla"
            | "rlc"
            | "rlca"
            | "rr"
            | "rra"
            | "rrc"
            | "rrca"
            | "rst"
            | "sbc"
            | "scf"
            | "set"
            | "sla"
            | "sra"
            | "srl"
            | "stop"
            | "sub"
            | "swap"
            | "xor"
    )
}

fn split_mnemonic_and_rest(code: &str) -> (&str, &str) {
    // Split by the first whitespace. `Line::code` already normalizes whitespace,
    // but keep this robust for callers using raw strings.
    for (idx, ch) in code.char_indices() {
        if ch.is_ascii_whitespace() {
            let mnemonic = &code[..idx];
            let rest = code[idx..].trim();
            return (mnemonic, rest);
        }
    }
    (code, "")
}

fn strip_leading_label(code: &str) -> &str {
    // RGBASM line layout allows: `[label:] instruction`.
    // Our preprocessor may preserve label-only lines as `Label:`.
    let code = code.trim_start();
    if code.is_empty() {
        return code;
    }

    let mut end = 0usize;
    for (idx, ch) in code.char_indices() {
        if ch.is_ascii_whitespace() {
            end = idx;
            break;
        }
    }
    if end == 0 {
        end = code.len();
    }
    let first_token = &code[..end];

    // Common/valid label tokens:
    // - `Foo:`
    // - `Foo::`
    // - `.loop:`
    // Also accept the no-whitespace form `Foo:ld a, b` defensively.
    if let Some(rest) = strip_label_token(first_token, &code[end..]) {
        return rest;
    }

    if let Some(colon_idx) = first_token.find(':') {
        let (maybe_label, after_colon) = first_token.split_at(colon_idx + 1);
        if maybe_label.ends_with(':') && is_label_token(maybe_label) && !after_colon.is_empty() {
            return &code[colon_idx + 1..];
        }
    }

    code
}

fn strip_label_token<'a>(token: &str, rest_after_token: &'a str) -> Option<&'a str> {
    if !is_label_token(token) {
        return None;
    }
    Some(rest_after_token)
}

fn is_label_token(token: &str) -> bool {
    if token.is_empty() {
        return false;
    }
    let starts_ok = token
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_' || c == '.');
    if !starts_ok {
        return false;
    }
    token.ends_with(':') || token.ends_with("::")
}

fn first_instruction_segment(code: &str) -> &str {
    // Split on the first top-level `::` (instruction separator).
    // Avoid splitting inside strings, block comments, parentheses/brackets/braces.
    let bytes = code.as_bytes();
    let mut i = 0usize;
    let mut bracket_depth: i32 = 0;
    let mut paren_depth: i32 = 0;
    let mut brace_depth: i32 = 0;
    let mut in_string: Option<u8> = None;
    let mut string_is_raw = false;

    while i < bytes.len() {
        let b = bytes[i];

        if in_string.is_some() {
            if !string_is_raw && b == b'\\' {
                i = (i + 2).min(bytes.len());
                continue;
            }
            if Some(b) == in_string {
                in_string = None;
                string_is_raw = false;
            }
            i += 1;
            continue;
        }

        // Block comments.
        if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
            i += 2;
            while i + 1 < bytes.len() {
                if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        if b == b'#' && i + 1 < bytes.len() && bytes[i + 1] == b'"' {
            in_string = Some(b'"');
            string_is_raw = true;
            i += 2;
            continue;
        }
        if b == b'"' || b == b'\'' {
            in_string = Some(b);
            string_is_raw = false;
            i += 1;
            continue;
        }

        match b {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth -= 1,
            b'(' => paren_depth += 1,
            b')' => paren_depth -= 1,
            b'{' => brace_depth += 1,
            b'}' => brace_depth -= 1,
            b':' if bracket_depth == 0
                && paren_depth == 0
                && brace_depth == 0
                && i + 1 < bytes.len()
                && bytes[i + 1] == b':' =>
            {
                return &code[..i];
            }
            _ => {}
        }

        i += 1;
    }

    code
}

fn strip_rgbasm_block_comments(code: &str) -> String {
    // Strip `/* ... */` comment spans, preserving string literals.
    // This is line-local; multi-line block comments will be handled per-line.
    let bytes = code.as_bytes();
    let mut out = String::with_capacity(code.len());
    let mut i = 0usize;
    let mut in_string: Option<u8> = None;
    let mut string_is_raw = false;

    while i < bytes.len() {
        let b = bytes[i];

        if in_string.is_some() {
            out.push(b as char);

            if !string_is_raw && b == b'\\' && i + 1 < bytes.len() {
                out.push(bytes[i + 1] as char);
                i += 2;
                continue;
            }
            if Some(b) == in_string {
                in_string = None;
                string_is_raw = false;
            }
            i += 1;
            continue;
        }

        if b == b'#' && i + 1 < bytes.len() && bytes[i + 1] == b'"' {
            out.push('#');
            out.push('"');
            in_string = Some(b'"');
            string_is_raw = true;
            i += 2;
            continue;
        }
        if b == b'"' || b == b'\'' {
            out.push(b as char);
            in_string = Some(b);
            string_is_raw = false;
            i += 1;
            continue;
        }

        if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
            // Replace the entire comment span with a single space to keep tokens separated.
            out.push(' ');
            i += 2;
            while i + 1 < bytes.len() {
                if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        out.push(b as char);
        i += 1;
    }

    out
}

fn split_operands(s: &str) -> Vec<&str> {
    // Split by commas, but keep bracket/paren/brace/string constructs intact.
    // This is designed to match RGBASM operand syntax well enough for pattern matching,
    // not to fully parse expressions.
    let mut out = Vec::new();
    let mut bracket_depth: i32 = 0;
    let mut paren_depth: i32 = 0;
    let mut brace_depth: i32 = 0;
    let mut in_string: Option<u8> = None;
    let mut string_is_raw = false;
    let mut start = 0usize;

    let bytes = s.as_bytes();
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];

        if in_string.is_some() {
            if !string_is_raw && b == b'\\' {
                i = (i + 2).min(bytes.len());
                continue;
            }
            if Some(b) == in_string {
                in_string = None;
                string_is_raw = false;
            }
            i += 1;
            continue;
        }

        // Skip block comments entirely.
        if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
            i += 2;
            while i + 1 < bytes.len() {
                if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    i += 2;
                    break;
                }
                i += 1;
            }
            continue;
        }

        if b == b'#' && i + 1 < bytes.len() && bytes[i + 1] == b'"' {
            in_string = Some(b'"');
            string_is_raw = true;
            i += 2;
            continue;
        }
        if b == b'"' || b == b'\'' {
            in_string = Some(b);
            string_is_raw = false;
            i += 1;
            continue;
        }

        match b {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth -= 1,
            b'(' => paren_depth += 1,
            b')' => paren_depth -= 1,
            b'{' => brace_depth += 1,
            b'}' => brace_depth -= 1,
            b',' if bracket_depth == 0 && paren_depth == 0 && brace_depth == 0 => {
                out.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }

        i += 1;
    }

    out.push(&s[start..]);
    out
}

pub fn canonicalize_rgbds_operand(op: &str) -> String {
    // Normalize common RGBDS synonyms so structural matching can treat them as equivalent.
    // Keep this conservative; expand later as patterns require.
    let lowered = op.trim().to_ascii_lowercase();

    // RGBASM is permissive about whitespace. For structural comparisons, remove
    // ASCII whitespace outside of string/char literals.
    let lowered = if lowered.contains('"') || lowered.contains('\'') {
        lowered
    } else {
        lowered
            .chars()
            .filter(|c| !c.is_ascii_whitespace())
            .collect()
    };

    match lowered.as_str() {
        "[hli]" | "[hl+]" => "[hli]".to_string(),
        "[hld]" | "[hl-]" => "[hld]".to_string(),
        other => other.to_string(),
    }
}

pub fn parse_rgbds_int(op: &str) -> Option<i64> {
    let s = op.trim();
    if s.is_empty() {
        return None;
    }

    if s.eq_ignore_ascii_case("false") {
        return Some(0);
    }
    if s.eq_ignore_ascii_case("true") {
        return Some(1);
    }

    let (sign, rest) = match s.as_bytes()[0] {
        b'+' => (1i64, &s[1..]),
        b'-' => (-1i64, &s[1..]),
        _ => (1i64, s),
    };

    let rest = rest.trim();
    if rest.is_empty() {
        return None;
    }

    let (radix, digits) = if let Some(hex) = rest.strip_prefix('$') {
        (16u32, hex)
    } else if let Some(bin) = rest.strip_prefix('%') {
        (2u32, bin)
    } else if let Some(oct) = rest.strip_prefix('&') {
        (8u32, oct)
    } else if let Some(hex) = rest.strip_prefix("0x").or_else(|| rest.strip_prefix("0X")) {
        (16u32, hex)
    } else {
        (10u32, rest)
    };

    let digits: String = digits.chars().filter(|c| *c != '_').collect();
    if digits.is_empty() {
        return None;
    }

    let unsigned = if radix == 10 {
        digits.parse::<i64>().ok()?
    } else {
        i64::from_str_radix(&digits, radix).ok()?
    };
    Some(sign * unsigned)
}

pub fn is_rgbds_zero_literal(op: &str) -> bool {
    matches!(parse_rgbds_int(op), Some(0))
}

#[derive(Clone, Debug)]
pub enum StepCondition {
    Regex(Arc<FancyRegex>),
    TextRegex(Arc<FancyRegex>),
    StrEq { left: StringExpr, right: StringExpr },
    CodeEq(String),
    CodeNe(String),
    CodeStartsWith(String),
    CodeEndsWith(String),
    CodeContains(String),
    CodeContainsWord(String),
    Any(Vec<StepCondition>),
    All(Vec<StepCondition>),
    Not(Box<StepCondition>),
    Instruction(InstructionCondition),
    IncDecSameTargetAsPrevLd { prev_step: usize },
}

#[derive(Clone, Debug)]
pub struct InstructionCondition {
    pub mnemonic: Option<String>,
    pub operands: Vec<OperandCondition>,
    pub has_cc: Option<bool>,
    pub cc_eq: Option<String>,
    pub cc_ne: Option<String>,
    pub cc_in: Option<Vec<String>>,
}

#[derive(Clone, Debug)]
pub enum OperandCondition {
    Eq(String),
    CanonEq(String),
    IsZeroLiteral,
    IsZeroNumericLiteral,
    IsOneNumericLiteral,
    NumberLiteralEq(i64),
    NumberLiteralNe(i64),
    NumberLiteralLt(i64),
    NumberLiteralLe(i64),
    NumberLiteralGt(i64),
    NumberLiteralGe(i64),
    IsReg8,
    IsReg16,
    IsReg16Stack,
    IsCc,
    IsMem,
    IsMemHl,
    IsMemHli,
    IsMemHld,
    IsMemR16,
    IsMemC,
    IsImm,
    IsNumberLiteral,
    IsU3Literal,
    IsRstVecLiteral,
    AnyOperand,
    Any(Vec<OperandCondition>),
    All(Vec<OperandCondition>),
    Not(Box<OperandCondition>),
}

impl OperandCondition {
    fn matches(&self, operand: &str) -> bool {
        match self {
            OperandCondition::Eq(want) => operand.eq_ignore_ascii_case(want),
            OperandCondition::CanonEq(want) => canonicalize_rgbds_operand(operand) == *want,
            OperandCondition::IsZeroLiteral => is_rgbds_zero_literal(operand),
            OperandCondition::IsZeroNumericLiteral => is_rgbds_zero_numeric_literal(operand),
            OperandCondition::IsOneNumericLiteral => is_rgbds_one_numeric_literal(operand),
            OperandCondition::NumberLiteralEq(want) => {
                matches!(parse_rgbds_int(operand), Some(got) if got == *want)
            }
            OperandCondition::NumberLiteralNe(want) => {
                matches!(parse_rgbds_int(operand), Some(got) if got != *want)
            }
            OperandCondition::NumberLiteralLt(want) => {
                matches!(parse_rgbds_int(operand), Some(got) if got < *want)
            }
            OperandCondition::NumberLiteralLe(want) => {
                matches!(parse_rgbds_int(operand), Some(got) if got <= *want)
            }
            OperandCondition::NumberLiteralGt(want) => {
                matches!(parse_rgbds_int(operand), Some(got) if got > *want)
            }
            OperandCondition::NumberLiteralGe(want) => {
                matches!(parse_rgbds_int(operand), Some(got) if got >= *want)
            }
            OperandCondition::IsReg8 => is_rgbds_reg8(operand),
            OperandCondition::IsReg16 => is_rgbds_reg16(operand),
            OperandCondition::IsReg16Stack => is_rgbds_reg16_stack(operand),
            OperandCondition::IsCc => is_rgbds_cc(operand),
            OperandCondition::IsMem => is_rgbds_mem_any(operand),
            OperandCondition::IsMemHl => is_rgbds_mem_hl(operand),
            OperandCondition::IsMemHli => is_rgbds_mem_hli(operand),
            OperandCondition::IsMemHld => is_rgbds_mem_hld(operand),
            OperandCondition::IsMemR16 => is_rgbds_mem_r16(operand),
            OperandCondition::IsMemC => is_rgbds_mem_c(operand),
            OperandCondition::IsImm => is_rgbds_immediate_like(operand),
            OperandCondition::IsNumberLiteral => parse_rgbds_int(operand).is_some(),
            OperandCondition::IsU3Literal => is_rgbds_u3_literal(operand),
            OperandCondition::IsRstVecLiteral => is_rgbds_rst_vec_literal(operand),
            OperandCondition::AnyOperand => true,
            OperandCondition::Any(options) => options.iter().any(|opt| opt.matches(operand)),
            OperandCondition::All(conds) => conds.iter().all(|cond| cond.matches(operand)),
            OperandCondition::Not(cond) => !cond.matches(operand),
        }
    }
}

fn is_ascii_word_char(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_'
}

fn contains_ascii_word(haystack: &str, needle: &str) -> bool {
    if needle.is_empty() {
        return false;
    }

    let needle = needle.as_bytes();
    let hay = haystack.as_bytes();
    if needle.len() > hay.len() {
        return false;
    }

    for start in 0..=hay.len() - needle.len() {
        if !hay[start..start + needle.len()].eq_ignore_ascii_case(needle) {
            continue;
        }

        let left_ok = start == 0 || !is_ascii_word_char(hay[start - 1]);
        let end = start + needle.len();
        let right_ok = end == hay.len() || !is_ascii_word_char(hay[end]);

        if left_ok && right_ok {
            return true;
        }
    }

    false
}

fn strip_rgbds_base_prefix(op: &str) -> &str {
    let bytes = op.as_bytes();
    if bytes.first().is_some_and(|b| matches!(b, b'%' | b'$' | b'&')) {
        &op[1..]
    } else {
        op
    }
}

fn is_rgbds_zero_numeric_literal(op: &str) -> bool {
    let rest = strip_rgbds_base_prefix(op.trim());
    !rest.is_empty() && rest.as_bytes().iter().all(|b| *b == b'0')
}

fn is_rgbds_one_numeric_literal(op: &str) -> bool {
    let rest = strip_rgbds_base_prefix(op.trim());
    let bytes = rest.as_bytes();
    if bytes.is_empty() || *bytes.last().unwrap() != b'1' {
        return false;
    }
    bytes[..bytes.len() - 1].iter().all(|b| *b == b'0')
}

fn operand_token_lower_no_space(op: &str) -> String {
    let no_space: String = op.chars().filter(|c| !c.is_ascii_whitespace()).collect();
    no_space.to_ascii_lowercase()
}

fn canonical_operand_for_kind_checks(op: &str) -> String {
    // Kind checks are deliberately lexical and conservative.
    // Remove ASCII whitespace; keep everything else.
    operand_token_lower_no_space(op.trim())
}

fn is_rgbds_reg8(op: &str) -> bool {
    matches!(
        canonical_operand_for_kind_checks(op).as_str(),
        "a" | "b" | "c" | "d" | "e" | "h" | "l"
    )
}

fn is_rgbds_reg16(op: &str) -> bool {
    matches!(
        canonical_operand_for_kind_checks(op).as_str(),
        "bc" | "de" | "hl" | "sp"
    )
}

fn is_rgbds_reg16_stack(op: &str) -> bool {
    matches!(
        canonical_operand_for_kind_checks(op).as_str(),
        "af" | "bc" | "de" | "hl"
    )
}

fn is_rgbds_cc(op: &str) -> bool {
    let op = canonical_operand_for_kind_checks(op);
    let op = op.strip_prefix('!').unwrap_or(op.as_str());
    matches!(op, "z" | "nz" | "c" | "nc")
}

fn is_control_flow_mnemonic_for_cc(mnemonic_lower: &str) -> bool {
    matches!(mnemonic_lower, "jr" | "jp" | "jmp" | "call" | "ret")
}

fn instruction_cc_operand(ins: &Instruction) -> Option<&str> {
    if !is_control_flow_mnemonic_for_cc(&ins.mnemonic) {
        return None;
    }

    let first = ins.operand(0)?;
    if is_rgbds_cc(first) {
        Some(first)
    } else {
        None
    }
}

fn instruction_effective_operands(ins: &Instruction) -> &[String] {
    if instruction_cc_operand(ins).is_some() {
        &ins.operands[1..]
    } else {
        &ins.operands[..]
    }
}

fn parse_bracketed_operand(op: &str) -> Option<String> {
    let op = canonical_operand_for_kind_checks(op);
    let inner = op.strip_prefix('[')?.strip_suffix(']')?;
    Some(inner.to_string())
}

fn is_rgbds_mem_any(op: &str) -> bool {
    parse_bracketed_operand(op).is_some()
}

fn is_rgbds_mem_hl(op: &str) -> bool {
    matches!(parse_bracketed_operand(op).as_deref(), Some("hl"))
}

fn is_rgbds_mem_hli(op: &str) -> bool {
    matches!(
        parse_bracketed_operand(op).as_deref(),
        Some("hli") | Some("hl+")
    )
}

fn is_rgbds_mem_hld(op: &str) -> bool {
    matches!(
        parse_bracketed_operand(op).as_deref(),
        Some("hld") | Some("hl-")
    )
}

fn is_rgbds_mem_r16(op: &str) -> bool {
    matches!(
        parse_bracketed_operand(op).as_deref(),
        Some("bc") | Some("de") | Some("hl")
    )
}

fn is_rgbds_mem_c(op: &str) -> bool {
    matches!(parse_bracketed_operand(op).as_deref(), Some("c"))
}

fn is_rgbds_immediate_like(op: &str) -> bool {
    // "Immediate" here means "not a register and not a bracketed memory reference".
    // This is intentionally permissive: symbols and expressions count as immediates.
    if op.trim().is_empty() {
        return false;
    }
    !is_rgbds_reg8(op)
        && !is_rgbds_reg16(op)
        && !is_rgbds_reg16_stack(op)
        && !is_rgbds_cc(op)
        && !is_rgbds_mem_any(op)
}

fn is_rgbds_u3_literal(op: &str) -> bool {
    match parse_rgbds_int(op) {
        Some(v) => (0..=7).contains(&v),
        None => false,
    }
}

fn is_rgbds_rst_vec_literal(op: &str) -> bool {
    match parse_rgbds_int(op) {
        Some(v) => matches!(v, 0x00 | 0x08 | 0x10 | 0x18 | 0x20 | 0x28 | 0x30 | 0x38),
        None => false,
    }
}

impl InstructionCondition {
    fn matches(&self, line: &Line) -> bool {
        let Some(ins) = parse_instruction(&line.code) else {
            return false;
        };

        if let Some(mnemonic) = self.mnemonic.as_ref()
            && ins.mnemonic != mnemonic.to_ascii_lowercase()
        {
            return false;
        }

        // Condition-code (cc) checks: many control-flow instructions encode a cc
        // as the first operand (e.g. `jr nz, LABEL`). Treat that cc as metadata,
        // and do not confuse it with register operands like `ld c, a`.
        let cc = instruction_cc_operand(&ins);
        let cc_present = cc.is_some();

        if let Some(expect_has_cc) = &self.has_cc {
            if *expect_has_cc != cc_present {
                return false;
            }
        }

        if let Some(want) = &self.cc_eq {
            if !cc_present {
                return false;
            }
            if !cc.is_some_and(|op| op.eq_ignore_ascii_case(want)) {
                return false;
            }
        }

        if let Some(want) = &self.cc_ne {
            if !cc_present {
                return false;
            }
            if cc.is_some_and(|op| op.eq_ignore_ascii_case(want)) {
                return false;
            }
        }

        if let Some(list) = &self.cc_in {
            if !cc_present {
                return false;
            }
            let cc = cc.unwrap_or("");
            if !list.iter().any(|x| x.eq_ignore_ascii_case(cc)) {
                return false;
            }
        }

        if !self.operands.is_empty() {
            // If the instruction uses a condition-code (cc) as the first operand
            // (e.g. `jr nz, LABEL`), treat the cc as metadata and skip it when
            // aligning the instruction's operands against the pattern's expected
            // operands. This ensures `instruction, operand1` refers to the jump
            // target rather than the cc.
            let effective_ins_operands: &[String] = instruction_effective_operands(&ins);

            if effective_ins_operands.len() == self.operands.len() {
                for (op, cond) in effective_ins_operands.iter().zip(self.operands.iter()) {
                    if !cond.matches(op) {
                        return false;
                    }
                }
            } else if effective_ins_operands.len() == 1 && self.operands.len() == 2 {
                // Support implicit accumulator forms like `sub 1` == `sub a, 1` for
                // common mnemonics that implicitly operate on `a`.
                match ins.mnemonic.as_str() {
                    "add" | "sub" | "adc" | "sbc" | "and" | "xor" | "or" | "cp" => {
                        // Only apply when the pattern expects an `a` as the first operand.
                        if let crate::OperandCondition::Eq(want) = &self.operands[0] {
                            if !want.eq_ignore_ascii_case("a") {
                                return false;
                            }
                        } else {
                            return false;
                        }

                        // Only apply implicit-`a` if the pattern's second operand is
                        // numeric-like (e.g. `is_number_literal`, `number_eq`,
                        // `is_zero_literal`, etc.) so we don't accidentally treat
                        // `add a` as `add a, a`.
                        fn numeric_like(cond: &crate::OperandCondition) -> bool {
                            use crate::OperandCondition::*;
                            match cond {
                                IsNumberLiteral
                                | IsImm
                                | IsU3Literal
                                | IsRstVecLiteral
                                | IsZeroLiteral
                                | IsZeroNumericLiteral
                                | IsOneNumericLiteral
                                | NumberLiteralEq(_)
                                | NumberLiteralNe(_)
                                | NumberLiteralLt(_)
                                | NumberLiteralLe(_)
                                | NumberLiteralGt(_)
                                | NumberLiteralGe(_) => true,
                                Any(opts) | All(opts) => opts.iter().any(|c| numeric_like(c)),
                                Not(c) => numeric_like(c),
                                _ => false,
                            }
                        }

                        if !numeric_like(&self.operands[1]) {
                            return false;
                        }

                        // Only treat single-operand forms as implicit-`a` when the single
                        // operand is an immediate-like value (e.g. `1`, `$ff`, a symbol).
                        let real_op = &effective_ins_operands[0];
                        if !is_rgbds_immediate_like(real_op) {
                            return false;
                        }

                        // Synthetic first operand `a`, then check the single real operand
                        // against the second expected condition.
                        if !self.operands[1].matches(real_op) {
                            return false;
                        }
                    }
                    _ => return false,
                }
            } else {
                return false;
            }
        }

        true
    }
}

impl StepCondition {
    pub fn matches(&self, line: &Line, _prev: &[Line]) -> bool {
        match self {
            StepCondition::Regex(re) => re.is_match(&line.code),
            StepCondition::TextRegex(re) => re.is_match(&line.text),
            StepCondition::StrEq { left, right } => {
                let Some(l) = left.eval(line, _prev) else {
                    return false;
                };
                let Some(r) = right.eval(line, _prev) else {
                    return false;
                };
                l == r
            }
            StepCondition::CodeEq(want) => line.code == *want,
            StepCondition::CodeNe(want) => line.code != *want,
            StepCondition::CodeStartsWith(prefix) => line.code.starts_with(prefix),
            StepCondition::CodeEndsWith(suffix) => line.code.ends_with(suffix),
            StepCondition::CodeContains(needle) => line.code.contains(needle),
            StepCondition::CodeContainsWord(needle) => contains_ascii_word(&line.code, needle),
            StepCondition::Any(conds) => conds.iter().any(|c| c.matches(line, _prev)),
            StepCondition::All(conds) => conds.iter().all(|c| c.matches(line, _prev)),
            StepCondition::Not(cond) => !cond.matches(line, _prev),
            StepCondition::Instruction(cond) => cond.matches(line),
            StepCondition::IncDecSameTargetAsPrevLd { prev_step } => {
                let Some(prev_line) = _prev.get(*prev_step) else {
                    return false;
                };
                let prev_code = prev_line.code.as_str();
                if !prev_code.starts_with("ld ") {
                    return false;
                }
                let Some((lhs, _rhs)) = prev_code.split_once(',') else {
                    return false;
                };
                let prev_dst = lhs.trim_start_matches("ld ").trim();

                let cur = line.code.as_str();
                if !(cur.starts_with("inc ") || cur.starts_with("dec ")) {
                    return false;
                }
                let cur_dst = cur.get(4..).unwrap_or("").trim();
                cur_dst.eq_ignore_ascii_case(prev_dst)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum StringField {
    Code,
    Text,
    Context,
    Comment,
    CommentLower,
    Label,

    InstructionMnemonic,
    InstructionOperand { idx: usize },
    InstructionCc,
    InstructionCcOrEmpty,
    InstructionRoot,
}

#[derive(Clone, Debug)]
pub enum StringBase {
    Current(StringField),
    Prev { idx: usize, field: StringField },
    Const(String),
}

#[derive(Clone, Debug)]
pub enum StringTransform {
    StripPrefix(String),
    AfterPrefix(String),
    BeforeComma,
    AfterComma,
    AfterCommaRaw,
    PairReg,
    ByteAt(usize),
    LastChar,
    LastToken,
    StripTrailingColon,
    SymbolLike,
    Trim,
    Lower,
}

#[derive(Clone, Debug)]
pub struct StringExpr {
    pub base: StringBase,
    pub transforms: Vec<StringTransform>,
}

impl StringExpr {
    pub fn eval<'a>(&self, current: &'a Line, prev: &'a [Line]) -> Option<String> {
        let mut s: String = match &self.base {
            StringBase::Current(field) => field.eval(current)?,
            StringBase::Prev { idx, field } => field.eval(prev.get(*idx)?)?,
            StringBase::Const(value) => value.clone(),
        };

        for t in &self.transforms {
            match t {
                StringTransform::StripPrefix(prefix) => {
                    s = s.strip_prefix(prefix).unwrap_or(&s).to_string();
                }
                StringTransform::AfterPrefix(prefix) => {
                    s = s.strip_prefix(prefix).unwrap_or("").to_string();
                }
                StringTransform::BeforeComma => {
                    s = s
                        .split_once(',')
                        .map(|(lhs, _)| lhs.trim())
                        .unwrap_or("")
                        .to_string();
                }
                StringTransform::AfterComma => {
                    s = split_after_comma(&s).unwrap_or("").to_string();
                }
                StringTransform::AfterCommaRaw => {
                    s = s
                        .split_once(',')
                        .map(|(_, rhs)| rhs)
                        .unwrap_or("")
                        .to_string();
                }
                StringTransform::PairReg => {
                    s = match s.as_str() {
                        "a" => "f",
                        "f" => "a",
                        "b" => "c",
                        "c" => "b",
                        "d" => "e",
                        "e" => "d",
                        "h" => "l",
                        "l" => "h",
                        "A" => "F",
                        "F" => "A",
                        "B" => "C",
                        "C" => "B",
                        "D" => "E",
                        "E" => "D",
                        "H" => "L",
                        "L" => "H",
                        _ => "",
                    }
                    .to_string();
                }
                StringTransform::ByteAt(idx) => {
                    s = s
                        .as_bytes()
                        .get(*idx)
                        .map(|b| (*b as char).to_string())
                        .unwrap_or_default();
                }
                StringTransform::LastChar => {
                    s = s
                        .as_bytes()
                        .last()
                        .map(|b| (*b as char).to_string())
                        .unwrap_or_default();
                }
                StringTransform::LastToken => {
                    s = last_token(&s).unwrap_or("").to_string();
                }
                StringTransform::StripTrailingColon => {
                    s = strip_trailing_colon(&s).to_string();
                }
                StringTransform::SymbolLike => {
                    s = extract_symbol_like(&s).to_string();
                }
                StringTransform::Trim => {
                    s = s.trim().to_string();
                }
                StringTransform::Lower => {
                    s = s.to_ascii_lowercase();
                }
            }
        }

        Some(s)
    }
}

impl StringField {
    fn eval(&self, line: &Line) -> Option<String> {
        match self {
            StringField::Code => Some(line.code.clone()),
            StringField::Text => Some(line.text.clone()),
            StringField::Context => Some(line.context.clone()),
            StringField::Comment => Some(line.comment.clone()),
            StringField::CommentLower => Some(line.comment_lower.clone()),

            StringField::InstructionMnemonic => Some(parse_instruction(&line.code)?.mnemonic),
            StringField::InstructionOperand { idx } => {
                let ins = parse_instruction(&line.code)?;
                instruction_effective_operands(&ins).get(*idx).cloned()
            }
            StringField::InstructionCc => {
                let ins = parse_instruction(&line.code)?;
                instruction_cc_operand(&ins).map(|s| s.to_string())
            }
            StringField::InstructionCcOrEmpty => {
                let ins = parse_instruction(&line.code)?;
                Some(instruction_cc_operand(&ins).unwrap_or("").to_string())
            }
            StringField::InstructionRoot => {
                let ins = parse_instruction(&line.code)?;
                instruction_effective_operands(&ins)
                    .first()
                    .map(|s| s.to_string())
            }
            StringField::Label => extract_label_field(line).map(|s| s.to_string()),
        }
    }
}

fn extract_label_field(line: &Line) -> Option<&str> {
    if let Some(lbl) = extract_leading_label(&line.code) {
        return Some(lbl);
    }

    // RGBDS allows bare local labels like `.loop` (no trailing `:`).
    // In practice these appear as a single, non-indented token on the line.
    let code = line.code.as_str();
    let is_non_indented = !line
        .text
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_whitespace());
    let is_single_token = !code.chars().any(|c| c.is_ascii_whitespace());

    (is_non_indented && is_single_token && code.starts_with('.')).then_some(code)
}

fn extract_leading_label(code: &str) -> Option<&str> {
    let code = code.trim_start();
    if code.is_empty() {
        return None;
    }

    let mut end = 0usize;
    for (idx, ch) in code.char_indices() {
        if ch.is_ascii_whitespace() {
            end = idx;
            break;
        }
    }
    if end == 0 {
        end = code.len();
    }
    let first_token = &code[..end];

    if strip_label_token(first_token, &code[end..]).is_some() {
        // trim trailing ':' or '::'
        let lbl = first_token.trim_end_matches(':');
        return Some(lbl);
    }

    if let Some(colon_idx) = first_token.find(':') {
        let (maybe_label, after_colon) = first_token.split_at(colon_idx + 1);
        if maybe_label.ends_with(':') && is_label_token(maybe_label) && !after_colon.is_empty() {
            let lbl = maybe_label.trim_end_matches(':');
            return Some(lbl);
        }
    }

    None
}

fn split_after_comma(s: &str) -> Option<&str> {
    s.split_once(',').map(|(_, rhs)| rhs.trim())
}

fn last_token(s: &str) -> Option<&str> {
    s.split_whitespace().last()
}

fn strip_trailing_colon(s: &str) -> &str {
    s.trim_end_matches(':')
}

fn extract_symbol_like(s: &str) -> &str {
    let mut end = 0usize;
    for (i, ch) in s.char_indices() {
        let ok = ch.is_ascii_alphanumeric() || matches!(ch, '_' | '.' | '%' | '$' | '&');
        if !ok {
            break;
        }
        end = i + ch.len_utf8();
    }
    &s[..end]
}

#[derive(Clone, Debug)]
pub struct PatternStep {
    pub rewind: Option<usize>,
    pub condition: StepCondition,
}

impl PatternStep {
    pub fn regex(re: Arc<FancyRegex>) -> Self {
        Self {
            rewind: None,
            condition: StepCondition::Regex(re),
        }
    }

    pub fn with_rewind_regex(rewind: usize, re: Arc<FancyRegex>) -> Self {
        Self {
            rewind: Some(rewind),
            condition: StepCondition::Regex(re),
        }
    }

    pub fn code_eq(code: impl Into<String>) -> Self {
        Self {
            rewind: None,
            condition: StepCondition::CodeEq(code.into()),
        }
    }

    pub fn code_ne(code: impl Into<String>) -> Self {
        Self {
            rewind: None,
            condition: StepCondition::CodeNe(code.into()),
        }
    }

    pub fn with_rewind(rewind: usize, condition: StepCondition) -> Self {
        Self {
            rewind: Some(rewind),
            condition,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MatchInstance {
    pub label: Option<Line>,
    pub lines: Vec<Line>,
}

pub fn find_pattern_instances(
    filename: &str,
    lines: &[Line],
    pattern_name: &str,
    steps: &[PatternStep],
) -> Vec<MatchInstance> {
    if steps.is_empty() {
        return Vec::new();
    }

    let suppress_prefix = format!("no-optimize {}", pattern_name.to_lowercase());

    let mut matches = Vec::new();
    let mut cur_label: Option<Line> = None;
    let mut prev_lines: Vec<Line> = Vec::new();
    let mut state: usize = 0;

    let n = lines.len();
    let mut i: isize = 0;
    while (i as usize) < n {
        let cur_line = &lines[i as usize];

        if is_top_level_label_line_pythonlike(cur_line) {
            cur_label = Some(cur_line.clone());
        }

        let step = &steps[state];
        let skip = cur_line.comment_lower.starts_with(&suppress_prefix);

        let matched = step.condition.matches(cur_line, &prev_lines);

        if !skip && matched {
            prev_lines.push(cur_line.clone());
            state += 1;

            if state == steps.len() {
                matches.push(MatchInstance {
                    label: cur_label.clone(),
                    lines: std::mem::take(&mut prev_lines),
                });
                state = 0;
            }
        } else if !skip {
            if let Some(rewind) = step.rewind {
                i -= rewind as isize;
                state = state.saturating_sub(rewind);
            } else {
                i -= state as isize;
                prev_lines.clear();
                state = 0;
            }
        }

        i += 1;
    }

    let _ = filename;
    matches
}

fn is_top_level_label_line_pythonlike(line: &Line) -> bool {
    let code = line.code.as_str();
    let first = code.chars().next();
    let begins_like_label =
        matches!(first, Some(c) if c.is_ascii_alphabetic() || c == '_') && code.contains(':');
    begins_like_label && !line.text.chars().next().is_some_and(|c| c.is_whitespace())
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LabelMode {
    /// Match the current optimize.py behavior: a label is any (non-indented) line whose
    /// first character is ASCII alphabetic or '_' and which contains ':' anywhere.
    PythonCompat,
    /// A more RGBDS-friendly label detector that also recognizes local labels like `.loop:`.
    /// This is intentionally token-based to avoid treating directives with ':' in operands as
    /// labels.
    ExtendedRgbds,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PreprocessOptions {
    pub label_mode: LabelMode,
}

impl Default for PreprocessOptions {
    fn default() -> Self {
        Self {
            label_mode: LabelMode::PythonCompat,
        }
    }
}

pub fn preprocess_properties(contents: &str) -> Vec<Line> {
    preprocess_properties_with_options(contents, PreprocessOptions::default())
}

pub fn preprocess_properties_with_options(contents: &str, options: PreprocessOptions) -> Vec<Line> {
    let mut processed = Vec::new();
    let mut cur_label = String::new();

    for (idx, raw) in splitlines_keep_newline(contents).enumerate() {
        let num = idx + 1;
        let text = raw.strip_suffix('\n').unwrap_or(raw).to_string();

        let (code_raw, comment_raw) = split_at_first_semicolon(&text);
        let code_trimmed_right = code_raw.trim_end();
        if code_trimmed_right.is_empty() {
            continue;
        }

        if let Some(label) = detect_label(code_trimmed_right, options.label_mode) {
            cur_label = label.to_string();
        }

        let code = normalize_whitespace(code_trimmed_right.trim_start());
        let comment = comment_raw.trim().to_string();
        let comment_lower = comment.to_lowercase();

        processed.push(Line {
            num,
            code,
            comment,
            comment_lower,
            text,
            context: cur_label.clone(),
        });
    }

    processed
}

fn splitlines_keep_newline(s: &str) -> impl Iterator<Item = &str> {
    // Equivalent to Python's splitlines(True) for '\n' lines.
    s.split_inclusive('\n')
}

fn split_at_first_semicolon(s: &str) -> (&str, &str) {
    match s.split_once(';') {
        Some((code, comment)) => (code, comment),
        None => (s, ""),
    }
}

fn detect_label(code: &str, mode: LabelMode) -> Option<&str> {
    match mode {
        LabelMode::PythonCompat => {
            let first = code.chars().next()?;
            ((first.is_ascii_alphabetic() || first == '_') && code.contains(':')).then_some(code)
        }
        LabelMode::ExtendedRgbds => {
            // Token-based: only consider the first non-whitespace token.
            // Valid label tokens include:
            // - Global labels: `Foo:`
            // - Double-colon globals: `Foo::`
            // - Local labels: `.loop:`
            let token = code.split_whitespace().next()?;
            let first = token.chars().next()?;
            let starts_like_label = first.is_ascii_alphabetic() || first == '_' || first == '.';
            let ends_like_label = token.ends_with(':') || token.ends_with("::");
            (starts_like_label && ends_like_label).then_some(token)
        }
    }
}

fn normalize_whitespace(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut in_space = false;
    for ch in s.chars() {
        if ch.is_whitespace() {
            if !in_space {
                out.push(' ');
                in_space = true;
            }
        } else {
            out.push(ch);
            in_space = false;
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_line(code: &str) -> Line {
        Line {
            num: 1,
            code: code.to_string(),
            comment: String::new(),
            comment_lower: String::new(),
            text: code.to_string(),
            context: String::new(),
        }
    }

    #[test]
    fn label_field_recognizes_bare_local_labels() {
        let line = Line {
            num: 1,
            code: ".okay".to_string(),
            comment: String::new(),
            comment_lower: String::new(),
            text: ".okay".to_string(),
            context: String::new(),
        };

        let expr = StringExpr {
            base: StringBase::Current(StringField::Label),
            transforms: Vec::new(),
        };

        assert_eq!(expr.eval(&line, &[]).as_deref(), Some(".okay"));
    }

    #[test]
    fn preprocess_skips_blank_code_lines() {
        let got = preprocess_properties("\n  ; comment-only\n\t\n");
        assert!(got.is_empty());
    }

    #[test]
    fn preprocess_normalizes_and_tracks_context() {
        let src = "Label:\n\tld\ta,  [hl] ; Hi\n\n  xor   a\n";
        let got = preprocess_properties(src);
        assert_eq!(got.len(), 3);

        assert_eq!(got[0].num, 1);
        assert_eq!(got[0].code, "Label:");
        assert_eq!(got[0].context, "Label:");

        assert_eq!(got[1].num, 2);
        assert_eq!(got[1].code, "ld a, [hl]");
        assert_eq!(got[1].comment, "Hi");
        assert_eq!(got[1].comment_lower, "hi");
        assert_eq!(got[1].context, "Label:");

        assert_eq!(got[2].num, 4);
        assert_eq!(got[2].code, "xor a");
        assert_eq!(got[2].context, "Label:");
    }

    #[test]
    fn python_compat_does_not_treat_local_labels_as_context() {
        let src = "RootLabel:\n.loop:\n  nop\n";
        let got = preprocess_properties(src);
        assert_eq!(got.len(), 3);
        assert_eq!(got[0].context, "RootLabel:");
        assert_eq!(got[1].code, ".loop:");
        assert_eq!(got[1].context, "RootLabel:");
        assert_eq!(got[2].context, "RootLabel:");
    }

    #[test]
    fn extended_mode_tracks_local_labels_as_context() {
        let src = "RootLabel:\n.loop:\n  nop\n";
        let got = preprocess_properties_with_options(
            src,
            PreprocessOptions {
                label_mode: LabelMode::ExtendedRgbds,
            },
        );
        assert_eq!(got.len(), 3);
        assert_eq!(got[0].context, "RootLabel:");
        assert_eq!(got[1].context, ".loop:");
        assert_eq!(got[2].context, ".loop:");
    }

    #[test]
    fn pattern_engine_finds_consecutive_matches() {
        let src = "Label:\n  a\n  b\n";
        let lines = preprocess_properties(src);
        let steps = [PatternStep::code_eq("a"), PatternStep::code_eq("b")];
        let got = find_pattern_instances("file.asm", &lines, "Test", &steps);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].label.as_ref().unwrap().code, "Label:");
        assert_eq!(got[0].lines.len(), 2);
        assert_eq!(got[0].lines[0].code, "a");
        assert_eq!(got[0].lines[1].code, "b");
    }

    #[test]
    fn pattern_engine_rewinds_when_configured() {
        // Steps: match "a", then try "b". If second step fails, rewind 1 and retry.
        // Input: a, a, b should yield one match.
        // optimize.py's rewind behavior does not trim prev_lines, so the match includes both "a" lines.
        let src = "Label:\n  a\n  a\n  b\n";
        let lines = preprocess_properties(src);
        let steps = [
            PatternStep::code_eq("a"),
            PatternStep::with_rewind(1, StepCondition::CodeEq("b".to_string())),
        ];
        let got = find_pattern_instances("file.asm", &lines, "Test", &steps);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].lines.len(), 3);
        assert_eq!(got[0].lines[0].code, "a");
        assert_eq!(got[0].lines[1].code, "a");
        assert_eq!(got[0].lines[2].code, "b");
    }

    #[test]
    fn pattern_engine_honors_suppression_comment() {
        let src = "Label:\n  a ; no-optimize test\n  b\n";
        let lines = preprocess_properties(src);
        let steps = [PatternStep::code_eq("a"), PatternStep::code_eq("b")];
        let got = find_pattern_instances("file.asm", &lines, "Test", &steps);
        assert!(got.is_empty());
    }

    #[test]
    fn parse_instruction_splits_operands_and_lowercases_mnemonic() {
        let ins = parse_instruction("LD a, [hl+]").unwrap();
        assert_eq!(ins.mnemonic, "ld");
        assert_eq!(
            ins.operands,
            vec!["a", "[hl+]"]
                .into_iter()
                .map(String::from)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn split_operands_does_not_split_inside_brackets() {
        let ins = parse_instruction("ld a, [hl+], b").unwrap();
        assert_eq!(ins.operands.len(), 3);
        assert_eq!(ins.operands[1], "[hl+]");
    }

    #[test]
    fn parse_instruction_returns_none_for_label_only_or_directive() {
        assert!(parse_instruction("Label:").is_none());
        assert!(parse_instruction(".loop:").is_none());
        assert!(parse_instruction("db 1, 2, 3").is_none());
        assert!(parse_instruction("SECTION \"X\", ROM0").is_none());
        assert!(parse_instruction("my_macro 1, 2").is_none());
    }

    #[test]
    fn parse_instruction_accepts_label_then_instruction_same_line() {
        let ins = parse_instruction("Label: ld a, [hli]").unwrap();
        assert_eq!(ins.mnemonic, "ld");
        assert_eq!(ins.operands, vec!["a".to_string(), "[hli]".to_string()]);
    }

    #[test]
    fn parse_instruction_ignores_block_comments_and_double_colon_chaining() {
        let ins = parse_instruction("ld a, /*x*/ [hl+] :: nop").unwrap();
        assert_eq!(ins.mnemonic, "ld");
        assert_eq!(ins.operands, vec!["a".to_string(), "[hl+]".to_string()]);
    }

    #[test]
    fn split_operands_does_not_split_inside_parentheses() {
        let ins = parse_instruction("ld a, (LOW(foo, bar))");
        // `LOW` doesn't take 2 args in rgbasm, but operand splitting should remain robust.
        let ins = ins.unwrap();
        assert_eq!(ins.operands.len(), 2);
        assert_eq!(ins.operands[1], "(LOW(foo, bar))");
    }

    #[test]
    fn operand_kind_checks_cover_registers_cc_memory_and_immediates() {
        assert!(OperandCondition::IsReg8.matches("A"));
        assert!(OperandCondition::IsReg8.matches(" l "));
        assert!(!OperandCondition::IsReg8.matches("hl"));

        assert!(OperandCondition::IsReg16.matches("HL"));
        assert!(OperandCondition::IsReg16.matches("sp"));
        assert!(!OperandCondition::IsReg16.matches("af"));

        assert!(OperandCondition::IsReg16Stack.matches("AF"));
        assert!(OperandCondition::IsReg16Stack.matches("hl"));
        assert!(!OperandCondition::IsReg16Stack.matches("sp"));

        assert!(OperandCondition::IsCc.matches("z"));
        assert!(OperandCondition::IsCc.matches("NZ"));
        assert!(OperandCondition::IsCc.matches("!c"));
        assert!(!OperandCondition::IsCc.matches("po"));

        assert!(OperandCondition::IsMem.matches("[hl]"));
        assert!(OperandCondition::IsMemHl.matches("[ HL ]"));
        assert!(OperandCondition::IsMemHli.matches("[hl+]"));
        assert!(OperandCondition::IsMemHli.matches("[hli]"));
        assert!(OperandCondition::IsMemHld.matches("[hl-]"));
        assert!(OperandCondition::IsMemR16.matches("[bc]"));
        assert!(OperandCondition::IsMemC.matches("[c]"));
        assert!(!OperandCondition::IsMemC.matches("c"));

        assert!(OperandCondition::IsImm.matches("$1234"));
        assert!(OperandCondition::IsImm.matches("SomeLabel"));
        assert!(!OperandCondition::IsImm.matches("[hl]"));

        assert!(OperandCondition::IsNumberLiteral.matches("%111"));
        assert!(OperandCondition::NumberLiteralEq(10).matches("%1010"));
        assert!(OperandCondition::NumberLiteralEq(10).matches("10"));
        assert!(OperandCondition::NumberLiteralLt(16).matches("$0f"));
        assert!(OperandCondition::NumberLiteralGe(255).matches("$ff"));
        assert!(!OperandCondition::NumberLiteralGt(10).matches("10"));
        assert!(!OperandCondition::NumberLiteralEq(10).matches("Label"));
        assert!(OperandCondition::IsU3Literal.matches("7"));
        assert!(!OperandCondition::IsU3Literal.matches("8"));
        assert!(OperandCondition::IsRstVecLiteral.matches("$38"));
        assert!(!OperandCondition::IsRstVecLiteral.matches("$30+8"));
    }

    #[test]
    fn canonicalize_rgbds_operand_normalizes_hl_autoinc_and_autodec() {
        assert_eq!(canonicalize_rgbds_operand("[hl+]"), "[hli]");
        assert_eq!(canonicalize_rgbds_operand("[hli]"), "[hli]");
        assert_eq!(canonicalize_rgbds_operand("[hl-]"), "[hld]");
        assert_eq!(canonicalize_rgbds_operand("[hld]"), "[hld]");
        assert_eq!(canonicalize_rgbds_operand("[ hl + ]"), "[hli]");
        assert_eq!(canonicalize_rgbds_operand("[ hl - ]"), "[hld]");
    }

    #[test]
    fn parse_rgbds_int_handles_common_radices_and_signs() {
        assert_eq!(parse_rgbds_int("0"), Some(0));
        assert_eq!(parse_rgbds_int("-1"), Some(-1));
        assert_eq!(parse_rgbds_int("+2"), Some(2));
        assert_eq!(parse_rgbds_int("$ff"), Some(255));
        assert_eq!(parse_rgbds_int("0xFF"), Some(255));
        assert_eq!(parse_rgbds_int("%1010"), Some(10));
        assert_eq!(parse_rgbds_int("&377"), Some(255));
        assert_eq!(parse_rgbds_int("false"), Some(0));
        assert_eq!(parse_rgbds_int("true"), Some(1));
        assert_eq!(parse_rgbds_int("1_000"), Some(1000));
    }

    #[test]
    fn is_rgbds_zero_literal_accepts_multiple_spellings() {
        assert!(is_rgbds_zero_literal("0"));
        assert!(is_rgbds_zero_literal("$00"));
        assert!(is_rgbds_zero_literal("0x0"));
        assert!(is_rgbds_zero_literal("%0"));
        assert!(is_rgbds_zero_literal("&0"));
        assert!(is_rgbds_zero_literal("false"));
        assert!(!is_rgbds_zero_literal("1"));
    }

    #[test]
    fn fancy_regex_supports_lookahead_patterns() {
        let re = Arc::new(FancyRegex::new(r"foo(?=bar)").unwrap());
        assert!(re.is_match("foobar"));
        assert!(!re.is_match("foobaz"));
    }

    #[test]
    fn step_condition_covers_code_predicates_and_combinators() {
        let line = make_line("ld a, [hl+]");
        assert!(StepCondition::CodeStartsWith("ld".into()).matches(&line, &[]));
        assert!(StepCondition::CodeEndsWith("+]".into()).matches(&line, &[]));
        assert!(StepCondition::CodeContains("a".into()).matches(&line, &[]));
        assert!(StepCondition::CodeNe("nop".into()).matches(&line, &[]));

        let any = StepCondition::Any(vec![
            StepCondition::CodeEq("nop".into()),
            StepCondition::CodeStartsWith("ld".into()),
        ]);
        assert!(any.matches(&line, &[]));

        let all = StepCondition::All(vec![
            StepCondition::CodeStartsWith("ld".into()),
            StepCondition::Not(Box::new(StepCondition::CodeEq("nop".into()))),
        ]);
        assert!(all.matches(&line, &[]));
    }

    #[test]
    fn instruction_condition_checks_mnemonic_and_operands() {
        let canonical = StepCondition::Instruction(InstructionCondition {
            mnemonic: Some("ld".into()),
            operands: vec![
                OperandCondition::Eq("a".into()),
                OperandCondition::CanonEq("[hli]".into()),
            ],
            has_cc: None,
            cc_eq: None,
            cc_ne: None,
            cc_in: None,
        });
        assert!(canonical.matches(&make_line("LD a, [hl+]"), &[]));

        let zero_literal = StepCondition::Instruction(InstructionCondition {
            mnemonic: Some("ld".into()),
            operands: vec![
                OperandCondition::Eq("b".into()),
                OperandCondition::Any(vec![OperandCondition::IsZeroLiteral]),
            ],
            has_cc: None,
            cc_eq: None,
            cc_ne: None,
            cc_in: None,
        });
        assert!(zero_literal.matches(&make_line("ld b, 0"), &[]));

        let wrong_count = StepCondition::Instruction(InstructionCondition {
            mnemonic: Some("ld".into()),
            operands: vec![OperandCondition::Eq("b".into())],
            has_cc: None,
            cc_eq: None,
            cc_ne: None,
            cc_in: None,
        });
        assert!(!wrong_count.matches(&make_line("ld b, 0"), &[]));

        let wildcard_second_operand = StepCondition::Instruction(InstructionCondition {
            mnemonic: Some("add".into()),
            operands: vec![
                OperandCondition::Eq("a".into()),
                OperandCondition::AnyOperand,
            ],
            has_cc: None,
            cc_eq: None,
            cc_ne: None,
            cc_in: None,
        });
        assert!(wildcard_second_operand.matches(&make_line("add a, b"), &[]));
        assert!(wildcard_second_operand.matches(&make_line("ADD a, $12"), &[]));
        assert!(!wildcard_second_operand.matches(&make_line("add a"), &[]));
    }

    #[test]
    fn inc_dec_same_target_as_prev_ld_matches_expected_target() {
        let prev = vec![make_line("ld hl, $c000")];
        let current = make_line("inc hl");
        let cond = StepCondition::IncDecSameTargetAsPrevLd { prev_step: 0 };
        assert!(cond.matches(&current, &prev));

        let wrong_prev = vec![make_line("nop")];
        assert!(!cond.matches(&current, &wrong_prev));

        let not_inc_dec = make_line("add hl, bc");
        assert!(!cond.matches(&not_inc_dec, &prev));
    }

    #[test]
    fn string_expr_chain_applies_transforms_for_str_eq() {
        let prev = make_line("add a, hl");
        let current = make_line("ld a, hl");

        let left = StringExpr {
            base: StringBase::Current(StringField::Code),
            transforms: vec![
                StringTransform::AfterPrefix("ld ".into()),
                StringTransform::BeforeComma,
                StringTransform::Trim,
            ],
        };

        let right = StringExpr {
            base: StringBase::Prev {
                idx: 0,
                field: StringField::Code,
            },
            transforms: vec![
                StringTransform::AfterPrefix("add ".into()),
                StringTransform::BeforeComma,
                StringTransform::Trim,
            ],
        };

        let cond = StepCondition::StrEq { left, right };
        assert!(cond.matches(&current, std::slice::from_ref(&prev)));

        let symbol = StringExpr {
            base: StringBase::Current(StringField::Code),
            transforms: vec![
                StringTransform::StripTrailingColon,
                StringTransform::SymbolLike,
            ],
        };
        assert_eq!(
            symbol.eval(&make_line("Label::"), &[]),
            Some("Label".to_string())
        );
    }

    #[test]
    fn string_expr_supports_instruction_operands_cc_and_root() {
        let op1 = StringExpr {
            base: StringBase::Current(StringField::InstructionOperand { idx: 0 }),
            transforms: vec![],
        };
        let op2 = StringExpr {
            base: StringBase::Current(StringField::InstructionOperand { idx: 1 }),
            transforms: vec![],
        };
        assert_eq!(op1.eval(&make_line("ld b, b"), &[]), Some("b".into()));
        assert_eq!(op2.eval(&make_line("ld b, b"), &[]), Some("b".into()));

        let cc = StringExpr {
            base: StringBase::Current(StringField::InstructionCc),
            transforms: vec![],
        };
        let root = StringExpr {
            base: StringBase::Current(StringField::InstructionRoot),
            transforms: vec![],
        };

        assert_eq!(cc.eval(&make_line("ld c, a"), &[]), None);
        assert_eq!(root.eval(&make_line("ld c, a"), &[]), Some("c".into()));

        // Condition codes are treated as instruction metadata, not operands.
        assert_eq!(op1.eval(&make_line("jr nz, Foo"), &[]), Some("Foo".into()));
        assert_eq!(op2.eval(&make_line("jr nz, Foo"), &[]), None);

        assert_eq!(cc.eval(&make_line("jr nz, Foo"), &[]), Some("nz".into()));
        assert_eq!(root.eval(&make_line("jr nz, Foo"), &[]), Some("Foo".into()));

        assert_eq!(cc.eval(&make_line("jr Foo"), &[]), None);
        assert_eq!(root.eval(&make_line("jr Foo"), &[]), Some("Foo".into()));

        assert_eq!(cc.eval(&make_line("ret z"), &[]), Some("z".into()));
        assert_eq!(root.eval(&make_line("ret z"), &[]), None);

        let op1_lower = StringExpr {
            base: StringBase::Current(StringField::InstructionOperand { idx: 0 }),
            transforms: vec![StringTransform::Lower],
        };
        let op2_lower = StringExpr {
            base: StringBase::Current(StringField::InstructionOperand { idx: 1 }),
            transforms: vec![StringTransform::Lower],
        };
        let cond = StepCondition::StrEq {
            left: op1_lower,
            right: op2_lower,
        };
        assert!(cond.matches(&make_line("ld a, A"), &[]));
    }
}
