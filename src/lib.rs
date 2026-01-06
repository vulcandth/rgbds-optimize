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

    let (mnemonic_raw, rest) = match code.split_once(' ') {
        Some((m, r)) => (m, r.trim()),
        None => (code, ""),
    };

    let mnemonic = mnemonic_raw.to_ascii_lowercase();
    let operands = if rest.is_empty() {
        Vec::new()
    } else {
        split_operands(rest)
            .into_iter()
            .map(|op| op.trim().to_string())
            .filter(|op| !op.is_empty())
            .collect()
    };

    Some(Instruction { mnemonic, operands })
}

fn split_operands(s: &str) -> Vec<&str> {
    // Split by commas, but keep bracketed expressions intact.
    // This is intentionally minimal: it does not attempt to parse strings or macros.
    let mut out = Vec::new();
    let mut depth: i32 = 0;
    let mut start = 0usize;
    for (i, ch) in s.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            ',' if depth == 0 => {
                out.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    out.push(&s[start..]);
    out
}

pub fn canonicalize_rgbds_operand(op: &str) -> String {
    // Normalize common RGBDS synonyms so structural matching can treat them as equivalent.
    // Keep this conservative; expand later as patterns require.
    let lowered = op.trim().to_ascii_lowercase();

    // RGBDS is fairly permissive about whitespace; make bracketed operands robust.
    let lowered = if lowered.starts_with('[') && lowered.ends_with(']') {
        lowered
            .chars()
            .filter(|c| !c.is_ascii_whitespace())
            .collect()
    } else {
        lowered
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
}

#[derive(Clone, Debug)]
pub enum OperandCondition {
    Eq(String),
    CanonEq(String),
    IsZeroLiteral,
    Any(Vec<OperandCondition>),
}

impl OperandCondition {
    fn matches(&self, operand: &str) -> bool {
        match self {
            OperandCondition::Eq(want) => operand.eq_ignore_ascii_case(want),
            OperandCondition::CanonEq(want) => canonicalize_rgbds_operand(operand) == *want,
            OperandCondition::IsZeroLiteral => is_rgbds_zero_literal(operand),
            OperandCondition::Any(options) => options.iter().any(|opt| opt.matches(operand)),
        }
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

        if !self.operands.is_empty() {
            if ins.operands.len() != self.operands.len() {
                return false;
            }
            for (op, cond) in ins.operands.iter().zip(self.operands.iter()) {
                if !cond.matches(op) {
                    return false;
                }
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
}

#[derive(Clone, Debug)]
pub struct StringExpr {
    pub base: StringBase,
    pub transforms: Vec<StringTransform>,
}

impl StringExpr {
    pub fn eval<'a>(&self, current: &'a Line, prev: &'a [Line]) -> Option<String> {
        let mut s: String = match &self.base {
            StringBase::Current(field) => field.get(current).to_string(),
            StringBase::Prev { idx, field } => field.get(prev.get(*idx)?).to_string(),
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
                    s = s.split_once(',').map(|(lhs, _)| lhs.trim()).unwrap_or("").to_string();
                }
                StringTransform::AfterComma => {
                    s = split_after_comma(&s).unwrap_or("").to_string();
                }
                StringTransform::AfterCommaRaw => {
                    s = s.split_once(',').map(|(_, rhs)| rhs).unwrap_or("").to_string();
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
            }
        }

        Some(s)
    }
}

impl StringField {
    fn get<'a>(&self, line: &'a Line) -> &'a str {
        match self {
            StringField::Code => &line.code,
            StringField::Text => &line.text,
            StringField::Context => &line.context,
            StringField::Comment => &line.comment,
            StringField::CommentLower => &line.comment_lower,
        }
    }
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
}
