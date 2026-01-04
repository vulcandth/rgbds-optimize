#![forbid(unsafe_code)]

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
    match op.to_ascii_lowercase().as_str() {
        "[hli]" | "[hl+]" => "[hli]".to_string(),
        "[hld]" | "[hl-]" => "[hld]".to_string(),
        other => other.to_string(),
    }
}

pub type ConditionFn = fn(&Line, &[Line]) -> bool;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PatternStep {
    pub rewind: Option<usize>,
    pub condition: ConditionFn,
}

impl PatternStep {
    pub fn new(condition: ConditionFn) -> Self {
        Self {
            rewind: None,
            condition,
        }
    }

    pub fn with_rewind(rewind: usize, condition: ConditionFn) -> Self {
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

        let step = steps[state];
        let skip = cur_line.comment_lower.starts_with(&suppress_prefix);

        if !skip && (step.condition)(cur_line, &prev_lines) {
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
                if prev_lines.len() >= rewind {
                    prev_lines.truncate(prev_lines.len() - rewind);
                } else {
                    prev_lines.clear();
                }
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

    fn is_a(line: &Line, _prev: &[Line]) -> bool {
        line.code == "a"
    }

    fn is_b(line: &Line, _prev: &[Line]) -> bool {
        line.code == "b"
    }

    #[test]
    fn pattern_engine_finds_consecutive_matches() {
        let src = "Label:\n  a\n  b\n";
        let lines = preprocess_properties(src);
        let steps = [PatternStep::new(is_a), PatternStep::new(is_b)];
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
        // Input: a, a, b should yield one match (a, b) starting at the second "a".
        let src = "Label:\n  a\n  a\n  b\n";
        let lines = preprocess_properties(src);
        let steps = [PatternStep::new(is_a), PatternStep::with_rewind(1, is_b)];
        let got = find_pattern_instances("file.asm", &lines, "Test", &steps);
        assert_eq!(got.len(), 1);
        assert_eq!(got[0].lines[0].code, "a");
        assert_eq!(got[0].lines[1].code, "b");
    }

    #[test]
    fn pattern_engine_honors_suppression_comment() {
        let src = "Label:\n  a ; no-optimize test\n  b\n";
        let lines = preprocess_properties(src);
        let steps = [PatternStep::new(is_a), PatternStep::new(is_b)];
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
    }
}
