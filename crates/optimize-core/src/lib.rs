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

pub fn preprocess_properties(contents: &str) -> Vec<Line> {
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

        if is_label_like(code_trimmed_right) {
            cur_label = code_trimmed_right.to_string();
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

fn is_label_like(code: &str) -> bool {
    let mut chars = code.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first.is_ascii_alphabetic() || first == '_') && code.contains(':')
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
}
