use crate::{FancyRegex as Regex, Line, PatternStep};
use std::sync::LazyLock;

fn is_volatile(code: &str) -> bool {
    [
        "[hli]", "[hld]", "[hl+]", "[hl-]", "[rJOYP]", "[rBGPD]", "[rOBPD]",
    ]
    .iter()
    .any(|r| code.contains(r))
}

fn is_not_really_hram(code: &str) -> bool {
    [
        "rROMB",
        "rROMB0",
        "rROMB1",
        "rRAMG",
        "rRAMB",
        "rRTCLATCH",
        "rRTCREG",
    ]
    .iter()
    .any(|r| code.contains(r))
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

fn byte_at(s: &str, idx: usize) -> Option<u8> {
    s.as_bytes().get(idx).copied()
}

fn pair_reg(b: u8) -> Option<u8> {
    match b {
        b'a' => Some(b'f'),
        b'f' => Some(b'a'),
        b'b' => Some(b'c'),
        b'c' => Some(b'b'),
        b'd' => Some(b'e'),
        b'e' => Some(b'd'),
        b'h' => Some(b'l'),
        b'l' => Some(b'h'),
        b'A' => Some(b'F'),
        b'F' => Some(b'A'),
        b'B' => Some(b'C'),
        b'C' => Some(b'B'),
        b'D' => Some(b'E'),
        b'E' => Some(b'D'),
        b'H' => Some(b'L'),
        b'L' => Some(b'H'),
        _ => None,
    }
}

fn is_label_definition_line(line: &Line) -> bool {
    matches!(
        line.text.chars().next(),
        Some(c) if c.is_ascii_alphabetic() || c == '_' || c == '.'
    ) && !line.code.contains(' ')
        && !matches!(
            line.code.to_ascii_lowercase().as_str(),
            "endc" | "endr" | "endm"
        )
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

static REDUNDANT_ARGUMENTS_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:add|adc|sub|sbc|and|xor|or|cp) a,").unwrap());
static NO_OP_ADD_SUB_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:add|sub) (?:a, )?(?:[%\$&]?0+|FALSE)$").unwrap());
static INEFFICIENT_HRAM_LOAD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld a, \[[hr][^l]").unwrap());
static INEFFICIENT_HRAM_STORE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld \[[hr][^l]").unwrap());
static A_EQ_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld a, (?:[%\$&]?0+|FALSE)$").unwrap());
static A_INC_DEC_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:add|sub) (?:a, )?[%\$&]?0*1$").unwrap());
static A_NOT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^xor (?:255|-[\$%&]?0*1|\$[Ff][Ff]|%11111111|&377)$").unwrap());
static LD_R_A_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld [bcdehl], a").unwrap());
static LD_A_IMM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld a, [^afbcdehl\[]").unwrap());
static SUB_R_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^sub [bcdehl]").unwrap());
static JUMP_NC_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^(jr|jp|jmp) nc,").unwrap());
static JUMP_C_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^(jr|jp|jmp) c,").unwrap());
static JUMP_NC_OR_C_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(jr|jp|jmp) n?c,").unwrap());
static JUMP_NZ_OR_ZC_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(jr|jp|jmp) n?[zc],").unwrap());
static JR_JP_JMP_ANY_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^(jr|jp|jmp) ").unwrap());
static SRL_A_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^srl a$").unwrap());
static LD_DST_A_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld ([bcdehl]|\[hl\]), a").unwrap());
static ADC_SBC_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(adc|sbc) [%\$&]?0+$").unwrap());
static LD_A_0_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld a, [%\$&]?0+$").unwrap());
static AND_IMM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^and (?:a, )?[^afbcdehl\[]").unwrap());
static CP_IMM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^cp (?:a, )?[^afbcdehl\[]").unwrap());
static LD_RH_A_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld [bcdehl], a$").unwrap());
static LD_A_RH_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld a, [bcdehl]$").unwrap());
static OR_R_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^or (?:a, )?[bcdehl]$").unwrap());
static LDH_R_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ldh? [abcdehl],").unwrap());

static ADD_A_LCE_OR_IMM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^add (?:a, )?(?:[lce]|[^afbdh\[])").unwrap());
static LD_LCE_A_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld [lce], a").unwrap());
static LD_A_HBD_OR_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld a, (?:[hbd]|[%\$&]?0+$)").unwrap());
static LD_A_BCDEHL_OR_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld a, (?:[bcdehl]|[%\$&]?0+$)").unwrap());
static ADC_BCDEHL_OR_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^adc (?:[bcdehl]|[%\$&]?0+$)").unwrap());
static SBC_BCDEHL_OR_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^sbc (?:[bcdehl]|[%\$&]?0+$)").unwrap());
static ADC_HBD_OR_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^adc (?:[hbd]|[%\$&]?0+$)").unwrap());
static LD_HBD_A_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld [hbd], a").unwrap());
static LD_HBD_0_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld [hbd], [%\$&]?0+$").unwrap());

static LD_PAIR_IMM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld (?:hl|bc|de), [^\[]").unwrap());
static ADD_LCE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^add (?:a, )?(?:[lce])$").unwrap());
static ADC_HBD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^adc (?:a, )?(?:[hbd])$").unwrap());
static SUB_LCE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^sub (?:a, )?(?:[lce])$").unwrap());

static LD_A_MEM_NOT_HBD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld a, \[[^hbd]").unwrap());
static LD_LH_A_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld [lh], a$").unwrap());

static LD_MEMHL_IMM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld \[hl\], [^afbcdehl\[]").unwrap());
static LD_MEMHL_R_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld \[hl\], [bcdehl]$").unwrap());
static LD_R_MEMHL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld [bcde], \[hl\]$").unwrap());

static LD_R_IMM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld [bcdehl], [^afbcdehl\[]").unwrap());
static LD_HL_IMM_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld hl, [^\[]").unwrap());
static LD_BC_IMM_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld bc, [^\[]").unwrap());

static LD_HL_IMM_OR_POP_HL_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:ld hl,|pop hl)").unwrap());
static POINTLESS_HLI_HLD_STEP1_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:ld a, \[hl[-+id]\]|ld \[hl[-+id]\], a)$").unwrap());
static CONTROL_FLOW_PREFIX_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(jr|jp|jmp|call|rst|ret|predef)").unwrap());
static WORD_HL_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r".*\bhl\b").unwrap());

static POINTLESS_JUMPS_STEP1_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(jr|jp|jmp|jump|sjump|jumpchannel|sound_jump) ").unwrap());

static CP_OR_0_OR_AND_FF_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(?:cp|or) [%\$&]?0+$|^and (?:255|-1|\$[Ff][Ff]|%11111111|&377)$").unwrap()
});
static CP_1_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^cp [%\$&]?0*1$").unwrap());

static JR_NZ_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^(jr|jp|jmp) nz,").unwrap());
static LD_ANY_0_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld .+, [%\$&]?0+$").unwrap());
static LD_BCDE_IMM_NOT_HBD_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld (?:bc|de), [^hbd]").unwrap());
static LD_ANY_IMM_NOT_REG_OR_MEM_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld .+, [^afbcdehl\[]").unwrap());

static PREFIX_A_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:rl|rlc|rr|rrc) a$").unwrap());

static AND_OR_XOR_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^(?:and|or|xor) ").unwrap());
static AND_OR_A_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:and a|or a|and a, a|or a, a)$").unwrap());

static AFFECTS_ZC_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(?:rlca|rrca|rla|rra|daa|pop af|add |adc |sub |sbc |and |or |xor |cp |rlc |rrc |rl |rr |sla |sra |swap |srl |ld hl, sp|ldhl sp)").unwrap()
});

static RET_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ret(?: .+)?$").unwrap());
static CALL_RST_ADDNTIMES_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^(?:call|rst) AddNTimes").unwrap());

static LD_WRAM_LOAD_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^ld a, \[w").unwrap());
static LD_WRAM_STORE_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^ld \[w.*?\], a").unwrap());

static TEXT_TRAILING_SPACE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"^(?:text|next1?|line|page|para|cont|prompt)\s*"[^"]* ""#).unwrap()
});
static TEXT_COMMAND_FOLLOWS_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"^(?:text_|sound_|start_asm|deciram|interpret_data|limited_interpret_data|link_wait_button|current_day|stop_compressing_text)").unwrap()
});

fn cond_no_op_ld_python(line: &Line, _prev: &[Line]) -> bool {
    // Python regex: r"ld ([abcdehl]), \1$" (note: requires ", ")
    let code = line.code.as_bytes();
    if !line.code.starts_with("ld ") {
        return false;
    }
    if code.len() != 7 {
        // "ld x, x" is always 7 bytes.
        return false;
    }
    let reg = code[3];
    if !matches!(reg, b'a' | b'b' | b'c' | b'd' | b'e' | b'h' | b'l') {
        return false;
    }
    code[4] == b',' && code[5] == b' ' && code[6] == reg
}

fn cond_inefficient_hram_load(line: &Line, _prev: &[Line]) -> bool {
    INEFFICIENT_HRAM_LOAD_RE.is_match(&line.code) && !is_not_really_hram(&line.code)
}

fn cond_inefficient_hram_store(line: &Line, _prev: &[Line]) -> bool {
    INEFFICIENT_HRAM_STORE_RE.is_match(&line.code)
        && line.code.ends_with(", a")
        && !is_not_really_hram(&line.code)
}

fn cond_a_eq_n_minus_a_step3(line: &Line, prev: &[Line]) -> bool {
    if !SUB_R_RE.is_match(&line.code) {
        return false;
    }
    let Some(prev0) = prev.first() else {
        return false;
    };
    byte_at(&line.code, 4).is_some_and(|b| byte_at(&prev0.code, 3).is_some_and(|p| b == p))
}

fn cond_a_carry_pq_step3(line: &Line, _prev: &[Line]) -> bool {
    LD_A_IMM_RE.is_match(&line.code) || matches!(line.code.as_str(), "xor a" | "inc a" | "dec a")
}

fn cond_jump_target_label(line: &Line, prev: &[Line], jump_idx: usize) -> bool {
    let Some(jump) = prev.get(jump_idx) else {
        return false;
    };
    let Some(target) = split_after_comma(&jump.code) else {
        return false;
    };
    strip_trailing_colon(&line.code) == target
}

fn cond_a_eq_x_plusminus_carry_step2(line: &Line, _prev: &[Line]) -> bool {
    if !line.code.starts_with("ld a,") {
        return false;
    }
    if !line.code.starts_with("ld a, [") {
        return true;
    }
    line.code == "ld a, [hl]"
}

fn cond_a_eq_carry_plusminus_x_step3(line: &Line, _prev: &[Line]) -> bool {
    if !(line.code.starts_with("adc ") || line.code.starts_with("sbc ")) {
        return false;
    }
    if !(line.code.starts_with("adc [") || line.code.starts_with("sbc [")) {
        return true;
    }
    matches!(line.code.as_str(), "adc [hl]" | "sbc [hl]")
}

fn cond_and_cp_same_operand(line: &Line, prev: &[Line]) -> bool {
    if !CP_IMM_RE.is_match(&line.code) {
        return false;
    }
    let Some(p0) = prev.first() else {
        return false;
    };
    let Some(and_rhs) = p0.code.get(4..) else {
        return false;
    };
    let Some(cp_rhs) = line.code.get(3..) else {
        return false;
    };
    and_rhs == cp_rhs
}

fn cond_mask_or_step5(line: &Line, prev: &[Line]) -> bool {
    if !OR_R_RE.is_match(&line.code) {
        return false;
    }
    let Some(p1) = prev.get(1) else {
        return false;
    };
    let Some(dst) = byte_at(&p1.code, 3) else {
        return false;
    };
    let Some(last) = line.code.as_bytes().last().copied() else {
        return false;
    };
    dst == last
}

fn cond_pair_add_step2(line: &Line, prev: &[Line]) -> bool {
    if !LD_LCE_A_RE.is_match(&line.code) {
        return false;
    }
    let Some(op) = prev[0]
        .code
        .replace("add a,", "add")
        .as_bytes()
        .get(4)
        .copied()
    else {
        return false;
    };
    let Some(dst) = byte_at(&line.code, 3) else {
        return false;
    };
    if matches!(op, b'l' | b'c' | b'e') {
        dst == op
    } else {
        true
    }
}

fn cond_pair_add_step3(line: &Line, prev: &[Line]) -> bool {
    if !LD_A_HBD_OR_0_RE.is_match(&line.code) {
        return false;
    }
    let Some(src) = byte_at(&line.code, 6) else {
        return false;
    };
    if matches!(src, b'h' | b'b' | b'd') {
        let Some(dst) = byte_at(&prev[1].code, 3) else {
            return false;
        };
        pair_reg(dst).is_some_and(|p| p == src)
    } else {
        true
    }
}

fn cond_pair_add_step4(line: &Line, prev: &[Line]) -> bool {
    if !ADC_HBD_OR_0_RE.is_match(&line.code) {
        return false;
    }
    let Some(src) = byte_at(&line.code, 4) else {
        return false;
    };
    if matches!(src, b'h' | b'b' | b'd') {
        let Some(dst) = byte_at(&prev[1].code, 3) else {
            return false;
        };
        pair_reg(dst).is_some_and(|p| p == src)
    } else {
        true
    }
}

fn cond_pair_add_step5(line: &Line, prev: &[Line]) -> bool {
    if !LD_HBD_A_RE.is_match(&line.code) {
        return false;
    }
    let Some(dst) = byte_at(&prev[1].code, 3) else {
        return false;
    };
    let Some(expected) = pair_reg(dst) else {
        return false;
    };
    byte_at(&line.code, 3).is_some_and(|b| b == expected)
}

fn cond_pair_add_jump_step4(line: &Line, prev: &[Line]) -> bool {
    if !line.code.starts_with("inc ") {
        return false;
    }
    let Some(dst) = byte_at(&prev[1].code, 3) else {
        return false;
    };
    let Some(expected) = pair_reg(dst) else {
        return false;
    };
    byte_at(&line.code, 4).is_some_and(|b| b == expected)
}

fn cond_pair_eq_foo_plus_a_step2(line: &Line, prev: &[Line]) -> bool {
    if !ADD_LCE_RE.is_match(&line.code) {
        return false;
    }
    byte_at(&line.code, line.code.len().saturating_sub(1)) == byte_at(&prev[0].code, 4)
}

fn cond_pair_eq_foo_plus_a_step3(line: &Line, prev: &[Line]) -> bool {
    if !LD_LCE_A_RE.is_match(&line.code) {
        return false;
    }
    byte_at(&line.code, 3) == byte_at(&prev[0].code, 4)
}

fn cond_pair_eq_foo_plus_a_step4(line: &Line, prev: &[Line]) -> bool {
    if !ADC_HBD_RE.is_match(&line.code) {
        return false;
    }
    byte_at(&line.code, line.code.len().saturating_sub(1)) == byte_at(&prev[0].code, 3)
}

fn cond_pair_eq_foo_plus_a_step5(line: &Line, prev: &[Line]) -> bool {
    if !SUB_LCE_RE.is_match(&line.code) {
        return false;
    }
    byte_at(&line.code, line.code.len().saturating_sub(1)) == byte_at(&prev[0].code, 4)
}

fn cond_pair_eq_foo_plus_a_step6(line: &Line, prev: &[Line]) -> bool {
    if !LD_HBD_A_RE.is_match(&line.code) {
        return false;
    }
    byte_at(&line.code, 3) == byte_at(&prev[0].code, 3)
}

fn cond_reg_plus_minus_carry_step3(line: &Line, prev: &[Line]) -> bool {
    if !line.code.starts_with("ld ") || !line.code.ends_with(", a") {
        return false;
    }
    let Some(dst) = byte_at(&line.code, 3) else {
        return false;
    };
    let Some(p0) = byte_at(&prev[0].code, 6) else {
        return false;
    };
    let Some(p1) = byte_at(&prev[1].code, 4) else {
        return false;
    };
    if !(dst == p0 || dst == p1) {
        return false;
    }
    prev[0].code.ends_with('0') || prev[1].code.ends_with('0')
}

fn cond_add_pair_self(code: &str) -> Option<&str> {
    // Python regex: r"add (hl|bc|de), \1$" (requires ", ")
    let (lhs, rhs) = code.strip_prefix("add ")?.split_once(", ")?;
    matches!(lhs, "hl" | "bc" | "de").then_some(())?;
    (rhs == lhs).then_some(lhs)
}

fn cond_pair_eq_a_mul_16_add(line: &Line, prev: &[Line]) -> bool {
    let Some(lhs_pair) = cond_add_pair_self(&line.code) else {
        return false;
    };

    let Some(first) = prev.first() else {
        return false;
    };
    let Some(r) = byte_at(&first.code, 3) else {
        return false;
    };
    match lhs_pair {
        "hl" => matches!(r, b'h' | b'l' | b'H' | b'L'),
        "bc" => matches!(r, b'b' | b'c' | b'B' | b'C'),
        "de" => matches!(r, b'd' | b'e' | b'D' | b'E'),
        _ => false,
    }
}

fn cond_pair_eq_a_mul_16_step2(line: &Line, prev: &[Line]) -> bool {
    let ok = (LD_LCE_A_RE.is_match(&line.code) || LD_HBD_0_RE.is_match(&line.code))
        && line.code != prev[0].code;
    if !ok {
        return false;
    }

    let Some(dst0) = byte_at(&prev[0].code, 3) else {
        return false;
    };
    let Some(dst1) = byte_at(&line.code, 3) else {
        return false;
    };
    pair_reg(dst0).is_some_and(|p| p == dst1)
}

fn cond_pair_eq_deref_foo_step3(line: &Line, prev: &[Line]) -> bool {
    if !LD_A_MEM_NOT_HBD_RE.is_match(&line.code) {
        return false;
    }

    let a1 = extract_symbol_like(prev[0].code.get(7..).unwrap_or(""));
    let a3 = extract_symbol_like(line.code.get(7..).unwrap_or(""));
    !a1.is_empty() && a1 == a3
}

fn cond_pair_load_pq_step2(line: &Line, prev: &[Line]) -> bool {
    if !LD_R_IMM_RE.is_match(&line.code) {
        return false;
    }
    if line.context != prev[0].context {
        return false;
    }
    let Some(dst0) = byte_at(&prev[0].code, 3) else {
        return false;
    };
    let Some(dst1) = byte_at(&line.code, 3) else {
        return false;
    };
    pair_reg(dst0).is_some_and(|p| p == dst1)
}

fn cond_a_eq_0_cmp(line: &Line, _prev: &[Line]) -> bool {
    CP_OR_0_OR_AND_FF_RE.is_match(&line.code)
}

fn cond_tail_call(line: &Line, _prev: &[Line]) -> bool {
    line.code.starts_with("call ") && !line.code.contains(',')
}

fn cond_tail_farcall(line: &Line, _prev: &[Line]) -> bool {
    line.code.starts_with("farcall ") && !line.code.contains(',')
}

fn cond_tail_predef(line: &Line, _prev: &[Line]) -> bool {
    line.code.starts_with("predef ")
}

fn cond_fallthrough_label(line: &Line, prev: &[Line]) -> bool {
    let Some(target) = prev[0].code.strip_prefix("call ") else {
        return false;
    };
    strip_trailing_colon(&line.code) == target.trim()
}

fn cond_conditional_call_step3(line: &Line, prev: &[Line]) -> bool {
    if JR_JP_JMP_ANY_RE.is_match(&line.code) && !line.code.contains(',') {
        let Some(target) = last_token(&line.code) else {
            return false;
        };
        let Some(jump_target) = split_after_comma(&prev[0].code) else {
            return false;
        };
        return target == jump_target;
    }

    strip_trailing_colon(&line.code) == split_after_comma(&prev[0].code).unwrap_or("")
}

fn cond_call_hl_step2(line: &Line, prev: &[Line]) -> bool {
    if !matches!(line.code.as_str(), "push bc" | "push de") {
        return false;
    }
    line.code.get(5..7) == prev[0].code.get(3..5)
}

fn cond_pointless_hli_hld_step2(line: &Line, _prev: &[Line]) -> bool {
    !CONTROL_FLOW_PREFIX_RE.is_match(&line.code) && !WORD_HL_RE.is_match(&line.code)
}

fn cond_pointless_jumps_step2(line: &Line, prev: &[Line]) -> bool {
    let Some(target) = last_token(&prev[0].code) else {
        return false;
    };
    strip_trailing_colon(&line.code) == target
        && (line.context == prev[0].context || line.context == line.code)
}

fn cond_useless_loads_step1(line: &Line, _prev: &[Line]) -> bool {
    (line.code.starts_with("ld ") || line.code.starts_with("ldh "))
        && line.code.contains(',')
        && !is_volatile(&line.code)
}

fn cond_useless_loads_step2(line: &Line, prev: &[Line]) -> bool {
    if !(line.code.starts_with("ld ") || line.code.starts_with("ldh ")) || !line.code.contains(',')
    {
        return false;
    }
    if matches!(line.code.as_str(), "ld h, [hl]" | "ld l, [hl]") {
        return false;
    }
    let left = line.code.split(',').next().unwrap_or("");
    let prev_left = prev[0].code.split(',').next().unwrap_or("");
    left == prev_left
}

fn cond_redundant_loads_step2(line: &Line, prev: &[Line]) -> bool {
    if !(line.code.starts_with("ld ") || line.code.starts_with("ldh ")) || !line.code.contains(',')
    {
        return false;
    }

    // Match optimize.py semantics:
    // - Compare destination as `code[3:].split(',')[0].strip()` (works for both `ld` and `ldh`)
    // - Compare source as `code.split(',')[1].strip()`
    let prev_dst = prev[0]
        .code
        .get(3..)
        .unwrap_or("")
        .split(',')
        .next()
        .unwrap_or("")
        .trim();
    let prev_src = prev[0]
        .code
        .split(',')
        .nth(1)
        .map(|s| s.trim())
        .unwrap_or("");

    let dst = line
        .code
        .get(3..)
        .unwrap_or("")
        .split(',')
        .next()
        .unwrap_or("")
        .trim();
    let src = line.code.split(',').nth(1).map(|s| s.trim()).unwrap_or("");

    dst == prev_src && src == prev_dst && line.context == prev[0].context
}

fn cond_similar_loads_step1(line: &Line, _prev: &[Line]) -> bool {
    if !(line.code.starts_with("ld ") || line.code.starts_with("ldh ")) || !line.code.contains(',')
    {
        return false;
    }
    if is_volatile(&line.code) {
        return false;
    }
    let src = line.code.split(',').nth(1).map(|s| s.trim()).unwrap_or("");
    !matches!(src, "a" | "f" | "b" | "c" | "d" | "e" | "h" | "l")
}

fn cond_similar_loads_step2(line: &Line, prev: &[Line]) -> bool {
    if !line.code.starts_with("ld a,") {
        return false;
    }
    line.code.split(',').nth(1) == prev[0].code.split(',').nth(1)
}

fn cond_redundant_inc_dec_step2(line: &Line, prev: &[Line]) -> bool {
    if !(line.code.starts_with("inc ") || line.code.starts_with("dec ")) {
        return false;
    }
    let target = line.code.get(4..).unwrap_or("").trim();
    let prev_dst = prev[0]
        .code
        .split(',')
        .next()
        .unwrap_or("")
        .get(2..)
        .unwrap_or("")
        .trim();
    target == prev_dst
}

fn cond_pair_three_step3(line: &Line, prev: &[Line]) -> bool {
    // optimize.py uses a negative lookahead. We keep a manual check to avoid backtracking overhead
    // in this hot path even though the regex engine now supports it.
    let Some(rest) = line.code.strip_prefix("add ") else {
        return false;
    };
    let Some((lhs, rhs)) = rest.split_once(", ") else {
        return false;
    };
    if !matches!(lhs, "hl" | "bc" | "de") {
        return false;
    }
    if rhs.starts_with("hl") || rhs.starts_with("bc") || rhs.starts_with("de") {
        return false;
    }
    byte_at(&line.code, 4) == byte_at(&prev[0].code, 3)
        && byte_at(&line.code, 8) == byte_at(&prev[1].code, 3)
}

fn cond_pair_dotdot_step2(line: &Line, prev: &[Line]) -> bool {
    if CONTROL_FLOW_PREFIX_RE.is_match(&line.code) {
        return false;
    }
    if line.code.get(..5) == prev[0].code.get(..5) {
        return false;
    }
    let pair = prev[0].code.get(3..5).unwrap_or("");
    if line.code.contains(&format!("[{pair}")) {
        return false;
    }
    if line.code.starts_with(&format!("push {pair}"))
        || line.code.starts_with(&format!("pop {pair}"))
    {
        return false;
    }
    !(line.code.starts_with('.') || line.code.ends_with(':'))
}

fn cond_pair_dotdot_incdec(line: &Line, prev: &[Line]) -> bool {
    if !(line.code.starts_with("inc ") || line.code.starts_with("dec ")) {
        return false;
    }
    let Some(dst) = byte_at(&prev[0].code, 3) else {
        return false;
    };
    byte_at(&line.code, 4).is_some_and(|b| b == dst)
}

fn cond_dec_a_then_addntimes_step2(line: &Line, _prev: &[Line]) -> bool {
    LD_BC_IMM_RE.is_match(&line.code) || line.code == "dec a"
}

fn cond_dec_a_then_addntimes_step3(line: &Line, prev: &[Line]) -> bool {
    if !(LD_BC_IMM_RE.is_match(&line.code) || line.code == "dec a") {
        return false;
    }
    (line.code == "dec a") != (prev[1].code == "dec a")
}

fn cond_redundant_ret_step2(line: &Line, prev: &[Line]) -> bool {
    if !RET_RE.is_match(&line.code) {
        return false;
    }
    if line.code == "ret" {
        return true;
    }
    if prev[0].code == "ret" {
        return true;
    }
    let prev_cc = prev[0]
        .code
        .split_whitespace()
        .last()
        .unwrap_or("")
        .trim_start_matches('n');
    let cur_cc = line
        .code
        .split_whitespace()
        .last()
        .unwrap_or("")
        .trim_start_matches('n');
    cur_cc == prev_cc
}

fn cond_wram_inc_dec_step3(line: &Line, prev: &[Line]) -> bool {
    if !LD_WRAM_STORE_RE.is_match(&line.code) {
        return false;
    }
    let lhs = line
        .code
        .split(", ")
        .next()
        .unwrap_or("")
        .trim_start_matches("ld ");
    let rhs = prev[0].code.split(", ").last().unwrap_or("");
    lhs == rhs
}

pub(crate) fn steps(builtin: &str) -> Option<Vec<PatternStep>> {
    match builtin {
        "py_redundant_arguments" => Some(vec![PatternStep::regex(&REDUNDANT_ARGUMENTS_RE)]),
        "py_nops" => Some(vec![
            PatternStep::new(|line, _| line.code != "halt"),
            PatternStep::new(|line, _| line.code == "nop"),
        ]),
        "py_no_op_ld" => Some(vec![PatternStep::new(cond_no_op_ld_python)]),
        "py_no_op_add_sub" => Some(vec![PatternStep::regex(&NO_OP_ADD_SUB_RE)]),
        "py_inefficient_hram_load" => Some(vec![PatternStep::new(cond_inefficient_hram_load)]),
        "py_inefficient_hram_store" => Some(vec![PatternStep::new(cond_inefficient_hram_store)]),
        "py_a_eq_0" => Some(vec![PatternStep::regex(&A_EQ_0_RE)]),
        "py_a_inc_dec" => Some(vec![PatternStep::regex(&A_INC_DEC_RE)]),
        "py_a_times_2" => Some(vec![PatternStep::new(|line, _| line.code == "sla a")]),
        "py_a_not" => Some(vec![PatternStep::regex(&A_NOT_RE)]),
        "py_a_eq_n_minus_a" => Some(vec![
            PatternStep::regex(&LD_R_A_RE),
            PatternStep::regex(&LD_A_IMM_RE),
            PatternStep::new(cond_a_eq_n_minus_a_step3),
        ]),
        "py_a_eq_carry_pq" => Some(vec![
            PatternStep::regex(&LD_A_IMM_RE),
            PatternStep::regex(&JUMP_NC_OR_C_RE),
            PatternStep::new(cond_a_carry_pq_step3),
            PatternStep::new(|line, prev| cond_jump_target_label(line, prev, 1)),
        ]),
        "py_a_inc_dec_if_carry" => Some(vec![
            PatternStep::regex(&JUMP_NC_RE),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc a" | "dec a")),
            PatternStep::with_rewind(1, |line, prev| cond_jump_target_label(line, prev, 0)),
        ]),
        "py_a_inc_dec_if_not_carry" => Some(vec![
            PatternStep::regex(&JUMP_C_RE),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc a" | "dec a")),
            PatternStep::with_rewind(1, |line, prev| cond_jump_target_label(line, prev, 0)),
        ]),
        "py_a_shift_right_3" => Some(vec![
            PatternStep::regex(&SRL_A_RE),
            PatternStep::regex(&SRL_A_RE),
            PatternStep::regex(&SRL_A_RE),
        ]),
        "py_a_eq_x_plusminus_carry" => Some(vec![
            PatternStep::regex(&LD_DST_A_RE),
            PatternStep::new(cond_a_eq_x_plusminus_carry_step2),
            PatternStep::regex(&ADC_SBC_0_RE),
        ]),
        "py_a_eq_carry_plusminus_x" => Some(vec![
            PatternStep::regex(&LD_DST_A_RE),
            PatternStep::regex(&LD_A_0_RE),
            PatternStep::new(cond_a_eq_carry_plusminus_x_step3),
        ]),
        "py_reg_conditional_ternary" => Some(vec![
            PatternStep::regex(&JUMP_NZ_OR_ZC_RE),
            PatternStep::regex(&LDH_R_RE),
            PatternStep::new(|line, _| {
                JR_JP_JMP_ANY_RE.is_match(&line.code)
                    && !line.code.contains(',')
                    && line.code != "jp hl"
            }),
            PatternStep::new(|line, prev| cond_jump_target_label(line, prev, 0)),
            PatternStep::new(|line, prev| {
                if LDH_R_RE.is_match(&line.code) {
                    return true;
                }
                (line.code == "xor a" || line.code == "xor a, a")
                    && prev[1].code.starts_with("ld")
                    && LDH_R_RE.is_match(&prev[1].code)
                    && prev[1].code.contains(" a,")
            }),
            PatternStep::new(|line, prev| {
                if line.code == prev[2].code {
                    return true;
                }
                let Some(target) = last_token(&prev[2].code) else {
                    return false;
                };
                strip_trailing_colon(&line.code) == target
            }),
        ]),
        "py_a_and_x_eq_x" => Some(vec![
            PatternStep::regex(&AND_IMM_RE),
            PatternStep::new(cond_and_cp_same_operand),
        ]),
        "py_a_mask_or" => Some(vec![
            PatternStep::regex(&AND_IMM_RE),
            PatternStep::regex(&LD_RH_A_RE),
            PatternStep::new(|line, prev| {
                LD_A_RH_RE.is_match(&line.code)
                    && byte_at(&prev[1].code, 3).is_some_and(|b| byte_at(&line.code, 6) != Some(b))
            }),
            PatternStep::regex(&AND_IMM_RE),
            PatternStep::new(cond_mask_or_step5),
        ]),

        "py_pair_add_a_or_n" => Some(vec![
            PatternStep::regex(&ADD_A_LCE_OR_IMM_RE),
            PatternStep::new(cond_pair_add_step2),
            PatternStep::new(cond_pair_add_step3),
            PatternStep::new(cond_pair_add_step4),
            PatternStep::new(cond_pair_add_step5),
        ]),
        "py_pair_add_a_or_n_jump" => Some(vec![
            PatternStep::regex(&ADD_A_LCE_OR_IMM_RE),
            PatternStep::new(cond_pair_add_step2),
            PatternStep::regex(&JUMP_NC_RE),
            PatternStep::new(cond_pair_add_jump_step4),
            PatternStep::new(|line, prev| cond_jump_target_label(line, prev, 2)),
        ]),
        "py_pair_eq_foo_plus_a" => Some(vec![
            PatternStep::regex(&LD_PAIR_IMM_RE),
            PatternStep::new(cond_pair_eq_foo_plus_a_step2),
            PatternStep::new(cond_pair_eq_foo_plus_a_step3),
            PatternStep::new(cond_pair_eq_foo_plus_a_step4),
            PatternStep::new(cond_pair_eq_foo_plus_a_step5),
            PatternStep::new(cond_pair_eq_foo_plus_a_step6),
        ]),
        "py_reg_plus_carry" => Some(vec![
            PatternStep::regex(&LD_A_BCDEHL_OR_0_RE),
            PatternStep::regex(&ADC_BCDEHL_OR_0_RE),
            PatternStep::new(cond_reg_plus_minus_carry_step3),
        ]),
        "py_reg_minus_carry" => Some(vec![
            PatternStep::regex(&LD_A_BCDEHL_OR_0_RE),
            PatternStep::regex(&SBC_BCDEHL_OR_0_RE),
            PatternStep::new(cond_reg_plus_minus_carry_step3),
        ]),
        "py_pair_eq_a_mul_16" => Some(vec![
            PatternStep::new(|line, _| {
                LD_LCE_A_RE.is_match(&line.code) || LD_HBD_0_RE.is_match(&line.code)
            }),
            PatternStep::new(cond_pair_eq_a_mul_16_step2),
            PatternStep::new(cond_pair_eq_a_mul_16_add),
            PatternStep::new(cond_pair_eq_a_mul_16_add),
            PatternStep::new(cond_pair_eq_a_mul_16_add),
            PatternStep::new(cond_pair_eq_a_mul_16_add),
        ]),
        "py_pair_eq_a_mul_16_rept" => Some(vec![
            PatternStep::new(|line, _| {
                LD_LCE_A_RE.is_match(&line.code) || LD_HBD_0_RE.is_match(&line.code)
            }),
            PatternStep::new(cond_pair_eq_a_mul_16_step2),
            PatternStep::new(|line, _| line.code.eq_ignore_ascii_case("rept 4")),
            PatternStep::new(cond_pair_eq_a_mul_16_add),
            PatternStep::new(|line, _| line.code.eq_ignore_ascii_case("endr")),
        ]),
        "py_hl_mul_2" => Some(vec![
            PatternStep::new(|line, _| line.code == "sla l"),
            PatternStep::new(|line, _| line.code == "rl h"),
        ]),
        "py_pair_eq_deref_foo" => Some(vec![
            PatternStep::regex(&LD_A_MEM_NOT_HBD_RE),
            PatternStep::regex(&LD_LH_A_RE),
            PatternStep::new(cond_pair_eq_deref_foo_step3),
            PatternStep::new(|line, prev| {
                if !LD_LH_A_RE.is_match(&line.code) {
                    return false;
                }
                let Some(dst1) = byte_at(&prev[1].code, 3) else {
                    return false;
                };
                let Some(dst4) = byte_at(&line.code, 3) else {
                    return false;
                };
                pair_reg(dst1).is_some_and(|p| p == dst4)
            }),
        ]),
        "py_pair_load_pq" => Some(vec![
            PatternStep::regex(&LD_R_IMM_RE),
            PatternStep::new(cond_pair_load_pq_step2),
        ]),
        "py_deref_hl_eq_n" => Some(vec![
            PatternStep::regex(&LD_A_IMM_RE),
            PatternStep::new(|line, _| line.code == "ld [hl], a"),
        ]),
        "py_deref_hl_inc_dec" => Some(vec![
            PatternStep::new(|line, _| line.code == "ld a, [hl]"),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc a" | "dec a")),
            PatternStep::with_rewind(1, |line, _| line.code == "ld [hl], a"),
        ]),
        "py_deref_hl_inc_dec_eq_a" => Some(vec![
            PatternStep::new(|line, _| line.code == "ld [hl], a"),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc hl" | "dec hl")),
        ]),
        "py_deref_hl_inc_dec_eq_n" => Some(vec![
            PatternStep::regex(&LD_MEMHL_IMM_RE),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc hl" | "dec hl")),
        ]),
        "py_a_eq_deref_hl_inc_dec" => Some(vec![
            PatternStep::new(|line, _| line.code == "ld a, [hl]"),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc hl" | "dec hl")),
        ]),
        "py_deref_hl_inc_dec_eq_r" => Some(vec![
            PatternStep::regex(&LD_MEMHL_R_RE),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc hl" | "dec hl")),
        ]),
        "py_r_eq_deref_hl_inc_dec" => Some(vec![
            PatternStep::regex(&LD_R_MEMHL_RE),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc hl" | "dec hl")),
        ]),
        "py_a_eq_0_cmp" => Some(vec![PatternStep::new(cond_a_eq_0_cmp)]),
        "py_a_eq_1_cmp" => Some(vec![PatternStep::regex(&CP_1_RE)]),
        "py_ei_ret" => Some(vec![
            PatternStep::new(|line, _| line.code == "ei"),
            PatternStep::new(|line, _| line.code == "ret"),
        ]),
        "py_tail_call" => Some(vec![
            PatternStep::new(cond_tail_call),
            PatternStep::new(|line, _| line.code == "ret"),
        ]),
        "py_tail_farcall" => Some(vec![
            PatternStep::new(cond_tail_farcall),
            PatternStep::new(|line, _| line.code == "ret"),
        ]),
        "py_tail_predef" => Some(vec![
            PatternStep::new(cond_tail_predef),
            PatternStep::new(|line, _| line.code == "ret"),
        ]),
        "py_fallthrough" => Some(vec![
            PatternStep::new(cond_tail_call),
            PatternStep::new(|line, _| line.code == "ret"),
            PatternStep::new(cond_fallthrough_label),
        ]),
        "py_conditional_call" => Some(vec![
            PatternStep::regex(&JUMP_NZ_OR_ZC_RE),
            PatternStep::new(cond_tail_call),
            PatternStep::new(cond_conditional_call_step3),
        ]),
        "py_conditional_return" => Some(vec![
            PatternStep::regex(&JUMP_NZ_OR_ZC_RE),
            PatternStep::new(|line, _| line.code == "ret"),
            PatternStep::new(|line, prev| cond_jump_target_label(line, prev, 0)),
        ]),
        "py_conditional_fallthrough" => Some(vec![
            PatternStep::regex(&JUMP_NZ_OR_ZC_RE),
            PatternStep::new(|line, _| {
                JR_JP_JMP_ANY_RE.is_match(&line.code)
                    && !line.code.contains(',')
                    && line.code != "jp hl"
            }),
            PatternStep::new(|line, prev| cond_jump_target_label(line, prev, 0)),
        ]),
        "py_call_hl" => Some(vec![
            PatternStep::new(|line, _| LD_BCDE_IMM_NOT_HBD_RE.is_match(&line.code)),
            PatternStep::new(cond_call_hl_step2),
            PatternStep::new(|line, _| line.code == "jp hl"),
            PatternStep::new(|line, prev| {
                let Some(target) = split_after_comma(&prev[0].code) else {
                    return false;
                };
                strip_trailing_colon(&line.code) == target
            }),
        ]),
        "py_pointless_hli_hld" => Some(vec![
            PatternStep::regex(&POINTLESS_HLI_HLD_STEP1_RE),
            PatternStep::with_rewind(1, cond_pointless_hli_hld_step2),
            PatternStep::new(|line, _| LD_HL_IMM_OR_POP_HL_RE.is_match(&line.code)),
        ]),
        "py_pointless_jumps" => Some(vec![
            PatternStep::new(|line, _| {
                POINTLESS_JUMPS_STEP1_RE.is_match(&line.code) && !line.code.contains(',')
            }),
            PatternStep::new(cond_pointless_jumps_step2),
        ]),
        "py_useless_loads" => Some(vec![
            PatternStep::new(cond_useless_loads_step1),
            PatternStep::new(cond_useless_loads_step2),
        ]),
        "py_redundant_loads" => Some(vec![
            PatternStep::new(cond_useless_loads_step1),
            PatternStep::new(cond_redundant_loads_step2),
        ]),
        "py_similar_loads" => Some(vec![
            PatternStep::new(cond_similar_loads_step1),
            PatternStep::new(cond_similar_loads_step2),
        ]),
        "py_conditionally_load_0" => Some(vec![
            PatternStep::new(|line, _| {
                line.code.starts_with("and ") || line.code.starts_with("or ")
            }),
            PatternStep::regex(&JR_NZ_RE),
            PatternStep::regex(&LD_ANY_0_RE),
        ]),
        "py_inefficient_prefix_opcodes" => Some(vec![PatternStep::regex(&PREFIX_A_RE)]),
        "py_redundant_and_or" => Some(vec![
            PatternStep::regex(&AND_OR_XOR_RE),
            PatternStep::regex(&AND_OR_A_RE),
        ]),
        "py_pointless_and_or_a" => Some(vec![
            PatternStep::regex(&AND_OR_A_RE),
            PatternStep::regex(&AFFECTS_ZC_RE),
        ]),
        "py_redundant_inc_dec" => Some(vec![
            PatternStep::new(|line, _| LD_ANY_IMM_NOT_REG_OR_MEM_RE.is_match(&line.code)),
            PatternStep::new(cond_redundant_inc_dec_step2),
        ]),
        "py_pair_eq_n_then_other_then_add" => Some(vec![
            PatternStep::regex(&LD_PAIR_IMM_RE),
            PatternStep::new(|line, prev| {
                LD_PAIR_IMM_RE.is_match(&line.code)
                    && byte_at(&line.code, 2) != byte_at(&prev[0].code, 3)
            }),
            PatternStep::with_rewind(1, cond_pair_three_step3),
        ]),
        "py_pair_eq_n_then_inc_dec" => Some(vec![
            PatternStep::regex(&LD_PAIR_IMM_RE),
            PatternStep::with_rewind(1, cond_pair_dotdot_step2),
            PatternStep::with_rewind(1, cond_pair_dotdot_incdec),
        ]),
        "py_pair_eq_n_and_other_add" => Some(vec![
            PatternStep::regex(&LD_PAIR_IMM_RE),
            PatternStep::with_rewind(1, cond_pair_dotdot_step2),
            PatternStep::regex(&LD_PAIR_IMM_RE),
            PatternStep::with_rewind(1, cond_pair_three_step3),
        ]),
        "py_dec_a_then_addntimes" => Some(vec![
            PatternStep::regex(&LD_HL_IMM_RE),
            PatternStep::new(cond_dec_a_then_addntimes_step2),
            PatternStep::new(cond_dec_a_then_addntimes_step3),
            PatternStep::regex(&CALL_RST_ADDNTIMES_RE),
        ]),
        "py_redundant_ret" => Some(vec![
            PatternStep::new(|line, _| line.code == "ret" || line.code.starts_with("ret ")),
            PatternStep::new(cond_redundant_ret_step2),
        ]),
        "py_stub_function" => Some(vec![
            PatternStep::new(|line, _| is_label_definition_line(line)),
            PatternStep::new(|line, _| line.code == "ret"),
        ]),
        "py_stub_jump" => Some(vec![
            PatternStep::new(|line, _| is_label_definition_line(line)),
            PatternStep::new(|line, _| line.code.starts_with("jr ") && !line.code.contains(',')),
        ]),
        "py_wram_inc_dec" => Some(vec![
            PatternStep::regex(&LD_WRAM_LOAD_RE),
            PatternStep::new(|line, _| matches!(line.code.as_str(), "inc a" | "dec a")),
            PatternStep::new(cond_wram_inc_dec_step3),
        ]),
        "py_trailing_string_space" => Some(vec![
            PatternStep::regex(&TEXT_TRAILING_SPACE_RE),
            PatternStep::new(|line, _| !TEXT_COMMAND_FOLLOWS_RE.is_match(&line.code)),
        ]),

        _ => None,
    }
}
