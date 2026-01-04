---
post_title: "Porting optimize.py to Rust (Checklist Plan)"
author1: "vulcandth"
post_slug: "optimize-py-port-to-rust"
microsoft_alias: ""
featured_image: ""
categories:
  - "Other"
tags:
  - "rgbds"
  - "game-boy"
  - "assembler"
  - "rust"
  - "static-analysis"
ai_note: "AI-assisted draft created in VS Code."
summary: "A checklist plan to port optimize.py to a Rust tool with config-driven patterns and output compatibility."
post_date: "2026-01-04"
---

## Goals and Non-Goals

- Goal: Port `optimize.py` to a Rust CLI that produces byte-for-byte matching output for the same inputs (including exit code), then expand to support optional, config-driven pattern packs.
- Goal: Make it useful for the wider gbdev community (not Pret-only): Pret-specific patterns must be optional/omittable.
- Goal: Support RGBDS syntax robustly, including forms like `[hl+]` in addition to `[hli]`, and not relying on Pret formatting conventions.
- Goal: Accuracy is the top priority; performance is a strong second.
- Non-goal: Changing the user-facing UX/output format during the initial compatibility phase.

## License

- [ ] License the Rust port under the MIT License.
- [ ] Add a `LICENSE` file with the standard MIT text.
- [ ] Ensure any third-party crates are MIT/Apache-2.0 compatible (or otherwise acceptable), and record them in `Cargo.lock`.

## 1. Repo Layout, Tooling, and Test Process (First Step)

- [x] Convert the repository into a Rust workspace (or single crate) with a clear layout:
  - `crates/optimize-cli/` (binary)
  - `crates/optimize-core/` (library: parsing + pattern engine)
  - `configs/` (pattern packs)
  - `tests/fixtures/` (small curated .asm samples)
- [x] Add `.gitignore` appropriate for Rust (`/target/`, editor temp files).
- [x] Add `Cargo.toml` with consistent metadata (name, version, license = MIT).
- [x] Define a repeatable test process:
  - `cargo test` for unit + integration tests
  - `cargo fmt --check` and `cargo clippy -- -D warnings` for hygiene
  - Optional CI (GitHub Actions) that runs tests on Linux/macOS/Windows.
- [x] Decide and document minimum supported Rust version (MSRV), then enforce it in CI.

## 2. Output Compatibility Contract (Lock This Down Early)

Match the Python tool’s observable behavior before adding new features.

- [x] Confirm and document the exact output format:
  - Pattern headers: `### {pattern_name} ###` (only when more than one pattern is enabled)
  - Context label lines: `{filename}:{label_line_number}:{label_text}`
  - Match lines: `{filename}:{line_number}:{original_line_text}`
  - Blank line between files if anything printed for that file
  - Final summary: `Found {N} instances.`
  - Exit code equals `N`
- [x] Define file traversal ordering for stable output (sort paths, then process in order).
- [x] Define text decoding behavior:
  - Python uses UTF-8 with `errors='strict'` and prints an error line on decode failure.
  - Rust must match this behavior (either fail that file and continue, or reproduce the same message format).
- [x] Define whitespace normalization and comment stripping rules:
  - Comments start at `;` (current script uses `partition(';')`).
  - The printed `text` is the original line (minus trailing `\n`).
  - The matched `code` is left-trimmed and has internal whitespace collapsed.

## 3. Assembly Line Parsing (RGBDS-Oriented, Not Pret-Oriented)

Implement a parser that reproduces Python’s preprocessing first, then optionally becomes more robust.

- [x] Implement `Line` representation equivalent to Python:
  - `num` (1-based)
  - `code` (normalized)
  - `comment` and `comment_lower`
  - `text` (original, without trailing newline)
  - `context` (current label)
- [x] Implement preprocessing rules matching Python:
  - Strip trailing newline
  - Split comment at first `;`
  - Skip blank `code` lines
  - Normalize whitespace sequences to a single space
  - Track context label with: “line begins with alpha or `_` and contains `:`”
- [x] Expand label/context detection beyond Pret conventions without breaking compatibility:
  - Support local labels like `.loop:`
  - Support RGBDS double-colon globals like `Func::`
  - Support labels containing digits and `.`
  - Decide how to treat macro labels / `rept` blocks (at minimum: don’t crash, keep context stable)

## 4. Pattern Engine Architecture (Accuracy First, Then Speed)

The Python tool is a small state machine per pattern, with optional rewind.

- [x] Implement the same state machine semantics:
  - Each pattern is a list of conditions, one per “state”.
  - `prev_lines` is the list of previously matched lines for this pattern.
  - When all conditions match, emit the match and reset.
  - If a condition is marked “allow rewind” (the tuple form in Python), step back and retry.
  - If a condition fails without rewind, reset and re-scan with correct index behavior.
- [x] Implement suppression comments exactly:
  - A match should be skipped if `comment_lower` starts with `"no-optimize " + pattern_name_lower`.
- [x] Ensure patterns do not match across labels unless the original logic allows it.

## 5. Regex vs Structured Parsing (Rust Compatibility Constraints)

Rust’s popular `regex` crate does not support backreferences; `optimize.py` uses them in places (example: `ld ([abcdehl]), \1`).

- [x] Inventory which patterns depend on backreferences or capture equality.
- [x] Choose an approach that keeps performance strong:
  - Preferred: parse instructions into `(mnemonic, operands[])` tokens and express patterns structurally.
  - Acceptable: keep regex for simple patterns, and implement the “backref” patterns with custom predicate functions.
  - Avoid making everything a backtracking regex (accuracy is critical, but speed is a strong second).
- [x] Ensure case-insensitivity where RGBDS accepts it (at minimum: match current behavior, which is mostly case-sensitive string comparisons plus some `.lower()` uses).

## 6. Config-Driven Pattern Packs (Community-Friendly)

The community goal is best met by separating “engine” from “pattern definitions” and shipping multiple optional packs.

- [ ] Design a config format for patterns (suggested: TOML or YAML):
	- Pattern metadata: name, pack, enabled-by-default, description
	- A list of steps/conditions (N-step pattern)
	- Condition types for a minimal-but-useful DSL:
		- `opcode_is`, `opcode_in`
		- `operand_eq` (string or normalized token)
		- `operand_matches` (regex)
		- `same_as_prev_operand` (capture-like behavior)
		- `context_eq_prev` (matches Python’s label-context restrictions)
		- `skip_if_comment_prefix` (suppression)
		- `allow_rewind: N`
- [ ] Implement a built-in “core” pack in Rust that matches today’s `optimize.py` patterns.
- [ ] Split Pret-specific rules into a separate optional config pack (disabled by default).
- [ ] Decide how to handle patterns that are hard to express declaratively:
	- Allow “builtin predicates” referenced from config by name.
	- Keep the public DSL stable for community contributions.

## 7. RGBDS Syntax Coverage Checklist (Beyond Pret Conventions)

This is a checklist of syntax cases to ensure the tool is robust on real-world RGBDS projects.

- [ ] Accept bracket variants as equivalent when appropriate:
	- Treat `[hli]`, `[hl+]` as the same addressing mode.
	- Treat `[hld]`, `[hl-]` as the same addressing mode.
- [ ] Handle numeric literal variants used in the ecosystem:
	- Hex: `$FF`, `0xFF` (if present), `&377` (octal style in some code)
	- Binary: `%1010`
	- Decimal: `123`
	- Signed immediates where valid
- [ ] Handle operand spacing variations:
	- `ld a,[hl]` vs `ld a, [hl]`
	- Multiple spaces / tabs
- [ ] Handle non-instruction lines safely:
	- Directives: `SECTION`, `db`, `dw`, `INCLUDE`, `REPT/ENDR`, `MACRO/ENDM`, etc.
	- Macro invocations and arguments
	- Continuation lines if the assembler supports them
- [ ] Ensure comment stripping does not break strings (Python currently does not special-case strings); decide whether to preserve compatibility or improve it behind a flag.

## 8. Verification: Match Python Output on Real Codebases

You requested matching output to the current output, with an explicit comparison using Pret’s `pokecrystal`.

- [ ] Add an integration test harness that runs both tools and diffs output:
	- Run `python3 optimize.py <paths...>`
	- Run `cargo run --release -- <paths...>`
	- Compare stdout exactly (byte-for-byte)
	- Compare exit code exactly
- [ ] Make the comparison deterministic:
	- Ensure both tools scan files in the same order (sort file list)
	- Ensure the same working directory and relative path printing
- [ ] Add a documented “manual verification” procedure:
	- Clone `https://github.com/pret/pokecrystal` to a known path
	- Run both tools against the same directories
	- Save outputs and run `diff -u`
- [ ] Optional CI job (if acceptable) that checks output parity against a pinned commit of `pret/pokecrystal`:
	- Use a submodule or `git clone --depth 1 --branch <pinned>`
	- Cache builds to keep CI reasonable

Example commands (adjust paths):

```bash
# Python baseline
python3 optimize.py /path/to/pokecrystal > /tmp/py.out; echo $? > /tmp/py.code

# Rust candidate
cargo run --release -- /path/to/pokecrystal > /tmp/rs.out; echo $? > /tmp/rs.code

diff -u /tmp/py.out /tmp/rs.out

diff -u /tmp/py.code /tmp/rs.code
```

## 9. Performance Plan (After Compatibility)

Once output parity is proven, optimize.

- [ ] Benchmark on a large repo (e.g., `pokecrystal`) in release mode.
- [ ] Profile hotspots (likely: regex matching, string allocations, per-line normalization).
- [ ] Targeted optimizations that should not affect correctness:
	- Pre-compile regexes once per pattern pack
	- Avoid per-line `String` allocations where possible (use slices + indices)
	- Use a fast instruction tokenizer for structural matching
	- Use `rayon` only if it can preserve deterministic output (often it cannot without extra work)

## 10. Documentation and Community Contribution Workflow

- [ ] Document how to write patterns in the config DSL, with examples.
- [ ] Provide a “core” pack that’s gbdev-generic and a “pret” pack for Pret-specific patterns.
- [ ] Add a short compatibility statement: “Initial goal is to match optimize.py output; new features may be added behind flags or new packs.”
- [ ] Add contribution guidelines: tests required for new patterns, and include fixture .asm cases.

## Definition of Done

- [ ] `cargo run --release -- <path>` produces the same output and exit code as `python3 optimize.py <path>` for representative repos.
- [ ] A config-driven pattern pack mechanism exists, and Pret-specific patterns can be excluded entirely.
- [ ] Syntax cases like `[hl+]` are handled correctly.
- [ ] Performance is competitive on large codebases without sacrificing accuracy.