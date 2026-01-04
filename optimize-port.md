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

- [x] License the Rust port under the MIT License.
- [x] Add a `LICENSE` file with the standard MIT text.
- [x] Ensure any third-party crates are MIT/Apache-2.0 compatible (or otherwise acceptable), and record them in `Cargo.lock`.

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

Note: This repo currently tracks the newest stable Rust toolchain via `rust-toolchain.toml` and CI.

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

- [x] Design a config format for patterns (suggested: TOML or YAML):
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
- [x] Implement a built-in “core” pack in Rust that matches today’s `optimize.py` patterns.
- [x] Split Pret-specific rules into a separate optional config pack (disabled by default).
- [x] Decide how to handle patterns that are hard to express declaratively:
  - Allow “builtin predicates” referenced from config by name.
  - Keep the public DSL stable for community contributions.

## 7. RGBDS Syntax Coverage Checklist (Beyond Pret Conventions)

This is a checklist of syntax cases to ensure the tool is robust on real-world RGBDS projects.


- [x] Accept bracket variants as equivalent when appropriate:
	- Treat `[hli]`, `[hl+]` as the same addressing mode.
	- Treat `[hld]`, `[hl-]` as the same addressing mode.

- [x] Handle numeric literal variants used in the ecosystem:
	- Hex: `$FF`, `0xFF` (if present), `&377` (octal style in some code)
	- Binary: `%1010`
	- Decimal: `123`
	- Signed immediates where valid

- [x] Handle operand spacing variations:
	- `ld a,[hl]` vs `ld a, [hl]`
	- Multiple spaces / tabs

- [x] Handle non-instruction lines safely:
	- Directives: `SECTION`, `db`, `dw`, `INCLUDE`, `REPT/ENDR`, `MACRO/ENDM`, etc.
	- Macro invocations and arguments
	- Continuation lines if the assembler supports them

- [x] Ensure comment stripping does not break strings (Python currently does not special-case strings); preserve compatibility by default, and consider a future optional flag for string-aware semicolon handling.

## 8. Verification: Match Python Output on Real Codebases

You requested matching output to the current output, with an explicit comparison using Pret’s `pokecrystal`.

- [x] Add an integration test harness that runs both tools and diffs output:
  - Runs on a small fixture corpus under `tests/fixtures/parity/`.
  - Compares stdout exactly (byte-for-byte).
  - Compares exit code exactly.
- [x] Make the comparison deterministic:
  - Collects and sorts the `.asm` file list.
  - Passes the explicit file list to both tools (so neither implementation uses its own directory traversal order).
  - Runs both commands from the same working directory.
- [x] Add a documented “manual verification” procedure.
- [x] Run the parity check on a real codebase (`pret/pokecrystal`-style repo) and confirm byte-for-byte matching output and exit code.
- [x] Optional CI job that checks output parity against a pinned commit of `pret/pokecrystal`.
  - Implemented as a GitHub Actions workflow: `.github/workflows/parity-pokecrystal.yml`
  - Pinned commit: `3c0d2b26a54dc93790bf967383283a491f91bf48`

Example commands (adjust paths):

```bash
# Quick one-shot parity check (collects/sorts files, then diffs Python vs Rust)
python3 tools/parity.py /path/to/pokecrystal

# Verified locally against pokecrystal-up:
python3 tools/parity.py /home/vulcandth/GitHub/pokecrystal-up

# Or, run the tools directly on the same explicit file list (deterministic ordering)
find /path/to/pokecrystal -name '*.asm' -print0 | sort -z | xargs -0 \
  python3 optimize.py > /tmp/py.out; echo $? > /tmp/py.code

find /path/to/pokecrystal -name '*.asm' -print0 | sort -z | xargs -0 \
  cargo run --release -p optimize -- --pack configs/pret.toml > /tmp/rs.out; echo $? > /tmp/rs.code

diff -u /tmp/py.out /tmp/rs.out

diff -u /tmp/py.code /tmp/rs.code
```

## 9. Performance Plan (After Compatibility)

Once output parity is proven, optimize.

- [x] Add a deterministic timing harness for Python vs Rust.
  - Implemented as `tools/bench.py` (dependency-free).
- [x] Benchmark on a large repo (e.g., `pokecrystal`) in release mode.
  - Verified locally on `pokecrystal-up` via `tools/bench.py`.
- [x] Profile hotspots (likely: regex matching, string allocations, per-line normalization).
  - Rust (Linux): `perf record -g -- cargo run --release -p optimize -- --pack configs/pret.toml <files...>`
  - Rust (nice UI): `cargo install flamegraph` then `cargo flamegraph -p optimize --root -- --pack configs/pret.toml <files...>`
  - Rust (WSL-friendly, no system `perf` required):
    - `cargo run --release -p optimize --features pprof -- --pack configs/pret.toml --pprof /tmp/optimize.svg --pprof-frequency 1000 <path>`
  - Python: `python3 -m cProfile -o /tmp/optimize.prof optimize.py <files...>` then `python3 -m pstats /tmp/optimize.prof`

- [x] Remove accidental per-line regex compilation in Rust patterns.
  - Outcome (pokecrystal-up, 3015 files): Rust ~1.46s vs Python ~25.8s (~17.6x faster).
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