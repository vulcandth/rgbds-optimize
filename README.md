---
post_title: "rgbds-optimize-rs"
author1: "vulcandth"
post_slug: "rgbds-optimize-rs"
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
summary: "Rust port of optimize.py for finding suboptimal RGBDS assembly patterns, with config-driven pattern packs and Python-compatible output."
post_date: "2026-01-04"
---

## Overview

This repository contains:

- A Rust CLI that scans `.asm` files and reports suboptimal instruction patterns.
- The original `optimize.py` reference implementation.
- Config-driven pattern packs in `configs/`.

## Install and Run

### Rust (recommended)

- Run on a directory (defaults to `configs/rgbds.yaml`):

```bash
cargo run --release -- .
```

- Run with a specific pattern pack:

```bash
cargo run --release -- --pack configs/pret.yaml path/to/repo
```

### Python (reference)

```bash
python3 optimize.py path/to/repo
```

## Pattern Packs

Pattern packs are YAML files under `configs/`. Each file embeds two sections:

- `builtins`: named optimization definitions. A builtin contains `steps`, where
  each step is a regex (compiled at runtime), literal `equals` match, or a
  registered `function` condition.
- `patterns`: the ordered list of optimizations to run for the pack. Each
  pattern references a builtin and can toggle `enabled_by_default` or add a
  description.

Example:

```yaml
pack: "core"

builtins:
  no_op_ld:
    steps:
      - regex: '^ld ([abcdehl]), \1$'

patterns:
  - name: "No-op ld"
    builtin: "no_op_ld"
    enabled_by_default: true
    description: "Flags ld r,r where the destination equals the source."
```

Notes:

- Regex strings are compiled when the YAML is loaded.
- Output order is the order of patterns in the YAML file.

## Compatibility

The compatibility goal is:

- For the same input files and pattern set, the Rust CLI output (stdout/stderr) and exit code match `optimize.py`.
- New functionality should be added behind flags and/or in new pattern packs to avoid breaking the compatibility contract.

## Parity Verification (Python vs Rust)

A deterministic parity harness exists at `tools/parity.py`.

- Quick one-shot parity check (collects/sorts files, then diffs Python vs Rust):

```bash
python3 tools/parity.py /path/to/repo
```

- Run with a specific pack and release profile:

```bash
python3 tools/parity.py --pack configs/pret.yaml --cargo-profile release /path/to/repo
```

CI parity against a pinned `pret/pokecrystal` commit is implemented in `.github/workflows/parity-pokecrystal.yml`.

## Benchmarking and Profiling

A deterministic timing harness exists at `tools/bench.py`.

```bash
python3 tools/bench.py --pack configs/pret.yaml /path/to/repo
```

## Third-Party Licenses

Generate the third-party license bundle:

```bash
cargo install cargo-about --locked
python3 tools/generate_third_party_licenses.py
```

This writes `THIRD_PARTY_LICENSES.html` in the repo root.

## Dependency Policy Checks

Run dependency/license/advisory checks:

```bash
cargo install cargo-deny --locked
cargo deny check
```

Profiling options:

- Rust (Linux, requires system `perf`):

```bash
perf record -g -- cargo run --release -- --pack configs/pret.yaml /path/to/repo
```

- Rust (flamegraph wrapper, requires `perf`):

```bash
cargo install flamegraph
cargo flamegraph --root -- --pack configs/pret.yaml /path/to/repo
```

- Rust (`pprof` feature, useful where `perf` is difficult to install):

```bash
cargo run --release --features pprof -- --pack configs/pret.yaml --pprof /tmp/optimize.svg --pprof-frequency 1000 /path/to/repo
```

- Python:

```bash
python3 -m cProfile -o /tmp/optimize.prof optimize.py /path/to/repo
python3 -m pstats /tmp/optimize.prof
```

## Contributing

When adding or changing patterns:

- Prefer updating YAML packs in `configs/` and referencing an existing built-in `builtin`.
- If new logic is needed, add a new built-in pattern and keep it self-contained.
- Add fixture coverage under `tests/fixtures/parity/`.
- Run `cargo test` and `python3 tools/parity.py <repo>` (or CI) before sending a PR.
