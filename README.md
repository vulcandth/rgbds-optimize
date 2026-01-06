![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/vulcandth/rgbds-optimize/ci.yml)
![GitHub License](https://img.shields.io/github/license/vulcandth/rgbds-optimize)
![Codecov](https://img.shields.io/codecov/c/github/vulcandth/rgbds-optimize)

## Overview

This repository contains:

- A Rust CLI that scans `.asm` files and reports suboptimal instruction patterns.
- The original `optimize.py` reference implementation.
- A runtime-loaded YAML configuration in `configs/packs.yaml`.

See `yaml-config.md` for the YAML schema and building blocks.

## Install and Run

### Rust (recommended)

- Run on a directory (defaults to `--config configs/packs.yaml --pack rgbds`):

```bash
cargo run --release -- .
```

- Run with an explicit config + pack:

```bash
cargo run --release -- --config configs/packs.yaml --pack pret path/to/repo
```

Note: legacy TOML-based pack configs have been removed; use `configs/packs.yaml`.

### Python (reference)

```bash
python3 optimize.py path/to/repo
```

## Pattern Packs

Pattern packs live in a single YAML file, loaded at runtime.

- `regexes`: named regex strings; compiled once at startup.
- `patterns`: reusable pattern definitions (named steps).
- `packs`: named lists of patterns to run.

Example (abridged):

```yaml
regexes:

  NO_OP_LD: '^ld ([abcdehl]), \1$'

patterns:

  py_no_op_ld:
    name: No-op ld
    steps:
      - when: { regex: NO_OP_LD }

packs:

  rgbds:
    patterns:
      - id: py_no_op_ld
```

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
python3 tools/parity.py --config configs/packs.yaml --pack pret --cargo-profile release /path/to/repo
```

CI parity against a pinned `pret/pokecrystal` commit is implemented in `.github/workflows/parity-pokecrystal.yml`.

## Benchmarking and Profiling

A deterministic timing harness exists at `tools/bench.py`.

```bash
python3 tools/bench.py --config configs/packs.yaml --pack pret /path/to/repo
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
perf record -g -- cargo run --release -- --config configs/packs.yaml --pack pret /path/to/repo
```

- Rust (flamegraph wrapper, requires `perf`):

```bash
cargo install flamegraph
cargo flamegraph --root -- --config configs/packs.yaml --pack pret /path/to/repo
```

- Rust (`pprof` feature, useful where `perf` is difficult to install):

```bash
cargo run --release --features pprof -- --config configs/packs.yaml --pack pret --pprof /tmp/optimize.svg --pprof-frequency 1000 /path/to/repo
```

- Python:

```bash
python3 -m cProfile -o /tmp/optimize.prof optimize.py /path/to/repo
python3 -m pstats /tmp/optimize.prof
```

## Contributing

When adding or changing patterns:

- Prefer editing `configs/packs.yaml`.
- Regexes are compiled once at startup; try to reuse named regexes.
- Add fixture coverage under `tests/fixtures/parity/`.
- Run `cargo test` and `python3 tools/parity.py <repo>` (or CI) before sending a PR.
