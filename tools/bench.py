#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Benchmark optimize.py vs the Rust port on the same file list.

This script:
- Recursively collects all `.asm` files under the provided path(s).
- Sorts them for deterministic ordering.
- Runs both tools from the repo root with the same explicit file list.
- Measures wall-clock time for multiple runs and reports basic stats.

It is intentionally dependency-free (no hyperfine required).
"""

from __future__ import annotations

import argparse
import os
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple


def normalize_newlines(b: bytes) -> bytes:
    """Normalize platform newlines for byte-for-byte parity checks."""

    return b.replace(b"\r\n", b"\n")


def gather_asm_files(paths: Sequence[Path]) -> List[Path]:
    """Return a sorted list of `.asm` files from the given input paths."""

    files: List[Path] = []

    for p in paths:
        if not p.exists():
            print("File not found:", p, file=sys.stderr)
            continue
        if p.is_file():
            if p.suffix.lower() == ".asm":
                files.append(p)
            continue

        files.extend(sorted(p.rglob("*.asm")))

    files = sorted({f for f in files})
    return files


def to_relpaths(files: Iterable[Path], base: Path) -> List[str]:
    """Convert absolute paths to repo-relative strings for stable invocation."""

    out: List[str] = []
    for f in files:
        try:
            out.append(str(f.relative_to(base)))
        except ValueError:
            out.append(str(f))
    return out


def run(cmd: Sequence[str], cwd: Path) -> Tuple[int, bytes, bytes, float]:
    """Run a command, returning (exit_code, stdout, stderr, seconds)."""

    start = time.perf_counter()
    env = {
        **os.environ,
        "PYTHONUTF8": "1",
        "PYTHONIOENCODING": "utf-8",
    }
    p = subprocess.run(
        cmd,
        cwd=str(cwd),
        capture_output=True,
        check=False,
        env=env,
    )
    end = time.perf_counter()
    return p.returncode, p.stdout, p.stderr, end - start


def format_seconds(s: float) -> str:
    """Format a duration for humans."""

    if s < 1.0:
        return f"{s * 1000.0:.1f} ms"
    return f"{s:.3f} s"


def summarize(samples: List[float]) -> str:
    """Return a compact summary for a non-empty list of durations."""

    if not samples:
        return "(no samples)"
    mean = statistics.mean(samples)
    med = statistics.median(samples)
    stdev = statistics.pstdev(samples)
    return (
        f"mean {format_seconds(mean)}, "
        f"median {format_seconds(med)}, "
        f"stdev {format_seconds(stdev)}"
    )


def main(argv: Sequence[str]) -> int:
    """CLI entrypoint."""

    parser = argparse.ArgumentParser()
    parser.add_argument("path", type=Path, nargs="+", help="File or dir to scan")
    parser.add_argument(
        "--pack",
        type=Path,
        default=Path("configs/pret.toml"),
        help="Pattern pack to use for the Rust binary.",
    )
    parser.add_argument(
        "--python",
        default=sys.executable,
        help="Python interpreter to run optimize.py (default: current).",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=5,
        help="Number of timed runs per tool (default: 5).",
    )
    parser.add_argument(
        "--warmup",
        type=int,
        default=1,
        help="Warm-up runs per tool (default: 1).",
    )
    parser.add_argument(
        "--cargo-profile",
        default="release",
        choices=["dev", "release"],
        help="Cargo profile for running the Rust binary.",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Skip `cargo build` before timing.",
    )
    parser.add_argument(
        "--check-parity",
        action="store_true",
        help="Fail if stdout/stderr/exit code differ on the first timed run.",
    )

    args = parser.parse_args(list(argv))

    repo_root = Path(__file__).resolve().parent.parent

    files = gather_asm_files(args.path)
    if not files:
        print("No .asm files found.", file=sys.stderr)
        return 2

    rel_files = to_relpaths(files, repo_root)

    fd, list_path_s = tempfile.mkstemp(
        prefix="rgbds-optimize-filelist-", suffix=".txt", dir=str(repo_root)
    )
    os.close(fd)
    list_path = Path(list_path_s)
    try:
        with open(list_path, "w", encoding="utf-8", newline="\n") as f:
            for p in rel_files:
                f.write(p)
                f.write("\n")

        py_cmd = [args.python, "optimize.py", "--file-list", str(list_path)]

        cargo_args = [
            "cargo",
            "run",
            "--quiet",
            "-p",
            "rgbds-optimize",
            "--bin",
            "optimize",
        ]
        if args.cargo_profile == "release":
            cargo_args.append("--release")

        rs_cmd = [
            *cargo_args,
            "--",
            "--pack",
            str(args.pack),
            "--file-list",
            str(list_path),
        ]

        if not args.no_build:
            build = ["cargo", "build", "-p", "rgbds-optimize", "--bin", "optimize"]
            if args.cargo_profile == "release":
                build.append("--release")
            print("Building Rust binary...", file=sys.stderr)
            b = subprocess.run(build, cwd=str(repo_root), check=False)
            if b.returncode != 0:
                return b.returncode

        print(f"Files: {len(rel_files)}", file=sys.stderr)

        def time_tool(
            name: str, cmd: List[str]
        ) -> Tuple[List[float], Tuple[int, bytes, bytes]]:
            for _ in range(max(0, args.warmup)):
                run(cmd, cwd=repo_root)

            samples: List[float] = []
            first_result: Tuple[int, bytes, bytes] | None = None
            for i in range(args.runs):
                code, out, err, seconds = run(cmd, cwd=repo_root)
                if i == 0:
                    first_result = (code, out, err)
                samples.append(seconds)
            assert first_result is not None
            print(f"{name}: {summarize(samples)}", file=sys.stderr)
            return samples, first_result

        py_times, (py_code, py_out, py_err) = time_tool("python", py_cmd)
        rs_times, (rs_code, rs_out, rs_err) = time_tool("rust", rs_cmd)

        if args.check_parity:
            py_out = normalize_newlines(py_out)
            rs_out = normalize_newlines(rs_out)
            py_err = normalize_newlines(py_err)
            rs_err = normalize_newlines(rs_err)
            ok = (
                (py_code == rs_code)
                and (py_out == rs_out)
                and (py_err == rs_err)
            )
            if not ok:
                print("Parity mismatch detected during benchmark.", file=sys.stderr)
                print(f"python exit={py_code}, rust exit={rs_code}", file=sys.stderr)
                return 1

    finally:
        list_path.unlink(missing_ok=True)

    py_mean = statistics.mean(py_times)
    rs_mean = statistics.mean(rs_times)
    speedup = py_mean / rs_mean if rs_mean > 0 else float("inf")

    print("\nResults")
    print(f"- Files: {len(rel_files)}")
    print(f"- Python mean: {format_seconds(py_mean)}")
    print(f"- Rust mean: {format_seconds(rs_mean)}")
    print(f"- Speedup (python/rust): {speedup:.2f}x")

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
