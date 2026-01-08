#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Compare optimize.py output against the Rust port.

This script collects all `.asm` files from the provided paths (recursively for
directories), sorts them, then passes the explicit file list to *both* tools.

Passing an explicit sorted file list avoids differences in directory traversal
order between implementations.
"""

from __future__ import annotations

import argparse
import difflib
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple


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

    # Normalize ordering across platforms.
    files = sorted({f for f in files})
    return files


def to_relpaths(files: Iterable[Path], base: Path) -> List[str]:
    """Convert absolute paths to repo-relative strings for stable printing."""

    out: List[str] = []
    for f in files:
        try:
            out.append(str(f.relative_to(base)))
        except ValueError:
            out.append(str(f))
    return out


def run(
    cmd: Sequence[str], cwd: Path, env: dict[str, str] | None = None
) -> Tuple[int, bytes, bytes]:
    """Run a command and return (exit_code, stdout, stderr)."""

    p = subprocess.run(
        cmd,
        cwd=str(cwd),
        capture_output=True,
        check=False,
        env=env,
    )
    return p.returncode, p.stdout, p.stderr


def normalize_newlines(b: bytes) -> bytes:
    """Normalize platform newlines for byte-for-byte parity checks."""

    return b.replace(b"\r\n", b"\n")


def _unified_diff_text(
    *,
    a: bytes,
    b: bytes,
    fromfile: str,
    tofile: str,
    context_lines: int,
) -> List[str]:
    """Return a unified diff between two byte streams.

    The diff is generated after UTF-8 decoding with replacement, since the
    optimizer outputs are intended to be UTF-8 text.
    """

    a_text = a.decode("utf-8", errors="replace")
    b_text = b.decode("utf-8", errors="replace")

    a_lines = a_text.splitlines(keepends=True)
    b_lines = b_text.splitlines(keepends=True)

    return list(
        difflib.unified_diff(
            a_lines,
            b_lines,
            fromfile=fromfile,
            tofile=tofile,
            n=context_lines,
        )
    )


def _print_diff(
    *,
    label: str,
    a: bytes,
    b: bytes,
    context_lines: int,
    max_lines: int | None,
) -> None:
    """Print a unified diff for an output stream if it differs."""

    diff_lines = _unified_diff_text(
        a=a,
        b=b,
        fromfile=f"python {label}",
        tofile=f"rust {label}",
        context_lines=context_lines,
    )

    if max_lines is not None and len(diff_lines) > max_lines:
        sys.stderr.writelines(diff_lines[:max_lines])
        sys.stderr.write(
            f"\n(diff truncated: {max_lines} of {len(diff_lines)} lines)\n"
        )
        return

    sys.stderr.writelines(diff_lines)


def main(argv: Sequence[str]) -> int:
    """CLI entrypoint."""

    parser = argparse.ArgumentParser()
    parser.add_argument("path", type=Path, nargs="*", default=[Path(".")])
    parser.add_argument(
        "--config",
        type=Path,
        default=Path("configs/packs.yaml"),
        help="YAML configuration file to use for the Rust binary.",
    )
    parser.add_argument(
        "--pack",
        default="pret",
        help="Pack name to select from the YAML configuration (default: pret).",
    )
    parser.add_argument(
        "--python",
        default=sys.executable,
        help="Python interpreter to run optimize.py (default: current).",
    )
    parser.add_argument(
        "--cargo-profile",
        default="release",
        choices=["dev", "release"],
        help="Cargo profile for running the Rust binary.",
    )
    parser.add_argument(
        "--diff-context",
        type=int,
        default=3,
        help="Number of context lines to show in unified diffs (default: 3).",
    )
    parser.add_argument(
        "--diff-max-lines",
        type=int,
        default=2000,
        help=(
            "Maximum number of diff lines to print per stream (default: 2000). "
            "Set to 0 for unlimited."
        ),
    )

    args = parser.parse_args(list(argv))

    repo_root = Path(__file__).resolve().parent.parent
    files = gather_asm_files(args.path)
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

        cargo_args = ["cargo", "run", "--quiet", "--bin", "optimize"]
        if args.cargo_profile == "release":
            cargo_args.append("--release")

        rs_cmd = [
            *cargo_args,
            "--",
            "--config",
            str(args.config),
            "--pack",
            str(args.pack),
            "--file-list",
            str(list_path),
        ]

        py_env = {
            **os.environ,
            "PYTHONUTF8": "1",
            "PYTHONIOENCODING": "utf-8",
        }

        py_code, py_out, py_err = run(py_cmd, cwd=repo_root, env=py_env)
        rs_code, rs_out, rs_err = run(rs_cmd, cwd=repo_root)
    finally:
        list_path.unlink(missing_ok=True)

    py_out = normalize_newlines(py_out)
    rs_out = normalize_newlines(rs_out)
    py_err = normalize_newlines(py_err)
    rs_err = normalize_newlines(rs_err)

    max_diff_lines: int | None
    if args.diff_max_lines == 0:
        max_diff_lines = None
    else:
        max_diff_lines = args.diff_max_lines

    ok = True

    if py_out != rs_out:
        sys.stderr.write("stdout differs (unified diff)\n")
        _print_diff(
            label="stdout",
            a=py_out,
            b=rs_out,
            context_lines=args.diff_context,
            max_lines=max_diff_lines,
        )
        ok = False

    if py_err != rs_err:
        sys.stderr.write("stderr differs (unified diff)\n")
        _print_diff(
            label="stderr",
            a=py_err,
            b=rs_err,
            context_lines=args.diff_context,
            max_lines=max_diff_lines,
        )
        ok = False

    if py_code != rs_code:
        sys.stderr.write(f"exit code differs: python={py_code} rust={rs_code}\n")
        ok = False

    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
