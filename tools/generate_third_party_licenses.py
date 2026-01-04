#!/usr/bin/env python3

from __future__ import annotations

import subprocess
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]


def main() -> int:
    out_path = REPO_ROOT / "THIRD_PARTY_LICENSES.html"

    cmd = [
        "cargo",
        "about",
        "generate",
        "--config",
        str(REPO_ROOT / "about.toml"),
        str(REPO_ROOT / "tools" / "third_party_licenses.hbs"),
    ]

    proc = subprocess.run(
        cmd,
        cwd=REPO_ROOT,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    if proc.returncode != 0:
        raise SystemExit(proc.stderr.strip() or f"cargo-about failed with {proc.returncode}")

    out_path.write_text(proc.stdout, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
