#![forbid(unsafe_code)]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn normalize_newlines(bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'\r' {
            if let Some(b'\n') = bytes.get(i + 1) {
                out.push(b'\n');
                i += 2;
                continue;
            }
        }

        out.push(bytes[i]);
        i += 1;
    }

    out
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn parity_fixture_dir(repo_root: &Path) -> PathBuf {
    repo_root.join("tests/fixtures/parity")
}

fn gather_asm_files_sorted(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![root.to_path_buf()];

    while let Some(dir) = stack.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };

        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path
                .extension()
                .and_then(|e| e.to_str())
                .is_some_and(|e| e.eq_ignore_ascii_case("asm"))
            {
                out.push(path);
            }
        }
    }

    out.sort_by(|a, b| a.as_os_str().cmp(b.as_os_str()));
    out
}

fn find_python() -> Option<&'static str> {
    ["python3", "python"].into_iter().find(|candidate| {
        Command::new(candidate)
            .arg("--version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .is_ok()
    })
}

fn run_checked(mut cmd: Command) -> Output {
    cmd.output()
        .unwrap_or_else(|err| panic!("failed to run {cmd:?}: {err}"))
}

#[test]
fn python_and_rust_outputs_match_on_fixtures() {
    let Some(python) = find_python() else {
        eprintln!("Skipping parity test: python interpreter not found");
        return;
    };

    let repo_root = repo_root();
    let fixture_dir = parity_fixture_dir(&repo_root);
    let mut files = gather_asm_files_sorted(&fixture_dir);
    assert!(
        !files.is_empty(),
        "expected parity fixtures under {fixture_dir:?}"
    );

    let rel_files: Vec<PathBuf> = files
        .drain(..)
        .map(|p| {
            p.strip_prefix(&repo_root)
                .unwrap_or_else(|_| panic!("fixture path is not under repo root: {p:?}"))
                .to_path_buf()
        })
        .collect();

    let py = {
        let mut cmd = Command::new(python);
        cmd.current_dir(&repo_root)
            .arg("optimize.py")
            .args(&rel_files);
        run_checked(cmd)
    };

    let optimize_bin = env!("CARGO_BIN_EXE_optimize");
    let rs = {
        let mut cmd = Command::new(optimize_bin);
        cmd.current_dir(&repo_root)
            .arg("--pack")
            .arg("configs/pret.toml")
            .args(&rel_files);
        run_checked(cmd)
    };

    let py_code = py.status.code().expect("python exit code missing");
    let rs_code = rs.status.code().expect("rust exit code missing");

    let py_stdout = normalize_newlines(&py.stdout);
    let rs_stdout = normalize_newlines(&rs.stdout);
    let py_stderr = normalize_newlines(&py.stderr);
    let rs_stderr = normalize_newlines(&rs.stderr);

    assert_eq!(
        py_stdout,
        rs_stdout,
        "stdout mismatch\n\npython stdout:\n{}\n\nrust stdout:\n{}\n",
        String::from_utf8_lossy(&py_stdout),
        String::from_utf8_lossy(&rs_stdout)
    );
    assert_eq!(
        py_stderr,
        rs_stderr,
        "stderr mismatch\n\npython stderr:\n{}\n\nrust stderr:\n{}\n",
        String::from_utf8_lossy(&py_stderr),
        String::from_utf8_lossy(&rs_stderr)
    );
    assert_eq!(py_code, rs_code, "exit code mismatch");
}
