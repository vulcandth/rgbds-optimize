#![forbid(unsafe_code)]

use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let args: Vec<OsString> = std::env::args_os().skip(1).collect();
    let input_paths: Vec<PathBuf> = if args.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        args.into_iter().map(PathBuf::from).collect()
    };

    let mut total_count: usize = 0;

    for path in input_paths {
        if !path.exists() {
            eprintln!("File not found: {}", path.display());
            continue;
        }

        if path.is_file() {
            total_count = total_count.saturating_add(optimize_file(&path));
            continue;
        }

        let asm_files = gather_asm_files_sorted(&path);
        for filename in asm_files {
            total_count = total_count.saturating_add(optimize_file(&filename));
        }
    }

    println!("Found {} instances.", total_count);

    // Python's sys.exit(N) truncates to an 8-bit exit status on Unix.
    std::process::exit((total_count & 0xFF) as i32);
}

fn is_asm_file(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.eq_ignore_ascii_case("asm"))
        .unwrap_or(false)
}

fn gather_asm_files_sorted(root: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let mut stack = vec![root.to_path_buf()];

    while let Some(dir) = stack.pop() {
        let Ok(entries) = fs::read_dir(&dir) else {
            continue;
        };

        for entry in entries.flatten() {
            let p = entry.path();
            if p.is_dir() {
                stack.push(p);
            } else if is_asm_file(&p) {
                out.push(p);
            }
        }
    }

    out.sort_by(|a, b| a.as_os_str().cmp(b.as_os_str()));
    out
}

fn optimize_file(path: &Path) -> usize {
    let bytes = match fs::read(path) {
        Ok(b) => b,
        Err(err) => {
            println!("ERROR!!! {}: {}", path.display(), err);
            println!();
            return 0;
        }
    };

    let contents = match String::from_utf8(bytes) {
        Ok(s) => s,
        Err(err) => {
            // Keep the same outer format as optimize.py. Matching Python's exact decode error
            // wording can be done later if strict parity is required.
            println!("ERROR!!! {}: {}", path.display(), err);
            println!();
            return 0;
        }
    };

    let _lines = optimize_core::preprocess_properties(&contents);
    0
}
