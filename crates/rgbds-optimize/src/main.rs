#![forbid(unsafe_code)]

use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};

#[cfg(all(feature = "pprof", unix))]
use std::fs::File;

#[derive(Debug)]
struct Args {
    pack_path: PathBuf,
    input_paths: Vec<PathBuf>,
    #[cfg(all(feature = "pprof", unix))]
    pprof_out: Option<PathBuf>,
    #[cfg(not(all(feature = "pprof", unix)))]
    pprof_out: Option<PathBuf>,
    #[cfg(all(feature = "pprof", unix))]
    pprof_frequency_hz: i32,
}

fn main() {
    let args: Vec<OsString> = std::env::args_os().skip(1).collect();

    let args = parse_args(args);

    #[cfg(not(all(feature = "pprof", unix)))]
    if args.pprof_out.is_some() {
        eprintln!(
            "--pprof is not available in this build. Rebuild with: cargo run -p rgbds-optimize --features pprof -- <args>"
        );
        std::process::exit(2);
    }

    #[cfg(all(feature = "pprof", unix))]
    let pprof_guard = args
        .pprof_out
        .as_ref()
        .map(|_| pprof::ProfilerGuard::new(args.pprof_frequency_hz).unwrap());

    let pack_contents = match fs::read_to_string(&args.pack_path) {
        Ok(s) => s,
        Err(err) => {
            eprintln!(
                "Failed to read pack config {}: {}",
                args.pack_path.display(),
                err
            );
            std::process::exit(2);
        }
    };
    let pack = match optimize_core::load_pattern_pack_toml(&pack_contents) {
        Ok(p) => p,
        Err(err) => {
            eprintln!("Invalid pack config {}: {}", args.pack_path.display(), err);
            std::process::exit(2);
        }
    };

    let input_paths: Vec<PathBuf> = if args.input_paths.is_empty() {
        vec![PathBuf::from(".")]
    } else {
        args.input_paths
    };

    let mut total_count: usize = 0;

    let enabled_pattern_count = pack
        .patterns
        .iter()
        .filter(|p| p.enabled_by_default)
        .count();

    for path in input_paths {
        if !path.exists() {
            eprintln!("File not found: {}", path.display());
            continue;
        }

        if path.is_file() {
            total_count =
                total_count.saturating_add(optimize_file(&path, &pack, enabled_pattern_count));
            continue;
        }

        let asm_files = gather_asm_files_sorted(&path);
        for filename in asm_files {
            total_count =
                total_count.saturating_add(optimize_file(&filename, &pack, enabled_pattern_count));
        }
    }

    println!("Found {} instances.", total_count);

    #[cfg(all(feature = "pprof", unix))]
    if let (Some(out_path), Some(guard)) = (args.pprof_out.as_ref(), pprof_guard.as_ref()) {
        match guard.report().build() {
            Ok(report) => {
                let Ok(file) = File::create(out_path) else {
                    eprintln!("Failed to write flamegraph to {}", out_path.display());
                    std::process::exit(2);
                };
                if let Err(err) = report.flamegraph(file) {
                    eprintln!(
                        "Failed to write flamegraph to {}: {}",
                        out_path.display(),
                        err
                    );
                    std::process::exit(2);
                }
            }
            Err(err) => {
                eprintln!("Failed to build pprof report: {}", err);
                std::process::exit(2);
            }
        }
    }

    #[cfg(windows)]
    {
        let exit_code: i32 = total_count.min(i32::MAX as usize) as i32;
        std::process::exit(exit_code);
    }

    #[cfg(not(windows))]
    {
        // Python's sys.exit(N) truncates to an 8-bit exit status on Unix.
        std::process::exit((total_count & 0xFF) as i32);
    }
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

fn parse_args(args: Vec<OsString>) -> Args {
    let mut pack_path: PathBuf = PathBuf::from("configs/rgbds.toml");
    let mut inputs: Vec<PathBuf> = Vec::new();
    let mut pprof_out: Option<PathBuf> = None;

    #[cfg(all(feature = "pprof", unix))]
    let mut pprof_frequency_hz: i32 = 100;

    let mut i = 0usize;
    while i < args.len() {
        if args[i] == "--pack" {
            if let Some(v) = args.get(i + 1) {
                pack_path = PathBuf::from(v);
                i += 2;
                continue;
            }
        }

        if args[i] == "--pprof" {
            if let Some(v) = args.get(i + 1) {
                pprof_out = Some(PathBuf::from(v));
                i += 2;
                continue;
            }
        }

        #[cfg(all(feature = "pprof", unix))]
        if args[i] == "--pprof-frequency" {
            if let Some(v) = args.get(i + 1).and_then(|v| v.to_str()) {
                if let Ok(parsed) = v.parse::<i32>() {
                    pprof_frequency_hz = parsed;
                    i += 2;
                    continue;
                }
            }
        }

        inputs.push(PathBuf::from(&args[i]));
        i += 1;
    }

    Args {
        pack_path,
        input_paths: inputs,
        pprof_out,
        #[cfg(all(feature = "pprof", unix))]
        pprof_frequency_hz,
    }
}

fn optimize_file(
    path: &Path,
    pack: &optimize_core::PatternPack,
    enabled_pattern_count: usize,
) -> usize {
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

    let lines = optimize_core::preprocess_properties(&contents);
    if lines.is_empty() {
        return 0;
    }

    let filename = path.display().to_string();
    let matches = optimize_core::run_pack_on_lines(&filename, &lines, pack);

    let mut printed_any = false;
    let mut count = 0usize;

    for (pattern_name, instances) in matches {
        if instances.is_empty() {
            continue;
        }

        if enabled_pattern_count > 1 {
            println!("### {} ###", pattern_name);
        }

        for m in instances {
            count = count.saturating_add(1);
            if let Some(label) = &m.label {
                println!("{}:{}:{}", filename, label.num, label.text);
            }
            for line in &m.lines {
                println!("{}:{}:{}", filename, line.num, line.text);
            }
            printed_any = true;
        }
    }

    if printed_any {
        println!();
    }

    count
}
