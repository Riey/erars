mod stdio_frontend;

use std::{collections::VecDeque, path::Path};

use erars_loader::{load_script, run_script, save_script};
use memory_stats::memory_stats;

#[derive(clap::Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(
        value_parser,
        default_value = ".",
        help = "ERA game path default is current path"
    )]
    target_path: String,

    #[clap(long, help = "Accept input value from file")]
    use_input: Option<std::path::PathBuf>,

    #[clap(
        long,
        default_value = "info",
        help = "Log level (error, warn, info, debug, trace)"
    )]
    log_level: String,

    #[clap(long, help = "Don't print logs")]
    quite: bool,

    #[clap(long, help = "Enable json mode")]
    json: bool,

    #[clap(long, help = "Save bytecode")]
    save: bool,

    #[clap(long, help = "Load bytecode")]
    load: bool,

    #[clap(long, help = "Just measure memory usage")]
    measure_memory: bool,
}

fn main() {
    use flexi_logger::*;

    let args: Args = clap::Parser::parse();

    let _handle = if args.quite {
        None
    } else {
        Some(
            Logger::try_with_str(&args.log_level)
                .unwrap()
                .rotate(
                    Criterion::AgeOrSize(Age::Day, 1024 * 1024),
                    Naming::Numbers,
                    Cleanup::KeepLogFiles(5),
                )
                .log_to_file(
                    FileSpec::default()
                        .directory(Path::new(&args.target_path).join("logs"))
                        .basename("erars"),
                )
                .write_mode(WriteMode::BufferAndFlush)
                .create_symlink("last_log.log")
                .use_utc()
                .start()
                .unwrap(),
        )
    };

    log_panics::init();

    let inputs = match args.use_input {
        Some(input) => ron::from_str(&std::fs::read_to_string(input).unwrap()).unwrap(),
        None => VecDeque::new(),
    };

    let system = Box::new(stdio_frontend::StdioFrontend::new(
        Path::new(&args.target_path).join("sav"),
        args.json,
        inputs,
    ));

    let (vm, mut ctx, mut tx) = if args.load {
        unsafe { load_script(&args.target_path, system).unwrap() }
    } else {
        run_script(&args.target_path, system).unwrap()
    };

    if args.measure_memory {
        measure_by_drop("TerminalVm", vm);
        measure_by_drop("VmContext", ctx);
        measure_by_drop("VirtualConsole", tx);
        println!(
            "Interner takes {}KB",
            erars_ast::get_interner().current_memory_usage() / 1024
        );
    } else if args.save {
        save_script(vm, ctx, &args.target_path).unwrap();
    } else {
        futures_executor::block_on(vm.start(&mut tx, &mut ctx));
    }
}

fn measure_by_drop<T>(name: &str, val: T) {
    let prev = memory_stats().unwrap().physical_mem;
    drop(val);
    let current = memory_stats().unwrap().physical_mem;
    println!("{name} takes {}KB", prev.saturating_sub(current) / 1024);
}
