mod stdio_frontend;

use std::path::Path;

use erars_ast::Value;
use erars_loader::{load_script, run_script, save_script};

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
                .log_to_file(FileSpec::default().directory("logs").basename("erars"))
                .write_mode(WriteMode::BufferAndFlush)
                .create_symlink("last_log.log")
                .use_utc()
                .start()
                .unwrap(),
        )
    };

    log_panics::init();

    let inputs = match args.use_input {
        Some(input) => {
            ron::from_str::<Vec<Value>>(&std::fs::read_to_string(input).unwrap()).unwrap()
        }
        None => Vec::new(),
    };

    let system = Box::new(stdio_frontend::StdioFrontend::new(
        Path::new(&args.target_path).join("sav"),
        args.json,
    ));

    let (vm, mut ctx, mut tx) = if args.load {
        unsafe { load_script(&args.target_path, inputs, system).unwrap() }
    } else {
        run_script(&args.target_path, inputs, system).unwrap()
    };

    if args.save {
        save_script(vm, ctx, &args.target_path).unwrap();
    } else {
        futures_executor::block_on(async move { vm.start(&mut tx, &mut ctx).await });
    }
}
