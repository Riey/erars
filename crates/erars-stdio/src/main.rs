mod stdio_frontend;

use std::{fs::File, io::BufWriter};

use erars_ast::{update_interner, Interner, Value};
use erars_loader::run_script;

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

    #[clap(long, help = "Save script file")]
    save: bool,

    #[clap(long, help = "Load script file")]
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

    unsafe {
        update_interner(Interner::new());
    }

    let (vm, mut ctx, tx) = run_script(args.target_path, inputs).unwrap();

    if args.save {
        erars_bytecode::write_to(BufWriter::new(File::create("game.era").unwrap()), &vm.dic)
            .unwrap();
    } else {
        let mut frontend = stdio_frontend::StdioFrontend::new(tx);
        frontend.run(&vm, &mut ctx).unwrap();
    }
}
