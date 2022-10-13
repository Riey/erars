mod stdio_frontend;

use std::{path::Path, time::Instant};

use erars_ast::Value;
use erars_loader::run_script;
use erars_vm::{ScriptSaveFormat, ScriptSaveFormatRef, TerminalVm, VmContext};

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

    let time = Instant::now();

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

    let (vm, mut ctx) = if args.load {
        let format: ScriptSaveFormat = bincode::deserialize_from(std::io::BufReader::new(
            std::fs::File::open("game.era").unwrap(),
        ))
        .unwrap();

        log::info!("dic: {:?}", format.dic);

        todo!();

        (
            TerminalVm {
                dic: format.dic,
                sav_path: Path::new(&args.target_path).join("sav"),
            },
            VmContext::new(format.header_info.into(), format.config.into()),
        )
    } else {
        run_script(args.target_path, inputs).unwrap()
    };

    let diff = time.elapsed();
    println!("Game load done! {}ms elapsed", diff.as_millis());

    if args.save {
        let time = Instant::now();
        bincode::serialize_into(
            &mut std::io::BufWriter::new(std::fs::File::create("game.era").unwrap()),
            &ScriptSaveFormatRef {
                dic: &vm.dic,
                config: &ctx.config,
                header_info: &ctx.header_info,
            },
        )
        .unwrap();
        let diff = time.elapsed();
        println!("Save done! {}ms elapsed", diff.as_millis());
    } else {
        let mut frontend = stdio_frontend::StdioFrontend::new(ctx.config.printc_width);
        frontend.run(&vm, &mut ctx).unwrap();
    }
}
