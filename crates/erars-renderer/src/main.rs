#![windows_subsystem = "windows"]

mod app;
mod atlas;
mod draw;
mod font;
mod gpu;
mod grid;
mod text;

use std::{path::Path, sync::Arc};

use app::{App, Wake};
use erars_compiler::Language;
use erars_loader::{load_config, load_script, run_script};
use winit::event_loop::EventLoop;

/// Build the ordered default-font candidate list: the configured family first,
/// then coherent CJK *monospace* families for the game language, then generic
/// monospace baselines. FontCtx picks the first one installed.
fn font_candidates(lang: Language, configured: &str) -> Vec<String> {
    let lang_fonts: &[&str] = match lang {
        Language::Korean => &[
            "D2Coding",
            "NanumGothicCoding",
            "GulimChe",
            "굴림체",
            "DotumChe",
            "돋움체",
            "Sarasa Mono K",
            "Noto Sans Mono CJK KR",
        ],
        Language::Japanese => &[
            "MS Gothic",
            "ＭＳ ゴシック",
            "Sarasa Mono J",
            "Noto Sans Mono CJK JP",
        ],
        Language::ChineseHans | Language::ChineseHant => &[
            "NSimSun",
            "Sarasa Mono SC",
            "Sarasa Mono TC",
            "Noto Sans Mono CJK SC",
        ],
    };

    let mut out: Vec<String> = Vec::new();
    if !configured.is_empty() {
        out.push(configured.to_string());
    }
    out.extend(lang_fonts.iter().map(|s| s.to_string()));
    out.extend(
        ["DejaVu Sans Mono", "Noto Sans Mono"]
            .iter()
            .map(|s| s.to_string()),
    );
    out
}

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(clap::Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(value_parser, default_value = ".", help = "ERA game path")]
    target_path: String,
    #[clap(long, default_value = "info", help = "Log level")]
    log_level: String,
    #[clap(long, help = "Don't print logs")]
    quite: bool,
    #[clap(long, help = "Load bytecode")]
    load: bool,
    #[clap(long, help = "Turn off ERB lint")]
    lint_off: bool,
}

fn main() {
    use flexi_logger::*;
    let args: Args = clap::Parser::parse();

    let _handle = if args.quite {
        None
    } else {
        Some(
            Logger::try_with_str(format!(
                "warn,wgpu_hal=off,naga=warn,erars={level},erars_renderer={level}",
                level = &args.log_level
            ))
            .unwrap()
            .log_to_file(
                FileSpec::default()
                    .directory(Path::new(&args.target_path).join("logs"))
                    .basename("erars"),
            )
            .write_mode(WriteMode::BufferAndFlush)
            .start()
            .unwrap(),
        )
    };
    log_panics::init();

    let config = load_config(&args.target_path);
    let event_loop = EventLoop::<Wake>::with_user_event().build().unwrap();
    let proxy = event_loop.create_proxy();

    let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(move || {
        let _ = proxy.send_event(Wake);
    }));

    let font_size = config.font_size;
    let line_height = config.line_height;
    let init_size = (config.window_width, config.window_height);
    // Ordered list of default-font candidates: the configured family first,
    // then coherent CJK monospace fonts for the game's language. FontCtx uses
    // the first that is actually installed so one font renders both Latin and
    // CJK on the same 1:2 grid.
    let font_candidates = font_candidates(config.lang, &config.font_family);

    let target_path = args.target_path.clone();
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .name("erars-runtime".into())
        .spawn(move || {
            let system_back = system.clone();
            let system = Box::new(system);
            let ret = if args.load {
                unsafe { load_script(&target_path, system, config) }
            } else {
                run_script(&target_path, system, config, false, !args.lint_off)
            };
            let normal = match ret {
                Ok((vm, mut ctx, mut tx)) => vm.start(&mut tx, &mut ctx),
                Err(err) => {
                    log::error!("Game loading failed: {err}");
                    false
                }
            };
            if normal {
                system_back.send_quit();
            }
        })
        .unwrap();

    let candidate_refs: Vec<&str> = font_candidates.iter().map(String::as_str).collect();
    let font = font::FontCtx::with_candidates(&candidate_refs, font_size, line_height);
    let mut app = App::new(font, receiver, init_size);
    event_loop.run_app(&mut app).unwrap();
}
