#![windows_subsystem = "windows"]

use std::path::Path;
use std::sync::Arc;

use erars_compiler::EraConfig;
use erars_loader::{load_config, load_script, run_script};
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest};
use erars_renderer::app::{App, AppConfig, Wake};
use erars_renderer::headless;
use erars_renderer::text::Shaper;
use winit::event_loop::EventLoop;

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
    #[clap(
        long,
        value_name = "PATH.png",
        help = "Headless: render the first screen to a PNG image and exit (no display needed)"
    )]
    headless_shot: Option<String>,
    #[clap(
        long,
        help = "Never use embedded bitmap strikes (e.g. MS Gothic 10-22 px); always \
                rasterize outlines. Applies to the window only; ignored under --headless-shot"
    )]
    no_bitmap_strikes: bool,
}

/// Spawn the VM runtime thread driving `system`.
fn spawn_vm(
    target_path: String,
    load: bool,
    lint: bool,
    system: erars_proxy_system::ProxySystem,
    config: EraConfig,
) {
    std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .name("erars-runtime".into())
        .spawn(move || {
            let system_back = system.clone();
            let system = Box::new(system);
            let ret = if load {
                unsafe { load_script(&target_path, system, config) }
            } else {
                run_script(&target_path, system, config, false, lint)
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
}

/// Headless capture: run the game until it first waits for input, then render
/// that screen (`window_width × window_height`; the input strip is shown when
/// the game is waiting for input) to a PNG file and exit. No window/display.
fn headless_shot(mut shaper: Shaper, receiver: ProxyReceiver, (w, h): (u32, u32), path: &str) {
    let mut frame = ConsoleFrame::default();
    let mut input: Option<&str> = None;
    // Drain requests until the game blocks for input (screen is settled).
    loop {
        match receiver.req_rx.recv() {
            Ok(SystemRequest::Redraw(f)) => frame = f,
            Ok(SystemRequest::Input(_)) => {
                input = Some("");
                break;
            }
            Ok(SystemRequest::Quit) | Err(_) => break,
        }
    }
    match headless::render_frame(&mut shaper, &frame, w, h, input, None) {
        Ok(img) => match headless::write_png(path, &img) {
            Ok(()) => println!("Wrote {path} ({}x{})", img.width, img.height),
            Err(e) => {
                eprintln!("Failed to write {path}: {e}");
                std::process::exit(1);
            }
        },
        Err(e) => {
            eprintln!("Headless render failed: {e}");
            std::process::exit(1);
        }
    }
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
                level = args.log_level
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
    let target_path = args.target_path.clone();
    let init_size = (config.window_width, config.window_height);
    let app_cfg = AppConfig {
        font_size: config.font_size,
        line_height: config.line_height,
        default_fg: config.fore_color,
        init_size,
        use_bitmap_strikes: !args.no_bitmap_strikes,
    };
    // Fonts: configured family → <game>/font → ERARS_FONT_DIR → per-language
    // CJK monospace → bundled Noto Sans Mono; metrics at scale 1.0 (the
    // window applies its real scale factor through Shaper::set_metrics).
    let shaper = headless::shaper_for(&config, Path::new(&target_path));

    // Headless capture mode: no window, no display server required.
    if let Some(path) = args.headless_shot.clone() {
        let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(|| {}));
        spawn_vm(target_path, args.load, !args.lint_off, system, config);
        headless_shot(shaper, receiver, init_size, &path);
        return;
    }

    let event_loop = EventLoop::<Wake>::with_user_event().build().unwrap();
    let proxy = event_loop.create_proxy();
    let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(move || {
        let _ = proxy.send_event(Wake);
    }));
    spawn_vm(target_path, args.load, !args.lint_off, system, config);

    let mut app = App::new(shaper, receiver, app_cfg);
    event_loop.run_app(&mut app).unwrap();
}
