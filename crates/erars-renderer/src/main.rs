#![windows_subsystem = "windows"]

mod app;
mod atlas;
#[allow(dead_code)] // the Layout-based build_instances is wired into app.rs/headless.rs in T9/T10
mod draw;
#[allow(dead_code)] // RasterFlags is consumed by the shaper/raster rewrite (T6/T8)
mod flags;
#[allow(dead_code)] // FontChain is wired into the app in T10; FontCtx stays until then
mod font;
mod gpu;
mod grid;
#[allow(dead_code)] // wired into app.rs / headless.rs by T10
mod layout;
#[allow(dead_code)]
mod headless;
#[allow(dead_code)] // GlyphRaster replaces atlas.rs in T10
mod raster;
#[cfg(test)]
mod test_support;
#[allow(dead_code)] // Shaper/CellMetrics are consumed by layout (T7) and the app (T10)
mod text;

use std::{path::Path, sync::Arc};

use app::{App, Wake};
use erars_compiler::Language;
use erars_loader::{load_config, load_script, run_script};
use winit::event_loop::EventLoop;

/// Build the ordered default-font candidate list for the legacy `FontCtx`:
/// the configured family first, then the per-language fixed-pitch CJK
/// families from `font::language_candidates`, then generic monospace
/// baselines. (Task 10 replaces this with `font::FontChain::new`.)
fn font_candidates(lang: Language, configured: &str) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    if !configured.is_empty() {
        out.push(configured.to_string());
    }
    out.extend(
        font::language_candidates(lang)
            .iter()
            .map(|s| s.to_string()),
    );
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
    #[clap(
        long,
        value_name = "PATH.png",
        help = "Headless: render the first screen to a PNG image and exit (no display needed)"
    )]
    headless_shot: Option<String>,
}

/// Spawn the VM runtime thread driving `system`.
fn spawn_vm(
    target_path: String,
    load: bool,
    lint: bool,
    system: erars_proxy_system::ProxySystem,
    config: erars_compiler::EraConfig,
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
/// the current screen (with an empty input strip) to a PNG file and exit.
fn headless_shot(
    mut shaper: text::Shaper,
    receiver: erars_proxy_system::ProxyReceiver,
    (w, h): (u32, u32),
    path: &str,
) {
    use erars_proxy_system::SystemRequest;
    let mut frame = erars_proxy_system::ConsoleFrame::default();
    // Drain requests until the game blocks for input (screen is settled).
    loop {
        match receiver.req_rx.recv() {
            Ok(SystemRequest::Redraw(f)) => frame = f,
            Ok(SystemRequest::Input(_)) | Ok(SystemRequest::Quit) | Err(_) => break,
        }
    }
    match headless::render_frame(&mut shaper, &frame, w, h, Some(""), None) {
        Some(img) => match headless::write_png(path, &img) {
            Ok(()) => println!("Wrote {path} ({w}x{h})"),
            Err(e) => eprintln!("Failed to write {path}: {e}"),
        },
        None => eprintln!("No GPU adapter available for headless rendering"),
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

    let font_size = config.font_size;
    let line_height = config.line_height;
    let init_size = (config.window_width, config.window_height);
    // Ordered list of default-font candidates: the configured family first,
    // then coherent CJK monospace fonts for the game's language. FontCtx uses
    // the first that is actually installed so one font renders both Latin and
    // CJK on the same 1:2 grid.
    let font_candidates = font_candidates(config.lang, &config.font_family);
    let build_font = move || {
        let refs: Vec<&str> = font_candidates.iter().map(String::as_str).collect();
        font::FontCtx::with_candidates(&refs, font_size, line_height)
    };
    let target_path = args.target_path.clone();

    // Headless capture mode: no window, no display server required.
    if let Some(path) = args.headless_shot.clone() {
        let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(|| {}));
        let shaper = headless::shaper_for(&config, Path::new(&args.target_path));
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

    let mut app = App::new(build_font(), receiver, init_size);
    event_loop.run_app(&mut app).unwrap();
}
