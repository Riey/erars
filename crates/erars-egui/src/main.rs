#![windows_subsystem = "windows"]

use std::{path::Path, sync::Arc};

use eframe::App;
use egui::{FontData, FontFamily, Widget};
use erars_ast::{Alignment, Value};
use erars_loader::{load_config, load_script, run_script};
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest, SystemResponse};
use erars_ui::{ConsoleLinePart, InputRequest, InputRequestType};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(clap::Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(
        value_parser,
        default_value = ".",
        help = "ERA game path default is current path"
    )]
    target_path: String,

    #[clap(
        long,
        default_value = "info",
        help = "Log level (error, warn, info, debug, trace)"
    )]
    log_level: String,

    #[clap(long, help = "Don't print logs")]
    quite: bool,

    #[clap(long, help = "Save bytecode")]
    save: bool,

    #[clap(long, help = "Load bytecode")]
    load: bool,

    #[clap(long, help = "Turn off ERB lint")]
    lint_off: bool,
}

fn load_font_data(source: fontdb::Source) -> egui::FontData {
    match source {
        fontdb::Source::Binary(bin) | fontdb::Source::SharedFile(_, bin) => {
            FontData::from_owned(Vec::from((*bin).as_ref()))
        }
        fontdb::Source::File(file) => FontData::from_owned(std::fs::read(file).unwrap()),
    }
}

fn main() {
    use flexi_logger::*;

    let args: Args = clap::Parser::parse();

    let _handle = if args.quite {
        None
    } else {
        Some(
            Logger::try_with_str(format!("warn,erars={}", &args.log_level))
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

    let config = load_config(&args.target_path);

    eframe::run_native(
        "erars",
        eframe::NativeOptions {
            renderer: eframe::Renderer::Wgpu,
            default_theme: eframe::Theme::Dark,
            viewport: egui::ViewportBuilder::default()
                .with_inner_size(egui::vec2(
                    config.window_width as _,
                    config.window_height as _,
                ))
                .with_drag_and_drop(true),
            ..Default::default()
        },
        Box::new(move |ctx| {
            let egui_ctx = ctx.egui_ctx.clone();
            let (system, receiver) =
                erars_proxy_system::new_proxy(Arc::new(move || egui_ctx.request_repaint()));
            let font_size = config.font_size;
            let line_height = config.line_height;

            let mut db = fontdb::Database::new();
            db.load_system_fonts();

            let config_font = db
                .query(&fontdb::Query {
                    families: &[
                        fontdb::Family::Name(&config.font_family),
                        fontdb::Family::Name("GulimChe"),
                        fontdb::Family::Name("D2Coding"),
                    ],
                    ..Default::default()
                })
                .unwrap();

            let fallback = db.query(&fontdb::Query {
                families: &[
                    fontdb::Family::Name("GulimChe"),
                    fontdb::Family::Name("D2Coding"),
                ],
                ..Default::default()
            });

            let jp_fallback = db.query(&fontdb::Query {
                families: &[fontdb::Family::Name("Meiryo")],
                ..Default::default()
            });

            let emoji_font = db.query(&fontdb::Query {
                families: &[
                    fontdb::Family::Name("Noto Color Emoji"),
                    fontdb::Family::Name("Noto Emoji"),
                    fontdb::Family::Name("Segoe UI Emoji"),
                ],
                ..Default::default()
            });

            std::thread::Builder::new()
                .stack_size(8 * 1024 * 1024)
                .name("erars-runtime".into())
                .spawn(move || {
                    let system_back = system.clone();
                    let system = Box::new(system);
                    let ret = if args.load {
                        unsafe { load_script(&args.target_path, system, config) }
                    } else {
                        run_script(&args.target_path, system, config, false, !args.lint_off)
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
            let data = load_font_data(db.face_source(config_font).unwrap().0);
            let fallback_data =
                fallback.map(|fallback| load_font_data(db.face_source(fallback).unwrap().0));
            let jp_fallback_data = jp_fallback
                .map(|jp_fallback| load_font_data(db.face_source(jp_fallback).unwrap().0));
            let emoji_data =
                emoji_font.map(|emoji_font| load_font_data(db.face_source(emoji_font).unwrap().0));

            let mut widgets = egui::style::Widgets::dark();
            widgets.noninteractive.bg_fill = egui::Color32::BLACK;
            widgets.noninteractive.fg_stroke.color = egui::Color32::WHITE;
            widgets.inactive.fg_stroke.color = egui::Color32::WHITE;
            widgets.hovered.fg_stroke.color = egui::Color32::YELLOW;

            let mut style = egui::Style {
                visuals: egui::Visuals {
                    button_frame: true,
                    extreme_bg_color: egui::Color32::BLACK,
                    faint_bg_color: egui::Color32::BLACK,
                    widgets,
                    panel_fill: egui::Color32::BLACK,
                    window_fill: egui::Color32::BLACK,
                    ..egui::Visuals::dark()
                },
                ..(*ctx.egui_ctx.style()).clone()
            };

            style.spacing.item_spacing.x = 0.0;
            style.spacing.item_spacing.y = (line_height.saturating_sub(font_size)) as f32;

            for font_id in style.text_styles.values_mut() {
                font_id.family = FontFamily::Monospace;
                font_id.size = font_size as f32;
            }

            let mut font_def = egui::FontDefinitions::default();
            font_def.families.insert(
                FontFamily::Monospace,
                vec![
                    "default".into(),
                    "fallback".into(),
                    "jp_fallback".into(),
                    "emoji".into(),
                ],
            );
            font_def.families.insert(
                FontFamily::Proportional,
                vec![
                    "default".into(),
                    "fallback".into(),
                    "jp_fallback".into(),
                    "emoji".into(),
                ],
            );
            font_def.font_data.insert("default".into(), data);
            if let Some(fallback_data) = fallback_data {
                font_def.font_data.insert("fallback".into(), fallback_data);
            } else {
                log::warn!("Can't find fallback font");
            }
            if let Some(jp_fallback_data) = jp_fallback_data {
                font_def.font_data.insert("jp_fallback".into(), jp_fallback_data);
            } else {
                log::warn!("Can't find jp fallback font");
            }
            if let Some(emoji_data) = emoji_data {
                font_def.font_data.insert("emoji".into(), emoji_data);
            } else {
                log::warn!("Can't find emoji font");
            }

            ctx.egui_ctx.set_fonts(font_def);
            ctx.egui_ctx.set_style(style);

            Box::new(EraApp::new(receiver, line_height))
        }),
    )
    .unwrap();
}

struct EraApp {
    current_req: Option<InputRequest>,
    need_scroll_down: bool,
    skip: bool,
    receiver: ProxyReceiver,
    console_frame: ConsoleFrame,
    input: String,
    line_height: u32,
}

impl EraApp {
    pub fn new(receiver: ProxyReceiver, line_height: u32) -> Self {
        Self {
            current_req: None,
            need_scroll_down: false,
            skip: false,
            receiver,
            console_frame: ConsoleFrame::default(),
            input: String::new(),
            line_height,
        }
    }
}

impl App for EraApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        while let Ok(req) = self.receiver.req_rx.try_recv() {
            match req {
                SystemRequest::Quit => {
                    ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                }
                SystemRequest::Input(req) => {
                    log::info!("Req <- {:?}", req.ty);

                    match req.ty {
                        InputRequestType::AnyKey | InputRequestType::EnterKey if self.skip => {
                            self.receiver.res_tx.send(SystemResponse::Empty).unwrap();
                        }
                        _ => {
                            self.skip = false;
                            self.current_req = Some(req);
                        }
                    }
                }
                SystemRequest::Redraw(console_frame) => {
                    self.console_frame = console_frame;
                    self.need_scroll_down = true;
                }
            }
        }

        self.draw_console(ctx, frame);
    }

    fn clear_color(&self, _visuals: &egui::Visuals) -> [f32; 4] {
        let [r, g, b] = self.console_frame.bg_color.0;

        [r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0, 1.0]
    }
}

impl EraApp {
    fn draw_console(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let current_input_gen = self.current_req.as_ref().map(|req| req.generation);
        let receiver = &self.receiver;

        if ctx.input(|i| i.pointer.button_down(egui::PointerButton::Secondary))
            && self.current_req.as_ref().map_or(true, |req| {
                matches!(
                    req.ty,
                    InputRequestType::AnyKey | InputRequestType::EnterKey
                )
            })
        {
            self.skip = true;
        }

        egui::TopBottomPanel::top("setting").show(ctx, |ui| {
            ui.menu_button("Setting", |ui| {
                if ui.button("Exit").clicked() {
                    ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                }
            });
        });

        egui::TopBottomPanel::bottom("text_input_panel").show(ctx, |ui| {
            let text_edit = ui.add_enabled(
                self.current_req.is_some(),
                egui::TextEdit::singleline(&mut self.input).desired_width(f32::INFINITY),
            );

            if self.current_req.is_some() && !text_edit.has_focus() {
                text_edit.request_focus();
            }
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let panel_size = ui.available_size();
            let console_show_lines = (panel_size.y / self.line_height as f32).floor() as usize;
            let padding = console_show_lines.saturating_sub(self.console_frame.lines.len());

            egui::ScrollArea::vertical()
                .max_width(ui.available_width())
                .stick_to_bottom(true)
                .auto_shrink([false, false])
                .show(ui, |ui| {
                    for _ in 0..padding {
                        ui.label("");
                    }
                    for line in self.console_frame.lines.iter() {
                        match line.align {
                            Alignment::Left => {
                                ui.horizontal_wrapped(|ui| {
                                    if ui.is_visible() {
                                        for part in line.parts.iter() {
                                            Self::draw_console_part(
                                                ui,
                                                &mut self.current_req,
                                                current_input_gen,
                                                part,
                                                receiver,
                                            );
                                        }
                                    }
                                });
                            }
                            Alignment::Center => {
                                ui.vertical_centered(|ui| {
                                    if ui.is_visible() {
                                        for part in line.parts.iter() {
                                            Self::draw_console_part(
                                                ui,
                                                &mut self.current_req,
                                                current_input_gen,
                                                part,
                                                receiver,
                                            );
                                        }
                                    }
                                });
                            }
                            Alignment::Right => {
                                ui.with_layout(
                                    egui::Layout::right_to_left(egui::Align::TOP),
                                    |ui| {
                                        if ui.is_visible() {
                                            for part in line.parts.iter().rev() {
                                                Self::draw_console_part(
                                                    ui,
                                                    &mut self.current_req,
                                                    current_input_gen,
                                                    part,
                                                    receiver,
                                                );
                                            }
                                        }
                                    },
                                );
                            }
                        }
                    }

                    if self.need_scroll_down {
                        ui.scroll_to_cursor(Some(egui::Align::BOTTOM));
                        self.need_scroll_down = false;
                    }
                });
        });

        match self.current_req.as_ref() {
            Some(req) => match req.ty {
                InputRequestType::AnyKey => {
                    if ctx.input(|i| i.pointer.button_clicked(egui::PointerButton::Primary))
                        || self.skip
                        || ctx.input(|i| {
                            i.events
                                .iter()
                                .any(|e| matches!(e, egui::Event::Key { pressed: true, .. }))
                        })
                    {
                        self.receiver.res_tx.send(SystemResponse::Empty).unwrap();
                        self.input.clear();
                        self.current_req = None;
                        log::info!("Res -> Empty");
                    }
                }
                InputRequestType::EnterKey | InputRequestType::ForceEnterKey => {
                    if ctx.input(|i| i.pointer.button_clicked(egui::PointerButton::Primary))
                        || self.skip
                        || ctx.input(|i| i.key_down(egui::Key::Enter))
                    {
                        self.receiver.res_tx.send(SystemResponse::Empty).unwrap();
                        self.input.clear();
                        self.current_req = None;
                        log::info!("Res -> Empty");
                    }
                }
                InputRequestType::Int => {
                    if ctx.input(|i| i.key_down(egui::Key::Enter)) {
                        if let Ok(i) = self.input.parse() {
                            log::info!("Res -> {i}");
                            self.receiver
                                .res_tx
                                .send(SystemResponse::Input(Value::Int(i)))
                                .unwrap();
                            self.input.clear();
                            self.current_req = None;
                        }
                    }
                }
                InputRequestType::Str => {
                    if ctx.input(|i| i.key_down(egui::Key::Enter)) {
                        log::info!("Res -> \"{}\"", self.input);
                        self.receiver
                            .res_tx
                            .send(SystemResponse::Input(Value::String(std::mem::take(
                                &mut self.input,
                            ))))
                            .unwrap();
                        self.input.clear();
                        self.current_req = None;
                    }
                }
            },
            None => {}
        }
    }

    fn draw_console_part(
        ui: &mut egui::Ui,
        current_input: &mut Option<InputRequest>,
        current_input_gen: Option<u32>,
        part: &ConsoleLinePart,
        receiver: &ProxyReceiver,
    ) {
        match part {
            ConsoleLinePart::Button(btn_parts, input_gen, value) => {
                for (text, style) in btn_parts.iter() {
                    let enabled = current_input_gen == Some(*input_gen);
                    let btn_res = EraButton::new(text.clone(), style, enabled).ui(ui);
                    if enabled && btn_res.on_hover_cursor(egui::CursorIcon::PointingHand).clicked()
                    {
                        log::info!("Res -> {value:?}");
                        *current_input = None;
                        receiver.res_tx.send(SystemResponse::Input(value.clone())).unwrap();
                    }
                }
            }
            ConsoleLinePart::Text(s, color) => {
                ui.colored_label(to_egui_color(color.color), s);
            }
            ConsoleLinePart::Line(text, color) => {
                let width = ui.available_width();
                let font_id = &ui.style().text_styles[&egui::TextStyle::Monospace];
                let char_width = text
                    .chars()
                    .map(|c| ui.fonts(|f| f.glyph_width(font_id, c)))
                    .sum::<f32>();
                let s = text.repeat((width / char_width).floor() as usize);
                ui.colored_label(to_egui_color(color.color), s);
            }
        }
    }
}

fn to_egui_color(c: erars_ui::Color) -> egui::Color32 {
    let [r, g, b] = c.0;
    egui::Color32::from_rgb(r, g, b)
}

struct EraButton {
    text: egui::WidgetText,
    color: egui::Color32,
    hl_color: egui::Color32,
    enabled: bool,
}

impl EraButton {
    pub fn new(text: String, style: &erars_ui::TextStyle, enabled: bool) -> Self {
        Self {
            text: text.into(),
            color: to_egui_color(style.color),
            hl_color: egui::Color32::YELLOW,
            enabled,
        }
    }
}

impl Widget for EraButton {
    fn ui(self, ui: &mut egui::Ui) -> egui::Response {
        let text = self.text.into_galley(
            ui,
            Some(false),
            ui.available_width(),
            egui::TextStyle::Button,
        );
        let (rect, response) = ui.allocate_exact_size(
            text.size(),
            if self.enabled {
                egui::Sense::click()
            } else {
                egui::Sense::hover()
            },
        );
        response.widget_info(|| egui::WidgetInfo::labeled(egui::WidgetType::Button, text.text()));

        if ui.is_rect_visible(rect) {
            let text_pos = ui.layout().align_size_within_rect(text.size(), rect).min;

            let color = if self.enabled && response.hovered() {
                self.hl_color
            } else {
                self.color
            };

            ui.painter().galley_with_override_text_color(text_pos, text, color);
        }

        response
    }
}
