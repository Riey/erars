#![windows_subsystem = "windows"]

use std::{path::Path, sync::Arc};

use erars_ast::{get_interner, Alignment, Value};
use erars_loader::{load_config, load_script, run_script};
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest, SystemResponse};
use erars_ui::{ConsoleLinePart, InputRequest, InputRequestType};
use futures_util::StreamExt;
use iced::{
    futures::channel::oneshot,
    theme::Palette,
    widget::{
        button, column, horizontal_rule, row, scrollable, slider, text, text::Shaping, text_input,
        vertical_space,
    },
    window, Element, Length, Subscription, Task,
};

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

fn main() {
    use flexi_logger::*;

    let args: Args = clap::Parser::parse();

    let _handle = if args.quite {
        None
    } else {
        Some(
            Logger::try_with_str(format!(
                "warn,wgpu_hal=off,erars={level},erars-iced={level},cosmic-text=trace",
                level = &args.log_level
            ))
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
    let (system, receiver) = erars_proxy_system::new_proxy(Arc::new(move || {}));
    let (interner_tx, interner_rx) = flume::bounded(0);
    let font_family = config.font_family.clone();
    let font_size = config.font_size;
    let line_height = config.line_height;

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
            interner_tx.send(get_interner()).unwrap();
            drop(interner_tx);
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

    let interner = interner_rx.recv().unwrap();
    drop(interner_rx);

    let mut app = iced::application("erars", EraApp::update, EraApp::view)
        .subscription(move |state| {
            Subscription::run_with_id(
                0,
                state
                    .receiver
                    .req_rx
                    .clone()
                    .into_stream()
                    .map(Message::SystemRequest),
            )
        })
        .antialiasing(true)
        .default_font(iced::Font::with_name(
            interner.resolve(&interner.get_or_intern(font_family)),
        ))
        .theme(EraApp::theme);

    for font in [
        "C:\\Windows\\Fonts\\msgothic.ttc",
        "C:\\Windows\\Fonts\\YuGothR.ttc",
        "C:\\Windows\\Fonts\\malgun.ttf",
        "C:\\Windows\\Fonts\\Gulim.ttc",
    ] {
        app = app.font(std::fs::read(font).unwrap());
    }

    app.run_with(move || (EraApp::new(interner, receiver, line_height), Task::none()))
        .unwrap();
}

struct EraApp {
    interner: &'static erars_ast::Interner,
    current_req: Option<InputRequest>,
    need_scroll_down: bool,
    skip: bool,
    console_frame: ConsoleFrame,
    input: String,
    line_height: u32,
    receiver: ProxyReceiver,

    current_scroll_offset: scrollable::RelativeOffset,
}

impl EraApp {
    pub fn new(
        interner: &'static erars_ast::Interner,
        receiver: ProxyReceiver,
        line_height: u32,
    ) -> Self {
        Self {
            interner,
            receiver,
            current_req: None,
            need_scroll_down: false,
            skip: false,
            console_frame: ConsoleFrame::default(),
            input: String::new(),
            line_height,

            current_scroll_offset: scrollable::RelativeOffset::START,
        }
    }

    fn send_input(&mut self, input: Value) {
        assert!(self.current_req.is_some());
        self.current_req = None;
        self.receiver.res_tx.send(SystemResponse::Input(input)).unwrap();
    }

    fn update(&mut self, message: Message) -> Task<Message> {
        log::info!("Message: {:?}", message);
        match message {
            Message::SystemRequest(req) => match req {
                SystemRequest::Quit => window::get_latest().and_then(window::close),
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
                    Task::none()
                }
                SystemRequest::Redraw(console_frame) => {
                    self.console_frame = console_frame;
                    self.need_scroll_down = true;
                    Task::none()
                }
            },
            Message::Scrolled(viewport) => {
                self.current_scroll_offset = viewport.relative_offset();
                Task::none()
            }
            Message::InputEdited(input) => {
                if let Some(req) = self.current_req.as_ref() {
                    match req.ty {
                        InputRequestType::Int => {
                            if input.chars().all(|c| c.is_ascii_digit()) {
                                self.input = input;
                            }
                        }
                        InputRequestType::AnyKey => {
                            self.send_input(Value::EMPTY);
                        }
                        _ => {
                            self.input = input;
                        }
                    }
                }
                Task::none()
            }
            Message::InputSubmitted => {
                let Some(req) = self.current_req.as_ref() else {
                    unreachable!();
                };

                let input = std::mem::take(&mut self.input);

                match req.ty {
                    InputRequestType::Int => {
                        let Ok(i) = input.parse() else {
                            unreachable!();
                        };
                        self.send_input(Value::Int(i));
                    }
                    _ => {
                        self.send_input(Value::String(input));
                    }
                }
                Task::none()
            }
            Message::ButtonClicked(value) => {
                self.send_input(value);
                Task::none()
            }
        }

        // self.draw_console(ctx, frame);
    }

    fn view(&self) -> Element<Message> {
        let current_input_gen = self.current_req.as_ref().map(|req| req.generation);

        // egui::TopBottomPanel::top("setting").show(ctx, |ui| {
        //     ui.menu_button("Setting", |ui| {
        //         if ui.button("Exit").clicked() {
        //             ctx.send_viewport_cmd(egui::ViewportCommand::Close);
        //         }
        //     });
        // });

        let mut console_content = Vec::new();

        for line in self.console_frame.lines.iter() {
            let mut line_content = Vec::new();
            for part in line.parts.iter() {
                line_content.push(self.draw_console_part(current_input_gen, part));
            }
            console_content.push(row(line_content).height(self.line_height as f32).into());
        }

        let console = scrollable(column(console_content))
            .direction(scrollable::Direction::Vertical(scrollable::Scrollbar::new()))
            .height(Length::Fill)
            .width(Length::Fill)
            .on_scroll(Message::Scrolled);
        column![
            console,
            text_input("", &self.input)
                .on_input(Message::InputEdited)
                .on_paste(Message::InputEdited)
                .on_submit(Message::InputSubmitted)
                .width(Length::Fill)
        ]
        .height(Length::Fill)
        .into()

        // egui::CentralPanel::default().show(ctx, |ui| {
        //     let panel_size = ui.available_size();
        //     let console_show_lines = (panel_size.y / self.line_height as f32).floor() as usize;
        //     let padding = console_show_lines.saturating_sub(self.console_frame.lines.len());

        //     egui::ScrollArea::vertical()
        //         .max_width(ui.available_width())
        //         .stick_to_bottom(true)
        //         .auto_shrink([false, false])
        //         .show(ui, |ui| {
        //             for _ in 0..padding {
        //                 ui.label("");
        //             }
        //             for line in self.console_frame.lines.iter() {
        //                 match line.align {
        //                     Alignment::Left => {
        //                         ui.horizontal_wrapped(|ui| {
        //                             if ui.is_visible() {
        //                                 for part in line.parts.iter() {
        //                                     Self::draw_console_part(
        //                                         ui,
        //                                         &mut self.current_req,
        //                                         current_input_gen,
        //                                         part,
        //                                         receiver,
        //                                     );
        //                                 }
        //                             }
        //                         });
        //                     }
        //                     Alignment::Center => {
        //                         ui.vertical_centered(|ui| {
        //                             if ui.is_visible() {
        //                                 for part in line.parts.iter() {
        //                                     Self::draw_console_part(
        //                                         ui,
        //                                         &mut self.current_req,
        //                                         current_input_gen,
        //                                         part,
        //                                         receiver,
        //                                     );
        //                                 }
        //                             }
        //                         });
        //                     }
        //                     Alignment::Right => {
        //                         ui.with_layout(
        //                             egui::Layout::right_to_left(egui::Align::TOP),
        //                             |ui| {
        //                                 if ui.is_visible() {
        //                                     for part in line.parts.iter().rev() {
        //                                         Self::draw_console_part(
        //                                             ui,
        //                                             &mut self.current_req,
        //                                             current_input_gen,
        //                                             part,
        //                                             receiver,
        //                                         );
        //                                     }
        //                                 }
        //                             },
        //                         );
        //                     }
        //                 }
        //             }

        //             if self.need_scroll_down {
        //                 ui.scroll_to_cursor(Some(egui::Align::BOTTOM));
        //                 self.need_scroll_down = false;
        //             }
        //         });
        // });

        // match self.current_req.as_ref() {
        //     Some(req) => match req.ty {
        //         InputRequestType::AnyKey => {
        //             if ctx.input(|i| i.pointer.button_clicked(egui::PointerButton::Primary))
        //                 || self.skip
        //                 || ctx.input(|i| {
        //                     i.events
        //                         .iter()
        //                         .any(|e| matches!(e, egui::Event::Key { pressed: true, .. }))
        //                 })
        //             {
        //                 self.receiver.res_tx.send(SystemResponse::Empty).unwrap();
        //                 self.input.clear();
        //                 self.current_req = None;
        //                 log::info!("Res -> Empty");
        //             }
        //         }
        //         InputRequestType::EnterKey | InputRequestType::ForceEnterKey => {
        //             if ctx.input(|i| i.pointer.button_clicked(egui::PointerButton::Primary))
        //                 || self.skip
        //                 || ctx.input(|i| i.key_down(egui::Key::Enter))
        //             {
        //                 self.receiver.res_tx.send(SystemResponse::Empty).unwrap();
        //                 self.input.clear();
        //                 self.current_req = None;
        //                 log::info!("Res -> Empty");
        //             }
        //         }
        //         InputRequestType::Int => {
        //             if ctx.input(|i| i.key_down(egui::Key::Enter)) {
        //                 if let Ok(i) = self.input.parse() {
        //                     log::info!("Res -> {i}");
        //                     self.receiver
        //                         .res_tx
        //                         .send(SystemResponse::Input(Value::Int(i)))
        //                         .unwrap();
        //                     self.input.clear();
        //                     self.current_req = None;
        //                 }
        //             }
        //         }
        //         InputRequestType::Str => {
        //             if ctx.input(|i| i.key_down(egui::Key::Enter)) {
        //                 log::info!("Res -> \"{}\"", self.input);
        //                 self.receiver
        //                     .res_tx
        //                     .send(SystemResponse::Input(Value::String(std::mem::take(
        //                         &mut self.input,
        //                     ))))
        //                     .unwrap();
        //                 self.input.clear();
        //                 self.current_req = None;
        //             }
        //         }
        //     },
        //     None => {}
        // }
    }

    fn make_font(&self, family: &str) -> iced::Font {
        if family.is_empty() {
            iced::Font::MONOSPACE
        } else {
            iced::Font::with_name(self.interner.resolve(&self.interner.get_or_intern(family)))
        }
    }

    fn draw_console_part<'a>(
        &'a self,
        current_input_gen: Option<u32>,
        part: &'a ConsoleLinePart,
    ) -> Element<'a, Message> {
        log::info!("Draw part: {:?}", part);
        match part {
            ConsoleLinePart::Button(btn_parts, input_gen, value) => row(btn_parts
                .iter()
                .map(|(s, style)| {
                    let enabled = current_input_gen == Some(*input_gen);
                    button(
                        text(s.as_str())
                            .line_height(1.0)
                            .shaping(Shaping::Advanced)
                            .font(self.make_font(&style.font_family)),
                    )
                    .on_press_maybe(if enabled {
                        Some(Message::ButtonClicked(value.clone()))
                    } else {
                        None
                    })
                    .height(self.line_height as f32)
                    .style(move |theme: &iced::Theme, status: button::Status| {
                        let mut text = to_iced_color(style.color);

                        if enabled {
                            match status {
                                button::Status::Hovered => {
                                    text = iced::Color::from_rgb8(0xff, 0xff, 0x00);
                                }
                                button::Status::Disabled => {
                                    text = iced::Color::from_rgb8(0x2f, 0x2f, 0x2f)
                                }
                                _ => {}
                            }
                        }
                        button::Style {
                            text_color: text,
                            background: None,
                            ..Default::default()
                        }
                    })
                    .into()
                })
                .collect::<Vec<_>>())
            .into(),
            ConsoleLinePart::Text(s, color) => text(s.as_str())
                .color(to_iced_color(color.color))
                .font(self.make_font(&color.font_family))
                .line_height(1.0)
                .shaping(Shaping::Advanced)
                .into(),
            ConsoleLinePart::Line(s, color) => {
                text(s.repeat(4))
                    .color(to_iced_color(color.color))
                    .font(self.make_font(&color.font_family))
                    .line_height(1.0)
                    .shaping(Shaping::Advanced)
                    .into()
                // let width = ui.available_width();
                // let font_id = &ui.style().text_styles[&egui::TextStyle::Monospace];
                // let char_width = text
                //     .chars()
                //     .map(|c| ui.fonts(|f| f.glyph_width(font_id, c)))
                //     .sum::<f32>();
                // let s = text.repeat((width / char_width).floor() as usize);
                // ui.colored_label(to_egui_color(color.color), s);
            }
        }
    }

    fn theme(&self) -> iced::Theme {
        iced::Theme::Dark
    }
}

#[inline]
fn to_iced_color(erars_ui::Color([r, g, b]): erars_ui::Color) -> iced::Color {
    iced::Color::from_rgb8(r, g, b)
}

// fn to_egui_color(c: erars_ui::Color) -> egui::Color32 {
//     let [r, g, b] = c.0;
//     egui::Color32::from_rgb(r, g, b)
// }

#[derive(Clone, Debug)]
enum Message {
    SystemRequest(SystemRequest),
    Scrolled(scrollable::Viewport),
    ButtonClicked(Value),
    InputEdited(String),
    InputSubmitted,
}
