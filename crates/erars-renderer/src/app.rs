use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use cosmic_text::SwashCache;
use erars_ast::Value;
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest, SystemResponse};
use erars_ui::{Color, ConsoleLine, ConsoleLinePart, FontStyle, InputRequest, InputRequestType, TextStyle};
use winit::application::ApplicationHandler;
use winit::event::{ElementState, MouseScrollDelta, WindowEvent};
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::{Key, NamedKey};
use winit::window::{Window, WindowId};

use crate::atlas::GlyphAtlas;
use crate::draw::build_instances;
use crate::font::FontCtx;
use crate::gpu::GpuContext;
use crate::grid::{ButtonRegion, Grid};

/// User event used to wake the loop when the VM sends a request.
#[derive(Debug, Clone, Copy)]
pub struct Wake;

pub struct App {
    font: FontCtx,
    swash: SwashCache,
    receiver: ProxyReceiver,
    window: Option<Arc<Window>>,
    gpu: Option<GpuContext>,
    atlas: Option<GlyphAtlas>,

    frame: ConsoleFrame,
    current_req: Option<InputRequest>,
    input: String,
    scroll_y: f32,
    stick_bottom: bool,
    hovered_button: Option<usize>,
    cursor: (f32, f32),
    buttons_cache: Vec<ButtonRegion>,
    init_size: (u32, u32),

    /// When the current input request times out (TINPUT), and the value to
    /// send on expiry.
    timeout_deadline: Option<Instant>,
    timeout_value: Value,
}

/// Current wall-clock time as Unix nanoseconds, matching `Timeout::timeout`.
fn current_unix_nanos() -> i128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as i128)
        .unwrap_or(0)
}

impl App {
    pub fn new(font: FontCtx, receiver: ProxyReceiver, init_size: (u32, u32)) -> Self {
        Self {
            font,
            swash: SwashCache::new(),
            receiver,
            window: None,
            gpu: None,
            atlas: None,
            frame: ConsoleFrame::default(),
            current_req: None,
            input: String::new(),
            scroll_y: 0.0,
            stick_bottom: true,
            hovered_button: None,
            cursor: (0.0, 0.0),
            buttons_cache: Vec::new(),
            init_size,
            timeout_deadline: None,
            timeout_value: Value::Int(0),
        }
    }

    fn send(&mut self, resp: SystemResponse) {
        let _ = self.receiver.res_tx.send(resp);
        self.current_req = None;
        self.input.clear();
        self.timeout_deadline = None;
    }

    /// Drain all pending VM requests, then request a redraw.
    fn drain_requests(&mut self, event_loop: &ActiveEventLoop) {
        while let Ok(req) = self.receiver.req_rx.try_recv() {
            match req {
                SystemRequest::Quit => event_loop.exit(),
                SystemRequest::Redraw(frame) => {
                    self.frame = frame;
                    self.stick_bottom = true;
                }
                SystemRequest::Input(req) => {
                    if let Some(t) = req.timeout.as_ref() {
                        let remaining_ns = (t.timeout - current_unix_nanos()).max(0) as u128;
                        self.timeout_deadline =
                            Some(Instant::now() + Duration::from_nanos(remaining_ns as u64));
                        self.timeout_value = t.default_value.clone();
                    } else {
                        self.timeout_deadline = None;
                    }
                    self.current_req = Some(req);
                }
            }
        }
        if let Some(w) = &self.window {
            w.request_redraw();
        }
    }

    fn input_line(&self) -> ConsoleLine {
        ConsoleLine {
            align: erars_ast::Alignment::Left,
            button_start: None,
            parts: vec![ConsoleLinePart::Text(
                format!("> {}_", self.input),
                TextStyle {
                    color: Color([200, 200, 200]),
                    font_family: "".into(),
                    font_style: FontStyle::NORMAL,
                },
            )],
        }
    }

    fn render(&mut self) {
        if self.gpu.is_none() || self.atlas.is_none() {
            return;
        }

        // Phase 1: compute layout without holding GPU borrows.
        let (win_w, win_h) = self.gpu.as_ref().unwrap().size();
        let cols = ((win_w as f32 / self.font.cell_w).floor() as usize).max(1);
        let active_gen = self.current_req.as_ref().map(|r| r.generation);

        let mut lines = self.frame.lines.clone();
        if self.current_req.is_some() {
            lines.push(self.input_line());
        }
        let hl = self.frame.hl_color.0;
        let hovered = self.hovered_button;

        let grid = Grid::build(&mut self.font, &lines, cols, active_gen, hovered, hl);
        self.buttons_cache = grid.buttons.clone();

        if self.stick_bottom {
            self.scroll_y = (grid.content_h - win_h as f32).max(0.0);
            self.stick_bottom = false;
        }

        let bg = self.frame.bg_color.0;
        let scroll = self.scroll_y;

        // Phase 2: GPU work.
        let gpu = self.gpu.as_mut().unwrap();
        let atlas = self.atlas.as_mut().unwrap();
        let instances = build_instances(
            &gpu.device,
            &gpu.queue,
            &mut self.font.font_system,
            &mut self.swash,
            atlas,
            &grid,
            scroll,
        );
        gpu.render(&atlas.view, &instances, bg);
    }

    fn on_click(&mut self) {
        let Some(active) = self.current_req.as_ref().map(|r| r.generation) else {
            return;
        };
        let (mx, my) = self.cursor;
        let my_content = my + self.scroll_y;
        let hit = self.buttons_cache.iter().find(|b| {
            b.input_gen == active
                && mx >= b.rect[0]
                && mx <= b.rect[0] + b.rect[2]
                && my_content >= b.rect[1]
                && my_content <= b.rect[1] + b.rect[3]
        });
        if let Some(b) = hit {
            let value = b.value.clone();
            self.send(SystemResponse::Input(value));
        }
    }

    fn update_hover(&mut self) {
        let active = self.current_req.as_ref().map(|r| r.generation);
        let (mx, my) = self.cursor;
        let my_content = my + self.scroll_y;
        self.hovered_button = self.buttons_cache.iter().position(|b| {
            active == Some(b.input_gen)
                && mx >= b.rect[0]
                && mx <= b.rect[0] + b.rect[2]
                && my_content >= b.rect[1]
                && my_content <= b.rect[1] + b.rect[3]
        });
    }

    fn submit(&mut self) {
        let Some(req) = self.current_req.clone() else {
            return;
        };
        match req.ty {
            InputRequestType::Int => {
                if let Ok(i) = self.input.trim().parse::<i64>() {
                    self.send(SystemResponse::Input(Value::Int(i)));
                }
            }
            InputRequestType::Str => {
                let s = std::mem::take(&mut self.input);
                self.send(SystemResponse::Input(Value::String(s)));
            }
            InputRequestType::AnyKey
            | InputRequestType::EnterKey
            | InputRequestType::ForceEnterKey => {
                self.send(SystemResponse::Empty);
            }
        }
    }
}

impl ApplicationHandler<Wake> for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let attrs = Window::default_attributes()
            .with_title("erars")
            .with_inner_size(winit::dpi::LogicalSize::new(
                self.init_size.0,
                self.init_size.1,
            ));
        let window = Arc::new(event_loop.create_window(attrs).unwrap());
        self.font.set_scale(window.scale_factor() as f32);
        let size = window.inner_size();
        let instance = wgpu::Instance::default();
        let surface = instance.create_surface(window.clone()).unwrap();
        let gpu = GpuContext::new(&instance, surface, size.width.max(1), size.height.max(1));
        let atlas = GlyphAtlas::new(&gpu.device);
        self.atlas = Some(atlas);
        self.gpu = Some(gpu);
        self.window = Some(window);
        self.drain_requests(event_loop);
    }

    fn user_event(&mut self, event_loop: &ActiveEventLoop, _: Wake) {
        self.drain_requests(event_loop);
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let Some(deadline) = self.timeout_deadline else {
            return;
        };
        if Instant::now() >= deadline {
            let v = self.timeout_value.clone();
            self.send(SystemResponse::Input(v));
        } else {
            // Wake again at the deadline so the timeout can fire on time.
            event_loop.set_control_flow(winit::event_loop::ControlFlow::WaitUntil(deadline));
        }
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                if let Some(gpu) = self.gpu.as_mut() {
                    gpu.resize(size.width, size.height);
                }
                self.stick_bottom = true;
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                self.font.set_scale(scale_factor as f32);
                self.stick_bottom = true;
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            WindowEvent::RedrawRequested => self.render(),
            WindowEvent::CursorMoved { position, .. } => {
                self.cursor = (position.x as f32, position.y as f32);
                self.update_hover();
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: winit::event::MouseButton::Left,
                ..
            } => {
                if let Some(req) = self.current_req.clone() {
                    match req.ty {
                        InputRequestType::AnyKey
                        | InputRequestType::EnterKey
                        | InputRequestType::ForceEnterKey => self.submit(),
                        _ => self.on_click(),
                    }
                    if let Some(w) = &self.window {
                        w.request_redraw();
                    }
                }
            }
            WindowEvent::MouseWheel { delta, .. } => {
                let dy = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y * self.font.cell_h,
                    MouseScrollDelta::PixelDelta(p) => p.y as f32,
                };
                self.scroll_y = (self.scroll_y - dy).max(0.0);
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            WindowEvent::KeyboardInput { event, .. } if event.state == ElementState::Pressed => {
                let Some(req) = self.current_req.clone() else {
                    return;
                };
                match &event.logical_key {
                    Key::Named(NamedKey::Enter) => self.submit(),
                    Key::Named(NamedKey::Backspace) => {
                        self.input.pop();
                    }
                    Key::Named(NamedKey::Space) if matches!(req.ty, InputRequestType::AnyKey) => {
                        self.submit();
                    }
                    Key::Character(s) => match req.ty {
                        InputRequestType::Int => {
                            if s.chars().all(|c| c.is_ascii_digit()) {
                                self.input.push_str(s);
                            }
                        }
                        InputRequestType::Str => self.input.push_str(s),
                        InputRequestType::AnyKey => self.submit(),
                        _ => {}
                    },
                    _ => {
                        if matches!(req.ty, InputRequestType::AnyKey) {
                            self.submit();
                        }
                    }
                }
                if let Some(w) = &self.window {
                    w.request_redraw();
                }
            }
            _ => {}
        }
    }
}
