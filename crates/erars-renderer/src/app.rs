//! winit application: owns the VM proxy, the shaper, the last layout and the
//! view state (scroll / hover / cursor / pending input).
//!
//! The layout is recomputed only when the frame, the content width or the
//! cell metrics change (spec Component 5). Hover, the active input generation
//! and scrolling change only what is drawn: Emuera recolours the pointed
//! button at draw time and never moves anything.
//!
//! View state: `scroll_rows` = whole rows hidden below the bottom of the row
//! area (0 = stuck to the bottom); `strip_h = line_h`; `view_h = window_h −
//! strip_h`; rows are bottom-anchored (slack at the top) — the arithmetic is
//! `draw::View`'s. The input strip shows `> {input}_` in the default colour.

use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use erars_ast::{Alignment, Value};
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest, SystemResponse};
use erars_ui::{
    Color, ConsoleLine, ConsoleLinePart, FontStyle, InputRequest, InputRequestType, TextStyle,
};
use winit::application::ApplicationHandler;
use winit::event::{ElementState, MouseScrollDelta, WindowEvent};
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::{Key, NamedKey};
use winit::window::{Window, WindowId};

use crate::draw::{build_instances, View};
use crate::gpu::{GpuContext, Instance};
use crate::layout::{layout, Geometry, Layout};
use crate::raster::GlyphRaster;
use crate::text::{CellMetrics, Shaper};

/// User event used to wake the loop when the VM sends a request.
#[derive(Debug, Clone, Copy)]
pub struct Wake;

/// Static configuration the app needs besides the shaper.
#[derive(Debug, Clone, Copy)]
pub struct AppConfig {
    /// `emuera.config` フォントサイズ (logical px).
    pub font_size: u32,
    /// `emuera.config` 一行の高さ (logical px).
    pub line_height: u32,
    /// `emuera.config` 文字色 — input strip colour until the first frame
    /// (afterwards `ConsoleFrame::fore_color`, the console's default colour).
    pub default_fg: [u8; 3],
    /// Initial window inner size (logical px): ウィンドウ幅 × ウィンドウ高さ.
    pub init_size: (u32, u32),
    /// `false` with `--no-bitmap-strikes`.
    pub use_bitmap_strikes: bool,
}

// ---------------------------------------------------------------------------
// Pure view arithmetic (spec Component 5 "View state"); unit-tested below.
// Row placement is `draw::View`'s; button geometry is Emuera's inclusive hit
// rect over `Layout.buttons` (spec Component 5 "Buttons").
// ---------------------------------------------------------------------------

/// Largest `scroll_rows` that still keeps the row area full.
pub fn max_scroll(rows: usize, visible: usize) -> usize {
    rows.saturating_sub(visible)
}

/// Clamp a requested scroll position into `[0, max]`.
pub fn clamp_scroll(requested: i64, max: usize) -> usize {
    requested.clamp(0, max as i64) as usize
}

/// Convert accumulated wheel pixels into whole rows (sign = direction) and
/// keep the remainder in `acc`.
pub fn wheel_rows(acc: &mut f64, delta: f64, line_h: u32) -> i64 {
    *acc += delta;
    let unit = line_h.max(1) as f64;
    let rows = (*acc / unit).trunc();
    *acc -= rows * unit;
    rows as i64
}

/// Wheel travel of `notches` mouse-wheel detents in pixels. One detent scrolls
/// one row, so a detent is `line_h` px; feeding this to [`wheel_rows`] lets
/// `LineDelta` and `PixelDelta` share one accumulator and one conversion.
pub fn wheel_notch_px(notches: f32, line_h: u32) -> f64 {
    notches as f64 * line_h as f64
}

/// The row drawn under screen `y` and the offset of `y` inside it, or `None`
/// for the top slack, the input strip and off-screen rows. Inverse of
/// [`View::row_y`]: row `r` covers `[row_y(r), row_y(r) + line_h)`.
pub fn row_at(rows: usize, view: &View, line_h: u32, y: i64) -> Option<(usize, i32)> {
    if line_h == 0 || y < 0 || y >= view.view_h as i64 {
        return None;
    }
    let bottom = rows.checked_sub(1)? - view.scroll_rows.min(rows - 1);
    let below = ((view.view_h as i64 - 1 - y) / line_h as i64) as usize;
    if below > bottom || below >= view.visible_rows(line_h) {
        return None;
    }
    let r = bottom - below;
    let top = view.row_y(rows, r, line_h)?;
    Some((r, (y - top as i64) as i32))
}

/// Button fragment under the cursor (physical px), as an index into
/// `layout.buttons`: the row from [`row_at`], then Emuera's inclusive hit rect
/// (spec Component 5) — `shift + x0 + x ≤ px ≤ shift + x0 + x + w` and
/// `0 ≤ dy ≤ min(font_px, line_h − 1)` (i.e. the rect
/// `[shift + x0 + x, row_y, w + 1, min(font_px + 1, line_h)]`) — restricted to
/// fragments of the active input generation. Fragments are checked in layout
/// order; the first hit wins.
pub fn hit_button(
    layout: &Layout,
    g: &Geometry,
    view: &View,
    active_gen: Option<u32>,
    cursor: (i64, i64),
) -> Option<usize> {
    let active = active_gen?;
    let (row, dy) = row_at(layout.rows.len(), view, g.m.line_h, cursor.1)?;
    let band = g.m.font_px.min(g.m.line_h.saturating_sub(1)) as i32;
    if dy < 0 || dy > band {
        return None;
    }
    let px = i32::try_from(cursor.0).ok()?;
    let x0 = layout.rows.get(row)?.x0;
    layout.buttons.iter().position(|b| {
        if b.row != row || b.input_gen != active {
            return false;
        }
        let left = g.m.shift as i32 + x0 + b.x;
        px >= left && px <= left + b.w as i32
    })
}

/// The input strip line: `> {input}_` in the console's default colour.
pub fn input_line(input: &str, fg: [u8; 3]) -> ConsoleLine {
    ConsoleLine {
        align: Alignment::Left,
        button_start: None,
        parts: vec![ConsoleLinePart::Text(
            format!("> {input}_"),
            TextStyle {
                color: Color(fg),
                font_family: "".into(),
                font_style: FontStyle::NORMAL,
            },
        )],
    }
}

/// Append per-page instance buckets (the input strip) onto the frame's buckets.
fn merge_pages(into: &mut Vec<Vec<Instance>>, from: Vec<Vec<Instance>>) {
    for (page, list) in from.into_iter().enumerate() {
        if into.len() <= page {
            into.resize_with(page + 1, Vec::new);
        }
        into[page].extend(list);
    }
}

// ---------------------------------------------------------------------------
// App
// ---------------------------------------------------------------------------

pub struct App {
    cfg: AppConfig,
    shaper: Shaper,
    receiver: ProxyReceiver,
    window: Option<Arc<Window>>,
    gpu: Option<GpuContext>,
    raster: Option<GlyphRaster>,

    frame: ConsoleFrame,
    /// Layout of `frame.lines` at `layout_w` / the current metrics.
    layout: Layout,
    /// Content width (physical px) `layout` was computed for.
    layout_w: u32,
    /// Cached one-line layout of the input strip; rebuilt when `strip_dirty`.
    strip: Option<Layout>,
    strip_dirty: bool,

    current_req: Option<InputRequest>,
    input: String,
    /// Whole rows hidden below the bottom of the row area (0 = stuck to the bottom).
    scroll_rows: usize,
    /// Accumulated wheel travel (px) not yet converted to rows. Both
    /// `LineDelta` and `PixelDelta` feed it, so a mixed stream still adds up.
    wheel_px: f64,
    /// Index into `layout.buttons` of the fragment under the cursor.
    hovered: Option<usize>,
    /// Cursor in physical px; `(-1, -1)` when outside the window.
    cursor: (i64, i64),

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
    pub fn new(shaper: Shaper, receiver: ProxyReceiver, cfg: AppConfig) -> Self {
        Self {
            cfg,
            shaper,
            receiver,
            window: None,
            gpu: None,
            raster: None,
            frame: ConsoleFrame {
                fore_color: Color(cfg.default_fg),
                ..ConsoleFrame::default()
            },
            layout: Layout::default(),
            layout_w: 0,
            strip: None,
            strip_dirty: true,
            current_req: None,
            input: String::new(),
            scroll_rows: 0,
            wheel_px: 0.0,
            hovered: None,
            cursor: (-1, -1),
            timeout_deadline: None,
            timeout_value: Value::Int(0),
        }
    }

    fn metrics(&self) -> CellMetrics {
        *self.shaper.metrics()
    }

    /// Surface size in physical px (the logical config size before the window exists).
    fn surface_size(&self) -> (u32, u32) {
        self.gpu.as_ref().map_or(self.cfg.init_size, |g| g.size())
    }

    fn geometry(&self) -> Geometry {
        Geometry::new(self.surface_size().0.max(1), self.metrics())
    }

    fn view(&self) -> View {
        let line_h = self.metrics().line_h;
        let (_, win_h) = self.surface_size();
        View {
            scroll_rows: self.scroll_rows,
            view_h: win_h.saturating_sub(line_h),
            strip_h: line_h,
        }
    }

    fn active_gen(&self) -> Option<u32> {
        self.current_req.as_ref().map(|r| r.generation)
    }

    fn request_redraw(&self) {
        if let Some(w) = &self.window {
            w.request_redraw();
        }
    }

    /// Recompute the layout for the current frame and surface width, then
    /// clamp the scroll position and re-derive the hovered fragment.
    fn relayout(&mut self) {
        let g = self.geometry();
        self.layout = layout(&self.frame.lines, &g, &mut self.shaper);
        self.layout_w = g.content_w;
        self.strip_dirty = true;
        self.clamp_scroll_state();
        self.update_hover();
    }

    /// Keep `scroll_rows` within `[0, rows − visible]` (never forces the bottom).
    fn clamp_scroll_state(&mut self) {
        let visible = self.view().visible_rows(self.metrics().line_h);
        let max = max_scroll(self.layout.rows.len(), visible);
        self.scroll_rows = clamp_scroll(self.scroll_rows as i64, max);
    }

    /// Scroll to `requested` rows (clamped). Returns whether the position changed.
    fn scroll_to(&mut self, requested: i64) -> bool {
        let visible = self.view().visible_rows(self.metrics().line_h);
        let max = max_scroll(self.layout.rows.len(), visible);
        let next = clamp_scroll(requested, max);
        let changed = next != self.scroll_rows;
        self.scroll_rows = next;
        changed
    }

    /// Re-derive `hovered` from the stored cursor. Returns whether it changed.
    fn update_hover(&mut self) -> bool {
        let next = hit_button(
            &self.layout,
            &self.geometry(),
            &self.view(),
            self.active_gen(),
            self.cursor,
        );
        let changed = next != self.hovered;
        self.hovered = next;
        changed
    }

    /// Apply a winit scale factor: new integer cell metrics from the primary
    /// font (clears the shape cache). The caller relayouts.
    fn apply_scale(&mut self, scale: f32) {
        let scale = if scale.is_finite() && scale > 0.0 {
            scale
        } else {
            1.0
        };
        if (scale - self.metrics().scale).abs() < f32::EPSILON {
            return;
        }
        let primary_id = self.shaper.chain().primary();
        let primary = self.shaper.chain().font(primary_id);
        let m =
            CellMetrics::from_primary(&primary, self.cfg.font_size, self.cfg.line_height, scale);
        log::info!(
            "scale {scale}: font_px {} half_w {} line_h {} baseline {} shift {}",
            m.font_px,
            m.half_w,
            m.line_h,
            m.baseline,
            m.shift
        );
        self.shaper.set_metrics(m);
    }

    fn send(&mut self, resp: SystemResponse) {
        let _ = self.receiver.res_tx.send(resp);
        self.current_req = None;
        self.input.clear();
        self.strip_dirty = true;
        self.timeout_deadline = None;
        self.update_hover();
    }

    /// Drain all pending VM requests, relayout if a frame arrived, then request a redraw.
    fn drain_requests(&mut self, event_loop: &ActiveEventLoop) {
        let mut new_frame = false;
        while let Ok(req) = self.receiver.req_rx.try_recv() {
            match req {
                SystemRequest::Quit => event_loop.exit(),
                SystemRequest::Redraw(frame) => {
                    self.frame = frame;
                    new_frame = true;
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
                    self.strip_dirty = true;
                }
            }
        }
        if new_frame {
            // A new frame always sticks to the bottom (Emuera).
            self.scroll_rows = 0;
            self.relayout();
        } else {
            // Only the active generation may have changed.
            self.update_hover();
        }
        self.request_redraw();
    }

    fn render(&mut self) {
        // `view()` is the only place window height and metrics become a `View`,
        // so the drawer and the hit test can never drift apart.
        let view = self.view();
        let (Some(gpu), Some(raster)) = (self.gpu.as_mut(), self.raster.as_mut()) else {
            return;
        };
        let (win_w, _) = gpu.size();
        let m = *self.shaper.metrics();
        let hl = self.frame.hl_color.0;
        let mut pages = build_instances(
            &self.layout,
            &view,
            self.hovered,
            hl,
            raster,
            &gpu.device,
            &gpu.queue,
            &mut self.shaper,
        );
        if self.current_req.is_some() {
            if self.strip_dirty || self.strip.is_none() {
                let line = input_line(&self.input, self.frame.fore_color.0);
                let g = Geometry::new(win_w.max(1), m);
                self.strip = Some(layout(&[line], &g, &mut self.shaper));
                self.strip_dirty = false;
            }
            // `View::strip()` lands the one-row layout on the bottom line_h px.
            let strip = self.strip.as_ref().expect("strip laid out above");
            let strip_pages = build_instances(
                strip,
                &view.strip(),
                None,
                hl,
                raster,
                &gpu.device,
                &gpu.queue,
                &mut self.shaper,
            );
            merge_pages(&mut pages, strip_pages);
        }
        let pairs = raster.pages_with(&pages);
        gpu.render(&pairs, self.frame.bg_color.0);
    }

    fn on_click(&mut self) {
        let hit = hit_button(
            &self.layout,
            &self.geometry(),
            &self.view(),
            self.active_gen(),
            self.cursor,
        );
        if let Some(i) = hit {
            let value = self.layout.buttons[i].value.clone();
            self.send(SystemResponse::Input(value));
        }
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
                self.cfg.init_size.0,
                self.cfg.init_size.1,
            ));
        let window = Arc::new(event_loop.create_window(attrs).unwrap());
        let size = window.inner_size();
        let instance = wgpu::Instance::default();
        let surface = instance.create_surface(window.clone()).unwrap();
        let gpu = GpuContext::new(&instance, surface, size.width.max(1), size.height.max(1));
        self.raster = Some(GlyphRaster::new(&gpu.device, self.cfg.use_bitmap_strikes));
        self.gpu = Some(gpu);
        self.apply_scale(window.scale_factor() as f32);
        self.window = Some(window);
        self.relayout();
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
            self.request_redraw();
            // The deadline is now in the past: leaving `WaitUntil` in place
            // would wake the loop on every iteration and spin a core.
            event_loop.set_control_flow(winit::event_loop::ControlFlow::Wait);
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
                if size.width.max(1) != self.layout_w {
                    self.relayout();
                } else {
                    // Height only: rows move, the layout does not.
                    self.clamp_scroll_state();
                    self.update_hover();
                }
                self.request_redraw();
            }
            WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                self.apply_scale(scale_factor as f32);
                self.relayout();
                self.request_redraw();
            }
            WindowEvent::RedrawRequested => self.render(),
            WindowEvent::CursorMoved { position, .. } => {
                self.cursor = (position.x.floor() as i64, position.y.floor() as i64);
                if self.update_hover() {
                    self.request_redraw();
                }
            }
            WindowEvent::CursorLeft { .. } => {
                self.cursor = (-1, -1);
                if self.update_hover() {
                    self.request_redraw();
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
                    self.request_redraw();
                }
            }
            WindowEvent::MouseWheel { delta, .. } => {
                // Wheel up (positive y) reveals older rows: scroll_rows grows.
                let line_h = self.metrics().line_h;
                let px = match delta {
                    MouseScrollDelta::LineDelta(_, y) => wheel_notch_px(y, line_h),
                    MouseScrollDelta::PixelDelta(p) => p.y,
                };
                let rows = wheel_rows(&mut self.wheel_px, px, line_h);
                if rows != 0 && self.scroll_to(self.scroll_rows as i64 + rows) {
                    self.update_hover();
                    self.request_redraw();
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
                self.strip_dirty = true;
                self.request_redraw();
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::font::FontChain;
    use erars_compiler::Language;
    use erars_ui::width::WidthTable;
    use std::path::PathBuf;

    const BUNDLED: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/assets/NotoSansMono-Regular.ttf");
    /// Pinned metrics (spec Testing §4): row geometry is font-independent.
    const M: CellMetrics = CellMetrics {
        scale: 1.0,
        font_px: 18,
        half_w: 9,
        line_h: 19,
        baseline: 15,
        shift: 3,
    };
    /// 480 px window, 19 px strip → 461 px row area → 24 rows, 5 px slack on top.
    const VIEW: View = View {
        scroll_rows: 0,
        view_h: 461,
        strip_h: 19,
    };

    fn shaper() -> Shaper {
        let lang = Language::Korean;
        Shaper::new(
            FontChain::from_files(&[PathBuf::from(BUNDLED)], lang),
            WidthTable::new(lang.encoding()),
            M,
        )
    }

    fn style() -> TextStyle {
        TextStyle {
            color: Color([255, 255, 255]),
            font_family: "".into(),
            font_style: FontStyle::NORMAL,
        }
    }

    fn text(s: &str) -> ConsoleLinePart {
        ConsoleLinePart::Text(s.to_owned(), style())
    }

    fn button(s: &str, gen: u32, v: i64) -> ConsoleLinePart {
        ConsoleLinePart::Button(vec![(s.to_owned(), style())], gen, Value::Int(v))
    }

    fn line(align: Alignment, parts: Vec<ConsoleLinePart>) -> ConsoleLine {
        ConsoleLine {
            align,
            button_start: None,
            parts,
        }
    }

    /// 30 one-row lines. Buttons in layout order: #0 line 5 gen 7; #1 line 6
    /// gen 6 (inactive); #2 line 6 gen 7 at x = 45 (after "BBBB" + " ");
    /// #3 line 29 gen 7 at x = 18 on a Right-aligned row ("AB" + "DDDD").
    fn thirty_lines() -> Vec<ConsoleLine> {
        (0..30)
            .map(|i| match i {
                5 => line(Alignment::Left, vec![button("AAAA", 7, 2)]),
                6 => line(
                    Alignment::Left,
                    vec![button("BBBB", 6, 3), text(" "), button("CCCC", 7, 4)],
                ),
                29 => line(Alignment::Right, vec![text("AB"), button("DDDD", 7, 1)]),
                _ => line(Alignment::Left, vec![text("row")]),
            })
            .collect()
    }

    #[test]
    fn scroll_arithmetic() {
        assert_eq!(max_scroll(30, 24), 6);
        assert_eq!(max_scroll(10, 24), 0);
        assert_eq!(clamp_scroll(-3, 10), 0);
        assert_eq!(clamp_scroll(12, 10), 10);
        assert_eq!(clamp_scroll(4, 10), 4);
        assert_eq!(clamp_scroll(4, 0), 0);
    }

    #[test]
    fn wheel_pixels_accumulate_into_rows() {
        let mut acc = 0.0;
        assert_eq!(wheel_rows(&mut acc, 10.0, 19), 0);
        assert_eq!(wheel_rows(&mut acc, 10.0, 19), 1);
        assert!((acc - 1.0).abs() < 1e-9);
        assert_eq!(wheel_rows(&mut acc, -40.0, 19), -2);
        assert!((acc + 1.0).abs() < 1e-9);
        acc = 0.0;
        assert_eq!(wheel_rows(&mut acc, 0.5, 0), 0);
    }

    #[test]
    fn wheel_line_notches_share_the_pixel_accumulator() {
        // A detent is one row: 0.5 detents at line_h 19 is 9.5 px.
        assert_eq!(wheel_notch_px(0.5, 19), 9.5);
        assert_eq!(wheel_notch_px(3.0, 22), 66.0);
        assert_eq!(wheel_notch_px(-1.0, 19), -19.0);

        let mut acc = 0.0;
        // Half a detent moves no whole row; the remainder is kept in px.
        assert_eq!(wheel_rows(&mut acc, wheel_notch_px(0.5, 19), 19), 0);
        assert!((acc - 9.5).abs() < 1e-9);
        // The second half completes exactly one row and leaves nothing over.
        assert_eq!(wheel_rows(&mut acc, wheel_notch_px(0.5, 19), 19), 1);
        assert!(acc.abs() < 1e-9);
        // Detents and raw pixels accumulate together, in either order.
        assert_eq!(wheel_rows(&mut acc, wheel_notch_px(-0.5, 19), 19), 0);
        assert_eq!(wheel_rows(&mut acc, -9.5, 19), -1);
        assert!(acc.abs() < 1e-9);
        // Whole detents convert to that many rows at any line height.
        assert_eq!(wheel_rows(&mut acc, wheel_notch_px(3.0, 22), 22), 3);
        assert!(acc.abs() < 1e-9);
    }

    #[test]
    fn row_at_matches_the_drawer_and_rejects_slack_and_strip() {
        // 30 rows, 24 visible: rows 6..=29 on screen, row 6 at top = 5.
        assert_eq!(row_at(30, &VIEW, 19, 2), None, "top slack");
        assert_eq!(row_at(30, &VIEW, 19, 5), Some((6, 0)));
        assert_eq!(row_at(30, &VIEW, 19, 23), Some((6, 18)));
        assert_eq!(row_at(30, &VIEW, 19, 24), Some((7, 0)));
        assert_eq!(row_at(30, &VIEW, 19, 460), Some((29, 18)));
        assert_eq!(row_at(30, &VIEW, 19, 461), None, "input strip");
        assert_eq!(row_at(30, &VIEW, 19, -1), None);
        assert_eq!(row_at(0, &VIEW, 19, 100), None);
        // 3 rows: bottom-anchored, row 0 at 461 − 3·19 = 404.
        assert_eq!(row_at(3, &VIEW, 19, 404), Some((0, 0)));
        assert_eq!(row_at(3, &VIEW, 19, 403), None);
        // Scrolled by one row: bottom row 28, row 5 at top = 5.
        let v1 = View {
            scroll_rows: 1,
            ..VIEW
        };
        assert_eq!(row_at(30, &v1, 19, 5), Some((5, 0)));
        assert_eq!(row_at(30, &v1, 19, 460), Some((28, 18)));
        // Every on-screen pixel maps to the row View::row_y draws there.
        for rows in [0usize, 1, 5, 24, 30] {
            for scroll in [0usize, 1, 6, 100] {
                let v = View {
                    scroll_rows: scroll,
                    ..VIEW
                };
                for y in -5i64..=480 {
                    match row_at(rows, &v, 19, y) {
                        Some((r, dy)) => {
                            let top = v.row_y(rows, r, 19).unwrap() as i64;
                            assert!(y >= top && y < top + 19, "rows {rows} scroll {scroll} y {y}");
                            assert_eq!(dy as i64, y - top);
                        }
                        None => {
                            for r in 0..rows {
                                if let Some(top) = v.row_y(rows, r, 19) {
                                    let top = top as i64;
                                    assert!(
                                        !(y >= top && y < top + 19 && y < v.view_h as i64),
                                        "missed rows {rows} scroll {scroll} y {y} r {r}"
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn input_line_is_default_coloured_plain_text() {
        let l = input_line("12", [192, 192, 192]);
        assert_eq!(l.align, Alignment::Left);
        match &l.parts[..] {
            [ConsoleLinePart::Text(s, st)] => {
                assert_eq!(s, "> 12_");
                assert_eq!(st.color, Color([192, 192, 192]));
                assert_eq!(st.font_style, FontStyle::NORMAL);
            }
            other => panic!("unexpected parts {other:?}"),
        }
    }

    #[test]
    fn hit_test_uses_emuera_inclusive_rects_and_whole_rows() {
        let mut sh = shaper();
        let g = Geometry::new(760, M);
        let l = layout(&thirty_lines(), &g, &mut sh);
        assert_eq!(l.rows.len(), 30);
        assert_eq!(l.buttons.len(), 4);
        // button 3: row 29 (bottom, top = 442), x = 18 after "AB", w = 36;
        // Right → x0 = 760 − 54.
        let x0 = l.rows[29].x0 as i64;
        assert_eq!(x0, 706, "Right = content_w - width");
        let left = 3 + x0 + 18;
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left, 442)), Some(3));
        assert_eq!(
            hit_button(&l, &g, &VIEW, Some(7), (left + 36, 460)),
            Some(3),
            "inclusive right/bottom"
        );
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left + 37, 450)), None);
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left - 1, 450)), None);
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (left, 461)), None, "strip");
        assert_eq!(
            hit_button(&l, &g, &VIEW, Some(8), (left, 450)),
            None,
            "stale gen"
        );
        assert_eq!(
            hit_button(&l, &g, &VIEW, None, (left, 450)),
            None,
            "no request"
        );
        // row 5 is hidden above the row area (top = −14): never hit, even at y in [0, 5).
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (3, 2)), None);
        // row 6 sits at top = 5: button 1 is gen 6 (inactive); button 2 starts at 3 + 45 = 48.
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (10, 10)), None);
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (48, 5)), Some(2));
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (84, 23)), Some(2));
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (84, 24)), None);
        // scrolled by one row: bottom row 28, row 5 now at top = 5, row 29 gone.
        let v1 = View {
            scroll_rows: 1,
            ..VIEW
        };
        assert_eq!(hit_button(&l, &g, &v1, Some(7), (3, 5)), Some(0));
        assert_eq!(hit_button(&l, &g, &v1, Some(7), (3, 4)), None);
        assert_eq!(hit_button(&l, &g, &v1, Some(7), (left, 442)), None);
        // line_h 22 with font 18: Emuera's band is FontSize + 1 px, so the
        // bottom 3 px of each row miss (row 29 top = 458 − 22 = 436).
        let g22 = Geometry::new(760, CellMetrics { line_h: 22, ..M });
        let v22 = View {
            scroll_rows: 0,
            view_h: 458,
            strip_h: 22,
        };
        assert_eq!(hit_button(&l, &g22, &v22, Some(7), (left, 454)), Some(3));
        assert_eq!(hit_button(&l, &g22, &v22, Some(7), (left, 455)), None);
    }
}
