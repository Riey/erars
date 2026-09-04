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

use erars_ast::Value;
use erars_proxy_system::{ConsoleFrame, ProxyReceiver, SystemRequest, SystemResponse};
use erars_ui::{win32, Color, InputRequest, InputRequestType, InputState, MouseKeyEvent};
use winit::application::ApplicationHandler;
use winit::event::{ElementState, MouseScrollDelta, WindowEvent};
use winit::event_loop::ActiveEventLoop;
use winit::keyboard::{Key, ModifiersState, NamedKey};
use winit::window::{Window, WindowId};

use crate::draw::{build_instances, cbg_quads, input_line, ImageCtx, View};
use crate::font;
use crate::gpu::{DrawGroup, Filter, GpuContext, RenderOutcome};
use crate::images::ImageTextures;
use crate::layout::{
    layout_frame_no_sweep, layout_no_sweep, ButtonRegion, Geometry, Layout, RowKind,
};
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

/// Button fragment under the cursor (physical px), as an index into
/// `layout.buttons`: each candidate fragment's row is placed on screen
/// forward (flow rows through [`View::row_y`], positioned ones through
/// [`View::place_y`]) and tested with Emuera's inclusive hit rect (spec
/// Component 5) — `base_x + x ≤ px ≤ base_x + x + w` and
/// `0 ≤ dy ≤ min(font_px, line_h − 1)` (i.e. the rect
/// `[base_x + x, row_y, w + 1, min(font_px + 1, line_h)]`) — restricted to
/// fragments of the active input generation.
///
/// Placed fragments are tested first, in **reverse** layout order, because
/// that is reverse paint order: a positioned box and an island cover the log
/// and each other (see [`crate::draw::Quads::overlays`]), and two islands may
/// even share a layer number and stack (`SYSTEM_DUNGEON.ERB:2630-2641`), so
/// the topmost one has to answer for the pixel. Flow fragments are then
/// tested in layout order, first hit wins, exactly as before.
///
/// A fragment inside a positioned box is additionally tested against that
/// box's content clip as a half-open rect, which is `rect.Contains` on the
/// clipped rect (`_Library/EvilMask/ConsoleDivPart.cs:99-105`): a button
/// outside its own box is not clickable, just as its ink is not drawn.
pub fn hit_button(
    layout: &Layout,
    g: &Geometry,
    view: &View,
    active_gen: Option<u32>,
    cursor: (i64, i64),
) -> Option<usize> {
    let active = active_gen?;
    let px = i32::try_from(cursor.0).ok()?;
    let py = i32::try_from(cursor.1).ok()?;
    let line_h = g.m.line_h;
    if line_h == 0 {
        return None;
    }
    // A flow row's band never reaches into the input strip; a placed row is
    // drawn over the whole surface, so only its own clip bounds it.
    let band = g.m.font_px.min(line_h - 1) as i32;
    let hits = |b: &ButtonRegion| {
        if b.input_gen != active {
            return false;
        }
        let Some(row) = layout.rows.get(b.row) else {
            return false;
        };
        let (top, clip) = match &row.kind {
            RowKind::Flow(n) => {
                let Some(top) = view.row_y(layout.flow_rows, *n, line_h) else {
                    return false;
                };
                // The strip covers the bottom `strip_h`, and `row_y` already
                // keeps flow rows above it.
                if py >= view.view_h as i32 {
                    return false;
                }
                (top, None)
            }
            RowKind::Placed(p) => {
                let Some(top) = view.place_y(layout.flow_rows, p, line_h) else {
                    return false;
                };
                (top, Some((p, top - p.y)))
            }
        };
        let dy = py - top;
        if dy < 0 || dy > band {
            return false;
        }
        let left = row.base_x(g.m.shift) + b.x;
        if px < left || px > left + b.w as i32 {
            return false;
        }
        match clip {
            None => true,
            Some((p, anchor_y)) => {
                let in_x = p.clip.x.is_none_or(|(a, c)| px >= a && px < c);
                let in_y = p
                    .clip
                    .y
                    .is_none_or(|(a, c)| py >= anchor_y + a && py < anchor_y + c);
                in_x && in_y
            }
        }
    };
    let placed = |b: &ButtonRegion| {
        layout
            .rows
            .get(b.row)
            .is_some_and(|r| r.placement().is_some())
    };
    layout
        .buttons
        .iter()
        .rposition(|b| placed(b) && hits(b))
        .or_else(|| layout.buttons.iter().position(|b| !placed(b) && hits(b)))
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
    /// One texture per bitmap the frame's inline images sample.
    images: ImageTextures,
    /// Start of the animation clock. Emuera latches an animation's start on
    /// its first draw; erars measures every animation from here (see
    /// [`ImageCtx`]).
    epoch: Instant,

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
    /// The keyboard half of [`InputState`], fed by every key event the window
    /// sees. Emuera asks `GetKeyState` per call; winit only reports keys while
    /// the window has focus, so this is the same information for as long as
    /// erars can observe it (see `docs/research` §5.11).
    keys: InputState,
    /// Index into `layout.buttons` of the fragment under the cursor.
    hovered: Option<usize>,
    /// Cursor in physical px; `(-1, -1)` when outside the window.
    cursor: (i64, i64),
    /// Live modifier state; INPUTMOUSEKEY folds it into the reported
    /// `KeyData`.
    modifiers: ModifiersState,

    /// When the current input request times out (TINPUT), and the value to
    /// send on expiry.
    timeout_deadline: Option<Instant>,
    timeout_value: Value,
    /// The CBG button value under the cursor, `-1` for none — Emuera's
    /// `selectingCBGButtonInt` (`GameView/EmueraConsole.cs:103`), which is
    /// front-end state there too.
    cbg_selecting: i32,
}

/// Current wall-clock time as Unix nanoseconds, matching `Timeout::timeout`.
fn current_unix_nanos() -> i128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as i128)
        .unwrap_or(0)
}

/// The Win32 virtual key WinForms would report for a winit key — the
/// vocabulary INPUTMOUSEKEY's `RESULT:1` speaks. `None` for a key WinForms
/// has no `Keys` value for, which Emuera could never report either.
fn vk_of(key: &Key) -> Option<i64> {
    Some(match key {
        Key::Named(named) => match named {
            NamedKey::Backspace => win32::VK_BACK,
            NamedKey::Tab => win32::VK_TAB,
            NamedKey::Enter => win32::VK_RETURN,
            NamedKey::Shift => win32::VK_SHIFT,
            NamedKey::Control => win32::VK_CONTROL,
            NamedKey::Alt => win32::VK_MENU,
            NamedKey::CapsLock => win32::VK_CAPITAL,
            NamedKey::Escape => win32::VK_ESCAPE,
            NamedKey::Space => win32::VK_SPACE,
            NamedKey::PageUp => win32::VK_PRIOR,
            NamedKey::PageDown => win32::VK_NEXT,
            NamedKey::End => win32::VK_END,
            NamedKey::Home => win32::VK_HOME,
            NamedKey::ArrowLeft => win32::VK_LEFT,
            NamedKey::ArrowUp => win32::VK_UP,
            NamedKey::ArrowRight => win32::VK_RIGHT,
            NamedKey::ArrowDown => win32::VK_DOWN,
            NamedKey::Insert => win32::VK_INSERT,
            NamedKey::Delete => win32::VK_DELETE,
            // `Keys.F1` is 112 and the function keys run on from there.
            NamedKey::F1 => 112,
            NamedKey::F2 => 113,
            NamedKey::F3 => 114,
            NamedKey::F4 => 115,
            NamedKey::F5 => 116,
            NamedKey::F6 => 117,
            NamedKey::F7 => 118,
            NamedKey::F8 => 119,
            NamedKey::F9 => 120,
            NamedKey::F10 => 121,
            NamedKey::F11 => 122,
            NamedKey::F12 => 123,
            _ => return None,
        },
        Key::Character(s) => win32::vk_from_char(s.chars().next()?)?,
        _ => return None,
    })
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
            images: ImageTextures::new(),
            epoch: Instant::now(),
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
            keys: InputState::default(),
            hovered: None,
            cursor: (-1, -1),
            modifiers: ModifiersState::empty(),
            timeout_deadline: None,
            timeout_value: Value::Int(0),
            cbg_selecting: -1,
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
    ///
    /// This is the only place that sweeps the shape cache: it is the only
    /// place that lays the whole log out again, so a sweep anywhere else
    /// (`render`'s input strip) would drop every log entry and re-shape the
    /// whole backlog on the next frame.
    fn relayout(&mut self) {
        let g = self.geometry();
        self.layout =
            layout_frame_no_sweep(&self.frame.lines, &self.frame.islands, &g, &mut self.shaper);
        self.shaper.sweep();
        self.layout_w = g.content_w;
        self.strip_dirty = true;
        self.clamp_scroll_state();
        self.update_hover();
    }

    /// Keep `scroll_rows` within `[0, rows − visible]` (never forces the bottom).
    fn clamp_scroll_state(&mut self) {
        let visible = self.view().visible_rows(self.metrics().line_h);
        let max = max_scroll(self.layout.flow_rows, visible);
        self.scroll_rows = clamp_scroll(self.scroll_rows as i64, max);
    }

    /// Scroll to `requested` rows (clamped). Returns whether the position changed.
    fn scroll_to(&mut self, requested: i64) -> bool {
        let visible = self.view().visible_rows(self.metrics().line_h);
        let max = max_scroll(self.layout.flow_rows, visible);
        let next = clamp_scroll(requested, max);
        let changed = next != self.scroll_rows;
        self.scroll_rows = next;
        changed
    }

    /// Re-derive `hovered` and `cbg_selecting` from the stored cursor.
    /// Returns whether either changed.
    ///
    /// `MoveMouse` (`GameView/EmueraConsole.cs:2009-2044`) tests the CBG
    /// button map *first* and, on a hit, clears the text hover outright: the
    /// plane masks the log's buttons wherever its map is opaque.
    fn update_hover(&mut self) -> bool {
        let (x, y) = self.mouse_key_pos();
        let selecting = self.frame.cbg.hit_test(&self.frame.images, x, y);
        let next = if selecting >= 0 {
            None
        } else {
            hit_button(
                &self.layout,
                &self.geometry(),
                &self.view(),
                self.active_gen(),
                self.cursor,
            )
        };
        let changed = next != self.hovered || selecting != self.cbg_selecting;
        self.hovered = next;
        self.cbg_selecting = selecting;
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
                // Answered straight on the channel: `send` also retires the
                // pending input request, and CHKFONT is not one.
                SystemRequest::ChkFont(name) => {
                    let found = font::find_family(self.shaper.chain().db(), &name).is_some();
                    let _ = self.receiver.res_tx.send(SystemResponse::ChkFont(found));
                }
                // Also not a wait: the answer is the state as of right now.
                SystemRequest::QueryState => {
                    let mut state = self.keys;
                    (state.mouse_x, state.mouse_y) = self.mouse_key_pos();
                    let _ = self.receiver.res_tx.send(SystemResponse::State(state));
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
        let images = ImageCtx {
            store: &self.frame.images,
            now_ms: self.epoch.elapsed().as_millis() as u64,
        };
        let fg = self.frame.fore_color.0;
        let mut quads = build_instances(
            &self.layout,
            &view,
            self.hovered,
            hl,
            fg,
            raster,
            &gpu.device,
            &gpu.queue,
            &mut self.shaper,
            images,
        );
        if self.current_req.is_some() {
            if self.strip_dirty || self.strip.is_none() {
                let line = input_line(&self.input, self.frame.fore_color.0);
                let g = Geometry::new(win_w.max(1), m);
                // Not `layout`: sweeping here would evict the log's clusters
                // (they are shaped in `relayout`, not per frame).
                self.strip = Some(layout_no_sweep(&[line], &g, &mut self.shaper));
                self.strip_dirty = false;
            }
            // `View::strip()` lands the one-row layout on the bottom line_h px.
            let strip = self.strip.as_ref().expect("strip laid out above");
            let strip_quads = build_instances(
                strip,
                &view.strip(),
                None,
                hl,
                fg,
                raster,
                &gpu.device,
                &gpu.queue,
                &mut self.shaper,
                images,
            );
            quads.merge(strip_quads);
        }
        quads.merge(cbg_quads(
            &self.frame.cbg,
            view.view_h as i32,
            self.cbg_selecting,
            images,
        ));
        quads.fit_pages(raster.page_count());
        self.images
            .sync(&gpu.device, &gpu.queue, &self.frame.images, &quads.bitmaps());
        let glyphs = raster.pages_with(&quads.glyphs);
        let under = self.images.pages_with(&quads.under);
        let inline = self.images.pages_with(&quads.images);
        let over = self.images.pages_with(&quads.over);
        // Emuera's merged depth loop, in four groups
        // (`GameView/EmueraConsole.cs:1557-1599`), then the placed boxes and
        // island overlays above it, lowest slice first (`Quads::overlays`).
        let overlays: Vec<_> = quads
            .overlays
            .iter()
            .map(|s| {
                (
                    raster.pages_with(&s.glyphs),
                    self.images.pages_with(&s.images),
                )
            })
            .collect();
        let mut groups = vec![
            DrawGroup {
                filter: Filter::Linear,
                pages: &under,
            },
            DrawGroup {
                filter: Filter::Nearest,
                pages: &glyphs,
            },
            DrawGroup {
                filter: Filter::Linear,
                pages: &inline,
            },
            DrawGroup {
                filter: Filter::Linear,
                pages: &over,
            },
        ];
        for (glyphs, images) in &overlays {
            groups.push(DrawGroup {
                filter: Filter::Nearest,
                pages: glyphs,
            });
            groups.push(DrawGroup {
                filter: Filter::Linear,
                pages: images,
            });
        }
        let outcome = gpu.render(&groups, self.frame.bg_color.0);
        if outcome == RenderOutcome::NeedsRedraw {
            // The surface was reconfigured and nothing was drawn; without this
            // the window keeps the stale frame until the next event arrives.
            self.request_redraw();
        }
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
            // A raw-event wait is answered by `mouse_key`, which every event
            // handler consults first, so nothing routes here.
            InputRequestType::MouseKey => {}
        }
    }

    /// Escape or a right click on a message wait: answer it and ask the
    /// console to fast-forward the following ones, like Emuera's
    /// `PressEnterKey(keySkip: true, ...)` (`MainWindow.cs:1170`, `:607`).
    /// A wait that needs a real value is left alone.
    fn submit_skip(&mut self) {
        let Some(req) = self.current_req.as_ref() else {
            return;
        };
        if matches!(
            req.ty,
            InputRequestType::AnyKey | InputRequestType::EnterKey
        ) {
            self.send(SystemResponse::InputSkip);
        }
    }

    /// Is a raw INPUTMOUSEKEY event wait in flight? While one is, the mouse
    /// and keyboard report events instead of driving the console.
    fn mouse_key_wait(&self) -> bool {
        matches!(
            self.current_req.as_ref().map(|r| r.ty),
            Some(InputRequestType::MouseKey)
        )
    }

    /// `RESULT:2`/`RESULT:3` for a mouse event: the cursor in client
    /// coordinates, with Y measured from the bottom edge as Emuera does
    /// (`EmueraConsole.cs:982-983`).
    ///
    /// The edge is the bottom of the *console area*, Emuera's
    /// `ClientHeight = window.MainPicBox.Height` (`:238`), which excludes the
    /// input strip — the same height the CBG plane's bottom-left origin and
    /// its button map are measured against, so a click cannot land on a
    /// different pixel than the one it was drawn on.
    fn mouse_key_pos(&self) -> (i64, i64) {
        (self.cursor.0, self.cursor.1 - self.client_height() as i64)
    }

    /// Emuera `ClientHeight`: the drawing area above the input strip.
    fn client_height(&self) -> u32 {
        self.view().view_h
    }

    /// Fold one key event into the [`InputState`] `GETKEY` reads.
    ///
    /// Windows flips a key's toggle bit on each press, which is the only thing
    /// `GETKEYTRIGGERED` looks at (`Creator.Method.cs:6725-6734`); erars flips
    /// it on each press winit reports as new, so an auto-repeat burst counts
    /// once — a held key triggers exactly on its first observation, as the
    /// C#'s comment describes.
    fn track_key(&mut self, event: &winit::event::KeyEvent) {
        let Some(code) = vk_of(&event.logical_key) else {
            return;
        };
        let Ok(vk) = u8::try_from(code) else {
            // `vk_of` only produces Win32 virtual keys, which are bytes.
            return;
        };
        match event.state {
            ElementState::Pressed => {
                if !event.repeat {
                    self.keys.flip_toggled(vk);
                }
                self.keys.set_down(vk, true);
            }
            ElementState::Released => self.keys.set_down(vk, false),
        }
    }

    /// `RESULT:4`/`RESULT:5` for a press. `RESULT:4` is the CBG button map's
    /// pixel under the cursor (`MouseDown`, `EmueraConsole.cs:1000-1014`),
    /// `-1` where no map is opaque, and the text button is read on top of it
    /// — Emuera fills both from one click.
    fn mouse_key_button(&self, ev: &mut MouseKeyEvent) {
        let (x, y) = self.mouse_key_pos();
        ev.mask = self.frame.cbg.hit_test(&self.frame.images, x, y) as i64;
        let hit = hit_button(
            &self.layout,
            &self.geometry(),
            &self.view(),
            self.active_gen(),
            self.cursor,
        );
        let Some(i) = hit else {
            return;
        };
        match &self.layout.buttons[i].value {
            Value::Int(v) => ev.button = *v,
            Value::String(s) => ev.button_str = Some(s.clone()),
        }
    }

    fn submit_mouse_key(&mut self, ev: MouseKeyEvent) {
        self.send(SystemResponse::MouseKey(ev));
        self.request_redraw();
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
            if self.mouse_key_wait() {
                // Emuera's expiry event, and no timeout message
                // (`EmueraConsole.cs:744`).
                self.submit_mouse_key(MouseKeyEvent::TIMEOUT);
            } else {
                let v = self.timeout_value.clone();
                self.send(SystemResponse::Input(v));
            }
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
        // Before the console gets a say: every key event updates the surface
        // GETKEY reads, whatever the console then does with it (or does not —
        // most of the arms below are guarded and return early).
        match &event {
            WindowEvent::KeyboardInput { event: key, .. } => self.track_key(key),
            // winit only delivers key events to the focused window, so a key
            // released elsewhere would stay down forever.
            WindowEvent::Focused(false) => self.keys.release_all(),
            _ => {}
        }
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
                button,
                ..
            } if self.mouse_key_wait() => {
                let (x, y) = self.mouse_key_pos();
                let mut ev = MouseKeyEvent {
                    kind: 1,
                    code: match button {
                        winit::event::MouseButton::Left => win32::MOUSE_LEFT,
                        winit::event::MouseButton::Right => win32::MOUSE_RIGHT,
                        winit::event::MouseButton::Middle => win32::MOUSE_MIDDLE,
                        winit::event::MouseButton::Back => win32::MOUSE_X1,
                        winit::event::MouseButton::Forward => win32::MOUSE_X2,
                        // Emuera can only ever name the five WinForms buttons.
                        winit::event::MouseButton::Other(_) => return,
                    },
                    x,
                    y,
                    ..MouseKeyEvent::default()
                };
                self.mouse_key_button(&mut ev);
                self.submit_mouse_key(ev);
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
            WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: winit::event::MouseButton::Right,
                ..
            } => {
                self.submit_skip();
                self.request_redraw();
            }
            WindowEvent::MouseWheel { delta, .. } if self.mouse_key_wait() => {
                // Emuera reports the raw `WM_MOUSEWHEEL` delta, 120 a notch.
                let line_h = self.metrics().line_h;
                let notches = match delta {
                    MouseScrollDelta::LineDelta(_, y) => y as f64,
                    MouseScrollDelta::PixelDelta(p) => p.y / line_h.max(1) as f64,
                };
                let (x, y) = self.mouse_key_pos();
                self.submit_mouse_key(MouseKeyEvent {
                    kind: 2,
                    code: (notches * win32::WHEEL_DELTA as f64).round() as i64,
                    x,
                    y,
                    ..MouseKeyEvent::default()
                });
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
            WindowEvent::ModifiersChanged(m) => self.modifiers = m.state(),
            WindowEvent::KeyboardInput { event, .. }
                if event.state == ElementState::Pressed && self.mouse_key_wait() =>
            {
                // Emuera sends `KeyCode` and `KeyData`, the latter being the
                // key with the modifier bits folded in
                // (`EmueraConsole.cs:1045`).
                let Some(code) = vk_of(&event.logical_key) else {
                    return;
                };
                let m = self.modifiers;
                let data = code
                    | if m.shift_key() { win32::MOD_SHIFT } else { 0 }
                    | if m.control_key() { win32::MOD_CONTROL } else { 0 }
                    | if m.alt_key() { win32::MOD_ALT } else { 0 };
                self.submit_mouse_key(MouseKeyEvent {
                    kind: 3,
                    code,
                    x: data,
                    ..MouseKeyEvent::default()
                });
            }
            WindowEvent::KeyboardInput { event, .. } if event.state == ElementState::Pressed => {
                let Some(req) = self.current_req.clone() else {
                    return;
                };
                match &event.logical_key {
                    Key::Named(NamedKey::Enter) => self.submit(),
                    Key::Named(NamedKey::Escape) => self.submit_skip(),
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
    use crate::layout::layout;
    use erars_ast::Alignment;
    use erars_compiler::Language;
    use erars_ui::width::WidthTable;
    use erars_ui::{ConsoleLine, ConsoleLinePart, FontStyle, TextStyle};
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

    /// Regression: the per-frame input strip must not sweep the log out of the
    /// shape cache. `relayout` (here: `layout`) shapes the whole log and sweeps;
    /// `render` then lays the strip out with `layout_no_sweep`, so both survive
    /// and the next frame re-shapes nothing.
    #[test]
    fn strip_layout_keeps_the_log_in_the_shape_cache() {
        let mut sh = shaper();
        let g = Geometry::new(760, M);
        let log = vec![line(Alignment::Left, vec![text("row")])];
        let _ = layout(&log, &g, &mut sh);
        assert!(sh.is_cached("row", &style()), "the log line was not cached");
        assert_eq!(sh.cache_len(), 1);

        // The app's per-frame path for the strip.
        let strip_fg = [192, 192, 192];
        let strip = input_line("12", strip_fg);
        let _ = layout_no_sweep(&[strip], &g, &mut sh);
        assert!(
            sh.is_cached("row", &style()),
            "the input strip swept the log's shape cache"
        );
        let strip_style = TextStyle {
            color: Color(strip_fg),
            ..style()
        };
        assert!(sh.is_cached("> 12_", &strip_style), "the strip was not cached");
        assert_eq!(sh.cache_len(), 2);

        // The next relayout keeps both: the strip was used in this generation.
        let _ = layout(&log, &g, &mut sh);
        assert!(sh.is_cached("row", &style()));
        assert!(sh.is_cached("> 12_", &strip_style));
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

    fn div_part(
        anchor: erars_ui::DivAnchor,
        (x, y): (i32, i32),
        (width, height): (Option<u32>, Option<u32>),
        lines: Vec<ConsoleLine>,
    ) -> ConsoleLinePart {
        ConsoleLinePart::Div(std::sync::Arc::new(erars_ui::ConsoleDiv {
            anchor,
            x,
            y,
            width,
            height,
            style: erars_ui::DivBox::default(),
            lines,
            alt_head: String::new(),
        }))
    }

    /// A positioned box covers the log, so its fragments answer for the pixels
    /// first (reverse paint order), and a fragment outside the box's own clip
    /// (`_Library/EvilMask/ConsoleDivPart.cs:99-105`) is not clickable at all.
    #[test]
    fn hit_test_prefers_placed_rows_and_honours_their_clip() {
        let mut sh = shaper();
        let g = Geometry::new(760, M);
        // Row 0 is a 36 px button at x 3..39. Row 1 prints a box that hangs
        // one row up (`ypos = −19`) and `xpos = −18` puts its content back at
        // x 3, right on top of that button. The box is one row tall, so its
        // second child line falls outside its own clip.
        let l = layout_frame_no_sweep(
            &[
                line(Alignment::Left, vec![button("AAAA", 7, 1)]),
                line(
                    Alignment::Left,
                    vec![
                        text("ab"),
                        div_part(
                            erars_ui::DivAnchor::Relative,
                            (-18, -19),
                            (Some(60), Some(19)),
                            vec![
                                line(Alignment::Left, vec![button("XY", 7, 2)]),
                                line(Alignment::Left, vec![button("Z", 7, 3)]),
                            ],
                        ),
                    ],
                ),
            ],
            &[],
            &g,
            &mut sh,
        );
        sh.sweep();
        assert_eq!((l.flow_rows, l.rows.len(), l.buttons.len()), (2, 4, 3));
        // Two flow rows, bottom-anchored: row 1 at 442, row 0 and the box at 423.
        assert_eq!(l.rows[2].placement().map(|p| (p.x, p.y)), Some((3, -19)));

        assert_eq!(
            hit_button(&l, &g, &VIEW, Some(7), (5, 425)),
            Some(1),
            "the box covers the log button, so the box answers"
        );
        assert_eq!(
            hit_button(&l, &g, &VIEW, Some(7), (30, 425)),
            Some(0),
            "past the box's 18 px fragment the log button is clickable again"
        );
        assert_eq!(hit_button(&l, &g, &VIEW, Some(7), (5, 422)), None);
        assert_eq!(
            hit_button(&l, &g, &VIEW, Some(7), (5, 442)),
            None,
            "the second child row is clipped out of its own box"
        );
    }

    #[test]
    fn vk_mapping_speaks_winforms_key_codes() {
        // Letters and digits carry their ASCII uppercase value whichever
        // case the layout produced.
        assert_eq!(vk_of(&Key::Character("z".into())), Some(90));
        assert_eq!(vk_of(&Key::Character("Z".into())), Some(90));
        assert_eq!(vk_of(&Key::Character("7".into())), Some(55));
        // Punctuation lands on the OEM codes, shifted or not.
        assert_eq!(vk_of(&Key::Character(";".into())), Some(186));
        assert_eq!(vk_of(&Key::Character(":".into())), Some(186));
        assert_eq!(vk_of(&Key::Character("/".into())), Some(191));
        // Named keys use the Keys enum values scripts compare against.
        assert_eq!(vk_of(&Key::Named(NamedKey::Enter)), Some(13));
        assert_eq!(vk_of(&Key::Named(NamedKey::Escape)), Some(27));
        assert_eq!(vk_of(&Key::Named(NamedKey::Space)), Some(32));
        assert_eq!(vk_of(&Key::Named(NamedKey::ArrowUp)), Some(38));
        assert_eq!(vk_of(&Key::Named(NamedKey::F5)), Some(116));
        assert_eq!(vk_of(&Key::Named(NamedKey::F12)), Some(123));
        // A key WinForms has no `Keys` value for is not reported at all.
        assert_eq!(vk_of(&Key::Named(NamedKey::BrightnessUp)), None);
        assert_eq!(vk_of(&Key::Character("あ".into())), None);
        // KeyData folds the modifier bits on top of the key code.
        assert_eq!(
            win32::VK_ESCAPE | win32::MOD_SHIFT | win32::MOD_CONTROL,
            27 | 0x0001_0000 | 0x0002_0000
        );
    }
}
