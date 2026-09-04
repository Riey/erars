use std::sync::Arc;

use anyhow::{bail, Context};
use erars_ast::Value;
use erars_ui::{
    cbg::CbgLayer, image::ImageStore, Color, ConsoleLine, InputRequest, InputState, MouseKeyEvent,
    VirtualConsole,
};
use erars_vm::SystemFunctions;
use flume::{unbounded, Receiver, Sender};

pub fn new_proxy(notify: Arc<dyn Fn() + Send + Sync>) -> (ProxySystem, ProxyReceiver) {
    let (req_tx, req_rx) = unbounded();
    let (res_tx, res_rx) = unbounded();

    (
        ProxySystem {
            req_tx,
            res_rx,
            notify,
            mes_skip: false,
        },
        ProxyReceiver { req_rx, res_tx },
    )
}

#[derive(Clone)]
pub struct ProxyReceiver {
    pub req_rx: Receiver<SystemRequest>,
    pub res_tx: Sender<SystemResponse>,
}

#[derive(Clone)]
pub struct ProxySystem {
    req_tx: Sender<SystemRequest>,
    res_rx: Receiver<SystemResponse>,
    notify: Arc<dyn Fn() + Send + Sync>,
    /// The front-end answered the last wait with "skip the messages"; the
    /// console picks this up in `input_redraw`.
    mes_skip: bool,
}

impl ProxySystem {
    fn wait_response(&self, req: SystemRequest) -> anyhow::Result<SystemResponse> {
        self.req_tx.send(req).context("Send SystemRequest")?;
        (self.notify)();
        self.res_rx.recv().context("Recv SystemResponse")
    }

    pub fn send_quit(&self) {
        self.req_tx.send(SystemRequest::Quit).ok();
        (self.notify)();
    }

    pub fn send_frame(&self, frame: ConsoleFrame) {
        self.req_tx.send(SystemRequest::Redraw(frame)).ok();
        (self.notify)();
    }
}

impl SystemFunctions for ProxySystem {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        match self.wait_response(SystemRequest::Input(req))? {
            SystemResponse::Empty => Ok(None),
            SystemResponse::Input(value) => Ok(Some(value)),
            SystemResponse::InputSkip => {
                self.mes_skip = true;
                Ok(None)
            }
            other => bail!("Input에 대한 응답이 잘못되었습니다: {}", other.kind()),
        }
    }

    fn take_mes_skip(&mut self) -> bool {
        std::mem::take(&mut self.mes_skip)
    }

    fn chk_font(&mut self, name: &str) -> anyhow::Result<bool> {
        match self.wait_response(SystemRequest::ChkFont(name.into()))? {
            SystemResponse::ChkFont(found) => Ok(found),
            other => bail!("ChkFont에 대한 응답이 잘못되었습니다: {}", other.kind()),
        }
    }

    fn input_state(&mut self) -> anyhow::Result<InputState> {
        match self.wait_response(SystemRequest::QueryState)? {
            SystemResponse::State(state) => Ok(state),
            other => bail!("QueryState에 대한 응답이 잘못되었습니다: {}", other.kind()),
        }
    }

    fn input_mouse_key(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
        painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<MouseKeyEvent> {
        self.redraw(vconsole, painted)?;
        match self.wait_response(SystemRequest::Input(req))? {
            SystemResponse::MouseKey(ev) => Ok(ev),
            other => bail!("InputMouseKey에 대한 응답이 잘못되었습니다: {}", other.kind()),
        }
    }

    fn redraw(
        &mut self,
        vconsole: &mut VirtualConsole,
        _painted: erars_vm::graphics::Painted<'_>,
    ) -> anyhow::Result<()> {
        self.req_tx
            .send(SystemRequest::Redraw(ConsoleFrame::from_vconsole(vconsole)))
            .context("Send SystemRequest")?;
        Ok(())
    }
}

#[derive(Default, Debug, Clone)]
pub struct ConsoleFrame {
    pub bg_color: Color,
    pub hl_color: Color,
    /// The configured default text colour (`文字色`); used for chrome the
    /// frontend draws itself, such as the input strip.
    pub fore_color: Color,
    pub lines: Vec<ConsoleLine>,
    /// The pixels behind this frame's `ConsoleLinePart::Image` parts. A
    /// handle, so a frame carries no bitmap data across the channel — the VM
    /// has already published everything these lines reference
    /// (`erars_vm::GraphicsStore::publish`).
    pub images: ImageStore,
    /// The console-background plane (`CBG*`), drawn behind and in front of
    /// `lines`. Shared, not copied: it changes only when a `CBG*` method
    /// runs.
    pub cbg: Arc<CbgLayer>,
    /// HTML_PRINT_ISLAND overlays, lowest layer first — drawn over `lines`
    /// and never scrolling with them.
    pub islands: Vec<(i64, Vec<ConsoleLine>)>,
}

impl ConsoleFrame {
    pub fn from_vconsole(vconsole: &VirtualConsole) -> Self {
        Self {
            bg_color: vconsole.bg_color,
            hl_color: vconsole.hl_color,
            fore_color: vconsole.default_color(),
            images: vconsole.images.clone(),
            cbg: vconsole.cbg.clone(),
            islands: vconsole
                .islands()
                .map(|(layer, lines)| (layer, lines.to_vec()))
                .collect(),
            lines: vconsole
                .lines
                .iter()
                .chain(if vconsole.last_line.is_empty() {
                    None
                } else {
                    Some(&vconsole.last_line)
                })
                .cloned()
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SystemRequest {
    Redraw(ConsoleFrame),
    Input(InputRequest),
    /// CHKFONT: only the front-end knows which font families it can load.
    ChkFont(String),
    /// GETKEY / GETKEYTRIGGERED / MOUSEX / MOUSEY: the front-end's live
    /// keyboard and cursor. Answered immediately — it is not a wait, so it
    /// must never be confused with [`SystemRequest::Input`].
    QueryState,
    Quit,
}

pub enum SystemResponse {
    Empty,
    Input(Value),
    /// Escape or a right click while waiting: answer the wait and start
    /// message skip (Emuera `PressEnterKey(keySkip: true, ...)`).
    InputSkip,
    /// INPUTMOUSEKEY: the one raw event the front-end saw.
    MouseKey(MouseKeyEvent),
    ChkFont(bool),
    State(InputState),
}

impl SystemResponse {
    /// Names the variant for the "wrong answer" diagnostics; the payloads are
    /// not all printable.
    fn kind(&self) -> &'static str {
        match self {
            Self::Empty => "Empty",
            Self::Input(_) => "Input",
            Self::InputSkip => "InputSkip",
            Self::MouseKey(_) => "MouseKey",
            Self::ChkFont(_) => "ChkFont",
            Self::State(_) => "State",
        }
    }
}
