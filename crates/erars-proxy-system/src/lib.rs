use std::future::Future;
use std::sync::Arc;

use anyhow::Context;
use erars_ast::Value;
use erars_ui::{Color, ConsoleLine, InputRequest, VirtualConsole};
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
}

impl ProxySystem {
    async fn wait_response(&self, req: SystemRequest) -> anyhow::Result<SystemResponse> {
        self.req_tx.send_async(req).await.context("Send SystemRequest")?;
        (self.notify)();
        self.res_rx.recv_async().await.context("Recv SystemResponse")
    }

    pub fn send_quit(&self) {
        self.req_tx.send(SystemRequest::Quit).ok();
        (self.notify)();
    }
}

impl SystemFunctions for ProxySystem {
    fn input(&mut self, req: InputRequest) -> impl Future<Output = anyhow::Result<Option<Value>>> + Send {
        async move {
            match self.wait_response(SystemRequest::Input(req)).await? {
                SystemResponse::Empty => Ok(None),
                SystemResponse::Input(value) => Ok(Some(value)),
            }
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> impl Future<Output = anyhow::Result<()>> + Send {
        async move {
            self.req_tx
                .send_async(SystemRequest::Redraw(ConsoleFrame::from_vconsole(vconsole)))
                .await
                .context("Send SystemRequest")?;
            Ok(())
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct ConsoleFrame {
    pub bg_color: Color,
    pub hl_color: Color,
    pub lines: Vec<ConsoleLine>,
}

impl ConsoleFrame {
    pub fn from_vconsole(vconsole: &VirtualConsole) -> Self {
        Self {
            bg_color: vconsole.bg_color,
            hl_color: vconsole.hl_color,
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

pub enum SystemRequest {
    Redraw(ConsoleFrame),
    Input(InputRequest),
    Quit,
}

pub enum SystemResponse {
    Empty,
    Input(Value),
}
