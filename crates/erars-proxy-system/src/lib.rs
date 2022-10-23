use anyhow::{bail, Context};
use erars_ast::Value;
use erars_ui::{Color, ConsoleLine, InputRequest, VirtualConsole};
use erars_vm::{
    SaveList, SerializableGlobalVariableStorage, SerializableVariableStorage, SystemFunctions,
};
use flume::{unbounded, Receiver, Sender};

pub fn new_proxy() -> (ProxySystem, ProxyReceiver) {
    let (req_tx, req_rx) = unbounded();
    let (res_tx, res_rx) = unbounded();

    (
        ProxySystem { req_tx, res_rx },
        ProxyReceiver { req_rx, res_tx },
    )
}

pub struct ProxyReceiver {
    pub req_rx: Receiver<SystemRequest>,
    pub res_tx: Sender<SystemResponse>,
}

#[derive(Clone)]
pub struct ProxySystem {
    req_tx: Sender<SystemRequest>,
    res_rx: Receiver<SystemResponse>,
}

impl ProxySystem {
    async fn wait_response(&self, req: SystemRequest) -> anyhow::Result<SystemResponse> {
        self.req_tx.send(req).context("Send SystemRequest")?;

        self.res_rx.recv_async().await.context("Recv SystemResponse")
    }

    pub fn send_quit(&self) {
        self.req_tx.send(SystemRequest::Quit).ok();
    }
}

#[async_trait::async_trait(?Send)]
impl SystemFunctions for ProxySystem {
    async fn input(
        &mut self,
        vconsole: &mut VirtualConsole,
        req: InputRequest,
    ) -> anyhow::Result<Option<Value>> {
        match self
            .wait_response(SystemRequest::Input(
                req,
                ConsoleFrame::from_vconsole(vconsole),
            ))
            .await?
        {
            SystemResponse::Empty => Ok(None),
            SystemResponse::Input(value) => Ok(Some(value)),
            _ => bail!("Invalid proxy response"),
        }
    }

    async fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        self.wait_response(SystemRequest::Redraw(ConsoleFrame::from_vconsole(vconsole)))
            .await?;
        Ok(())
    }

    async fn load_local_list(&mut self) -> anyhow::Result<SaveList> {
        match self.wait_response(SystemRequest::LoadLocalList).await? {
            SystemResponse::SaveList(list) => Ok(list),
            _ => bail!("Invalid proxy response"),
        }
    }
    async fn load_local(
        &mut self,
        idx: u32,
    ) -> anyhow::Result<Option<SerializableVariableStorage>> {
        match self.wait_response(SystemRequest::LoadLocal(idx)).await? {
            SystemResponse::LocalSav(sav) => Ok(sav),
            _ => bail!("Invalid proxy response"),
        }
    }
    async fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>> {
        match self.wait_response(SystemRequest::LoadGlobal).await? {
            SystemResponse::GlobalSav(sav) => Ok(sav),
            _ => bail!("Invalid proxy response"),
        }
    }
    async fn save_local(
        &mut self,
        idx: u32,
        sav: SerializableVariableStorage,
    ) -> anyhow::Result<()> {
        self.wait_response(SystemRequest::SaveLocal(idx, sav)).await?;
        Ok(())
    }
    async fn remove_local(&mut self, idx: u32) -> anyhow::Result<()> {
        self.wait_response(SystemRequest::RemoveLocal(idx)).await?;
        Ok(())
    }
    async fn save_global(&mut self, sav: SerializableGlobalVariableStorage) -> anyhow::Result<()> {
        self.wait_response(SystemRequest::SaveGlobal(sav)).await?;
        Ok(())
    }

    fn clone_functions(&self) -> Box<dyn erars_vm::SystemFunctions> {
        Box::new(self.clone())
    }
}

#[derive(Default)]
pub struct ConsoleFrame {
    pub bg_color: Color,
    pub hl_color: Color,
    pub last_line: ConsoleLine,
    pub lines: Vec<ConsoleLine>,
}

impl ConsoleFrame {
    pub fn from_vconsole(vconsole: &VirtualConsole) -> Self {
        Self {
            bg_color: vconsole.bg_color,
            hl_color: vconsole.hl_color,
            lines: vconsole.lines.iter().cloned().collect(),
            last_line: vconsole.last_line.clone(),
        }
    }
}

pub enum SystemRequest {
    Redraw(ConsoleFrame),
    Input(InputRequest, ConsoleFrame),
    SaveLocal(u32, SerializableVariableStorage),
    SaveGlobal(SerializableGlobalVariableStorage),
    LoadLocal(u32),
    LoadGlobal,
    RemoveLocal(u32),
    LoadLocalList,

    Quit,
}

pub enum SystemResponse {
    Empty,
    Input(Value),
    SaveList(SaveList),
    LocalSav(Option<SerializableVariableStorage>),
    GlobalSav(Option<SerializableGlobalVariableStorage>),
}