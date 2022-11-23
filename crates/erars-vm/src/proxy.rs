use std::sync::Arc;

use anyhow::{bail, Context};
use erars_ast::Value;
use erars_ui::{Color, ConsoleLine, InputRequest, VirtualConsole, ConsoleSerde};
use crate::{
    SaveList, SerializableGlobalVariableStorage, SerializableVariableStorage, SystemFunctions,
};

#[derive(Clone)]
pub struct ProxySystem {
    sock: zmq::Socket,
}

impl ProxySystem {
    fn wait_response(&self, req: SystemRequest) -> anyhow::Result<SystemResponse> {
        self.sock.send()
        self.req_tx.send(req).context("Send SystemRequest")?;
        (self.notify)();
        self.res_rx.recv_async().context("Recv SystemResponse")
    }

    pub fn send_quit(&self) {
        self.req_tx.send(SystemRequest::Quit).ok();
        (self.notify)();
    }
}

impl ProxySystem {
    fn input(&mut self, req: InputRequest) -> anyhow::Result<Option<Value>> {
        match self.wait_response(SystemRequest::Input(req))? {
            SystemResponse::Empty => Ok(None),
            SystemResponse::Input(value) => Ok(Some(value)),
            _ => bail!("Invalid proxy response"),
        }
    }

    fn redraw(&mut self, vconsole: &mut VirtualConsole) -> anyhow::Result<()> {
        self.req_tx
            .send(SystemRequest::Redraw(ConsoleFrame::from_vconsole(vconsole)))
            .context("Send SystemRequest")?;
        Ok(())
    }

    fn load_local_list(&mut self) -> anyhow::Result<SaveList> {
        match self.wait_response(SystemRequest::LoadLocalList)? {
            SystemResponse::SaveList(list) => Ok(list),
            _ => bail!("Invalid proxy response"),
        }
    }
    fn load_local(
        &mut self,
        idx: u32,
    ) -> anyhow::Result<Option<SerializableVariableStorage>> {
        match self.wait_response(SystemRequest::LoadLocal(idx))? {
            SystemResponse::LocalSav(sav) => Ok(sav),
            _ => bail!("Invalid proxy response"),
        }
    }
    fn load_global(&mut self) -> anyhow::Result<Option<SerializableGlobalVariableStorage>> {
        match self.wait_response(SystemRequest::LoadGlobal)? {
            SystemResponse::GlobalSav(sav) => Ok(sav),
            _ => bail!("Invalid proxy response"),
        }
    }
    fn save_local(
        &mut self,
        idx: u32,
        sav: SerializableVariableStorage,
    ) -> anyhow::Result<()> {
        self.wait_response(SystemRequest::SaveLocal(idx, sav))?;
        Ok(())
    }
    fn remove_local(&mut self, idx: u32) -> anyhow::Result<()> {
        self.wait_response(SystemRequest::RemoveLocal(idx))?;
        Ok(())
    }
    fn save_global(&mut self, sav: SerializableGlobalVariableStorage) -> anyhow::Result<()> {
        self.wait_response(SystemRequest::SaveGlobal(sav))?;
        Ok(())
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

pub enum SystemRequest<'a> {
    Redraw(ConsoleSerde<'a>),
    Input(InputRequest),
    SaveLocal(u32, SerializableVariableStorage),
    SaveGlobal(SerializableGlobalVariableStorage),
    LoadLocal(u32),
    LoadGlobal,
    RemoveLocal(u32),
    LoadLocalList,

    Quit,
}

#[derive(serde::Deserialize)]
pub enum SystemResponse {
    Empty,
    Input(Value),
    SaveList(SaveList),
    LocalSav(Option<SerializableVariableStorage>),
    GlobalSav(Option<SerializableGlobalVariableStorage>),
}
