use slab::Slab;
use std::{
    net::SocketAddr,
    sync::{
        atomic::{AtomicBool, Ordering::SeqCst},
        Arc,
    },
    time::Duration,
};
use tokio::sync::Mutex as AsyncMutex;

use erars_ast::Value;
use parking_lot::RwLock;

use axum::{
    extract::{
        ws::{Message, WebSocket},
        WebSocketUpgrade,
    },
    http::StatusCode,
    routing::{get, post},
    Router,
};
use tower_http::compression::CompressionLayer;

use crate::ui::{Color, ConsoleLine, ConsoleResult, InputRequest};

use super::{EraApp, VirtualConsole};

pub struct HttpBackend {
    pub port: u16,
}

impl HttpBackend {
    pub fn new(port: u16) -> Self {
        Self { port }
    }
}

#[derive(serde::Deserialize)]
struct GetRootQuery {
    #[serde(default)]
    from: usize,
}

async fn start(
    port: u16,
    chan: Arc<super::ConsoleChannel>,
    clients: Arc<AsyncMutex<Slab<(usize, WebSocket)>>>,
    vconsole: Arc<RwLock<VirtualConsole>>,
) -> anyhow::Result<()> {
    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    let vconsole_ = vconsole.clone();

    let app = Router::new()
        .layer(CompressionLayer::new())
        .route(
            "/listen",
            get(|ws: WebSocketUpgrade| async move {
                ws.on_upgrade(|socket: WebSocket| async move {
                    let mut clients = clients.lock().await;
                    let key = clients.vacant_key();
                    clients.insert((key, socket));
                })
            }),
        )
        .route(
            "/",
            get(|axum::extract::Query(params): axum::extract::Query<GetRootQuery>| async move {
                let vconsole = vconsole.read();

                #[derive(serde::Serialize)]
                struct Ret<'a> {
                    current_req: Option<InputRequest>,
                    bg_color: Color,
                    hl_color: Color,
                    lines: &'a [ConsoleLine],
                }

                (
                    StatusCode::OK,
                    [("Content-Type", "text/json")],
                    serde_json::to_string(&Ret {
                        current_req: vconsole.current_req,
                        bg_color: vconsole.bg_color,
                        hl_color: vconsole.hl_color,
                        lines: vconsole.lines().get(params.from..).unwrap_or(&[]),
                    })
                    .unwrap(),
                )
            }),
        )
        .route(
            "/input",
            post(|request: String| async move {
                let mut vconsole = vconsole_.write();

                log::info!(
                    "[UI] {current_req:?} <- {request}",
                    current_req = vconsole.current_req
                );

                match vconsole.current_req {
                    Some(InputRequest::Anykey | InputRequest::EnterKey) => {
                        chan.send_ret(ConsoleResult::Value(Value::Int(0)));
                        vconsole.current_req = None;
                        StatusCode::OK
                    }
                    Some(InputRequest::Int) => match request.trim().parse::<i64>() {
                        Ok(i) => {
                            chan.send_ret(ConsoleResult::Value(Value::Int(i)));
                            vconsole.current_req = None;
                            StatusCode::OK
                        }
                        _ => {
                            log::error!("{request} is not Int");
                            StatusCode::BAD_REQUEST
                        }
                    },
                    Some(InputRequest::Str) => {
                        chan.send_ret(ConsoleResult::Value(Value::String(request)));
                        vconsole.current_req = None;
                        StatusCode::OK
                    }
                    None => StatusCode::GONE,
                }
            }),
        );

    log::info!("Listening on {addr}");
    eprintln!("Listening on {addr}");

    axum::Server::bind(&addr).serve(app.into_make_service()).await?;

    Ok(())
}

impl EraApp for HttpBackend {
    fn run(&mut self, chan: Arc<super::ConsoleChannel>) -> anyhow::Result<()> {
        let rt = tokio::runtime::Builder::new_multi_thread().enable_all().build()?;

        let _guard = rt.enter();
        let end = Arc::new(AtomicBool::new(false));
        let need_redraw = Arc::new(AtomicBool::new(false));

        let end_inner = end.clone();
        chan.set_exit_fn(move || {
            end_inner.store(true, SeqCst);
        });
        let need_redraw_inner = need_redraw.clone();
        chan.set_redraw_fn(move || {
            need_redraw_inner.store(true, SeqCst);
        });

        let vconsole = Arc::new(RwLock::new(VirtualConsole::new()));
        let clients = Arc::new(AsyncMutex::new(Slab::<(usize, WebSocket)>::new()));

        rt.spawn(start(
            self.port,
            chan.clone(),
            clients.clone(),
            vconsole.clone(),
        ));

        rt.block_on(async move {
            while !end.load(SeqCst) {
                if need_redraw.swap(false, SeqCst) {
                    let mut vconsole = vconsole.write();
                    if vconsole.current_req.is_none() {
                        while let Some(msg) = chan.recv_msg() {
                            vconsole.push_msg(msg);

                            if vconsole.current_req.is_some() {
                                break;
                            }
                        }
                    }
                    let mut clients = clients.lock().await;
                    let mut invalid_clients = Vec::new();
                    for (_, (idx, client)) in clients.iter_mut() {
                        if client.send(Message::Binary(vec![1])).await.is_err() {
                            invalid_clients.push(*idx);
                        }
                    }
                    for invalid_idx in invalid_clients {
                        clients.remove(invalid_idx);
                    }
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    continue;
                }
                tokio::time::sleep(Duration::from_millis(500)).await;
            }
        });

        rt.shutdown_background();

        Ok(())
    }
}
