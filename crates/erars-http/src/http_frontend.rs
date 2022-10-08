use slab::Slab;
use std::{net::SocketAddr, sync::Arc};
use tokio::sync::{Mutex as AsyncMutex, Notify};

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
use tower_http::cors;

use erars_ui::{Color, ConsoleLine, InputRequest, InputRequestType, VirtualConsole};

pub struct HttpFrontend {
    pub port: u16,
}

impl HttpFrontend {
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
            get(
                |axum::extract::Query(params): axum::extract::Query<GetRootQuery>| async move {
                    let vconsole = vconsole.read();

                    #[derive(serde::Serialize)]
                    struct Ret<'a> {
                        current_req: Option<&'a InputRequest>,
                        bg_color: Color,
                        hl_color: Color,
                        lines: &'a [ConsoleLine],
                    }

                    (
                        StatusCode::OK,
                        [("Content-Type", "text/json")],
                        serde_json::to_string(&Ret {
                            current_req: vconsole.current_req.as_ref(),
                            bg_color: vconsole.bg_color,
                            hl_color: vconsole.hl_color,
                            lines: vconsole.lines().get(params.from..).unwrap_or(&[]),
                        })
                        .unwrap(),
                    )
                },
            ),
        )
        .route(
            "/input",
            post(|request: String| async move {
                let mut vconsole = vconsole_.write();

                log::info!(
                    "[UI] {current_req:?} <- {request}",
                    current_req = vconsole.current_req
                );

                match vconsole.current_req.as_ref() {
                    Some(req) => match req.ty {
                        InputRequestType::AnyKey | InputRequestType::EnterKey => {
                            chan.send_input(Value::Int(0), req.generation);
                            vconsole.current_req = None;
                            StatusCode::OK
                        }
                        InputRequestType::Int => match request.trim().parse::<i64>() {
                            Ok(i) => {
                                chan.send_input(Value::Int(i), req.generation);
                                vconsole.current_req = None;
                                StatusCode::OK
                            }
                            _ => {
                                log::error!("{request} is not Int");
                                StatusCode::BAD_REQUEST
                            }
                        },
                        InputRequestType::Str => {
                            chan.send_input(Value::String(request), req.generation);
                            vconsole.current_req = None;
                            StatusCode::OK
                        }
                    },
                    None => StatusCode::GONE,
                }
            }),
        )
        .layer(CompressionLayer::new())
        .layer(cors::CorsLayer::permissive());

    log::info!("Listening on {addr}");
    eprintln!("Listening on {addr}");

    axum::Server::bind(&addr).serve(app.into_make_service()).await?;

    Ok(())
}

impl HttpFrontend {
    pub fn run(&mut self, chan: Arc<super::ConsoleChannel>) -> anyhow::Result<()> {
        let rt = tokio::runtime::Builder::new_multi_thread().enable_all().build()?;

        let _guard = rt.enter();
        let end = Arc::new(Notify::new());
        let need_redraw = Arc::new(Notify::new());

        let end_inner = end.clone();
        chan.set_exit_fn(move || {
            end_inner.notify_one();
        });
        let need_redraw_inner = need_redraw.clone();
        chan.set_redraw_fn(move || {
            need_redraw_inner.notify_one();
        });

        let vconsole = Arc::new(RwLock::new(VirtualConsole::new()));
        let clients = Arc::new(AsyncMutex::new(Slab::<(usize, WebSocket)>::new()));

        rt.spawn(start(
            self.port,
            chan.clone(),
            clients.clone(),
            vconsole.clone(),
        ));

        rt.spawn(async move {
            loop {
                need_redraw.notified().await;
                {
                    let mut vconsole_ = vconsole.write();
                    while let Some(msg) = chan.recv_msg() {
                        vconsole_.push_msg(msg);
                    }
                    if let Some((timeout, gen, default_value)) = vconsole_.timeout.take() {
                        let vconsole = vconsole.clone();
                        let chan = chan.clone();
                        let clients = clients.clone();
                        tokio::spawn(async move {
                            tokio::time::sleep_until(tokio::time::Instant::from_std(timeout)).await;
                            if chan.send_input(default_value, gen) {
                                // clear current_req
                                match &mut vconsole.write().current_req {
                                    opt_req
                                        if opt_req
                                            .as_ref()
                                            .map_or(false, |req| req.generation == gen) =>
                                    {
                                        *opt_req = None;
                                    }
                                    _ => {}
                                }
                                log::debug!("Timeout {gen}");
                                let mut clients = clients.lock().await;
                                send_code(event_codes::TIMEOUT, &mut clients).await;
                            }
                        });
                    }
                }
                let mut clients = clients.lock().await;
                send_code(event_codes::REDRAW, &mut clients).await;
                drop(clients);
                continue;
            }
        });

        rt.block_on(end.notified());

        rt.shutdown_background();

        Ok(())
    }
}

async fn send_code(code: u8, clients: &mut Slab<(usize, WebSocket)>) {
    let mut invalid_clients = Vec::new();
    for (_, (idx, client)) in clients.iter_mut() {
        if client.send(Message::Binary(vec![code])).await.is_err() {
            invalid_clients.push(*idx);
        }
    }
    for invalid_idx in invalid_clients {
        clients.remove(invalid_idx);
    }
}

mod event_codes {
    pub const REDRAW: u8 = 1;
    pub const TIMEOUT: u8 = 2;
}
