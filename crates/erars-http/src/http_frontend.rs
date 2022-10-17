use erars_vm::{TerminalVm, VmContext, VmResult};
use slab::Slab;
use std::{net::SocketAddr, sync::Arc};
use tokio::sync::Mutex as AsyncMutex;

use erars_ast::Value;
use flume::{bounded, Sender};
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

use erars_ui::{Color, InputRequest, InputRequestType, LinesFrom, VirtualConsole};

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
    clients: Arc<AsyncMutex<Slab<(usize, WebSocket)>>>,
    input_tx: Sender<(Option<Value>, Option<String>)>,
    vconsole: Arc<RwLock<(VirtualConsole, Option<InputRequest>)>>,
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

                    (
                        StatusCode::OK,
                        [("Content-Type", "text/json")],
                        serde_json::to_string(
                            &vconsole.0.make_serializable(vconsole.1.as_ref(), params.from),
                        )
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
                    current_req = vconsole.1
                );

                match vconsole.1.as_ref() {
                    Some(req) => match req.ty {
                        InputRequestType::AnyKey | InputRequestType::EnterKey => {
                            input_tx.send((None, None)).unwrap();
                            vconsole.1 = None;
                            StatusCode::OK
                        }
                        InputRequestType::Int => match request.trim().parse::<i64>() {
                            Ok(i) => {
                                input_tx.send((Some(Value::Int(i)), None)).unwrap();
                                vconsole.1 = None;
                                StatusCode::OK
                            }
                            _ => {
                                log::error!("{request} is not Int");
                                StatusCode::BAD_REQUEST
                            }
                        },
                        InputRequestType::Str => {
                            input_tx.send((Some(Value::String(request)), None)).unwrap();
                            vconsole.1 = None;
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
    pub fn run(
        &mut self,
        vm: TerminalVm,
        mut ctx: VmContext,
        mut vconsole: VirtualConsole,
    ) -> anyhow::Result<()> {
        let rt = tokio::runtime::Builder::new_multi_thread().enable_all().build()?;

        let _guard = rt.enter();
        let (input_tx, input_rx) = bounded(8);

        let vconsole_buf = Arc::new(RwLock::new((
            VirtualConsole::new(ctx.config.printc_width, ctx.config.max_log),
            None,
        )));
        let clients = Arc::new(AsyncMutex::new(Slab::<(usize, WebSocket)>::new()));

        rt.spawn(start(
            self.port,
            clients.clone(),
            input_tx.clone(),
            vconsole_buf.clone(),
        ));

        rt.block_on(async move {
            loop {
                match vm.run_state(&mut vconsole, &mut ctx) {
                    VmResult::Exit => break,
                    VmResult::Redraw => {
                        vconsole_buf.write().0 = vconsole.clone();
                        let mut clients = clients.lock().await;
                        send_code(event_codes::REDRAW, &mut clients).await;
                    }
                    VmResult::Input { req, set_result } => {
                        let ty = req.ty;

                        {
                            if let Some(timeout) = req.timeout.clone() {
                                let gen = req.generation;
                                let vconsole_buf = vconsole_buf.clone();
                                let clients = clients.clone();
                                let input_tx = input_tx.clone();
                                tokio::spawn(async move {
                                    let target = time::OffsetDateTime::from_unix_timestamp_nanos(
                                        timeout.timeout,
                                    )
                                    .unwrap();
                                    let diff = time::OffsetDateTime::now_utc() - target;
                                    let instant = std::time::Instant::now() + diff;
                                    tokio::time::sleep_until(tokio::time::Instant::from_std(
                                        instant,
                                    ))
                                    .await;

                                    let has_timeout: bool;

                                    {
                                        let mut vconsole_buf = vconsole_buf.write();
                                        has_timeout = vconsole_buf
                                            .1
                                            .as_ref()
                                            .map_or(false, |req| req.generation == gen);

                                        if has_timeout {
                                            input_tx
                                                .send((
                                                    Some(timeout.default_value),
                                                    timeout.timeout_msg,
                                                ))
                                                .unwrap();
                                            vconsole_buf.1 = None;
                                        }
                                    }
                                    if has_timeout {
                                        log::info!("Timeout {gen}");
                                        let mut clients = clients.lock().await;
                                        send_code(event_codes::TIMEOUT, &mut clients).await;
                                    }
                                });
                            }

                            let mut vconsole_buf = vconsole_buf.write();
                            vconsole_buf.0 = vconsole.clone();
                            vconsole_buf.1 = Some(req);
                        }

                        {
                            let mut clients = clients.lock().await;
                            send_code(event_codes::REDRAW, &mut clients).await;
                        }

                        let (input, timeout_msg) = match input_rx.recv_async().await {
                            Ok(value) => value,
                            Err(_) => break,
                        };

                        if let Some(timeout_msg) = timeout_msg {
                            vconsole.print_line(timeout_msg);
                        }

                        match ty {
                            InputRequestType::Int if set_result => {
                                ctx.var.set_result(input.unwrap().try_into_int().unwrap());
                            }
                            InputRequestType::Str if set_result => {
                                ctx.var.set_results(input.unwrap().try_into_str().unwrap());
                            }
                            InputRequestType::AnyKey | InputRequestType::EnterKey => {}
                            InputRequestType::Int | InputRequestType::Str => {
                                ctx.push(input.unwrap());
                            }
                        }
                    }
                }
            }
        });

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
