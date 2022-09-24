use std::{
    net::SocketAddr,
    sync::{
        atomic::{AtomicBool, Ordering::SeqCst},
        Arc,
    },
    time::Duration,
};

use erars_ast::Value;
use parking_lot::Mutex;

use axum::{
    http::StatusCode,
    routing::{get, post},
    Router,
};
use tower_http::compression::CompressionLayer;

use crate::ui::{ConsoleMessage, ConsoleResult, InputRequest};

use super::EraApp;

pub struct HttpBackend {
    pub port: u16,
}

impl HttpBackend {
    pub fn new(port: u16) -> Self {
        Self { port }
    }
}

async fn start(port: u16, chan: Arc<super::ConsoleChannel>) -> anyhow::Result<()> {
    let addr = SocketAddr::from(([127, 0, 0, 1], port));
    let current_req = Arc::new(Mutex::new(None));
    let msgs = Arc::new(Mutex::new(Vec::new()));

    let current_req_ = current_req.clone();
    let chan_ = chan.clone();

    let app = Router::new()
        .layer(CompressionLayer::new())
        .route(
            "/",
            get(|| async move {
                let mut current_req = current_req_.lock();

                let mut msgs = msgs.lock();
                if current_req.is_none() {
                    while let Some(msg) = chan_.recv_msg() {
                        match msg {
                            ConsoleMessage::Input(req) => {
                                *current_req = Some(req);
                            }
                            msg => msgs.push(msg),
                        }
                    }
                }

                (
                    StatusCode::OK,
                    [("Content-Type", "text/json")],
                    serde_json::to_string(&*msgs).unwrap(),
                )
            }),
        )
        .route(
            "/input",
            post(|request: String| async move {
                let mut current_req = current_req.lock();
                let mut msgs = msgs_.lock();
                if current_req.is_none() {
                    while let Some(msg) = chan.recv_msg() {
                        match msg {
                            ConsoleMessage::Input(req) => {
                                *current_req = Some(req);
                            }
                            msg => msgs.push(msg),
                        }
                    }
                }

                log::info!("[UI] {current_req:?} <- {request}");

                match *current_req {
                    Some(InputRequest::Anykey | InputRequest::EnterKey) => {
                        chan.send_ret(ConsoleResult::Value(Value::Int(0)));
                        *current_req = None;
                        StatusCode::OK
                    }
                    Some(InputRequest::Int) => match request.parse::<i64>() {
                        Ok(i) => {
                            chan.send_ret(ConsoleResult::Value(Value::Int(i)));
                            *current_req = None;
                            StatusCode::OK
                        }
                        _ => {
                            log::error!("{request} is not Int");
                            StatusCode::BAD_REQUEST
                        }
                    },
                    Some(InputRequest::Str) => {
                        chan.send_ret(ConsoleResult::Value(Value::String(request)));
                        *current_req = None;
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
        let rt = tokio::runtime::Builder::new_current_thread().enable_all().build()?;

        let _guard = rt.enter();
        let end = Arc::new(AtomicBool::new(false));

        let end_inner = end.clone();
        chan.set_exit_fn(move || {
            end_inner.store(true, SeqCst);
        });

        rt.spawn(start(self.port, chan));

        rt.block_on(async move {
            while !end.load(SeqCst) {
                tokio::time::sleep(Duration::from_millis(500)).await;
            }
        });

        rt.shutdown_background();

        Ok(())
    }
}
