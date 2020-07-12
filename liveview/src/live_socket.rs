use crate::{
    live_view::{LiveViewAction, LiveViewId},
    Changes, LiveViewRegistry, RenderedTemplate,
};
use actix::{Actor, ActorContext, AsyncContext, Handler, Message, Recipient, StreamHandler};
use actix_web::HttpRequest;
use actix_web_actors::ws;
use serde::{Deserialize, Serialize};
use std::time::{Duration, Instant};

/// How often heartbeat pings are sent
const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
/// How long before lack of client response causes a timeout
const CLIENT_TIMEOUT: Duration = Duration::from_secs(10);

/// Message received from the client
#[derive(Debug, Deserialize)]
pub enum ServerMessage {
    /// Start a new live view from the specified name.
    SpawnLiveView { name: String },
    LiveView {
        id: LiveViewId,
        action: LiveViewAction,
    },
}

#[derive(Debug, Serialize)]
pub enum ClientMessage {
    Template {
        template: RenderedTemplate,
        id: LiveViewId,
    },
    Changes(Changes),
}

pub struct LiveSocketContext {
    request: HttpRequest,
}

impl LiveSocketContext {
    pub fn app_data<T: 'static>(&self) -> Option<&T> {
        self.request.app_data()
    }
}

// TODO: This might need to be an enum if we get more messages eventually.
#[derive(Message, Debug, Serialize)]
#[rtype(result = "()")]
pub struct SocketViewMessage {
    pub message: ClientMessage,
}

pub struct LiveSocket {
    registry: LiveViewRegistry,
    context: LiveSocketContext,
    live_views: Vec<Recipient<LiveViewAction>>,
    heart_beat: Instant,
}

impl Actor for LiveSocket {
    type Context = ws::WebsocketContext<Self>;

    /// Method is called on actor start. We start the heartbeat process here.
    fn started(&mut self, ctx: &mut Self::Context) {
        self.hb(ctx);
    }
}

impl Handler<SocketViewMessage> for LiveSocket {
    type Result = ();

    fn handle(&mut self, msg: SocketViewMessage, ctx: &mut Self::Context) -> Self::Result {
        ctx.text(serde_json::to_string(&msg.message).unwrap());
    }
}

/// Handler for `ws::Message`
impl StreamHandler<Result<ws::Message, ws::ProtocolError>> for LiveSocket {
    fn handle(&mut self, msg: Result<ws::Message, ws::ProtocolError>, ctx: &mut Self::Context) {
        match msg {
            Ok(ws::Message::Ping(msg)) => {
                self.heart_beat = Instant::now();
                ctx.pong(&msg);
            }
            Ok(ws::Message::Pong(_)) => {
                self.heart_beat = Instant::now();
            }
            Ok(ws::Message::Text(text)) => {
                info!("Received WS message {:?}", text);
                let message: ServerMessage = match serde_json::from_str(&text) {
                    Ok(message) => message,
                    Err(_) => return warn!("Received unknown message from client"),
                };

                match message {
                    ServerMessage::SpawnLiveView { name } => {
                        let id = LiveViewId(self.live_views.len());
                        let live_view =
                            match self.registry.spawn(&name, id, ctx.address(), &self.context) {
                                Some(live_view) => live_view,
                                None => {
                                    return warn!("Live view with name {:?} not registered", name)
                                }
                            };
                        self.live_views.push(live_view);
                    }
                    ServerMessage::LiveView { id, action } => {
                        if self.live_views[id.0].do_send(action).is_err() {
                            warn!("An error occurred handling message");
                        }
                    }
                }
            }
            Ok(ws::Message::Binary(_)) => {}
            Ok(ws::Message::Close(reason)) => {
                ctx.close(reason);
                ctx.stop();
            }
            _ => ctx.stop(),
        }
    }
}

impl LiveSocket {
    pub fn new(registry: LiveViewRegistry, request: HttpRequest) -> Self {
        Self {
            registry,
            heart_beat: Instant::now(),
            live_views: Vec::new(),
            context: LiveSocketContext { request },
        }
    }

    /// helper method that sends ping to client every second.
    ///
    /// also this method checks heartbeats from client
    fn hb(&self, ctx: &mut <Self as Actor>::Context) {
        ctx.run_interval(HEARTBEAT_INTERVAL, |act, ctx| {
            // check client heartbeats
            if Instant::now().duration_since(act.heart_beat) > CLIENT_TIMEOUT {
                // heartbeat timed out
                println!("Websocket Client heartbeat failed, disconnecting!");

                // stop actor
                ctx.stop();

                // don't try to send a ping
                return;
            }

            ctx.ping(b"");
        });
    }
}
