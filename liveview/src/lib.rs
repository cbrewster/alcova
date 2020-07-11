use actix::prelude::*;
use actix::{Actor, StreamHandler};
use actix_web_actors::ws;
use serde::{Deserialize, Serialize};
use std::time::{Duration, Instant};

/// How often heartbeat pings are sent
const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
/// How long before lack of client response causes a timeout
const CLIENT_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Debug, Serialize)]
pub enum Slot {
    /// These never change
    Static(&'static str),

    /// This field may change, but it will include the initial data for first render
    Dynamic(String),
}

#[derive(Debug, Serialize)]
pub struct RenderedTemplate {
    pub slots: Vec<Slot>,
}

impl std::fmt::Display for RenderedTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for slot in &self.slots {
            let text = match slot {
                Slot::Static(text) => text,
                Slot::Dynamic(text) => text.as_str(),
            };
            write!(f, "{}", text)?
        }
        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub struct Changes {
    /// Changes to the rendered template.
    /// Changes are a tuple where the first element is the index of the dynamic slot.
    pub changes: Vec<(usize, String)>,
}

#[derive(Debug, Deserialize)]
pub struct ServerMessage {
    action: String,
    value: String,
}

/// A live template.
///
/// Knows how track changes within itself.
pub trait LiveTemplate {
    fn render(&self) -> RenderedTemplate;

    fn changes(&self, old_template: &Self) -> Changes;

    /// Render the template to a string.
    /// This is useful to render the template for regular HTTP requests.
    fn render_to_string(&self) -> String {
        self.render().to_string()
    }

    fn render_with_wrapper(&self) -> String {
        format!("<div id=\"rs-root\">{}</div>", self.render_to_string())
    }
}

pub struct LiveSocket<T: LiveView> {
    heart_beat: Instant,
    view: T,
    old_template: Option<T::Template>,
}

impl<T> Actor for LiveSocket<T>
where
    T: LiveView + Unpin + 'static,
{
    type Context = ws::WebsocketContext<Self>;

    /// Method is called on actor start. We start the heartbeat process here.
    fn started(&mut self, ctx: &mut Self::Context) {
        self.hb(ctx);
    }
}

/// Handler for `ws::Message`
impl<T> StreamHandler<Result<ws::Message, ws::ProtocolError>> for LiveSocket<T>
where
    T: LiveView + Unpin + 'static,
{
    fn started(&mut self, ctx: &mut Self::Context) {
        let template = self.view.render();
        ctx.text(serde_json::to_string(&ClientMessage::Template(template.render())).unwrap());
        self.old_template = Some(template);
    }

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
                let event: ServerMessage = match serde_json::from_str(&text) {
                    Ok(event) => event,
                    Err(_) => return,
                };
                self.view.handle_event(&event.action, &event.value);
                let new_template = self.view.render();
                let changes = new_template.changes(self.old_template.as_ref().unwrap());

                self.old_template = Some(new_template);
                ctx.text(serde_json::to_string(&ClientMessage::Changes(changes)).unwrap());
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

impl<T> LiveSocket<T>
where
    T: LiveView + Unpin + 'static,
{
    pub fn new(view: T) -> Self {
        Self {
            view,
            old_template: None,
            heart_beat: Instant::now(),
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

pub trait LiveView {
    type Template: LiveTemplate + Unpin;

    fn handle_event(&mut self, event: &str, value: &str);

    fn render(&self) -> Self::Template;
}

#[derive(Debug, Serialize)]
pub enum ClientMessage {
    Template(RenderedTemplate),
    Changes(Changes),
}
