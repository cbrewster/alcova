use crate::{
    live_socket::{ClientMessage, SocketViewMessage},
    LiveSocket, LiveTemplate,
};
use actix::{Actor, Addr, Context, Handler, Message};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct LiveViewId(pub usize);

pub type LiveViewContext<T> = Context<LiveViewActor<T>>;

pub trait LiveView: Sized + Unpin + 'static {
    type Template: LiveTemplate + Unpin;

    fn name() -> &'static str;

    fn mount() -> Self;

    fn handle_event(&mut self, event: &str, value: &str, ctx: &mut LiveViewContext<Self>);

    fn template(&self) -> Self::Template;
}

#[derive(Message, Debug, Deserialize)]
#[rtype(result = "()")]
pub struct LiveViewAction {
    action: String,
    value: Option<String>,
}

pub struct LiveViewActor<T: LiveView> {
    id: LiveViewId,
    pub view: T,
    socket: Addr<LiveSocket>,
    old_template: Option<T::Template>,
}

impl<T: LiveView + Unpin + 'static> LiveViewActor<T> {
    pub fn new(id: LiveViewId, socket: Addr<LiveSocket>) -> Self {
        LiveViewActor {
            id,
            view: T::mount(),
            socket,
            old_template: None,
        }
    }

    pub fn send_changes(&mut self) {
        let template = self.view.template();
        let message = ClientMessage::Changes(template.changes(self.old_template.as_ref().unwrap()));
        self.old_template = Some(template);
        self.socket.do_send(SocketViewMessage { message });
    }
}

impl<T> Actor for LiveViewActor<T>
where
    T: LiveView + Unpin + 'static,
{
    type Context = Context<Self>;

    fn started(&mut self, _ctx: &mut Self::Context) {
        let template = self.view.template();
        let message = ClientMessage::Template {
            template: template.render(),
            id: self.id,
        };
        self.old_template = Some(template);
        self.socket.do_send(SocketViewMessage { message });
    }
}

impl<T> Handler<LiveViewAction> for LiveViewActor<T>
where
    T: LiveView + Unpin + 'static,
{
    type Result = ();

    fn handle(&mut self, msg: LiveViewAction, ctx: &mut Self::Context) -> Self::Result {
        let value = msg.value.unwrap_or(String::new());
        self.view.handle_event(&msg.action, &value, ctx);
        self.send_changes();
    }
}
