use crate::{
    live_socket::{ClientMessage, LiveSocketContext, SocketViewMessage},
    LiveSocket, LiveTemplate,
};
use actix::{Actor, Addr, Context, Handler, Message};
use actix_web::{HttpRequest, HttpResponse, Responder};
use serde::{Deserialize, Serialize};
use std::any::Any;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct LiveViewId(pub usize);

#[derive(Message)]
#[rtype(result = "()")]
pub struct LiveViewMessage(pub Box<dyn Any + Send>);

pub type LiveViewContext<T> = Context<LiveViewActor<T>>;

pub trait LiveView: Sized + Unpin + 'static {
    type Template: LiveTemplate + Unpin;

    fn name() -> &'static str;

    fn mount(socket_ctx: &LiveSocketContext) -> Self;

    fn started(&mut self, _ctx: &mut LiveViewContext<Self>) {}

    fn handle_event(&mut self, _event: &str, _value: &str, _ctx: &mut LiveViewContext<Self>) {}

    fn handle_message(&mut self, _message: LiveViewMessage, _ctx: &mut LiveViewContext<Self>) {}

    fn template(&self) -> Self::Template;

    fn to_string(&self) -> String {
        self.template().render_with_wrapper(Self::name())
    }

    fn to_response(self) -> LiveViewResponse<Self> {
        LiveViewResponse { live_view: self }
    }
}

pub struct LiveViewResponse<T> {
    live_view: T,
}

impl<T> Responder for LiveViewResponse<T>
where
    T: LiveView,
{
    type Error = actix_web::Error;
    type Future = futures::future::Ready<Result<HttpResponse, actix_web::Error>>;

    fn respond_to(self, _req: &HttpRequest) -> Self::Future {
        let body = self.live_view.to_string();

        // Create response and set content type
        futures::future::ready(Ok(HttpResponse::Ok().body(body)))
    }
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
    pub fn new(id: LiveViewId, socket: Addr<LiveSocket>, context: &LiveSocketContext) -> Self {
        LiveViewActor {
            id,
            view: T::mount(context),
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

    fn started(&mut self, ctx: &mut Self::Context) {
        self.view.started(ctx);
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

impl<T> Handler<LiveViewMessage> for LiveViewActor<T>
where
    T: LiveView + Unpin + 'static,
{
    type Result = ();

    fn handle(&mut self, msg: LiveViewMessage, ctx: &mut Self::Context) -> Self::Result {
        self.view.handle_message(msg, ctx);
        self.send_changes();
    }
}

impl LiveViewMessage {
    pub fn get<T: Any>(&self) -> Option<&T> {
        self.0.downcast_ref()
    }
}
