use crate::{
    live_socket::{ClientMessage, LiveSocketContext, SocketViewMessage},
    LiveSocket, LiveTemplate,
};
use actix::{Actor, ActorContext, Addr, Context, Handler, Message};
use actix_web::{HttpRequest, HttpResponse, Responder};
use jsonwebtoken::{encode, EncodingKey, Header};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct LiveViewId(pub usize);

pub type LiveViewContext<T> = Context<LiveViewActor<T>>;

pub(crate) fn signing_secret() -> String {
    std::env::var("ALCOVA_SECRET_KEY").unwrap_or_else(|_| {
        warn!("No secret key set! Using unsecure default");
        "secret".into()
    })
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Claims<T> {
    exp: u64,
    pub(crate) data: T,
}

impl<T> Claims<T> {
    fn new(minutes: u64, data: T) -> Self {
        let exp = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs()
            + (minutes * 60);
        Self { exp, data }
    }
}

pub trait LiveView: Sized + Unpin + 'static {
    type Template: LiveTemplate + Unpin;
    type SessionData: Serialize + DeserializeOwned;

    fn name() -> &'static str;

    fn mount(socket_ctx: &LiveSocketContext, session: Self::SessionData) -> Self;

    fn started(&mut self, _ctx: &mut LiveViewContext<Self>) {}

    fn handle_event(&mut self, _event: &str, _value: &str, _ctx: &mut LiveViewContext<Self>) {}

    fn template(&self) -> Self::Template;

    fn to_string(&self, session: &Self::SessionData) -> String {
        let key = signing_secret();

        // TODO: Not sure how we should handle tokens expiring. Maybe reload the page on the
        // client?
        let claims = Claims::new(60, session);

        let token = encode(
            &Header::default(),
            &claims,
            &EncodingKey::from_secret(key.as_bytes()),
        )
        .unwrap();
        self.template()
            .render_with_wrapper(Self::name(), token.as_str())
    }

    fn to_response(self, session: Self::SessionData) -> LiveViewResponse<Self> {
        LiveViewResponse {
            live_view: self,
            session,
        }
    }
}

pub trait LiveMessage: Message<Result = ()> {}

pub struct LiveViewResponse<T: LiveView> {
    live_view: T,
    session: T::SessionData,
}

impl<T> Responder for LiveViewResponse<T>
where
    T: LiveView,
{
    type Error = actix_web::Error;
    type Future = futures::future::Ready<Result<HttpResponse, actix_web::Error>>;

    fn respond_to(self, _req: &HttpRequest) -> Self::Future {
        let body = self.live_view.to_string(&self.session);

        // Create response and set content type
        futures::future::ready(Ok(HttpResponse::Ok().body(body)))
    }
}

#[derive(Message, Debug, Deserialize)]
#[rtype(result = "()")]
pub enum LiveViewMessage {
    ClientAction(LiveViewAction),
    Stop,
}

#[derive(Debug, Deserialize)]
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
    pub fn new(
        id: LiveViewId,
        socket: Addr<LiveSocket>,
        context: &LiveSocketContext,
        session: T::SessionData,
    ) -> Self {
        LiveViewActor {
            id,
            view: T::mount(context, session),
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

impl<T> Handler<LiveViewMessage> for LiveViewActor<T>
where
    T: LiveView + Unpin + 'static,
{
    type Result = ();

    fn handle(&mut self, msg: LiveViewMessage, ctx: &mut Self::Context) -> Self::Result {
        match msg {
            LiveViewMessage::ClientAction(LiveViewAction { action, value }) => {
                let value = value.unwrap_or(String::new());
                self.view.handle_event(&action, &value, ctx);
                self.send_changes();
            }
            LiveViewMessage::Stop => ctx.stop(),
        }
    }
}

pub trait LiveHandler<M: LiveMessage>
where
    Self: LiveView,
{
    fn handle(&mut self, msg: M, ctx: &mut LiveViewContext<Self>);
}

impl<T, M> Handler<M> for LiveViewActor<T>
where
    T: LiveView + Unpin + LiveHandler<M> + 'static,
    M: LiveMessage,
{
    type Result = ();

    fn handle(&mut self, msg: M, ctx: &mut Self::Context) -> Self::Result {
        self.view.handle(msg, ctx);
        self.send_changes();
    }
}
