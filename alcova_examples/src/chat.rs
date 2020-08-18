use crate::RootTemplate;
use actix::{Actor, Addr, AsyncContext, Context, Handler, Message, Recipient};
use actix_identity::Identity;
use actix_web::{http, web, HttpRequest, Responder};
use alcova::{
    LiveHandler, LiveMessage, LiveSocketContext, LiveTemplate, LiveView, LiveViewContext,
};
use serde::{Deserialize, Serialize};

pub struct Lobby {
    room: Addr<ChatRoom>,
}

impl Lobby {
    pub fn new() -> Self {
        Self {
            room: ChatRoom::new().start(),
        }
    }
}

#[derive(Message)]
#[rtype(result = "()")]
struct Subscribe(Recipient<ChatMessage>);

#[derive(Message, Clone)]
#[rtype(result = "()")]
struct ChatMessage(String);

impl LiveMessage for ChatMessage {}

struct ChatRoom {
    subscribers: Vec<Recipient<ChatMessage>>,
}

impl ChatRoom {
    fn new() -> Self {
        Self {
            subscribers: Vec::new(),
        }
    }
}

impl Actor for ChatRoom {
    type Context = Context<Self>;
}

impl Handler<Subscribe> for ChatRoom {
    type Result = ();

    fn handle(&mut self, msg: Subscribe, _ctx: &mut Self::Context) -> Self::Result {
        self.subscribers.push(msg.0);
    }
}

impl Handler<ChatMessage> for ChatRoom {
    type Result = ();

    fn handle(&mut self, msg: ChatMessage, _ctx: &mut Self::Context) -> Self::Result {
        self.subscribers.retain(|sub| sub.connected());
        for subscriber in &self.subscribers {
            if let Err(_) = subscriber.do_send(msg.clone()) {
                warn!("Error sending message!");
            }
        }
    }
}

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/login.html.alcova")]
struct LoginTemplate;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    name: String,
}

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/chat.html.alcova")]
pub struct ChatTemplate {
    messages: Vec<String>,
    new_message: String,
    name: String,
}

impl ChatTemplate {
    fn new(name: String) -> Self {
        Self {
            messages: Vec::new(),
            new_message: String::new(),
            name,
        }
    }
}

pub struct ChatLive {
    assigns: ChatTemplate,
    room: Addr<ChatRoom>,
}

impl ChatLive {
    fn new(room: Addr<ChatRoom>, session: Session) -> Self {
        Self {
            assigns: ChatTemplate::new(session.name),
            room,
        }
    }
}

impl LiveHandler<ChatMessage> for ChatLive {
    fn handle(&mut self, msg: ChatMessage, _ctx: &mut LiveViewContext<Self>) {
        self.assigns.messages.push(msg.0);
    }
}

impl LiveView for ChatLive {
    type Template = ChatTemplate;
    type SessionData = Session;

    fn name() -> &'static str {
        "chat"
    }

    fn mount(socket_ctx: &LiveSocketContext, session: Session) -> Self {
        let lobby = socket_ctx.app_data::<web::Data<Lobby>>().unwrap();
        Self::new(lobby.room.clone(), session)
    }

    fn started(&mut self, ctx: &mut LiveViewContext<Self>) {
        let me = ctx.address().recipient();
        self.room.do_send(Subscribe(me));
    }

    fn handle_event(&mut self, event: &str, value: &str, _ctx: &mut LiveViewContext<Self>) {
        match event {
            "message" => {
                self.assigns.new_message = value.into();
            }
            "send" => {
                let message = std::mem::replace(&mut self.assigns.new_message, String::new());
                self.room
                    .do_send(ChatMessage(format!("{}: {}", self.assigns.name, message)));
            }
            event => warn!("Received unknown event: {}", event),
        }
    }

    fn template(&self) -> Self::Template {
        self.assigns.clone()
    }
}

async fn chat(id: Identity, lobby: web::Data<Lobby>, req: HttpRequest) -> impl Responder {
    let name = match id.identity() {
        Some(name) => name,
        None => {
            return web::HttpResponse::Found()
                .set_header(
                    http::header::LOCATION,
                    req.url_for_static("chat-login").unwrap().to_string(),
                )
                .finish()
        }
    };
    let session = Session { name };

    // TODO: Change API so session doesn't need to be cloned.
    let root_layout = RootTemplate {
        inner: ChatLive::new(lobby.room.clone(), session.clone()).to_string(&session),
    };

    web::HttpResponse::Ok().body(root_layout.to_string())
}

async fn login_form(id: Identity, req: HttpRequest) -> impl Responder {
    if id.identity().is_some() {
        return web::HttpResponse::Found()
            .set_header(
                http::header::LOCATION,
                req.url_for_static("chat").unwrap().to_string(),
            )
            .finish();
    }

    let root_layout = RootTemplate {
        inner: LoginTemplate.to_string(),
    };

    web::HttpResponse::Ok().body(root_layout.to_string())
}

#[derive(Deserialize)]
struct FormData {
    name: String,
}

async fn login(id: Identity, form: web::Form<FormData>, req: HttpRequest) -> impl Responder {
    id.remember(form.name.to_string());

    web::HttpResponse::Found()
        .set_header(
            http::header::LOCATION,
            req.url_for_static("chat").unwrap().to_string(),
        )
        .finish()
}

async fn logout(id: Identity, req: HttpRequest) -> impl Responder {
    id.forget();

    web::HttpResponse::Found()
        .set_header(
            http::header::LOCATION,
            req.url_for_static("chat-login").unwrap().to_string(),
        )
        .finish()
}

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(
        web::resource("/chat")
            .name("chat")
            .route(web::get().to(chat)),
    );
    cfg.service(
        web::resource("/chat/login")
            .name("chat-login")
            .route(web::post().to(login))
            .route(web::get().to(login_form)),
    );
    cfg.route("/chat/logout", web::get().to(logout));
}
