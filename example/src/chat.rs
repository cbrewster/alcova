use crate::RootTemplate;
use actix::{Actor, Addr, AsyncContext, Context, Handler, Message, Recipient};
use actix_web::{web, Responder};
use alcova_macros::LiveTemplate;
use liveview::{LiveSocketContext, LiveTemplate, LiveView, LiveViewContext, LiveViewMessage};

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
struct Subscribe(Recipient<LiveViewMessage>);

#[derive(Message, Clone)]
#[rtype(result = "()")]
struct ChatMessage(String);

struct ChatRoom {
    subscribers: Vec<Recipient<LiveViewMessage>>,
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
            let message = LiveViewMessage(Box::new(msg.clone()));
            if let Err(_) = subscriber.do_send(message) {
                warn!("Error sending message!");
            }
        }
    }
}

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/chat.html.rlt")]
pub struct ChatTemplate {
    messages: Vec<String>,
    new_message: String,
    name: String,
}

impl ChatTemplate {
    fn new() -> Self {
        Self {
            messages: Vec::new(),
            new_message: String::new(),
            name: String::new(),
        }
    }
}

pub struct ChatLive {
    assigns: ChatTemplate,
    room: Addr<ChatRoom>,
}

impl ChatLive {
    fn new(room: Addr<ChatRoom>) -> Self {
        Self {
            assigns: ChatTemplate::new(),
            room,
        }
    }
}

impl LiveView for ChatLive {
    type Template = ChatTemplate;

    fn name() -> &'static str {
        "chat"
    }

    fn mount(socket_ctx: &LiveSocketContext) -> Self {
        let lobby = socket_ctx.app_data::<web::Data<Lobby>>().unwrap();
        Self::new(lobby.room.clone())
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
            "name" => {
                self.assigns.name = value.into();
            }
            "send" => {
                let message = std::mem::replace(&mut self.assigns.new_message, String::new());
                self.room
                    .do_send(ChatMessage(format!("{}: {}", self.assigns.name, message)));
            }
            event => warn!("Received unknown event: {}", event),
        }
    }

    fn handle_message(&mut self, message: LiveViewMessage, _ctx: &mut LiveViewContext<Self>) {
        if let Some(message) = message.get::<ChatMessage>() {
            self.assigns.messages.push(message.0.clone());
        }
    }

    fn template(&self) -> Self::Template {
        self.assigns.clone()
    }
}

async fn chat(lobby: web::Data<Lobby>) -> impl Responder {
    let root_layout = RootTemplate {
        inner: ChatLive::new(lobby.room.clone()).to_string(),
    };
    root_layout.to_response()
}

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::resource("/chat").route(web::get().to(chat)));
}
