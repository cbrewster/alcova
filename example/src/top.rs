use crate::RootTemplate;
use actix::{AsyncContext, Message};
use actix_web::{web, Responder};
use alcova_macros::LiveTemplate;
use liveview::{
    LiveHandler, LiveMessage, LiveSocketContext, LiveTemplate, LiveView, LiveViewContext,
};
use std::{io::Read, process::Stdio, time::Duration};

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/top.html.rlt")]
pub struct TopTemplate {
    top: String,
}

impl TopTemplate {
    fn new() -> Self {
        Self { top: String::new() }
    }
}

pub struct TopLive {
    assigns: TopTemplate,
}

#[derive(Message)]
#[rtype(result = "()")]
struct Tick;

impl LiveMessage for Tick {}

impl LiveHandler<Tick> for TopLive {
    fn handle(&mut self, _msg: Tick, _ctx: &mut LiveViewContext<Self>) {
        let mut top = std::process::Command::new("top")
            .arg("-l1")
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to spawn top");
        self.assigns.top.clear();
        top.stdout
            .as_mut()
            .unwrap()
            .read_to_string(&mut self.assigns.top)
            .expect("Failed to read top");
        top.kill().expect("Failed to kill top");
    }
}

impl TopLive {
    fn new() -> Self {
        Self {
            assigns: TopTemplate::new(),
        }
    }
}

impl LiveView for TopLive {
    type Template = TopTemplate;
    type SessionData = ();

    fn name() -> &'static str {
        "top"
    }

    fn mount(_socket_ctx: &LiveSocketContext, _session: ()) -> Self {
        Self::new()
    }

    fn started(&mut self, ctx: &mut LiveViewContext<Self>) {
        ctx.address().do_send(Tick);
        ctx.run_interval(Duration::from_secs(1), |_actor, ctx| {
            ctx.address().do_send(Tick);
        });
    }

    fn handle_event(&mut self, _event: &str, _value: &str, _ctx: &mut LiveViewContext<Self>) {}

    fn template(&self) -> Self::Template {
        self.assigns.clone()
    }
}

async fn top() -> impl Responder {
    let root_layout = RootTemplate {
        inner: TopLive::new().to_string(&()),
    };
    root_layout.to_response()
}

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::resource("/top").route(web::get().to(top)));
}
