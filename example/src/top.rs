use crate::RootTemplate;
use actix::{Arbiter, AsyncContext, Message};
use actix_web::{web, Responder};
use alcova_macros::LiveTemplate;
use liveview::{
    LiveHandler, LiveMessage, LiveSocketContext, LiveTemplate, LiveView, LiveViewContext,
};
use std::{process::Stdio, time::Duration};
use tokio::process::Command;

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
struct TopResult(String);

impl LiveMessage for TopResult {}

#[derive(Message)]
#[rtype(result = "()")]
struct Tick;

impl LiveMessage for Tick {}

impl LiveHandler<Tick> for TopLive {
    fn handle(&mut self, _msg: Tick, ctx: &mut LiveViewContext<Self>) {
        let addr = ctx.address();

        let execution = async move {
            // Use "-l1" on mac
            let top = Command::new("top")
                .arg("-bn1")
                .env("TERM", "xterm")
                .stdout(Stdio::piped())
                .spawn()
                .expect("Failed to spawn top");

            let res = String::from_utf8(top.wait_with_output().await.expect("top failed").stdout)
                .expect("top output not valid utf8");
            addr.do_send(TopResult(res));
        };

        Arbiter::spawn(execution);
    }
}

impl LiveHandler<TopResult> for TopLive {
    fn handle(&mut self, msg: TopResult, _ctx: &mut LiveViewContext<Self>) {
        self.assigns.top = msg.0;
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
