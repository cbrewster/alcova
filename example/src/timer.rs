use crate::RootTemplate;
use actix::{AsyncContext, Message, SpawnHandle};
use actix_web::{web, Responder};
use alcova_macros::LiveTemplate;
use liveview::{
    LiveHandler, LiveMessage, LiveSocketContext, LiveTemplate, LiveView, LiveViewContext,
};
use std::time::{Duration, Instant};

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/timer.html.rlt")]
pub struct TimerTemplate {
    duration: Duration,
}

fn format_time(time: &Duration) -> String {
    let seconds = time.as_secs();
    let minutes = seconds / 60;
    let seconds = seconds % 60;
    let millis = (time.as_millis() % 1000) / 10;
    format!("{:02}:{:02}.{:02}", minutes, seconds, millis)
}

impl TimerTemplate {
    fn new() -> Self {
        Self {
            duration: Duration::from_secs(0),
        }
    }
}

pub struct TimerLive {
    assigns: TimerTemplate,
    started_at: Instant,
    timer: Option<SpawnHandle>,
}

#[derive(Message)]
#[rtype(result = "()")]
struct Tick;

impl LiveMessage for Tick {}

impl LiveHandler<Tick> for TimerLive {
    fn handle(&mut self, _msg: Tick, _ctx: &mut LiveViewContext<Self>) {
        self.assigns.duration = self.started_at.elapsed();
    }
}

impl TimerLive {
    fn new() -> Self {
        Self {
            assigns: TimerTemplate::new(),
            started_at: Instant::now(),
            timer: None,
        }
    }
}

impl LiveView for TimerLive {
    type Template = TimerTemplate;
    type SessionData = ();

    fn name() -> &'static str {
        "timer"
    }

    fn mount(_socket_ctx: &LiveSocketContext, _session: ()) -> Self {
        Self::new()
    }

    fn handle_event(&mut self, event: &str, _value: &str, ctx: &mut LiveViewContext<Self>) {
        match event {
            "start" => {
                self.started_at = Instant::now();

                let handle = ctx.run_interval(Duration::from_millis(10), |_actor, ctx| {
                    ctx.address().do_send(Tick);
                });
                self.timer = Some(handle);
            }
            "stop" => {
                if let Some(timer) = self.timer.take() {
                    ctx.cancel_future(timer);
                }
            }
            _ => {}
        }
    }

    fn template(&self) -> Self::Template {
        self.assigns.clone()
    }
}

async fn timer() -> impl Responder {
    let root_layout = RootTemplate {
        inner: TimerLive::new().to_string(&()),
    };
    root_layout.to_response()
}

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::resource("/timer").route(web::get().to(timer)));
}
