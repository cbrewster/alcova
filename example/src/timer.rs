use crate::RootTemplate;
use actix::{AsyncContext, SpawnHandle};
use actix_web::{web, HttpResponse, Responder};
use alcova_macros::LiveTemplate;
use liveview::{LiveTemplate, LiveView, LiveViewContext};
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
    let millis = time.as_millis() % 1000;
    format!("{:02}:{:02}.{:03}", minutes, seconds, millis)
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

    fn name() -> &'static str {
        "timer"
    }

    fn mount() -> Self {
        Self::new()
    }

    fn handle_event(&mut self, event: &str, _value: &str, ctx: &mut LiveViewContext<Self>) {
        match event {
            "start" => {
                self.started_at = Instant::now();

                let handle = ctx.run_interval(Duration::from_millis(10), |actor, _ctx| {
                    actor.view.assigns.duration = actor.view.started_at.elapsed();
                    actor.send_changes();
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

async fn fruit() -> impl Responder {
    let initial_template = TimerTemplate::new();
    let root_layout = RootTemplate {
        inner: initial_template.render_with_wrapper(TimerLive::name()),
    };
    HttpResponse::Ok().body(root_layout.render_to_string())
}

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::resource("/timer").route(web::get().to(fruit)));
}
