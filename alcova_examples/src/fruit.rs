use crate::RootTemplate;
use actix_web::{web, Responder};
use alcova::{LiveSocketContext, LiveTemplate, LiveView, LiveViewContext};

fn fruits() -> Vec<&'static str> {
    include_str!("../fruits.txt").lines().collect()
}

#[derive(Debug, Clone, PartialEq)]
struct Test {
    name: String,
    option: Option<String>,
}

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/fruit.html.rlt")]
pub struct FruitTemplate {
    options: Vec<String>,
    selected: Vec<String>,
    search: String,
    test: Test,
}

impl FruitTemplate {
    fn new() -> Self {
        Self {
            options: Vec::new(),
            selected: Vec::new(),
            search: String::new(),
            test: Test {
                name: "Connor".into(),
                option: Some("Hello".into()),
            },
        }
    }
}

pub struct FruitLive {
    assigns: FruitTemplate,
}

impl FruitLive {
    fn new() -> Self {
        Self {
            assigns: FruitTemplate::new(),
        }
    }
}

impl LiveView for FruitLive {
    type Template = FruitTemplate;
    type SessionData = ();

    fn name() -> &'static str {
        "fruit"
    }

    fn mount(_socket_ctx: &LiveSocketContext, _session: ()) -> Self {
        Self::new()
    }

    fn handle_event(&mut self, event: &str, value: &str, _ctx: &mut LiveViewContext<Self>) {
        match event {
            "search" => {
                self.assigns.search = value.to_string();
                if self.assigns.search.is_empty() {
                    self.assigns.options.clear();
                    return;
                }
                let search = value.to_lowercase();
                self.assigns.options = fruits()
                    .iter()
                    .filter(|fruit| !self.assigns.selected.contains(&fruit.to_string()))
                    .filter(|fruit| fruit.to_lowercase().contains(&search))
                    .map(|fruit| fruit.to_string())
                    .collect();
            }
            "select" => {
                self.assigns.selected.push(value.to_string());
                self.assigns.search = String::new();
                self.assigns.options.clear();
            }
            "remove" => {
                if let Some(index) = self.assigns.selected.iter().position(|item| item == value) {
                    self.assigns.selected.remove(index);
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
    let root_layout = RootTemplate {
        inner: FruitLive::new().to_string(&()),
    };
    root_layout.to_response()
}

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::resource("/fruit").route(web::get().to(fruit)));
}
