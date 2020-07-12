use crate::RootTemplate;
use actix_web::{web, HttpResponse, Responder};
use alcova_macros::LiveTemplate;
use liveview::{LiveTemplate, LiveView, LiveViewContext};

fn fruits() -> Vec<&'static str> {
    include_str!("../fruits.txt").lines().collect()
}

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/fruit.html.rlt")]
pub struct FruitTemplate {
    options: Vec<String>,
    selected: Vec<String>,
    search: String,
}

impl FruitTemplate {
    fn new() -> Self {
        Self {
            options: Vec::new(),
            selected: Vec::new(),
            search: String::new(),
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

    fn name() -> &'static str {
        "fruit"
    }

    fn mount() -> Self {
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
    let initial_template = FruitTemplate::new();
    let root_layout = RootTemplate {
        inner: initial_template.render_with_wrapper(FruitLive::name()),
    };
    HttpResponse::Ok().body(root_layout.render_to_string())
}

pub fn config(cfg: &mut web::ServiceConfig) {
    cfg.service(web::resource("/fruit").route(web::get().to(fruit)));
}
