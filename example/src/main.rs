use actix_files as fs;
use actix_web::{middleware, web, App, Error, HttpRequest, HttpResponse, HttpServer, Responder};
use actix_web_actors::ws;
use alcova_macros::LiveTemplate;
use listenfd::ListenFd;
use liveview::{Changes, LiveSocket, LiveTemplate, LiveView, RenderedTemplate, Slot};

fn fruits() -> Vec<&'static str> {
    include_str!("../fruits.txt").lines().collect()
}

#[derive(LiveTemplate)]
#[alcova(template = "templates/layout.html.rlt")]
struct RootTemplate {
    inner: String,
}

#[derive(Debug, Clone, LiveTemplate, PartialEq)]
#[alcova(template = "templates/hello.html.rlt")]
struct HelloTemplate {
    options: Vec<String>,
    selected: Option<String>,
    search: String,
}

impl HelloTemplate {
    fn new() -> Self {
        Self {
            options: Vec::new(),
            selected: None,
            search: String::new(),
        }
    }
}

struct HelloView {
    assigns: HelloTemplate,
}

impl HelloView {
    fn new() -> Self {
        Self {
            assigns: HelloTemplate::new(),
        }
    }
}

impl LiveView for HelloView {
    type Template = HelloTemplate;

    fn handle_event(&mut self, event: &str, value: &str) {
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
                    .filter(|fruit| fruit.to_lowercase().contains(&search))
                    .map(|fruit| fruit.to_string())
                    .collect();
            }
            "select" => {
                self.assigns.selected = Some(value.to_string());
                self.assigns.search = String::new();
                self.assigns.options.clear();
            }
            _ => {}
        }
    }

    fn render(&self) -> Self::Template {
        self.assigns.clone()
    }
}

/// do websocket handshake and start `MyWebSocket` actor
async fn ws_index(r: HttpRequest, stream: web::Payload) -> Result<HttpResponse, Error> {
    let view = HelloView::new();
    let res = ws::start(LiveSocket::new(view), &r, stream);
    res
}

async fn hello() -> impl Responder {
    let initial_template = HelloTemplate::new();
    let root_layout = RootTemplate {
        inner: initial_template.render_with_wrapper(),
    };
    HttpResponse::Ok().body(root_layout.render_to_string())
}

#[actix_rt::main]
async fn main() -> Result<(), std::io::Error> {
    env_logger::init();
    let mut listenfd = ListenFd::from_env();
    let port = std::env::var("PORT").unwrap_or("3000".into());
    let host = std::env::var("HOST").unwrap_or("127.0.0.1".into());

    let mut server = HttpServer::new(|| {
        App::new()
            // enable logger
            .wrap(middleware::Logger::default())
            // websocket route
            .route("/", web::get().to(hello))
            .service(web::resource("/ws").route(web::get().to(ws_index)))
            // static files
            .service(fs::Files::new("/", "static/").index_file("index.html"))
    });

    server = if let Some(l) = listenfd.take_tcp_listener(0).unwrap() {
        server.listen(l)?
    } else {
        server.bind(format!("{}:{}", host, port))?
    };

    server.run().await
}
