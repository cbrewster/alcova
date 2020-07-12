#[macro_use]
extern crate log;

mod fruit;
mod timer;

use actix_files as fs;
use actix_web::{middleware, web, App, Error, HttpRequest, HttpResponse, HttpServer, Responder};
use actix_web_actors::ws;
use alcova_macros::LiveTemplate;
use fruit::FruitLive;
use listenfd::ListenFd;
use liveview::{LiveSocket, LiveTemplate, LiveViewRegistry};
use timer::TimerLive;

#[derive(LiveTemplate)]
#[alcova(template = "templates/layout.html.rlt")]
struct RootTemplate {
    inner: String,
}

#[derive(LiveTemplate)]
#[alcova(template = "templates/index.html.rlt")]
struct IndexTemplate;

/// do websocket handshake and start `MyWebSocket` actor
async fn ws_index(
    registry: web::Data<LiveViewRegistry>,
    r: HttpRequest,
    stream: web::Payload,
) -> Result<HttpResponse, Error> {
    // TODO: Cleanup... Data gives us an Arc but we have an Arc in the Arc
    // Maybe just require making the registry inside HttpServer::new
    let registry = &*registry.into_inner();
    let res = ws::start(LiveSocket::new(registry.clone()), &r, stream);
    res
}

async fn index() -> impl Responder {
    let root_layout = RootTemplate {
        inner: IndexTemplate.render_to_string(),
    };
    HttpResponse::Ok().body(root_layout.render_to_string())
}

#[actix_rt::main]
async fn main() -> Result<(), std::io::Error> {
    env_logger::init();
    let mut listenfd = ListenFd::from_env();
    let port = std::env::var("PORT")
        .unwrap_or("3000".into())
        .parse()
        .expect("Failed to parse PORT");

    let registry = LiveViewRegistry::builder()
        .register::<FruitLive>()
        .register::<TimerLive>()
        .build();

    let mut server = HttpServer::new(move || {
        App::new()
            .data(registry.clone())
            // enable logger
            .wrap(middleware::Logger::default())
            // websocket route
            .service(web::resource("/ws").route(web::get().to(ws_index)))
            .route("/", web::get().to(index))
            .configure(fruit::config)
            .configure(timer::config)
            // static files
            .service(fs::Files::new("/", "static/").index_file("index.html"))
    });

    server = if let Some(l) = listenfd.take_tcp_listener(0).unwrap() {
        info!("Watching...");
        server.listen(l)?
    } else {
        server.bind(("0.0.0.0", port))?
    };

    server.run().await
}
