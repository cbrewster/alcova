#[macro_use]
extern crate log;

mod chat;
mod fruit;
mod timer;
mod top;

use actix_files as fs;
use actix_identity::{CookieIdentityPolicy, IdentityService};
use actix_web::{middleware, web, App, Error, HttpRequest, HttpResponse, HttpServer, Responder};
use actix_web_actors::ws;
use alcova::{LiveSocket, LiveTemplate, LiveViewRegistry};
use chat::{ChatLive, Lobby};
use fruit::FruitLive;
use listenfd::ListenFd;
use timer::TimerLive;
use top::TopLive;

#[derive(LiveTemplate)]
#[alcova(template = "templates/layout.html.alcova")]
struct RootTemplate {
    inner: String,
}

#[derive(LiveTemplate)]
#[alcova(template = "templates/index.html.alcova")]
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
    let res = ws::start(LiveSocket::new(registry.clone(), r.clone()), &r, stream);
    res
}

async fn index() -> impl Responder {
    let root_layout = RootTemplate {
        inner: IndexTemplate.to_string(),
    };
    root_layout.to_response()
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
        .register::<ChatLive>()
        .register::<TopLive>()
        .build();

    let chat_lobby = web::Data::new(Lobby::new());

    let mut server = HttpServer::new(move || {
        App::new()
            .wrap(IdentityService::new(
                CookieIdentityPolicy::new(&[0; 32])
                    .name("auth-cookie")
                    .secure(false),
            ))
            .data(registry.clone())
            .app_data(chat_lobby.clone())
            // enable logger
            .wrap(middleware::Logger::default())
            // websocket route
            .service(web::resource("/ws").route(web::get().to(ws_index)))
            .route("/", web::get().to(index))
            .configure(fruit::config)
            .configure(timer::config)
            .configure(chat::config)
            .configure(top::config)
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
