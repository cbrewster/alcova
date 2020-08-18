use crate::{
    live_socket::LiveSocketContext,
    live_view::{signing_secret, Claims, LiveViewActor, LiveViewId, LiveViewMessage},
    LiveSocket, LiveView,
};
use actix::{Actor, Addr, Recipient};
use jsonwebtoken::{decode, DecodingKey, Validation};
use std::{collections::HashMap, sync::Arc};

trait LiveViewSpawner: Sync + Send {
    fn spawn(
        &self,
        id: LiveViewId,
        socket: Addr<LiveSocket>,
        context: &LiveSocketContext,
        session_data: &str,
    ) -> Recipient<LiveViewMessage>;
}

impl<F> LiveViewSpawner for F
where
    F: Sync
        + Send
        + for<'r> Fn(
            LiveViewId,
            Addr<LiveSocket>,
            &'r LiveSocketContext,
            &'r str,
        ) -> Recipient<LiveViewMessage>,
{
    fn spawn<'a>(
        &self,
        id: LiveViewId,
        socket: Addr<LiveSocket>,
        ctx: &'a LiveSocketContext,
        session_data: &'a str,
    ) -> Recipient<LiveViewMessage> {
        self(id, socket, ctx, session_data)
    }
}

pub struct LiveViewRegistryBuilder {
    live_views: HashMap<String, Box<dyn LiveViewSpawner>>,
}

#[derive(Clone)]
pub struct LiveViewRegistry {
    live_views: Arc<HashMap<String, Box<dyn LiveViewSpawner>>>,
}

impl LiveViewRegistryBuilder {
    pub fn build(self) -> LiveViewRegistry {
        LiveViewRegistry {
            live_views: Arc::new(self.live_views),
        }
    }

    pub fn register<T: LiveView + Unpin + 'static>(mut self) -> Self {
        self.live_views.insert(
            T::name().into(),
            Box::new(
                |id, socket, context: &LiveSocketContext, session_data: &str| {
                    let key = signing_secret();
                    let token = decode::<Claims<_>>(
                        session_data,
                        &DecodingKey::from_secret(key.as_bytes()),
                        &Validation::default(),
                    )
                    .expect("Failed to deserialize session data");
                    LiveViewActor::<T>::new(id, socket, context, token.claims.data)
                        .start()
                        .recipient()
                },
            ),
        );
        self
    }
}

impl LiveViewRegistry {
    pub fn builder() -> LiveViewRegistryBuilder {
        LiveViewRegistryBuilder {
            live_views: HashMap::new(),
        }
    }

    pub fn spawn<T: AsRef<str>>(
        &self,
        name: T,
        id: LiveViewId,
        socket: Addr<LiveSocket>,
        ctx: &LiveSocketContext,
        session_data: &str,
    ) -> Option<Recipient<LiveViewMessage>> {
        self.live_views
            .get(name.as_ref())
            .map(|spawner| spawner.spawn(id, socket, ctx, session_data))
    }
}
