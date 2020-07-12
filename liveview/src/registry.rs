use crate::{
    live_socket::LiveSocketContext,
    live_view::{LiveViewAction, LiveViewActor, LiveViewId},
    LiveSocket, LiveView,
};
use actix::{Actor, Addr, Recipient};
use std::{collections::HashMap, sync::Arc};

trait LiveViewSpawner: Sync + Send {
    fn spawn(
        &self,
        id: LiveViewId,
        socket: Addr<LiveSocket>,
        context: &LiveSocketContext,
    ) -> Recipient<LiveViewAction>;
}

impl<F> LiveViewSpawner for F
where
    F: Sync
        + Send
        + for<'r> Fn(LiveViewId, Addr<LiveSocket>, &'r LiveSocketContext) -> Recipient<LiveViewAction>,
{
    fn spawn<'a>(
        &self,
        id: LiveViewId,
        socket: Addr<LiveSocket>,
        ctx: &'a LiveSocketContext,
    ) -> Recipient<LiveViewAction> {
        self(id, socket, ctx)
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
            Box::new(|id, socket, context: &LiveSocketContext| {
                LiveViewActor::<T>::new(id, socket, context)
                    .start()
                    .recipient()
            }),
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
    ) -> Option<Recipient<LiveViewAction>> {
        self.live_views
            .get(name.as_ref())
            .map(|spawner| spawner.spawn(id, socket, ctx))
    }
}
