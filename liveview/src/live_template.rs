use serde::Serialize;

#[derive(Debug, Serialize)]
pub enum Slot {
    /// These never change
    Static(&'static str),

    /// This field may change, but it will include the initial data for first render
    Dynamic(String),
}

#[derive(Debug, Serialize)]
pub struct RenderedTemplate {
    pub slots: Vec<Slot>,
}

impl std::fmt::Display for RenderedTemplate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for slot in &self.slots {
            let text = match slot {
                Slot::Static(text) => text,
                Slot::Dynamic(text) => text.as_str(),
            };
            write!(f, "{}", text)?
        }
        Ok(())
    }
}

#[derive(Debug, Serialize)]
pub struct Changes {
    /// Changes to the rendered template.
    /// Changes are a tuple where the first element is the index of the dynamic slot.
    pub changes: Vec<(usize, String)>,
}

/// A live template.
///
/// Knows how track changes within itself.
pub trait LiveTemplate {
    fn render(&self) -> RenderedTemplate;

    fn changes(&self, old_template: &Self) -> Changes;

    /// Render the template to a string.
    /// This is useful to render the template for regular HTTP requests.
    fn render_to_string(&self) -> String {
        self.render().to_string()
    }

    fn render_with_wrapper(&self, view: &str) -> String {
        format!(
            "<div id=\"rs-root\" rs-view=\"{}\">{}</div>",
            view,
            self.render_to_string()
        )
    }
}
