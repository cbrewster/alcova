window.onload = () => {
    if (location.protocol !== 'https:') {
        ws = new WebSocket(`ws://${location.host}/ws`);
    } else {
        ws = new WebSocket(`wss://${location.host}/ws`);
    }

    let root = document.querySelector("#rs-root");

    let template = null;

    ws.onmessage = (msg) => {
        let event = JSON.parse(msg.data);
        if (event.Template) {
            template = event.Template;
            update(ws, root, template);
        }
        if (event.Changes) {
            applyChanges(template, event.Changes.changes);
            update(ws, root, template);
        }
    }
};

function update(ws, root, template) {
    morphdom(root, `<div id="rs-root">` + renderTemplate(template) + `</div>`, {
        childrenOnly: true
    });

    let clickers = document.querySelectorAll("[rs-click]");
    clickers.forEach((clicker) => {
        clicker.onclick = () => {
            ws.send(JSON.stringify({
                action: clicker.getAttribute("rs-click"),
                value: clicker.getAttribute("rs-value")
            }));
        }
    });

    let fields = document.querySelectorAll("[rs-change]");
    fields.forEach((field) => {
        field.oninput = (e) => {
            ws.send(JSON.stringify({
                action: field.getAttribute("rs-change"),
                value: e.target.value,
            }));
        }
    });
}

function renderTemplate(template) {
    let string = "";

    template.slots.forEach((slot) => {
        if (slot.Dynamic) {
            string += slot.Dynamic;
        }

        if (slot.Static) {
            string += slot.Static;
        }
    });

    return string;
}

function applyChanges(template, changes) {
    changes.forEach((change) => {
        template.slots[change[0]].Dynamic = change[1];
    });
}

