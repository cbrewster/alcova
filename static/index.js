window.onload = () => {
    if (location.protocol !== 'https:') {
        ws = new WebSocket(`ws://${location.host}/ws`);
    } else {
        ws = new WebSocket(`wss://${location.host}/ws`);
    }

    let root = document.querySelector("#rs-root");

    root.onclick = (e) => {
        if (e.target.getAttribute("rs-click")) {
            ws.send(JSON.stringify({
                LiveView: {
                    id: liveViewId,
                    action: {
                        action: e.target.getAttribute("rs-click"),
                        value: e.target.getAttribute("rs-value")
                    },
                }
            }));
        }
    }

    root.oninput = (e) => {
        if (e.target.getAttribute("rs-change")) {
            ws.send(JSON.stringify({
                LiveView: {
                    id: liveViewId,
                    action: {
                        action: e.target.getAttribute("rs-change"),
                        value: e.target.value,
                    },
                }
            }));
        }
    }

    root.onsubmit = (e) => {
        if (e.target.getAttribute("rs-submit")) {
            e.preventDefault();
            const formJson = JSON.stringify(formDataToJson(e.target));
            ws.send(JSON.stringify({
                LiveView: {
                    id: liveViewId,
                    action: {
                        action: e.target.getAttribute("rs-submit"),
                        value: formJson,
                    },
                }
            }));
        }
    }

    ws.onopen = () => {
        console.log("Connecting to live view...");
        ws.send(JSON.stringify({
            SpawnLiveView: {
                name: root.getAttribute("rs-view"),
                session: root.getAttribute("rs-session")
            }
        }));
    };

    let template = null;
    let liveViewId = null;

    ws.onmessage = (msg) => {
        let event = JSON.parse(msg.data);
        if (event.Template) {
            template = event.Template.template;
            liveViewId = event.Template.id;
            update(root, template);
        }
        if (event.Changes) {
            applyChanges(template, event.Changes.changes);
            update(root, template);
        }
    }
};

function formDataToJson(formElement) {
    const formData = new FormData(formElement).entries();

    let jsonObject = {};

    for (const [key, value]  of formData) {
        jsonObject[key] = value;
    }
    return jsonObject;
}

function update(root, template) {
    morphdom(root, `<div id="rs-root">${renderTemplate(template)}</div>`, {
        childrenOnly: true
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

