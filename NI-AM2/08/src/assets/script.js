/*
 * Create form to request access token from Google's OAuth 2.0 server.
 */
function oauthSignIn() {
    // Google's OAuth 2.0 endpoint for requesting an access token
    var oauth2Endpoint = "https://accounts.google.com/o/oauth2/v2/auth";

    // Create <form> element to submit parameters to OAuth 2.0 endpoint.
    var form = document.createElement("form");
    form.setAttribute("method", "GET"); // Send as a GET request.
    form.setAttribute("action", oauth2Endpoint);

    // Parameters to pass to OAuth 2.0 endpoint.
    var params = {
        client_id: "1043221774854-touho8hpuedavkjggq753oamr3ii5530.apps.googleusercontent.com",
        redirect_uri: "https://localhost:8443/callback",
        response_type: "token",
        scope: "https://www.googleapis.com/auth/drive.metadata.readonly",
        include_granted_scopes: "true",
        state: "state",
    };

    // Add form parameters as hidden input values.
    for (var p in params) {
        var input = document.createElement("input");
        input.setAttribute("type", "hidden");
        input.setAttribute("name", p);
        input.setAttribute("value", params[p]);
        form.appendChild(input);
    }

    // Add form to page and submit it to open the OAuth 2.0 endpoint.
    document.body.appendChild(form);
    form.submit();
}

function parseAndPersistToken() {
    const parsed = new URLSearchParams(window.location.hash.slice(1));
    if (!parsed.has("access_token") || !parsed.has("token_type")) return;

    const token = parsed.get("access_token");
    const tokenType = parsed.get("token_type");
    localStorage.setItem("token", token);
    localStorage.setItem("tokenType", tokenType);

    window.location.replace("/");
}

async function dowloadFileList(token, tokenType) {
    const res = await fetch("https://www.googleapis.com/drive/v3/files", {
        headers: {
            Authorization: `${tokenType} ${token}`,
        },
    });
    // {
    //     "kind": "drive#fileList",
    //     "nextPageToken": string,
    //     "incompleteSearch": boolean,
    //     "files": [
    //       {
    //     id: "16mew3VOUzVjZk049PYy05FMlXmUjO20"
    //     kind: "drive#file"
    //     mimeType: "application/vnd.google-apps.shortcut"
    //     name: "Archiv.zip"
    //       }
    //     ]
    //   }
    const body = await res.json();
    return body.files;
}

window.addEventListener("load", async function () {
    const button = document.querySelector("#signInButton");

    button.addEventListener("click", function () {
        oauthSignIn();
    });
    if (window.location.pathname === "/callback") {
        parseAndPersistToken();
    }

    const token = this.localStorage.getItem("token");
    const tokenType = this.localStorage.getItem("tokenType");
    if (token && tokenType) {
        const files = await dowloadFileList(token, tokenType);
        console.log(files);
        const container = document.createElement("div");
        for (let file of files) {
            const node = document.createElement("div");
            node.innerHTML = file.name;
            container.append(node);
        }
        document.body.append(container);
    }
});
