# Demo 9 – Sign in with Google (OpenID Connect)

Protects a resource with the OAuth 2.0 **authorization code flow**, using Google
as the OpenID Connect provider. A console application starts a Daraja HTTP server
on port 80 and opens `http://127.0.0.1/index.html` in the browser. The first
request triggers a redirect to the Google sign-in page; afterwards the page shows
the claims from the returned ID token.

## How it works

| Unit | Mapping | Role |
|------|---------|------|
| `TOpenIDAuthFilter` | `*.html` | Gate. No session credentials → create a `state` value and redirect to the callback. Otherwise decode the `id_token` (JWT), copy the claims into the session and continue the filter chain. |
| `TOpenIDCallbackResource` | `/openidcallback` | No `code` parameter → redirect to Google's `auth_uri` (scope `openid profile email`). With a `code` → verify `state`, then POST to `token_uri` to exchange the code for tokens and store them in the session. |
| `TRootResource` | `/index.html` | Renders the signed-in user's name, e-mail, issuer, subject and access-token hash. |
| `TdjNCSALogFilter` | `/*` | Access logging. |

Client credentials and endpoint URLs are read from `client_secret.json`
(`OpenIDHelper.LoadClientSecrets`).

## Setup

1. In the [Google Cloud Console](https://console.cloud.google.com/apis/credentials)
   create an **OAuth 2.0 Client ID** of type *Web application*.
2. Add `http://127.0.0.1/openidcallback` as an authorized redirect URI.
3. Download the JSON and save it next to the executable as `client_secret.json`.

The repository ships a sample `client_secret.json` for a demo project; replace it
with your own for real use. The redirect URI is also set in `MainUnit.pas`
(`REDIRECT_URI`) and must match the value registered with Google.

## Build & run

* **Lazarus / FPC:** `C:\lazarus\lazbuild.exe OpenIDConnectBackend.lpi`
* **Delphi:** compile `OpenIDConnectBackend.dpr` (see the project's build recipe).

Run `OpenIDConnectBackend.exe` and press Enter in the console to stop the server.
Port 80 must be free, and the OpenSSL DLLs (`libeay32.dll`, `ssleay32.dll`,
included) must be present for Indy's HTTPS calls to Google.

## Requirements

* [Daraja HTTP Framework](https://github.com/michaelJustin/daraja-framework)
* [Indy](https://github.com/IndySockets) + [OpenSSL binaries](https://github.com/IndySockets/OpenSSL-Binaries)
* [JsonDataObjects](https://github.com/ahausladen/JsonDataObjects) (Delphi only; FPC uses `fpjson`)

## Limitations

This is unsupported example code. The ID token signature is **not** verified,
`nonce` and PKCE are not implemented, and `TOpenIDAuthFilter` overwrites `sub`
and `email` with placeholder values before storing them. Do not use as-is in
production.
