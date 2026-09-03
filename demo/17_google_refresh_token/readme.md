# Demo 17 – Google refresh token (OAuth 2.0 + PKCE)

A console application starts a Daraja HTTP server on port 80 and opens
`http://127.0.0.1/index.html`. It runs the OAuth 2.0 **authorization code flow
with PKCE** against Google Identity, requesting offline access so that a
**refresh token** is returned. The result page shows the access token, its
lifetime and the refresh token, plus a button that exchanges the refresh token
for a fresh access token.

## How it works

All classes live in [MainUnit.pas](MainUnit.pas); endpoints and the client ID
are passed as context init parameters.

| Class | Mapping | Role |
|-------|---------|------|
| `TAuthFilter` | `*.html` | No `access_token` in the session → generate `code_verifier` / `code_challenge` (S256) and `state`, then redirect to Google's authorize endpoint (`response_type=code`, `access_type=offline`, `response_mode=form_post`). Otherwise pass the request through. |
| `TAuthResponseResource` | `/auth-response` | Receives the `form_post` callback, checks `state`, exchanges `code` + `code_verifier` for tokens at the token endpoint, stores them in the session and redirects to `/index.html`. |
| `TRootResource` | `/index.html` | `GET` renders the tokens; `POST` calls the token endpoint with `grant_type=refresh_token` and shows the old vs. new access token. |
| `TdjNCSALogFilter` | `/*` | Access logging. |

## Setup

You must supply your own Google Cloud project. The `ClientId` constant in
`MainUnit.pas` is only a placeholder and will not authenticate.

1. In the [Google Cloud Console](https://console.cloud.google.com/apis/credentials)
   create an OAuth client of type **installed application** (Desktop app / UWP) –
   a public client with **no client secret**.
2. Add `http://127.0.0.1/auth-response` as an authorized redirect URI.
3. Configure the OAuth consent screen and add your account as a test user.
4. Put the resulting client ID into the `ClientId` constant in `MainUnit.pas`
   (adjust `RedirectURI` and the endpoint constants if needed).

## Build & run

* **Delphi:** compile `GoogleRefreshTokenExample.dpr`.
* **Lazarus / FPC:** compile the same `.dpr` with FPC in Delphi mode, adding the
  `source`, `source/optional`, Indy and JsonDataObjects search paths.

Run the executable (port 80 must be free) and press Enter in the console to stop.
The bundled OpenSSL DLLs (`libeay32.dll`, `ssleay32.dll`) are required for Indy's
HTTPS calls.

## Requirements

* [Daraja HTTP Framework](https://github.com/michaelJustin/daraja-framework) (`source` + `source/optional`)
* [Indy](https://github.com/IndySockets) 10.6.3 + [OpenSSL binaries](https://github.com/IndySockets/OpenSSL-Binaries)
* [JsonDataObjects](https://github.com/ahausladen/JsonDataObjects)
* Delphi 2009+ or Lazarus / FPC 3.2

## Security notes

* **PKCE** (Proof Key for Code Exchange) protects the authorization code against
  interception; no client secret is used.
* `response_mode=form_post` keeps the code and tokens out of the redirect URL and
  browser history.
* `state` is verified on the callback.
* Unsupported example code: the ID token signature is not validated and tokens
  are kept in the server session only.
