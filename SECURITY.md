# Security and hardening

This page is for developers who run a Daraja server where untrusted clients
can reach it. It explains what the framework protects against, what it
leaves to you, and how to configure the rest.

Daraja is a thin dispatch layer over Indy's HTTP server. Indy reads the
socket and parses the HTTP syntax: the request line, headers, chunked
bodies, multipart bodies and query parameters. How well a server copes with
malformed bytes therefore depends largely on Indy. Use Indy 10.6.2 or
10.6.3 (see the README); fixes for header parsing and request smuggling
land in Indy, not here.

## Put a reverse proxy in front

For anything exposed to the internet, run Daraja behind a reverse proxy
(nginx, Apache, HAProxy, IIS, a cloud load balancer) and let the proxy
handle:

- TLS termination
- request-rate limiting
- a hard ceiling on request body and header size
- slow-client protection

The limits below reduce what a single Daraja process has to absorb; they
don't replace that layer.

## Resource limits

`TdjHTTPServer` ships with **no protective limits**: unlimited connections,
no read timeout, and no request-body cap. Indy reads a whole request body
into memory (or a temp file) before your code runs.

You can set limits today through the Indy server that every
`TdjHTTPConnector` exposes as its `HTTPServer` property. Create the
connector yourself so you can configure it before `Server.Start`:

```pascal
Connector := TdjHTTPConnector.Create(Server.Handler);
Connector.Host := '127.0.0.1';
Connector.Port := 8080;

Connector.HTTPServer.MaxConnections := 200;
Connector.HTTPServer.ListenQueue := 64;
Connector.HTTPServer.OnConnect := Limits.HandleConnect;                 // sets IOHandler.ReadTimeout
Connector.HTTPServer.OnHeadersAvailable := Limits.HandleHeadersAvailable; // rejects a large Content-Length
Connector.HTTPServer.OnHeadersBlocked := Limits.HandleHeadersBlocked;     // turns that into a 413

Server.AddConnector(Connector);
```

| Concern | Setting | Effect |
|---|---|---|
| Connection flood | `MaxConnections` | excess connections are refused |
| Listen backlog | `ListenQueue` | caps pending connections at the socket level |
| Slowloris (a client that sends slowly or not at all) | `IOHandler.ReadTimeout`, set per connection in `OnConnect` | an idle connection is dropped instead of holding a thread forever |
| Large request body | check `Content-Length` in `OnHeadersAvailable`, answer in `OnHeadersBlocked` | `413` before the body is read |

`demo/18_hardening_limits` is a complete, runnable version of this, with
the values explained. The numbers there are a starting point, not a
recommendation.

What configuration alone cannot fix (tracked in
[#531](https://github.com/michaelJustin/daraja-framework/issues/531)):

- A **chunked** request body has no `Content-Length`, so the
  `OnHeadersAvailable` check can't see its size.
- The total number and size of headers is not capped. Indy only limits a
  single header line (about 16 KB).
- Session growth (next section).

## Sessions

A context created with sessions enabled, for example
`TdjWebAppContext.Create('app', True)`, creates a new session for **every
request that arrives without a session cookie**. Indy's session list has no
size limit, so a client that never sends the cookie back can create
sessions until the server runs out of memory. Each one lives for
`SessionTimeOut` (10 minutes by default).

- Enable sessions only on contexts that need them.
- Shorten `Connector.HTTPServer.SessionTimeOut` where you can. This narrows
  the window but doesn't close it.
- Rely on the reverse proxy's rate limiting for the rest.

## Bind address

`TdjServer` binds to `127.0.0.1` unless you give it another host. Keep it
that way behind a reverse proxy on the same machine. Bind to a public
interface only when you mean to.

## URL patterns are not authorization

A filter mapped to `/admin/*` is a routing rule. It doesn't know who the
caller is. An authentication or authorization filter must check the
caller's identity itself (for example, from the session) before passing the
request on. If a resource needs protecting, check identity in the
component as well.

Before any URL pattern is matched, Daraja normalizes the request target:
`.`, `..` and repeated `/` are collapsed per path segment, and a
`;`-parameter is stripped. A target containing a NUL or another control
character is rejected with `400`. A filter and the component it guards
always see the same path.

Two things are not normalized:

- **Case.** `/Admin/x` doesn't match a filter on `/admin/*`. If something
  behind your routes treats paths case-insensitively, such as static files
  on Windows, a filter on `/admin/*` doesn't protect `/ADMIN/...`.
- **Encoded slashes** (`%2F`, `%5C`). Don't rely on them being treated as
  path separators, or as ordinary characters.

## Threading: one instance per component and filter

The framework creates **one instance** of each registered Web Component and
Web Filter, and every concurrent request runs through that same instance on
its own thread.

- Don't keep per-request data in instance fields. Two requests running at
  the same time read and write the same field, and one user's data can end
  up in another user's response.
- Use local variables for anything request-specific.
- Protect shared state explicitly, for example with a critical section.

The same applies to anything a component holds, such as a database
connection or an HTTP client.

## Error responses

When a Web Component or Web Filter raises an unhandled exception, the
client gets a generic `500 Internal Server Error`. The full detail
(component class, exception class and message, and a stack trace where
available) goes to the server log.

- The detailed error page, with exception text and stack trace, is shown
  only when the `DARAJA_PROJECT_STAGE_DEVELOPMENT` symbol is defined.
  **Never define it in a production build.** Exception messages often
  contain file paths, SQL, connection strings or host names.
- To show your own error page, set the context's `ErrorHandler` (see
  `demo/19_error_handler`). The failure detail is available to it through
  `TdjServerContext` (`LastErrorStatusCode`, `LastErrorExceptionClass`,
  `LastErrorExceptionMessage`). The demo prints the exception message on
  the page to show what's available. A production page shouldn't.
- By default, a component whose `Init` fails is logged and skipped, and the
  server starts anyway. Set `TdjServer.StrictStart := True` before `Start`
  if you'd rather the server fail to start than run with missing parts.

## HTTP methods

- `HEAD` runs your `OnGet` in full and discards the body, so a `HEAD`
  request costs as much as a `GET` and has the same side effects. Override
  `OnHead` if that matters.
- `TRACE` answers `405` unless you override `OnTrace`; the request is not
  echoed back.
- An unrecognised method answers `501`.

## Output encoding

Daraja doesn't encode your output. `djGlobal.HTMLEncode` escapes only
`<`, `>`, `&` and `"`. It's safe for HTML element text and double-quoted
attributes, but not for single-quoted attributes, JavaScript, CSS or URLs.
For those, use an encoder made for the context.

Daraja doesn't add security response headers (`Content-Security-Policy`,
`X-Content-Type-Options`, `X-Frame-Options`, `Strict-Transport-Security`).
Set them in your components, in a filter, or at the reverse proxy.

## Optional units

The units in `source/optional/` are unsupported example code. Review them
before relying on them:

- `TdjDefaultWebComponent` (static files) rejects `..` traversal and checks
  that the resolved path stays inside the content folder. It doesn't
  resolve symbolic links, so a link inside the folder can point outside it.
- `TdjNCSALogFilter` writes the request line and the user name from the
  `Authorization` header to the access log without escaping. A client can
  put line breaks in those values and forge log lines. Don't feed its
  output to anything that trusts the line structure.
- `TdjStatisticsFilter` needs a context with sessions enabled. Without
  sessions it raises a configuration error.
