# Hardening limits on an existing server instance

Shows how to apply connection, timeout, and request-body-size limits to a
running `TdjHTTPConnector` / Indy `TIdCustomHTTPServer` instance using only
public API that already ships with Daraja and Indy — no changes to the
framework source. Run and open <http://127.0.0.1:8080/demo/echo>.

## Concerns addressed

`TdjHTTPServer` ships with no protective limits by default: unlimited
connections, no read/inactivity timeout, no request-body cap, and no early
rejection point before Indy reads a request body into memory. See
[issue #531](https://github.com/michaelJustin/daraja-framework/issues/531)
for the full writeup. This demo covers the pieces of that finding that are
reachable purely from application code, via the `TdjHTTPConnector.HTTPServer`
property (the getting-started guide already shows this pattern for
`MaxConnections` and a thread pool):

| Concern | Mechanism used here | Effect |
|---|---|---|
| Unbounded connections | `HTTPServer.MaxConnections` | caps concurrent connections; excess connections are refused (and logged, per `TdjHTTPServer.DoMaxConnectionsExceeded`) |
| Listen backlog | `HTTPServer.ListenQueue` | caps pending (not-yet-accepted) connections at the OS socket level |
| Slowloris (a client that dribbles headers or a body, or sends nothing) | `HTTPServer.OnConnect` sets `AContext.Connection.IOHandler.ReadTimeout` per connection | Indy aborts the read and raises `EIdReadTimeout` once a connection goes idle past the timeout, freeing the worker thread instead of pinning it indefinitely |
| Unbounded request body | `HTTPServer.OnHeadersAvailable` checks `Content-Length` and sets `VContinueProcessing := False` if it's over the limit; `HTTPServer.OnHeadersBlocked` turns the rejection into a `413` | rejects an oversized declared body *before* Indy reads a single byte of it, instead of paying the memory/thread cost first (the "no early 413" problem in the P1 writeup) |
| Session growth window | `HTTPServer.SessionTimeOut` shortened | narrows the window a session (and its memory) stays alive; see caveat below |

## What this demo does *not* fix

Two P1 items genuinely need a library-level change, not just configuration,
and are called out here rather than silently left out:

- **Session growth is not capped.** With auto-sessions enabled, every
  request without a session cookie still creates a session
  (`TIdHTTPDefaultSessionList` has no size cap), so a client with no cookie
  jar can still create unbounded sessions for the shortened TTL. Shortening
  `SessionTimeOut` (done here) reduces the *window*, it does not close the
  hole — that needs a cap on the session list itself, or skipping session
  creation for requests that are going to 404 anyway. Tracked in #531.
- **A chunked-encoded body has no `Content-Length` header**, so
  `OnHeadersAvailable`'s check in this demo cannot catch it — a chunked
  request can still grow unbounded in memory. Catching that case needs a
  running-total check as the body streams in, which is not something the
  `OnHeadersAvailable` hook (fired once, before the body) can do.
- **Aggregate header count/bytes is still loose.** Indy caps a single
  header *line* at ~16 KB (`IdMaxLineLengthDefault`), but there's no
  built-in cap on the total number of headers or their combined size.

For a production deployment, also put a reverse proxy in front for TLS
termination, request-rate limiting, and a hard body-size ceiling — the
limits here reduce what a single Daraja process has to absorb on its own,
they don't replace that layer.

## Try it

```
curl http://127.0.0.1:8080/demo/echo
curl -X POST --data 'hello' http://127.0.0.1:8080/demo/echo          # 200
curl -X POST --data-binary @/some/2MB/file http://127.0.0.1:8080/demo/echo  # 413, before the body is read
```
