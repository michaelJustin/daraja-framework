# Changelog

All notable changes to the Daraja HTTP Framework are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Releases are tagged `vMAJOR.MINOR.PATCH` and published at
<https://github.com/michaelJustin/daraja-framework/releases>.

## [Unreleased]

### Internal

- New test coverage: `TdjWebFilterChain.DoFilter`'s zero-filter, short-circuit
  (a filter that never calls `Next.DoFilter`) and mid-chain-exception paths,
  and `TdjWebFilterMapping.AppliesTo`'s URL-pattern matching. (#503)
- New test coverage: connector/server lifecycle — `TdjHTTPConnector`'s
  bind/activate on `Start`, deactivate on `Stop`, socket release on `Destroy`,
  double-start and stop-when-never-started as no-ops, and mutating `Port`
  while running leaving the live binding untouched; `TdjServerBase.Handle`'s
  guard on the wrapped handler's own started state. (#504)
- New test coverage: `TdjHandlerWrapper.GetSession`'s three branches —
  returning an existing session as-is, returning `nil` when absent and
  `Create` is `False`, and creating a new session via the HTTP server's
  `CreateSession` when absent and `Create` is `True`. (#505)
- New test coverage: the shared `TdjGenericHolder`/`TdjGenericWebComponent`/
  `TdjGenericWebFilter` base classes reused by every web-component and
  web-filter holder subclass — the `Init(Config)`/`Config` contract, and
  `TdjGenericWebComponent`'s own (previously untested) `GetSession`. (#506)
- New test coverage: `djGlobal.HTMLEncode`'s character escaping and
  `TdjInitParameters`'s case-sensitive key contract; audited
  `djPlatform`/`djHTTPConstants` and confirmed they're low-risk
  constant/wrapper units with no code of their own left to test. (#507)

## [3.3.0] - 2026-09-23

### Security

- `TdjWebComponentHandler.InvokeService`'s 500 response no longer discloses
  the component class name, the exception class name, or the exception
  message by default — the body is now generic
  (`500 Internal Server Error`) unless `DARAJA_PROJECT_STAGE_DEVELOPMENT` is
  defined, matching the existing gating of the stack-trace block. The full
  detail (component/exception class and message, plus stack trace when
  available) is now always logged server-side via the framework logger,
  regardless of what the client response discloses. (#520)
- `TdjHTTPServer.MyOnException` and `TdjHTTPConnector.OnCommand` now log a
  malformed request line (`EIdHTTPErrorParsingCommand`) and a client that
  dribbles or never finishes a request (`EIdReadTimeout`, e.g. Slowloris) at
  `Debug` instead of `Warn`/`Error`, since spraying malformed requests could
  otherwise inflate the log volume for free. Every other exception keeps its
  existing level. (#521)
- `TdjHTTPConnector.OnCommand` left `ResponseNo` at `-1` when it swallowed an
  exception from request handling, which depending on Indy's handling of an
  invalid status could send the client an empty or malformed response
  instead of a clean 500. It now sets `ResponseNo := 500` with the same
  generic, detail-free body as `TdjWebComponentHandler.InvokeService`'s
  default error page. (#517)
- The request target is now normalized once, centrally, before routing:
  `.`, `..` and repeated `/` are collapsed by path segment, and a
  Servlet-style `;`-parameter is stripped, before a filter's or component's
  URL pattern is matched against it. A target carrying an embedded NUL or
  other raw control byte is rejected with `400` instead of reaching
  routing. This also fixes a real bypass: `TdjWebComponentHandler.Handle`
  used to match a filter's prefix pattern against the raw,
  context-prefixed target while matching the guarded component against the
  context-stripped path, so a prefix-pattern filter (e.g. `/secure/*`)
  silently never applied on any non-root context. (#530)

### Added

- `TdjWebComponentHolder.LoadOnStartup`: an integer property, mirroring the
  Servlet spec's `load-on-startup` element, that controls the order in which
  Web Components are initialized when their context starts — lower values
  first, ties (including the default, 0) broken by registration order. (#492)
- A negative `LoadOnStartup` now defers initialization of a Web Component
  until the first request that matches it, instead of starting it eagerly at
  context start. Concurrent first requests are safe (only one triggers
  `Init`); if that `Init` raises, the triggering request gets a `500`
  response and the component is retried, not permanently unavailable, on the
  next request. (#496)
- `TdjWebComponent.OnGetETag`: an overridable method, alongside
  `OnGetLastModified`, that lets a Web Component supply an ETag for
  conditional GET/HEAD. When a request carries `If-None-Match`, it alone
  decides whether the response is `304 Not Modified`, per RFC 7232 Section
  3.3; `If-Modified-Since` is only consulted when the request has no
  `If-None-Match`. Matching is weak (a leading `W/` is ignored on either
  side), and `If-None-Match: *` matches any current representation. (#430)

### Changed

- `TdjWebComponent.OnOptions` now defaults to `200` with an `Allow` header
  listing the HTTP methods the component supports (i.e. the `On*` handlers it
  overrides, plus the implied `HEAD`), and no response body, instead of
  `405 Method Not Allowed`. A `405` response — whether from a not-overridden
  handler or set explicitly — now also carries the same `Allow` header. (#429)
- Replaced the single `EWebComponentException` with an exception hierarchy:
  `EDarajaException` is now the common base, with `EDarajaConfigException`,
  `EDarajaMappingException` and `EDarajaLifecycleException` for configuration,
  registration/mapping and lifecycle errors respectively, so callers can
  distinguish the cause of a failure. `EWebComponentException` is removed;
  code catching it should catch `EDarajaException` or one of the specific
  subclasses instead. (#435)

### Fixed

- `TdjLifeCycle.Start` could run `DoStart` twice for the same instance when
  two threads called `Start` concurrently before either had finished; it now
  re-checks the started state after acquiring its lock. (#496)
- A `TdjWebComponent.Init` exception during `TdjWebComponentHolder.DoStart`
  was swallowed, leaving the holder marked as started with a half-initialized
  component instead of stopped; it now propagates so the holder is correctly
  left not started. (#496)
- A `304 Not Modified` response from `TdjWebComponent`'s conditional GET/HEAD
  handling omitted the `Date` and `Last-Modified` headers that a `200`
  response to the same request would have carried; it now sends both (and
  `ETag`, if the component supplies one), per RFC 7232 Section 4.1. (#430)
- A failed `TdjLifeCycle.Start` left `FStarted` / `FStopped` at their
  pre-`Start` values without undoing whatever `DoStart` had partially started
  (e.g. connectors already bound before a later one failed), so `Destroy`'s
  `if IsStarted then Stop` never reached them and they leaked for the
  lifetime of the process. `Start` now best-effort rolls back via `DoStop` on
  a `DoStart` failure before re-raising the original exception. (#498)
- `TdjStatisticsFilter.DoFilter` dereferenced `Request.Session.Content`
  unconditionally, so a context without auto-sessions enabled (or a client
  with no session cookie) crashed with an access violation on every
  request. It now raises a clear `EDarajaConfigException` explaining that
  the filter's context must be created with sessions enabled, instead of
  AVing. (#516)

### Internal

- `HTTPTestCase` (test helper base class): extracted the URL-normalization
  logic duplicated across nearly every `Check*` method into `ResolveURL`,
  and the `hoNoProtocolErrorException` toggle duplicated across nine methods
  into `AllowErrorResponseCodes`. No behavior change. (#509)
- New test coverage: `TdjHandlerCollection`'s per-handler exception isolation
  on `Start`/`Handle`, and its start/stop-on-mutation behavior in
  `AddHandler`/`RemoveHandler`; `TdjContextHandler.ContextMatches`'
  path-prefix and connector-name whitelist matching. (#501)
- `TdjWebComponent` and `TdjWebFilter` doc comments now spell out the
  single-instance threading contract (one instance per registered
  component/filter, shared across all concurrent request threads — do not
  keep per-request state in instance fields); the getting-started guide's
  "Web Components and multi-threading" section was fixed (a broken code
  example) and expanded to match. (#513)
- `TdjPathMap.Matches`'s unreachable `stUnknown` branch now asserts (debug
  builds only, via `{$IFDEF DEBUG}`) instead of raising a bare `Exception`,
  so a future bug here would surface as a routing 404 rather than an
  uncaught exception turning into a 500. No behavior change: registration
  already rejects `stUnknown` patterns (#422), so the branch stays
  unreachable in practice. (#519)
- Version constant set to `3.3.0`. (#515)

## [3.2.0] - 2026-09-09

### Security

- `TdjDefaultWebComponent` rejects path traversal: `..`/`.` in the requested
  path are resolved and the result is verified to stay inside the static
  content directory, otherwise a 404 is returned. (#419)

### Added

- `TdjServer.RemoveConnector` removes a previously added connector (stopping it
  first if the server is running), and `TdjServer.GetConnector(Index)` returns
  the connector at a position so the connectors can be enumerated together with
  `ConnectorCount`. (#433)

### Changed

- `TdjServer.AddConnector`: adding a second connector for a `host:port` that is
  already registered now raises `EWebComponentException` with a clear message
  instead of letting a raw `EListError` escape, matching `TdjServer.Add`
  (context) and the web-filter name check. (#433)
- `TdjDefaultWebComponent` serves every static file the same way: it sets
  `Content-Disposition: inline` and hands the file to Indy's `SmartServeFile`,
  instead of special-casing `text/html` with a plain content stream. HTML
  responses therefore gain conditional GET (`304 Not Modified`),
  `Last-Modified` and the OS file-transfer fast path, and still render in the
  browser rather than being offered as a download. (#466)
- `TdjWebComponent.OnHead` is derived from the GET handler instead of always
  responding `405 Method Not Allowed`. A component which overrides `OnGet` now
  answers `HEAD` requests with the GET headers — including the `Content-Length`
  the GET would have produced and the `OnGetLastModified` conditional handling
  — and no body. A component which overrides neither still returns 405. Note
  that the GET handler runs in full for a `HEAD` request, so its cost and any
  side effects apply; override `OnHead` to handle `HEAD` separately. (#428)
- `TdjWebComponent.Service`: an unrecognised HTTP method now responds
  `501 Not Implemented` instead of falling through to 404. (#423)
- `TdjPathMap`: a prefix pattern `/foo/*` now also matches the bare path
  `/foo`, following Servlet path-mapping semantics. Suffix (`*.ext`) matching
  stays case-sensitive as the spec intends. (#427)
- `TdjWebComponentHandler.AddWebFilter` validates the URL pattern when the
  filter is registered, so an invalid pattern fails at registration time
  instead of raising during request handling. (#422)
- Registering a second web filter with a name that is already in use now
  raises `EWebComponentException` with a clear message instead of letting a
  raw `EListError` escape. (#421)
- The fallback 404 page from `TdjHandlerList` is now a well-formed minimal
  HTML document with a `text/html` content type and the HTML-encoded target
  (previously it had empty status text and no content type). (#431)
- **Breaking:** `GetInitParameterNames` on `IContext`, `IWebComponentConfig`,
  `IContextConfig` and `IWebFilterConfig` now returns a `TdjStringArray`
  (`array of string`, returned by value) instead of a `TList<string>` that the
  caller had to free. Callers replace `L := Config.GetInitParameterNames; try
  ... finally L.Free; end;` with a plain `for Name in Config.GetInitParameterNames
  do`. (#436)
- **Breaking:** `TdjLifeCycle.Started` and `TdjLifeCycle.Stopped` are now
  read-only properties (they were read/write, and writing one silently flipped
  the other). Use `Start` / `Stop` to change lifecycle state, or `IsStarted` /
  `IsStopped` to query it. The protected guards `CheckStarted` / `CheckStopped`
  are renamed `CheckNotStarted` / `CheckNotStopped` — they raise when the
  lifecycle is *already* in that state, and the old names read like the opposite
  assertion. (#438)
- The holder and mapping list types (`TdjWebComponentHolders`,
  `TdjWebFilterHolders`, `TdjWebComponentMappings`, `TdjWebFilterMappings`) now
  have a parameterless `Create` that applies the Delphi 2009 `TObjectList<T>`
  comparer workaround internally, so call sites no longer repeat
  `Create(TComparer<T>.Default)`. `TdjWebComponentMappings` and
  `TdjWebFilterMappings` are classes now, not bare aliases. (#466)
- `TdjHandlers` (`TList<IHandler>`) moved from `djInterfaces` to
  `djHandlerCollection`, its only user; `djInterfaces` no longer pulls in
  `Generics.Collections`. (#466)
- `TdjWebComponentHandler.GetFilterChain` moved from `strict private` to
  `protected`, joining `FindComponent` / `AddMapping` as documented extension
  points. (#465)

### Removed

- `IContext.Init` / `TdjContext.Init(const Config: IContextConfig)`. It was
  never called anywhere in the framework and carried a `TODO` questioning
  whether overwriting the config field was safe; the API docs implied a context
  lifecycle step that does not exist. (#426, #446)
- The never-completed "map a web filter to a named web component" machinery
  (`TdjMultiMap`, `TdjWebFilterMapping.WebComponentNames`, the name branch of
  the filter chain). It had no public API and had been dead since June 2024.
  This also removes the shared-object-ownership hazard behind #411. (#425, #411)
- `TdjServer.DoStart`: redundant nested `try..except`. (#440)
- `TdjContext.ValidateContextPath`: the two `Assert` lines; the character
  whitelist below already rejects `/` and `\` in every build. (#449)
- Dead code: broken `{$IFDEF LOG_CREATE}` `Trace` blocks in `TdjServerBase`,
  the empty `TdjContextHandlerCollection` constructor/destructor, and the
  commented-out deprecated methods in `TdjWebComponentContextHandler`. (#439)
- More dead code: the ~25 commented-out `// Trace(...)` lines calling a helper
  the framework no longer has, and in `TdjWebComponentHandler` the
  commented-out `CheckStoreContext` call, the "assign name if empty" block and
  the empty `{$IFDEF}` scaffolding around three `Logger.Warn` calls in the
  500-error handler. No behaviour change. (#463)

### Fixed

- `TdjWebComponentHandler` no longer double-owns web filter mappings:
  `FWebFilterPathMappings` is now an explicitly non-owning view of
  `FWebFilterMappings`. (#411)
- `TdjHandlerWrapper.RemoveHandler`: the directly wrapped handler can now
  actually be removed, and container delegation tests the wrapped handler
  rather than the argument. (#420)
- `TdjWebComponentHandler.AddWebFilter` leaves ownership of the holder with
  the caller on any failure (`WebFilters.Extract`); the holder is then freed
  by `TdjWebComponentContextHandler.AddWebFilter`, closing a leak. (#424)
- `TdjLifeCycle.Stop` now marks the component stopped even when the custom
  `DoStop` code raises (the exception is still logged and swallowed, as
  before), so a failed stop can no longer leave it reporting as started.
  `ILifeCycle.Stop` documents this swallow-and-still-stop behaviour. (#438)

### Documentation

- Init parameters are documented as following the Servlet model: names are
  case-sensitive, `GetInitParameterNames` returns them in an unspecified
  order, and a duplicate key raises. Stated on
  `TdjInitParameters`, the `IContext` / `I*Config` methods and the
  `SetInitParameter` / `Add` doc-comments. (#437)
- Documented the HEAD/OPTIONS/conditional-GET limitations on
  `TdjWebComponent`, the connector/context ownership rules on `TdjServer`,
  and the holder ownership contract. (#443, #434)
- Added a path-traversal / security note to the `TdjDefaultWebComponent`
  class doc-comment. (#444)
- Fixed doc-comment copy/paste errors (`Servive`, "Start the handler" on a
  `DoStop`). (#445)
- Added this `CHANGELOG.md`. (#441)
- The internal `IWriteableConfig` interface is excluded from the generated API
  documentation; it exists only for framework-internal casts. (#446)
- The config-before-init `TODO` comments in `TdjContext` and
  `TdjWebComponentHolder` are replaced by a note stating the invariant: the
  config is created in the constructor because `GetContextConfig` / `Add` run
  at configuration time, before the context is started. (#464)
- Stray `TODO` markers in `source/` resolved as comment-only changes: the
  `DoStop` "raise?" markers now state that the exception is swallowed on
  purpose (matching `TdjLifeCycle.Stop`, #438), and the four unresolved
  member-visibility musings in `djHandlerWrapper`, `djGenericWebComponent`,
  `djGenericWebFilter` and `djAbstractConfig` are dropped. (#466)
- Added a `README.md` to the demo projects that lacked one.

### Internal

- The FPC unit-test suite runs in CI on both Windows and Linux. A new
  `ConsoleCI` build mode carries `-dDARAJA_SKIP_SERVER_TESTS`, so the
  loopback-HTTP-server suites (`TSessionTests`, `TAPIConfigTests`) run via
  the local `run-fpc` / `run-delphi` scripts instead. (#452)
- New test coverage: `CheckUniqueName` rejecting a second holder under a name
  already in use, and `GetFilterChain` short-circuiting on an empty request
  path (#465); `Content-Disposition: inline` for HTML and non-HTML static
  files and a `304` on a conditional GET, with `CheckConditionalGETIs304` /
  `CheckGETResponseHeaderEquals` helpers in `HTTPTestCase` (#466).
- The Doxygen workflow derives `PROJECT_NUMBER` from `source/djGlobal.pas`
  and runs on every push to `master` and on version tags. (#456)
- Version constant set to `3.2.0`. (#451)

## [3.1.2] - 2026-09-08

### Fixed

- `IWebFilterConfig.GetFilterName` now returns the filter name instead of an
  empty string. `SetName` was added to `IWriteableConfig`; `TdjWebFilterHolder`
  passes its `Name` into the config before `Init`. (#415)
- `TdjWebFilterHolder` and `TdjWebComponentHolder` clear their instance field
  after freeing it (in `DoStop`, and on a failed filter init), removing a
  dangling-pointer / repeated-stop hazard. (#415)
- `TdjPathMap` establishes its sort order when URL patterns are added rather than
  as a side effect of every `GetMatches` lookup. (#415)
- `djNCSALogFilter`: correct timezone suffix for UTC offsets west of Greenwich
  (`DecodeTime` was called on a negative `TDateTime`). (#416)

### Changed

- `djStatisticsFilter.RequestsActive` widened to `Int64` to match the backing
  counter and the other accessors. (#417)

### Internal

- The FPC `Console` test build is compiled with heap tracing (`-gh`) and writes
  `heaptrace.log`; `UNIT-TESTS.md` documents checking it on every run. A leaked
  `TTestFilter` in `djWebFilterTests` was fixed. (#448)
- Version constant set to `3.1.2`. (#418)

## [3.1.1] - 2026-05-20

### Changed

- Use parametrized logging. (#406)
- Comment out deprecated methods. (#408)

## [3.1.0] - 2026-05-17

### Changed

- Requires slf4p 1.0.8.
- `TdjLoggerFactory.GetLogger` calls use a class reference. (#396)

[Unreleased]: https://github.com/michaelJustin/daraja-framework/compare/v3.3.0...HEAD
[3.3.0]: https://github.com/michaelJustin/daraja-framework/compare/v3.2.0...v3.3.0
[3.2.0]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.2...v3.2.0
[3.1.2]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.1...v3.1.2
[3.1.1]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.0...v3.1.1
[3.1.0]: https://github.com/michaelJustin/daraja-framework/compare/v3.0.6...v3.1.0
