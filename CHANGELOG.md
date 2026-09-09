# Changelog

All notable changes to the Daraja HTTP Framework are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Releases are tagged `vMAJOR.MINOR.PATCH` and published at
<https://github.com/michaelJustin/daraja-framework/releases>.

## [Unreleased]

_Work tracked under the [3.2.0 milestone](https://github.com/michaelJustin/daraja-framework/milestone/17)._

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
- Version constant set to `3.2.0-SNAPSHOT`. (#451)

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

[Unreleased]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.2...HEAD
[3.1.2]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.1...v3.1.2
[3.1.1]: https://github.com/michaelJustin/daraja-framework/compare/v3.1.0...v3.1.1
[3.1.0]: https://github.com/michaelJustin/daraja-framework/compare/v3.0.6...v3.1.0
