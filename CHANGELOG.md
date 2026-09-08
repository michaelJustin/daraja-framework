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

### Changed

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

### Documentation

- Init parameters are documented as following the Servlet model: names are
  case-sensitive, `GetInitParameterNames` returns them in an unspecified
  order in a caller-owned list, and a duplicate key raises. Stated on
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

### Internal

- The FPC unit-test suite runs in CI on both Windows and Linux. A new
  `ConsoleCI` build mode carries `-dDARAJA_SKIP_SERVER_TESTS`, so the
  loopback-HTTP-server suites (`TSessionTests`, `TAPIConfigTests`) run via
  the local `run-fpc` / `run-delphi` scripts instead. (#452)
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
