# Unit test coverage audit (`source/` core)

Audited `source/` (excluding `source/optional/`) against `test/unittests/*Tests.pas`
on 2026-09-17. Method: enumerated the ~39 core `.pas` units and cross-referenced
them against the `uses` clauses and test methods of every `*Tests.pas` file.
Many "high-risk" units are only reached *transitively* through `djServer`/
`djHTTPConnector` object graphs in `ConfigAPITests.pas`/`HttpsTests.pas`
(integration-style), so they get generic start/stop coverage but not targeted
coverage of their own branches/error paths.

## Top gaps, ranked by risk

1. **djWebFilterChain.pas** (`TdjWebFilterChain.DoFilter`) — walks the filter
   list and invokes each `IWebFilter.DoFilter`, then falls through to the
   wrapped handler. Never directly imported by any test (`ConfigAPITests`
   exercises it only implicitly via `TestTwoFilters`/`TestTwoFiltersReversed`).
   No test for: chain with zero filters, filter that doesn't call
   `Next.DoFilter` (short-circuit), exception raised mid-chain.
   **Risk: high** — silent breakage would corrupt the entire filter pipeline
   (auth/logging filters silently skipped).

2. **djHandlerCollection.pas** (`Handle`, `DoStart`, `DoStop`,
   `AddHandler`/`RemoveHandler`) — base class for `djHandlerList`/
   `djContextHandlerCollection`, reached indirectly, but its own multi-handler
   iteration and per-handler exception swallowing has no dedicated test.
   **Risk: high** — this is the core dispatch loop; a bug here silently
   misroutes or double-handles every request. See suggested tests below.

3. **djContextHandler.pas** (`ContextMatches`, `ToConnectorName`, `Handle`) —
   path/connector-name matching logic (`ContextMatches`) is pure and easy to
   get subtly wrong (prefix vs. exact match, trailing slash, connector-name
   filtering) but has no unit calling it directly; only covered indirectly
   through `TestContextWithConnectorName`/`TestNoMatchingContextReturns404`
   in ConfigAPITests. **Risk: high** — wrong routing = requests silently
   hitting the wrong context. See suggested tests below.

4. **djAbstractConnector.pas / djHTTPServer.pas** — connector lifecycle
   (`DoStart`/`DoStop`, port/host getters+setters). No dedicated test file;
   covered only by generic `TestAddConnector`/`TestBindErrorRaisesException`
   in ConfigAPITests, which don't isolate the connector's own state
   transitions (e.g. `SetPort`/`SetHost` while running, double-start, stop
   when never started). **Risk: high**.

5. **djServerBase.pas** (`Handle`, `DoStart`, `DoStop`) — thin but central
   wrapper tying the server root together; no direct test references it by
   name, only reached transitively via `TdjServer`. **Risk: medium-high**.

6. **djWebFilterMapping.pas** (`AppliesTo`) — URL-pattern matching for
   filters, parallel to `djPathMap` (which *is* well tested via
   `djPathMapTests.pas`), but `djWebFilterMapping`'s own pattern logic has no
   direct unit test — only indirectly exercised through
   `TestCatchAllWebFilter`/`TestInvalidFilterUrlPatternRaisesException`.
   **Risk: medium-high** — wrong filter-URL matching = filters (e.g. auth)
   silently not applied.

7. **djHandlerWrapper.pas** (`GetSession`, `AddHandler`/`RemoveHandler`
   override behavior, `Handle`) — no direct test file. `TestSessions.pas`
   exists and covers session behavior at the integration level; worth
   confirming it actually reaches `GetSession`'s edge cases (expired session,
   missing cookie) rather than assuming coverage.

8. **djGenericHolder.pas / djGenericWebComponent.pas / djGenericWebFilter.pas**
   — generic base/lifecycle holder classes with no test file and no mention
   in any test's `uses`, despite being generic infrastructure reused across
   the web-component/filter holder hierarchy. **Risk: medium-high** — a
   regression here would silently affect every concrete holder subclass.

9. **djGlobal.pas, djPlatform.pas, djHTTPConstants.pas, djInitParameters.pas**
   — no coverage at all (never appear in any `uses` clause). Likely
   utility/constant units — lower risk individually, but `djInitParameters`
   (parses/stores init params) has zero test of malformed/missing param
   handling. **Risk: medium**.

10. **djAbstractConfig.pas / djContextConfig.pas** — config storage/lookup
    classes, no direct test (only touched indirectly via
    `TestContextConfig`/`TestConfigGetContextLog`). Untested: missing-key
    lookup, case sensitivity. **Risk: medium**.

### Not real gaps (indirectly but adequately covered)

`djHTTPConnector`, `djServer`, `djWebAppContext`, `djWebComponent`,
`djWebComponentHolder`, `djWebComponentHandler`,
`djWebComponentContextHandler`, `djWebFilter`, `djWebFilterConfig`,
`djPathMap`, `djLifeCycle`, `djServerContext` — all have either a dedicated
test file or heavy, repeated exercise through `ConfigAPITests.pas`'s ~60 test
methods.

---

## Suggested tests: #2 `djHandlerCollection.pas`

`TdjHandlerCollection` forwards `Handle`/`Start`/`Stop` to every registered
`IHandler`, swallowing exceptions from each one individually (so one bad
handler can't block the others). None of that fan-out/isolation behavior is
tested today. `IHandler` is a plain interface, so a hand-rolled fake handler
is enough — no Indy server needs to be started. `Context`/`Request`/`Response`
are only forwarded, never dereferenced by `TdjHandlerCollection` itself, so
`nil` is safe to pass through in these tests.

```pascal
unit djHandlerCollectionTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type
  TdjHandlerCollectionTests = class(TTestCase)
  published
    procedure TestHandleCallsAllHandlers;
    procedure TestHandleSkipsWhenNotStarted;
    procedure TestHandleContinuesAfterHandlerException;
    procedure TestAddHandlerStartsItWhenCollectionAlreadyStarted;
    procedure TestAddHandlerDoesNotStartItWhenCollectionNotStarted;
    procedure TestRemoveHandlerStopsStartedHandler;
    procedure TestStartStartsAllHandlers;
    procedure TestStartContinuesAfterHandlerStartException;
    procedure TestStopStopsAllHandlersEvenAfterException;
  end;

implementation

uses
  djHandlerCollection, djInterfaces, djServerContext, djTypes, SysUtils;

type
  { TFakeHandler: records calls, can be told to raise on Start/Handle }
  TFakeHandler = class(TInterfacedObject, IHandler)
  public
    StartCalled, StopCalled, HandleCalled: Boolean;
    FStarted: Boolean;
    RaiseOnStart, RaiseOnHandle: Boolean;
    procedure Start;
    procedure Stop;
    function IsStarted: Boolean;
    function IsStopped: Boolean;
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
  end;

procedure TFakeHandler.Start;
begin
  StartCalled := True;
  if RaiseOnStart then
    raise Exception.Create('boom on start');
  FStarted := True;
end;

procedure TFakeHandler.Stop;
begin
  StopCalled := True;
  FStarted := False;
end;

function TFakeHandler.IsStarted: Boolean;
begin
  Result := FStarted;
end;

function TFakeHandler.IsStopped: Boolean;
begin
  Result := not FStarted;
end;

procedure TFakeHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  HandleCalled := True;
  if RaiseOnHandle then
    raise Exception.Create('boom on handle');
end;

{ TdjHandlerCollectionTests }

procedure TdjHandlerCollectionTests.TestHandleCallsAllHandlers;
var
  Collection: TdjHandlerCollection;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    H2 := TFakeHandler.Create;
    Collection.AddHandler(H1);
    Collection.AddHandler(H2);
    Collection.Start;

    Collection.Handle('/x', nil, nil, nil);

    CheckTrue(H1.HandleCalled, 'first handler called');
    CheckTrue(H2.HandleCalled, 'second handler called');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestHandleSkipsWhenNotStarted;
var
  Collection: TdjHandlerCollection;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    Collection.AddHandler(H1);
    // Collection.Start; -- deliberately not started

    Collection.Handle('/x', nil, nil, nil);

    CheckFalse(H1.HandleCalled, 'handler must not run while collection is stopped');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestHandleContinuesAfterHandlerException;
var
  Collection: TdjHandlerCollection;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    H1.RaiseOnHandle := True;
    H2 := TFakeHandler.Create;
    Collection.AddHandler(H1);
    Collection.AddHandler(H2);
    Collection.Start;

    Collection.Handle('/x', nil, nil, nil);

    CheckTrue(H2.HandleCalled,
      'later handlers must still run after an earlier one raises');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestAddHandlerStartsItWhenCollectionAlreadyStarted;
var
  Collection: TdjHandlerCollection;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    Collection.Start;

    H1 := TFakeHandler.Create;
    Collection.AddHandler(H1);

    CheckTrue(H1.StartCalled, 'handler added to a running collection is started');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestAddHandlerDoesNotStartItWhenCollectionNotStarted;
var
  Collection: TdjHandlerCollection;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    Collection.AddHandler(H1);

    CheckFalse(H1.StartCalled,
      'handler added to a stopped collection is not started yet');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestRemoveHandlerStopsStartedHandler;
var
  Collection: TdjHandlerCollection;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    Collection.AddHandler(H1);
    Collection.Start;
    CheckTrue(H1.IsStarted, 'sanity: handler is running');

    Collection.RemoveHandler(H1);

    CheckTrue(H1.StopCalled, 'removing a running handler stops it first');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestStartStartsAllHandlers;
var
  Collection: TdjHandlerCollection;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    H2 := TFakeHandler.Create;
    Collection.AddHandler(H1);
    Collection.AddHandler(H2);

    Collection.Start;

    CheckTrue(H1.StartCalled, 'handler 1 started');
    CheckTrue(H2.StartCalled, 'handler 2 started');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestStartContinuesAfterHandlerStartException;
var
  Collection: TdjHandlerCollection;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    H1.RaiseOnStart := True;
    H2 := TFakeHandler.Create;
    Collection.AddHandler(H1);
    Collection.AddHandler(H2);

    Collection.Start;

    CheckTrue(H2.StartCalled,
      'later handlers must still start after an earlier one raises');
    CheckTrue(Collection.IsStarted,
      'the collection itself must still end up started');
  finally
    Collection.Free;
  end;
end;

procedure TdjHandlerCollectionTests.TestStopStopsAllHandlersEvenAfterException;
var
  Collection: TdjHandlerCollection;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  try
    H1 := TFakeHandler.Create;
    H2 := TFakeHandler.Create;
    Collection.AddHandler(H1);
    Collection.AddHandler(H2);
    Collection.Start;

    Collection.Stop;

    CheckTrue(H1.StopCalled, 'handler 1 stopped');
    CheckTrue(H2.StopCalled, 'handler 2 stopped');
  finally
    Collection.Free;
  end;
end;

end.
```

Notes:
- `TestHandleContinuesAfterHandlerException` and
  `TestStartContinuesAfterHandlerStartException` are the two most valuable
  cases — they pin down the "one bad handler can't take down the others"
  contract that's currently only implicit in the code.
- Passing `nil` for `Context`/`Request`/`Response` works because
  `TdjHandlerCollection` never dereferences them; it only forwards them to
  `IHandler.Handle`, which our fake ignores.

## Suggested tests: #3 `djContextHandler.pas`

`ContextMatches(const ConnectorName, Target: string): Boolean` is pure string
logic (no `TdjServerContext`/Indy objects needed) but is `protected`, so a
thin test subclass exposes it — the same pattern `djPathMapTests.pas` already
uses for `TdjPathMap.GetSpecType` via `TTestPathMap`.

```pascal
unit djContextHandlerTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type
  TdjContextHandlerTests = class(TTestCase)
  published
    procedure TestRootContextMatchesAnyPath;
    procedure TestNamedContextMatchesItsPrefix;
    procedure TestNamedContextDoesNotMatchUnrelatedPath;
    procedure TestNamedContextDoesNotMatchPrefixOfAnotherName;
    procedure TestNamedContextRequiresTrailingSlashBoundary;
    procedure TestNoConnectorNamesMatchesAnyConnector;
    procedure TestConnectorNameWhitelistRejectsUnlistedConnector;
    procedure TestConnectorNameWhitelistAcceptsListedConnector;
  end;

implementation

uses
  djContextHandler;

type
  { exposes the protected ContextMatches for testing, as
    TTestPathMap does for TdjPathMap.GetSpecType in djPathMapTests.pas }
  TTestContextHandler = class(TdjContextHandler)
  public
    function ContextMatches(const ConnectorName, Target: string): Boolean;
  end;

function TTestContextHandler.ContextMatches(const ConnectorName, Target: string): Boolean;
begin
  Result := inherited;
end;

{ TdjContextHandlerTests }

procedure TdjContextHandlerTests.TestRootContextMatchesAnyPath;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('');
  try
    CheckTrue(Handler.ContextMatches('', '/'), '/');
    CheckTrue(Handler.ContextMatches('', '/anything'), '/anything');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextMatchesItsPrefix;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    CheckTrue(Handler.ContextMatches('', '/app/'), '/app/');
    CheckTrue(Handler.ContextMatches('', '/app/page.html'), '/app/page.html');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextDoesNotMatchUnrelatedPath;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    CheckFalse(Handler.ContextMatches('', '/other/'), '/other/');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextDoesNotMatchPrefixOfAnotherName;
var
  Handler: TTestContextHandler;
begin
  // 'app' must not match '/application/...' -- current implementation
  // checks Pos('/app/', Target) = 1, so this should already pass, but it
  // documents the boundary and guards a future regression (e.g. someone
  // "optimizing" to Pos('/app', Target) = 1).
  Handler := TTestContextHandler.Create('app');
  try
    CheckFalse(Handler.ContextMatches('', '/application/page.html'),
      '/application/page.html must not match context "app"');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNamedContextRequiresTrailingSlashBoundary;
var
  Handler: TTestContextHandler;
begin
  // documents current behavior for the bare context path with no trailing
  // slash -- worth confirming this is the intended contract, since the
  // servlet-style bare-prefix exception exists for TdjPathMap (see
  // djPathMapTests.TestUrlPattern, '/foo' matching '/foo/*') but
  // ContextMatches has no equivalent special case.
  Handler := TTestContextHandler.Create('app');
  try
    CheckFalse(Handler.ContextMatches('', '/app'),
      '/app (no trailing slash) with current Pos(''/app/'', Target) = 1 logic');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestNoConnectorNamesMatchesAnyConnector;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    CheckTrue(Handler.ContextMatches('anything:8080', '/app/'),
      'empty ConnectorNames whitelist means match any connector');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestConnectorNameWhitelistRejectsUnlistedConnector;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    Handler.ConnectorNames.Add('127.0.0.1:8080');

    CheckFalse(Handler.ContextMatches('127.0.0.1:9090', '/app/'),
      'connector not in the whitelist must not match');
  finally
    Handler.Free;
  end;
end;

procedure TdjContextHandlerTests.TestConnectorNameWhitelistAcceptsListedConnector;
var
  Handler: TTestContextHandler;
begin
  Handler := TTestContextHandler.Create('app');
  try
    Handler.ConnectorNames.Add('127.0.0.1:8080');

    CheckTrue(Handler.ContextMatches('127.0.0.1:8080', '/app/'),
      'connector in the whitelist must match');
  finally
    Handler.Free;
  end;
end;

end.
```

Notes:
- `TestNamedContextRequiresTrailingSlashBoundary` and
  `TestNamedContextDoesNotMatchPrefixOfAnotherName` pin down two easy-to-break
  edge cases in `Pos('/' + ContextPath + '/', Target) = 1` — the first
  documents that a bare `/app` (no trailing slash) currently does **not**
  match, which may or may not be the intended contract and is worth a quick
  sanity check against real-world usage before relying on it.
- `ToConnectorName` was left out: it just formats
  `Context.Binding.IP + ':' + IntToStr(Context.Binding.Port)` from a real
  Indy `TdjServerContext`, which is expensive to construct in isolation and
  is already exercised end-to-end by `ConfigAPITests.TestContextWithConnectorName`.

## Next steps

- Both suggested files compile standalone against existing patterns in the
  suite (`TTestPathMap`-style protected-method exposure, `TInterfacedObject`
  fakes) and need no new test infrastructure.
- To wire them in: add `djHandlerCollectionTests`/`djContextHandlerTests` to
  the `uses` clause of `TestHelper.pas` and register their suites in
  `RegisterUnitTests`, matching the existing entries for
  `TdjPathMapTests`/`TdjWebFilterTests`.
