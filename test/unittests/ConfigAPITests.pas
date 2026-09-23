(*

    Daraja HTTP Framework
    Copyright (c) 2016 Michael Justin

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.


    You can be released from the requirements of the license by purchasing
    a commercial license. Buying such a license is mandatory as soon as you
    develop commercial activities involving the Daraja framework without
    disclosing the source code of your own applications. These activities
    include: offering paid services to customers as an ASP, shipping Daraja
    with a closed source product.

*)

unit ConfigAPITests;

interface

{$I IdCompilerDefines.inc}

uses
  HTTPTestCase,
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type
  { TAPIConfigTests }

  TAPIConfigTests = class(THTTPTestCase)
  private

  published
    procedure ConfigOneContext;

    procedure AddContextToServer;

    // Multiple contexts may have the same context path and they are
    // called in order until one handles the request.
    procedure AddTwoContextWithSameNameRaisesException;

    procedure ConfigTwoContexts;

    // context
    procedure StopContext;
    procedure StopStartContext;
    procedure TestWebAppContextMemoryLeaks;
    procedure TestWebAppContextMemoryLeaksWithWebComponent;

    //
    procedure ConfigAbsolutePath;

    // init method
    procedure TestInitCanReadWebComponentContext;
    procedure TestInitv3CanReadWebComponentContext;

    // exceptions
    procedure TestExceptionInInitStopsComponent;
    procedure TestExceptionInServiceReturns500;

    // ErrorHandler (issue #528)
    procedure TestErrorHandlerReplacesDefault500Page;
    procedure TestErrorHandlerRaisingFallsBackToDefault500Page;
    procedure TestFilterExceptionReachesErrorHandler;
    procedure TestErrorHandlerSetBeforeStartIsStartedAndStopped;

    // lazy (negative LoadOnStartup) load-on-startup (issue #496)
    procedure TestNegativeLoadOnStartupNotStartedAtServerStart;
    procedure TestNegativeLoadOnStartupStartsOnFirstRequest;
    procedure TestConcurrentFirstRequestsInitLazyComponentOnce;
    procedure TestFailingLazyInitReturns500ThenRetries;

    // context match
    procedure TestNoMatchingContextReturns404;

    // default handler
    procedure TestDefaultHandler;
    procedure TestDefaultHandlerInContext;

    // web component tests
    procedure TestNoMethodReturns405;
    procedure TestPOSTMethodResponse;
    procedure TestRegisterTwoMappings;

    // Web Component init parameter
    procedure TestTdjWebComponentHolder_SetInitParameter;
    procedure TestConfigureWebComponentByWebComponentHolder;

    procedure TestContextConfig;

    procedure TestConfigGetContextLog;

    // Test character encoding (UTF-8)
    procedure TestCharSet;
    procedure TestContentType;

    procedure TestContextWithConnectorName;

    procedure TestIPv6ConnectionToLoopback;

    procedure TestAddConnector;
    procedure TestAddDuplicateConnectorRaisesException;
    procedure TestRemoveConnector;
    procedure TestRemoveUnknownConnectorRaisesException;
    procedure TestEnumerateConnectors;
    procedure TestThreadPool;

    procedure TestBindErrorRaisesException;
    procedure TestFailedStartStopsPartiallyStartedConnectors;

    // test overriding the TdjWebComponent.OnGetLastModified method
    // (since 1.2.10)
    procedure TestCachedGetRequest;

    // test that HEAD is derived from the GET handler (issue #428)
    procedure TestHeadRequestUsesGetHandler;
    procedure TestHeadRequestWithoutGetHandlerReturns405;
    procedure TestCachedHeadRequest;

    // test overriding the TdjWebComponent.OnGetETag method (issue #430)
    procedure TestETagResponseHeaderIsSent;
    procedure TestETagConditionalGetIs304WhenETagMatches;
    procedure TestETagConditionalGetIsFreshWhenETagDiffers;
    procedure TestIfNoneMatchWildcardIs304;
    procedure TestIfNoneMatchTakesPrecedenceOverIfModifiedSince;
    procedure TestNotModifiedResponseIncludesDateAndLastModified;

    // test that OPTIONS and 405 responses carry an Allow header (issue #429)
    procedure TestOptionsRequestListsOverriddenMethods;
    procedure TestOptionsRequestWithoutOverridesListsOptionsOnly;
    procedure TestMethodNotAllowedResponseIncludesAllowHeader;

    procedure TestOnlyAFilter;
    procedure TestFilter;
    procedure TestPrefixFilterAppliesInNonRootContext;
    procedure TestPrefixFilterAppliesAfterPathNormalization;
    procedure TestTwoFilters;
    procedure TestTwoFiltersReversed;
    procedure TestTwoFiltersAndTwoWebComponents;
    procedure TestFilterWithInit;
    procedure TestFilterV3WithInit;
    procedure TestFilterInitCanReadContextConfiguration;
    procedure TestFilterInitCanReadFilterName;
    //procedure TestOneFilterAndTwoWebComponents;

    procedure TestMapFilterTwiceToSameWebComponentRaisesException;
    procedure TestInvalidFilterUrlPatternRaisesException;
    //procedure TestMapFilterWithUnknownComponentNameRaisesException;
    //procedure TestWebFilterHolderInit;
    //procedure TestWebFilterHolderInitHavingTwoInstances;
    procedure TestCatchAllWebFilter;
    procedure TestExceptionInComponentInitWithWebFilter;
    procedure TestExceptionInComponentServiceWithWebFilter;
    procedure TestExceptionInComponentOnGetWithWebFilter;
    procedure TestWebFilterDestroyFilter;

  end;

implementation

uses
  djWebAppContext, djInterfaces, djWebComponent, djWebComponentHolder,
  djWebComponentContextHandler, djServer, djDefaultHandler,
  djHTTPConnector, djServerInterfaces, djContextHandlerCollection, djHandlerList, djTypes,
  djAbstractHandler, djServerContext, djWebFilter, djWebFilterHolder,
  djWebFilterConfig,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdServerInterceptLogFile, IdSchedulerOfThreadPool, IdGlobal, IdException,
  IdResourceStrings, IdHTTP,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils, Classes, SyncObjs;

type
  EUnitTestException = class(Exception);

{ TAPIConfigTests }

// this web component returns '' as HTTP GET response ------------------------

type
  TExamplePage = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TExamplePage }

procedure TExamplePage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'example';
end;

procedure TAPIConfigTests.TestWebAppContextMemoryLeaks;
var
  Context: TdjWebAppContext;
begin
  Context := TdjWebAppContext.Create('web');
  Context.Free;
end;

procedure TAPIConfigTests.TestWebAppContextMemoryLeaksWithWebComponent;
var
  Context: TdjWebAppContext;
begin
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '/example');
  Context.Free;
end;

procedure TAPIConfigTests.ConfigAbsolutePath;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('web');
    Context.AddWebComponent(TExamplePage, '/hello.html');

    Server.Add(Context);
    Server.Start;

    // Test the correct path
    CheckGETResponseEquals('example', '/web/hello.html');

    // Test non-existent path
    CheckGETResponse404('/web/bar');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestRegisterTwoMappings;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('example');
    Context.Add(TExamplePage, '/index.html');
    Context.Add(TExamplePage, '*.txt');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example', '/example/index.html');
    CheckGETResponseEquals('example', '/example/test.txt');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestDefaultHandler;
var
  Server: TdjServer;
  HandlerList: IHandlerContainer;
begin
  Server := TdjServer.Create;
  try
    HandlerList := TdjHandlerList.Create;
    HandlerList.AddHandler(TdjDefaultHandler.Create);
    Server.Handler := HandlerList;
    Server.Start;

    CheckGETResponseContains('Daraja Framework');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestDefaultHandlerInContext;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  HandlerList: IHandlerContainer;
  DefaultHandler: IHandler;
begin
  Server := TdjServer.Create;
  try
    // create the 'test' context
    Context := TdjWebAppContext.Create('test');
    Context.Add(TExamplePage, '/example');
    Server.Add(Context);
    // add a handlerlist with a TdjDefaultHandler
    DefaultHandler := TdjDefaultHandler.Create;
    HandlerList := TdjHandlerList.Create;
    HandlerList.AddHandler(DefaultHandler);
    Server.AddHandler(HandlerList);
    Server.Start;

    CheckGETResponseContains('Daraja Framework');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.AddTwoContextWithSameNameRaisesException;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('foo');
    Context.Add(TExamplePage, '/bar');
    Server.Add(Context);

    Context := TdjWebAppContext.Create('foo');
    Context.Add(TExamplePage, '/bar2');

    {$IFDEF FPC}
    ExpectException(EDarajaMappingException, 'Context path "foo" is already registered.');
    {$ELSE}
    ExpectedException := EDarajaMappingException;
    {$ENDIF}

    Server.Add(Context);


  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.ConfigOneContext;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Context := TdjWebAppContext.Create('foo');
  Context.Add(TExamplePage, '/bar');

  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example', '/foo/bar');
    CheckGETResponse404('/foo2/bar');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.AddContextToServer;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('foo');
    Context.Add(TExamplePage, '/bar');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example', '/foo/bar');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestIPv6ConnectionToLoopback;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Server.AddConnector('::1');
    Context := TdjWebAppContext.Create('example');
    Context.Add(TExamplePage, '/index.html');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example', 'http://[::1]:8080/example/index.html');

  finally
    Server.Free;
  end;
end;

// TCmpWithInitV3 -----------------------------------------------------
type
  TCmpWithInitv3 = class(TdjWebComponent)
  strict private
    StaticContent: string;
  public
    procedure Init; override;
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TCmpWithInitv3.Init;
begin
  StaticContent := 'from init';

  if Config <> nil then StaticContent := StaticContent + ' 1';
  if Config.GetContext <> nil then StaticContent := StaticContent + ' 2';
  if Config.GetContext.GetContextConfig <> nil then StaticContent := StaticContent + ' 3';
end;

procedure TCmpWithInitv3.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := StaticContent;
end;

procedure TAPIConfigTests.TestInitv3CanReadWebComponentContext;
var
  Context: TdjWebAppContext;
  Server: TdjServer;
begin
  Context := TdjWebAppContext.Create('');
  Context.Add(TCmpWithInitv3, '/');
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;
    CheckGETResponseEquals('from init 1 2 3', '/');
  finally
    Server.Free;
  end;
end;

// TCmpWithInit -----------------------------------------------------
type
  TCmpWithInit = class(TdjWebComponent)
  strict private
    StaticContent: string;
  public
    procedure Init(const Config: IWebComponentConfig); override;
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TCmpWithInit.Init(const Config: IWebComponentConfig);
begin
  inherited Init(Config);

  StaticContent := 'from init';

  if Config <> nil then StaticContent := StaticContent + ' 1';
  if Config.GetContext <> nil then StaticContent := StaticContent + ' 2';
  if Config.GetContext.GetContextConfig <> nil then StaticContent := StaticContent + ' 3';
end;

procedure TCmpWithInit.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := StaticContent;
end;

procedure TAPIConfigTests.TestInitCanReadWebComponentContext;
var
  Context: TdjWebAppContext;
  Server: TdjServer;
begin
  Context := TdjWebAppContext.Create('');
  Context.Add(TCmpWithInit, '/');
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;
    CheckGETResponseEquals('from init 1 2 3', '/');
  finally
    Server.Free;
  end;
end;

// TCmpReturnsInitParams -----------------------------------------------------
type
  TCmpReturnsInitParams = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TCmpReturnsInitParams.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := GetWebComponentConfig.GetInitParameter('test');
end;

procedure TAPIConfigTests.TestTdjWebComponentHolder_SetInitParameter;
var
  Server: TdjServer;
  Holder: TdjWebComponentHolder;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('context');
    Holder := Context.AddWebComponent(TCmpReturnsInitParams, '/*');
    Holder.SetInitParameter('test', 'success');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('success', '/context/123');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestConfigureWebComponentByWebComponentHolder;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('context');
    with Context.Add(TCmpReturnsInitParams, '/*') do
    begin
      SetInitParameter('test', 'success');
    end;
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('success', '/context/');

  finally
    Server.Free;
  end;
end;

// TestWrapper ---------------------------------------------------------------

type
  THelloHandler = class(TdjAbstractHandler)
  public
    procedure Handle(const {%H-}Target: string; {%H-}Context: TdjServerContext;
      {%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

{ THelloHandler }

procedure THelloHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'Hello world!';
  Response.ResponseNo := 200;
end;

type
  TNoMethodComponent = class(TdjWebComponent)
  end;

procedure TAPIConfigTests.TestBindErrorRaisesException;
var
  Server1: TdjServer;
  Server2: TdjServer;
  Context: TdjWebAppContext;
begin
  Server1 := TdjServer.Create;
  try
    Server1.Start;
    Server2 := TdjServer.Create;
    try
      Context := TdjWebAppContext.Create('get');
      Context.Add(TNoMethodComponent, '/hello');
      Server2.Add(Context);

      try
        Server2.Start;
      except
        on E: EIdCouldNotBindSocket do
          CheckEquals(RSCouldNotBindSocket, E.Message);
        on E: Exception do
          Fail(E.Message);
      end;

    finally
      Server2.Free;
    end;
  finally
    Server1.Free;
  end;
end;

// https://github.com/michaelJustin/daraja-framework/issues/498 -------------
// a failed Start must roll back whatever DoStart managed to start before
// failing, so nothing is left running (and leaked) behind a server whose
// IsStarted is False.
procedure TAPIConfigTests.TestFailedStartStopsPartiallyStartedConnectors;
var
  Server: TdjServer;
  Connector1: IConnector;
  ProbeServer: TdjServer;
begin
  Server := TdjServer.Create;
  try
    Server.AddConnector('127.0.0.1', 8080);
    Server.AddConnector('127.0.0.1', 8181);

    Connector1 := Server.GetConnector(0);

    // Make connector 2 collide with connector 1's port only now, i.e. after
    // both were registered without tripping AddConnector's duplicate-name
    // check. StartConnectors then binds connector 1 successfully and fails
    // to bind connector 2, which is the scenario from the issue.
    Server.GetConnector(1).Port := 8080;

    try
      Server.Start;
      Fail('Expected Start to raise when the second connector fails to bind');
    except
      on E: Exception do
        ; // expected: DoStart fails while starting connector 2
    end;

    CheckFalse(Server.IsStarted,
      'Server must not be marked as started after a failed Start');
    CheckFalse(Connector1.IsStarted,
      'Connector 1 must be rolled back (stopped) after the failed Start, ' +
      'not left running/leaked (issue #498)');

    // Prove there is no leak: a fresh server can now bind the same port.
    ProbeServer := TdjServer.Create('127.0.0.1', 8080);
    try
      ProbeServer.Start;
      ProbeServer.Stop;
    finally
      ProbeServer.Free;
    end;
  finally
    Server.Free;
  end;
end;

// this web component raises an exception in the Init method -----------------
type
  TExceptionInInitComponent = class(TdjWebComponent)
  public
    procedure Init; override;
  end;

{ TExceptionInInitComponent }

procedure TExceptionInInitComponent.Init;
begin
  raise EUnitTestException.Create('error');
end;

procedure TAPIConfigTests.TestExceptionInInitStopsComponent;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx');
    Context.AddWebComponent(TExceptionInInitComponent, '/exception');
    Server.Add(Context);
    Server.Start;

    // The exception in Init() must not leave the component wedged in a
    // half-started state; the request that triggered it gets a 500.
    CheckGETResponse500('/ctx/exception');

  finally
    Server.Free;
  end;
end;

// lazy (negative LoadOnStartup) load-on-startup: issue #496 ------------------

type
  TLazyPage = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TLazyPage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'lazy';
end;

procedure TAPIConfigTests.TestNegativeLoadOnStartupNotStartedAtServerStart;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  Holder: TdjWebComponentHolder;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx');
    Holder := Context.AddWebComponent(TLazyPage, '/lazy');
    Holder.LoadOnStartup := -1;
    Server.Add(Context);
    Server.Start;

    CheckFalse(Holder.IsStarted,
      'a component with a negative LoadOnStartup must not be started at server start');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestNegativeLoadOnStartupStartsOnFirstRequest;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  Holder: TdjWebComponentHolder;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx');
    Holder := Context.AddWebComponent(TLazyPage, '/lazy');
    Holder.LoadOnStartup := -1;
    Server.Add(Context);
    Server.Start;

    CheckFalse(Holder.IsStarted);

    CheckGETResponseEquals('lazy', '/ctx/lazy');

    CheckTrue(Holder.IsStarted,
      'the first matching request must Init the component on demand');
  finally
    Server.Free;
  end;
end;

// a thread that performs a single GET request against a fixed URL, using its
// own TIdHTTP instance so several of these can safely race each other
type
  TGetRequestThread = class(TThread)
  private
    FURL: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const URL: string);
  end;

constructor TGetRequestThread.Create(const URL: string);
begin
  inherited Create(False);
  FURL := URL;
  FreeOnTerminate := False;
end;

procedure TGetRequestThread.Execute;
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create;
  try
    try
      HTTP.Get(FURL);
    except
      // ignored: this test only cares how many times Init actually ran
    end;
  finally
    HTTP.Free;
  end;
end;

var
  // guards ConcurrentInitCount, incremented from TConcurrentLazyPage.Init,
  // which may run concurrently on the server's worker threads
  ConcurrentInitCS: TCriticalSection;
  ConcurrentInitCount: Integer;

type
  TConcurrentLazyPage = class(TdjWebComponent)
  public
    procedure Init; override;
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TConcurrentLazyPage.Init;
begin
  inherited;

  // widen the race window so concurrent first requests actually overlap
  Sleep(50);

  ConcurrentInitCS.Enter;
  try
    Inc(ConcurrentInitCount);
  finally
    ConcurrentInitCS.Leave;
  end;
end;

procedure TConcurrentLazyPage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'ok';
end;

procedure TAPIConfigTests.TestConcurrentFirstRequestsInitLazyComponentOnce;
const
  ThreadCount = 10;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  Holder: TdjWebComponentHolder;
  Threads: array[0..ThreadCount - 1] of TGetRequestThread;
  I: Integer;
begin
  ConcurrentInitCount := 0;
  ConcurrentInitCS := TCriticalSection.Create;
  try
    Server := TdjServer.Create;
    try
      Context := TdjWebAppContext.Create('ctx');
      Holder := Context.AddWebComponent(TConcurrentLazyPage, '/lazy');
      Holder.LoadOnStartup := -1;
      Server.Add(Context);
      Server.Start;

      for I := 0 to ThreadCount - 1 do
      begin
        Threads[I] := TGetRequestThread.Create('http://127.0.0.1:8080/ctx/lazy');
      end;

      for I := 0 to ThreadCount - 1 do
      begin
        Threads[I].WaitFor;
        Threads[I].Free;
      end;

      CheckEquals(1, ConcurrentInitCount,
        'concurrent first requests must construct/Init the lazy component exactly once');
    finally
      Server.Free;
    end;
  finally
    ConcurrentInitCS.Free;
  end;
end;

var
  // reset at the start of TestFailingLazyInitReturns500ThenRetries
  FailingLazyInitAttempts: Integer;

type
  TFailingLazyPage = class(TdjWebComponent)
  public
    procedure Init; override;
    procedure OnGet({%H-}Request: TdjRequest; {%H-}Response: TdjResponse); override;
  end;

procedure TFailingLazyPage.Init;
begin
  inherited;

  Inc(FailingLazyInitAttempts);
  raise EUnitTestException.Create('lazy init failure');
end;

procedure TFailingLazyPage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  // unreachable: Init always raises
end;

procedure TAPIConfigTests.TestFailingLazyInitReturns500ThenRetries;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  Holder: TdjWebComponentHolder;
begin
  FailingLazyInitAttempts := 0;
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx');
    Holder := Context.AddWebComponent(TFailingLazyPage, '/lazy');
    Holder.LoadOnStartup := -1;
    Server.Add(Context);
    Server.Start;

    CheckGETResponse500('/ctx/lazy');
    CheckFalse(Holder.IsStarted,
      'a failed lazy Init must not leave the holder half-started');
    CheckEquals(1, FailingLazyInitAttempts);

    // the next request must retry Init from scratch, not stay permanently
    // wedged from the first failure
    CheckGETResponse500('/ctx/lazy');
    CheckEquals(2, FailingLazyInitAttempts);
  finally
    Server.Free;
  end;
end;

// test exception in service -------------------------------------------------
type
  TExceptionComponent = class(TdjWebComponent)
  public
    procedure Service({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse); override;
  end;

{ TExceptionComponent }

procedure TExceptionComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  raise EUnitTestException.Create('test');
end;

procedure TAPIConfigTests.TestExceptionInServiceReturns500;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx');
    Context.AddWebComponent(TExceptionComponent, '/exception');
    Server.Add(Context);
    Server.Start;

    CheckGETResponse500('/ctx/exception');

  finally
    Server.Free;
  end;
end;

// test ErrorHandler (issue #528) ---------------------------------------------
type
  { TTestErrorHandler }
  TTestErrorHandler = class(TdjAbstractHandler)
  public
    RaiseInsteadOfHandling: Boolean;
  protected
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  end;

  { TLifecycleTrackingHandler }
  TLifecycleTrackingHandler = class(TdjAbstractHandler)
  public
    WasStarted, WasStopped: Boolean;
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure Handle({%H-}const {%H-}Target: string; {%H-}Context: TdjServerContext;
      {%H-}Request: TdjRequest; {%H-}Response: TdjResponse); override;
  end;

  { TExceptionFilter }
  TExceptionFilter = class(TdjWebFilter)
  public
    procedure DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

  { TOKComponent: a component that never raises, used to isolate a filter's
    own exception (TestFilterExceptionReachesErrorHandler) from a component
    exception. }
  TOKComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TTestErrorHandler }

procedure TTestErrorHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  if RaiseInsteadOfHandling then
  begin
    raise Exception.Create('ErrorHandler itself failed');
  end;

  Response.ContentType := 'text/plain';
  Response.ContentText := 'custom error page: ' + Context.LastErrorExceptionClass
    + ' / ' + Context.LastErrorExceptionMessage;
end;

{ TLifecycleTrackingHandler }

procedure TLifecycleTrackingHandler.DoStart;
begin
  inherited;
  WasStarted := True;
end;

procedure TLifecycleTrackingHandler.DoStop;
begin
  WasStopped := True;
  inherited;
end;

procedure TLifecycleTrackingHandler.Handle(const Target: string;
  Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse);
begin
  //
end;

{ TExceptionFilter }

procedure TExceptionFilter.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  raise Exception.Create('filter boom');
end;

{ TOKComponent }

procedure TOKComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'ok';
end;

procedure TAPIConfigTests.TestErrorHandlerReplacesDefault500Page;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx-eh');
    Context.AddWebComponent(TExceptionComponent, '/exception');
    Context.ErrorHandler := TTestErrorHandler.Create;
    Server.Add(Context);
    Server.Start;

    // the custom page, not the framework's generic 500 body, and with the
    // failing exception's class/message populated on the context beforehand
    CheckGETResponse500ContentEquals('custom error page: EUnitTestException / test',
      '/ctx-eh/exception');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestErrorHandlerRaisingFallsBackToDefault500Page;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  Handler: TTestErrorHandler;
begin
  Handler := TTestErrorHandler.Create;
  Handler.RaiseInsteadOfHandling := True;

  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx-eh-raises');
    Context.AddWebComponent(TExceptionComponent, '/exception');
    Context.ErrorHandler := Handler;
    Server.Add(Context);
    Server.Start;

    // ErrorHandler itself raised; the request must still get a definite
    // response (the framework's default), not crash or hang
    CheckGETResponse500('/ctx-eh-raises/exception');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestFilterExceptionReachesErrorHandler;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('ctx-eh-filter');
    Context.Add(TOKComponent, '*.html');
    Context.Add(TExceptionFilter, '/*');
    Context.ErrorHandler := TTestErrorHandler.Create;
    Server.Add(Context);
    Server.Start;

    // the component itself never raises -- only the filter does -- so this
    // confirms ErrorHandler covers filter exceptions too, not just a Web
    // Component's Service method
    CheckGETResponse500ContentEquals('custom error page: Exception / filter boom',
      '/ctx-eh-filter/page.html');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestErrorHandlerSetBeforeStartIsStartedAndStopped;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  Handler: TLifecycleTrackingHandler;
begin
  Handler := TLifecycleTrackingHandler.Create;

  Context := TdjWebAppContext.Create('ctx-eh-lifecycle');
  // set before Start -- the normal configure-then-start order. SetErrorHandler
  // only starts it immediately if the context is already running, so this is
  // exactly the case the DoStart/DoStop fix covers.
  Context.ErrorHandler := Handler;

  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckTrue(Handler.WasStarted,
      'ErrorHandler set before Start must still be started with its context');

    Server.Stop;

    CheckTrue(Handler.WasStopped,
      'ErrorHandler must be stopped when its context stops');
  finally
    Server.Free;
  end;
end;

// ---
type
  TGetComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse);  override;
  end;

{ TGetComponent }

procedure TGetComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'Hello';
end;

// ---
type
  TCachedGetComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse);  override;
    function OnGetLastModified({%H-}Request: TdjRequest): TDateTime; override;
  end;

{ TCachedGetComponent }

procedure TCachedGetComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'CachedGET';
end;

function TCachedGetComponent.OnGetLastModified(Request: TdjRequest): TDateTime;
begin
  Result := Now;
end;

// ---
type
  TETagComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse);  override;
    function OnGetETag({%H-}Request: TdjRequest): string; override;
  end;

{ TETagComponent }

procedure TETagComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'ETagGET';
end;

function TETagComponent.OnGetETag(Request: TdjRequest): string;
begin
  Result := '"fixed-etag-value"';
end;

// a component whose Last-Modified and ETag disagree, so tests can tell
// which one the framework actually used to decide a 304 (issue #430)
type
  TCachedGetWithETagComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse);  override;
    function OnGetLastModified({%H-}Request: TdjRequest): TDateTime; override;
    function OnGetETag({%H-}Request: TdjRequest): string; override;
  end;

{ TCachedGetWithETagComponent }

procedure TCachedGetWithETagComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'CachedGETWithETag';
end;

function TCachedGetWithETagComponent.OnGetLastModified(Request: TdjRequest): TDateTime;
begin
  // always "fresh" by date, so a 304 can only come from If-None-Match
  Result := Date - 1;
end;

function TCachedGetWithETagComponent.OnGetETag(Request: TdjRequest): string;
begin
  Result := '"fixed-etag-value"';
end;

procedure TAPIConfigTests.TestNoMatchingContextReturns404;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('example');
    Context.Add(TExamplePage, '*.html');
    Server.Add(Context);
    Server.Start;

    CheckGETResponse404('/example2/a.html');
    CheckGETResponse404('/Example/b.html');
    CheckGETResponse200('/example/c.html');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestNoMethodReturns405;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TNoMethodComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckGETResponse405('/get/hello')

  finally
    Server.Free;
  end;
end;

// this web component declares a POST handler --------------------------------

type
  TPostComponent = class(TdjWebComponent)
  public
    procedure OnPost({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TPostComponent }

procedure TPostComponent.OnPost(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'thank you for POSTing';
end;

procedure TAPIConfigTests.TestPOSTMethodResponse;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('example');
    Context.Add(TPostComponent, '/index.html');
    Server.Add(Context);
    Server.Start;

    CheckPOSTResponseEquals('thank you for POSTing', '/example/index.html');
    CheckGETResponse405('/example/index.html');
  finally
    Server.Free;
  end;
end;

// Service method is overriden -----------------------------------------------

type
  THello2WebComponent = class(TdjWebComponent)
  public
    procedure Service({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      Response: TdjResponse); override;
  end;

{ THello2WebComponent }

procedure THello2WebComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'Hello universe!';
end;

procedure TAPIConfigTests.ConfigTwoContexts;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create and register component 1
    Context := TdjWebAppContext.Create('foo');
    Context.Add(TExamplePage, '/bar');
    Server.Add(Context);
    // create and register component 2
    Context := TdjWebAppContext.Create('foo2');
    Context.Add(THello2WebComponent, '/bar2');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example', '/foo/bar');
    CheckGETResponseEquals('Hello universe!', '/foo2/bar2');
    CheckGETResponse404('/foo2/bar');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.StopContext;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('');
    Server.Add(Context);
    Server.Start;
    Context.Stop;
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.StopStartContext;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('');
    Server.Add(Context);
    Server.Start;
    Context.Stop;
    Context.Start;
  finally
    Server.Free;
  end;
end;

// this web component writes to the context log ----------
type
  TLogComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TLogComponent }

procedure TLogComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
var
  Value: string;
begin
  Config.GetContext.Log('This is a log message sent from TLogComponent.OnGet ...');

  Value := Config.GetContext.GetInitParameter('key');

  Config.GetContext.Log('Value=' + Value);

  Response.ContentText := 'TLogComponent';
end;

procedure TAPIConfigTests.TestConfigGetContextLog;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('log');
    Context.SetInitParameter('key', 'Context init parameter value');
    Context.Add(TLogComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseContains('TLogComponent', '/log/hello')

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestAddConnector;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  Connector: TdjHTTPConnector;
  Intercept: TIdServerInterceptLogFile;
begin
  Intercept := TIdServerInterceptLogFile.Create;
  try
    Server := TdjServer.Create;
    try
      // add a configured connector
      Connector := TdjHTTPConnector.Create(Server.Handler);
      // TODO DOC not TdjHTTPConnector.Create(Server)!
      Connector.Host := '127.0.0.1';
      Connector.Port := 8080;
      // new property "HTTPServer" in 1.5
      // here used to set a file based logger for the HTTP server
      Connector.HTTPServer.Intercept := Intercept;
      Intercept.Filename := 'httpIntercept.log';
      Server.AddConnector(Connector);
      Context := TdjWebAppContext.Create('get');
      Context.Add(TGetComponent, '/hello');
      Server.Add(Context);
      Server.Start;

      CheckGETResponseEquals('Hello', '/get/hello');

    finally
      Server.Free;
    end;
  finally
    Intercept.Free
  end;
end;

procedure TAPIConfigTests.TestAddDuplicateConnectorRaisesException;
var
  Server: TdjServer;
begin
  Server := TdjServer.Create;
  try
    Server.AddConnector('127.0.0.1', 8080);

    {$IFDEF FPC}
    ExpectException(EDarajaMappingException, '');
    {$ELSE}
    ExpectedException := EDarajaMappingException;
    {$ENDIF}
    Server.AddConnector('127.0.0.1', 8080);
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestRemoveConnector;
var
  Server: TdjServer;
  Connector: IConnector;
begin
  Server := TdjServer.Create;
  try
    Connector := TdjHTTPConnector.Create(Server.Handler);
    Connector.Host := '127.0.0.1';
    Connector.Port := 8080;
    Server.AddConnector(Connector);
    CheckEquals(1, Server.ConnectorCount);

    Server.RemoveConnector(Connector);
    CheckEquals(0, Server.ConnectorCount);

    // removable and re-addable
    Server.AddConnector(Connector);
    CheckEquals(1, Server.ConnectorCount);
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestRemoveUnknownConnectorRaisesException;
var
  Server: TdjServer;
  Connector: IConnector;
begin
  Server := TdjServer.Create;
  try
    Connector := TdjHTTPConnector.Create(Server.Handler);
    Connector.Host := '127.0.0.1';
    Connector.Port := 9999;

    {$IFDEF FPC}
    ExpectException(EDarajaMappingException, '');
    {$ELSE}
    ExpectedException := EDarajaMappingException;
    {$ENDIF}
    Server.RemoveConnector(Connector);
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestEnumerateConnectors;
var
  Server: TdjServer;
  I: Integer;
  Ports: string;
begin
  Server := TdjServer.Create;
  try
    Server.AddConnector('127.0.0.1', 8181);
    Server.AddConnector('127.0.0.1', 8080);
    Server.AddConnector('127.0.0.1', 8282);

    CheckEquals(3, Server.ConnectorCount);

    Ports := '';
    for I := 0 to Server.ConnectorCount - 1 do
      Ports := Ports + IntToStr(Server.GetConnector(I).Port) + ' ';

    // preserves insertion order
    CheckEquals('8181 8080 8282 ', Ports);
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestThreadPool;
var
  SchedulerOfThreadPool: TIdSchedulerOfThreadPool;
  Server: TdjServer;
  Context: TdjWebAppContext;
  Connector: TdjHTTPConnector;
begin
  Server := TdjServer.Create;
  try
    // add a configured connector
    Connector := TdjHTTPConnector.Create(Server.Handler);
    // TODO DOC not TdjHTTPConnector.Create(Server)!
    Connector.Host := '127.0.0.1';
    Connector.Port := 8080;
    SchedulerOfThreadPool := TIdSchedulerOfThreadPool.Create(Connector.HTTPServer);
    SchedulerOfThreadPool.PoolSize := 20;
    // set thread pool scheduler
    Connector.HTTPServer.Scheduler := SchedulerOfThreadPool;
    Server.AddConnector(Connector);
    Context := TdjWebAppContext.Create('get');
    Context.Add(TGetComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('Hello', '/get/hello');

    Server.Stop;
  finally
    Server.Free;
  end;
end;

// test context init parameter

type
  TContextInitParamComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TContextInitParamComponent }

procedure TContextInitParamComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
var
  InitParamValue: string;
begin
  InitParamValue := Config.GetContext.GetInitParameter('a');

  // WriteLn('>>>> a=' + InitParamValue);
  Response.ContentText := InitParamValue;
end;

procedure TAPIConfigTests.TestContextConfig;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('example');
    Context.Add(TContextInitParamComponent, '/index.html');
    Context.SetInitParameter('a', 'myValue');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('myValue', '/example/index.html');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestContextWithConnectorName;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  ContextPublic: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Server.AddConnector('127.0.0.1', 8181);
    Server.AddConnector('127.0.0.1', 8080);
    Server.AddConnector('127.0.0.1', 8282); // unused, just to see the order
    // configure for context on standard port
    ContextPublic := TdjWebAppContext.Create('public');
    ContextPublic.AddWebComponent(TExamplePage, '/hello');
    // configure for context on special port
    Context := TdjWebAppContext.Create('get');
    Context.Add(TExamplePage, '/hello');
    Context.ConnectorNames.Add('127.0.0.1:8181');
    Server.Add(ContextPublic);
    Server.Add(Context);
    Server.Start;

    // this does not work as the connector listens on port 8181
    CheckGETResponse404('/get/hello');

    // this works (special port)
    CheckGETResponseEquals('example', 'http://127.0.0.1:8181/get/hello');

    // this works (default port)
    CheckGETResponseEquals('example', 'http://' + DEFAULT_BINDING_IP + ':' +
      IntToStr(DEFAULT_BINDING_PORT) + '/public/hello');

  finally
    Server.Free;
  end;
end;

// ---------------------------------------------------------------------------

{ TCharSetComponent }

type
  TCharSetComponent = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse);
      override;
  end;

procedure TCharSetComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := '中文';
  Response.ContentType := 'text/plain';
  Response.CharSet := 'utf-8';
end;

procedure TAPIConfigTests.TestCharSet;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TCharSetComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    {$IFDEF STRING_IS_ANSI}
    DestEncoding := IndyTextEncoding_UTF8; // TODO document
    {$ENDIF}

    CheckGETResponseEquals('中文', '/get/hello');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestContentType;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TCharSetComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    {$IFDEF STRING_IS_ANSI}
    DestEncoding := IndyTextEncoding_UTF8; // TODO document
    {$ENDIF}

    CheckContentTypeEquals('text/plain', '/get/hello');

  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestCachedGetRequest;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('cached');
    Context.Add(TCachedGetComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    // set "If-Modified-Since" header to yesterday to enforce a fresh response
    CheckCachedGETResponseEquals(Date - 1, 'CachedGET', '/cached/index.html');

    // set "If-Modified-Since" header to Now to get "304 resource not modified"
    CheckCachedGETResponseIs304(Now, '/cached/index.html');

  finally
    Server.Free;
  end;
end;

// a component which overrides OnGet only must answer HEAD requests with the
// GET headers and no body (issue #428)
procedure TAPIConfigTests.TestHeadRequestUsesGetHandler;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TGetComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckHEADMatchesGET('/get/hello');

  finally
    Server.Free;
  end;
end;

// if OnGet is not overridden either, HEAD still returns 405 (issue #428)
procedure TAPIConfigTests.TestHeadRequestWithoutGetHandlerReturns405;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TNoMethodComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckHEADResponse405('/get/hello');

  finally
    Server.Free;
  end;
end;

// HEAD runs the cached GET path, so it honors If-Modified-Since (issue #428)
procedure TAPIConfigTests.TestCachedHeadRequest;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('cached');
    Context.Add(TCachedGetComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    CheckCachedHEADResponseIs304(Now, '/cached/index.html');

  finally
    Server.Free;
  end;
end;

// a component overriding OnGetETag sends an ETag header (issue #430)
procedure TAPIConfigTests.TestETagResponseHeaderIsSent;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('etag');
    Context.Add(TETagComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    CheckGETResponseHeaderEquals('ETag', '"fixed-etag-value"', '/etag/index.html');

  finally
    Server.Free;
  end;
end;

// a matching If-None-Match yields 304, whether by an exact match or a
// round-tripped ETag (issue #430)
procedure TAPIConfigTests.TestETagConditionalGetIs304WhenETagMatches;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('etag');
    Context.Add(TETagComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    CheckIfNoneMatchGETResponseIs304('"fixed-etag-value"', '/etag/index.html');
    CheckConditionalGETWithETagIs304('/etag/index.html');

  finally
    Server.Free;
  end;
end;

// a non-matching If-None-Match still returns the full, fresh response
// (issue #430)
procedure TAPIConfigTests.TestETagConditionalGetIsFreshWhenETagDiffers;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('etag');
    Context.Add(TETagComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    CheckIfNoneMatchGETResponseEquals('"some-other-etag"', 'ETagGET', '/etag/index.html');

  finally
    Server.Free;
  end;
end;

// If-None-Match: * matches any current representation (issue #430)
procedure TAPIConfigTests.TestIfNoneMatchWildcardIs304;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('etag');
    Context.Add(TETagComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    CheckIfNoneMatchGETResponseIs304('*', '/etag/index.html');

  finally
    Server.Free;
  end;
end;

// RFC 7232 Section 3.3: If-None-Match alone decides the outcome; a
// non-matching If-None-Match forces a fresh response even when
// If-Modified-Since would, by itself, have produced a 304 (issue #430)
procedure TAPIConfigTests.TestIfNoneMatchTakesPrecedenceOverIfModifiedSince;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('cached');
    Context.Add(TCachedGetWithETagComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    // If-Modified-Since matches the component's Last-Modified (Date - 1),
    // but If-None-Match does not match its ETag: the response must be fresh.
    CheckGETResponseCodeWithConditionalHeaders(Date - 1, '"some-other-etag"', 200,
      '/cached/index.html');

    // both match: 304.
    CheckGETResponseCodeWithConditionalHeaders(Date - 1, '"fixed-etag-value"', 304,
      '/cached/index.html');

  finally
    Server.Free;
  end;
end;

// a 304 response must carry Date and Last-Modified, not just the status
// line (issue #430)
procedure TAPIConfigTests.TestNotModifiedResponseIncludesDateAndLastModified;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('cached');
    Context.Add(TCachedGetComponent, '*.html');
    Server.Add(Context);
    Server.Start;

    CheckCachedGETResponseIs304WithDateAndLastModified(Now, '/cached/index.html');

  finally
    Server.Free;
  end;
end;

// OPTIONS lists the overridden On* handlers, plus the implied HEAD (issue #429)
procedure TAPIConfigTests.TestOptionsRequestListsOverriddenMethods;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TGetComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckOPTIONSAllowHeaderEquals('GET, HEAD, OPTIONS', '/get/hello');

  finally
    Server.Free;
  end;
end;

// a component with no overrides still answers OPTIONS, listing OPTIONS only (issue #429)
procedure TAPIConfigTests.TestOptionsRequestWithoutOverridesListsOptionsOnly;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TNoMethodComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckOPTIONSAllowHeaderEquals('OPTIONS', '/get/hello');

  finally
    Server.Free;
  end;
end;

// a 405 response (from a not-overridden handler) carries an Allow header (issue #429)
procedure TAPIConfigTests.TestMethodNotAllowedResponseIncludesAllowHeader;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('get');
    Context.Add(TGetComponent, '/hello');
    Server.Add(Context);
    Server.Start;

    CheckPOSTResponse405AllowHeaderEquals('GET, HEAD, OPTIONS', '/get/hello');

  finally
    Server.Free;
  end;
end;

type

  { TTestFilter }

  TTestFilter = class(TdjWebFilter)
  public
    procedure DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

  { TTestFilterV3WithInit }

  TTestFilterV3WithInit = class(TdjWebFilter)
  strict private
    FInitParam: string;
  public
    procedure Init; override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TTestFilterWithInit }

  TTestFilterWithInit = class(TdjWebFilter)
  strict private
    FInitParam: string;
  public
    procedure Init(const Config: IWebFilterConfig); override;
    procedure DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

  { TFilterWithInitReadsContextConfiguration }

  TFilterWithInitReadsContextConfiguration = class(TdjWebFilter)
  strict private
    StaticContent: string;
  public
    procedure Init(const Config: IWebFilterConfig); override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TFilterReadsFilterName }

  TFilterReadsFilterName = class(TdjWebFilter)
  strict private
    FFilterName: string;
  public
    procedure Init(const Config: IWebFilterConfig); override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TTestFilterA }

  TTestFilterA = class(TdjWebFilter)
  public
    procedure DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

  { TTestFilterB }

  TTestFilterB = class(TdjWebFilter)
  public
    procedure DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

procedure TFilterWithInitReadsContextConfiguration.Init(const Config: IWebFilterConfig);
begin
  StaticContent := 'from init';

  if Config <> nil then StaticContent := StaticContent + ' 1';
  if Config.GetContext <> nil then StaticContent := StaticContent + ' 2';
  if Config.GetContext.GetContextConfig <> nil then StaticContent := StaticContent + ' 3';
end;

procedure TFilterWithInitReadsContextConfiguration.DoFilter(
  Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse;
  const Chain: IWebFilterChain);
begin
  Chain.DoFilter(Context, Request, Response);
  Response.ContentText := StaticContent;
end;

{ TFilterReadsFilterName }

procedure TFilterReadsFilterName.Init(const Config: IWebFilterConfig);
begin
  FFilterName := Config.GetFilterName;
end;

procedure TFilterReadsFilterName.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  Chain.DoFilter(Context, Request, Response);
  Response.ContentText := 'filter name=' + FFilterName;
end;

{ TTestFilter }

procedure TTestFilter.DoFilter(Context: TdjServerContext; Request: TdjRequest;
  Response: TdjResponse; const Chain: IWebFilterChain);
begin
   Chain.DoFilter(Context, Request, Response);
   Response.ContentText := Response.ContentText + ' (filtered)';
end;

{ TTestFilterV3WithInit }

procedure TTestFilterV3WithInit.Init;
begin
  FInitParam := Config.GetInitParameter('key');
end;

procedure TTestFilterV3WithInit.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  Chain.DoFilter(Context, Request, Response);

  if Response.ContentText <> '' then
    Response.ContentText := Response.ContentText + ', ';

  Response.ContentText := Response.ContentText + 'Param key=' + FInitParam;
end;


{ TTestFilterWithInit }

procedure TTestFilterWithInit.Init(const Config: IWebFilterConfig);
begin
  FInitParam := Config.GetInitParameter('key');
end;

procedure TTestFilterWithInit.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  Chain.DoFilter(Context, Request, Response);

  if Response.ContentText <> '' then
    Response.ContentText := Response.ContentText + ', ';

  Response.ContentText := Response.ContentText + 'Param key=' + FInitParam;
end;

{ TTestFilterA }

procedure TTestFilterA.DoFilter(Context: TdjServerContext; Request: TdjRequest;
  Response: TdjResponse; const Chain: IWebFilterChain);
begin
  Chain.DoFilter(Context, Request, Response);
  Response.ContentText := Response.ContentText + ' (A)';
end;

{ TTestFilterB }

procedure TTestFilterB.DoFilter(Context: TdjServerContext; Request: TdjRequest;
  Response: TdjResponse; const Chain: IWebFilterChain);
begin
  Chain.DoFilter(Context, Request, Response);
  Response.ContentText := Response.ContentText + ' (B)';
end;

procedure TAPIConfigTests.TestOnlyAFilter;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TTestFilter, '*.html');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponse404('/web/index.html');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestFilter;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.html');
  Context.Add(TTestFilter, '*.html');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example (filtered)', '/web/index.html');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestPrefixFilterAppliesInNonRootContext;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // Regression test for #530: GetFilterChain used to be matched against the
  // raw, context-prefixed target while FindComponent matched the
  // context-stripped path, so a prefix-pattern filter like '/secure/*'
  // silently never fired outside a root context. Both now match the same
  // normalized, context-relative path.
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '/secure/index.html');
  Context.Add(TTestFilter, '/secure/*');

  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example (filtered)', '/web/secure/index.html');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestPrefixFilterAppliesAfterPathNormalization;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // A '..' segment must not let a request dodge a prefix-pattern filter:
  // the target is normalized before routing, so this resolves to
  // '/web/secure/index.html' and the filter still applies.
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '/secure/index.html');
  Context.Add(TTestFilter, '/secure/*');

  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example (filtered)', '/web/x/../secure/index.html');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestTwoFilters;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.html');
  Context.Add(TTestFilterA, '*.html');
  Context.Add(TTestFilterB, '*.html');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example (A) (B)', '/web/index.html');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestTwoFiltersReversed;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.html');
  Context.Add(TTestFilterB, '*.html');
  Context.Add(TTestFilterA, '*.html');
  Server := TdjServer.Create;

  // run
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example (B) (A)', '/web/index.html');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestTwoFiltersAndTwoWebComponents;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.filterA');
  Context.Add(TGetComponent, '*.filterB');
  Context.Add(TTestFilterA, '*.filterA');
  Context.Add(TTestFilterB, '*.filterB');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example (A)', '/web/page.filterA');
    CheckGETResponseEquals('Hello (B)', '/web/page.filterB');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestFilterWithInit;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  FilterHolder: TdjWebFilterHolder;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.SetInitParameter('a', 'b');
  Context.Add(TExamplePage, '*.filter');
  FilterHolder := Context.Add(TTestFilterWithInit, '*.filter');
  FilterHolder.SetInitParameter('key', 'Hello, World!');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;
    CheckGETResponseEquals('example, Param key=Hello, World!', '/web/page.filter');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestFilterV3WithInit;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.SetInitParameter('a', 'b');
  Context.Add(TExamplePage, '*.filter');
  with Context.Add(TTestFilterV3WithInit, '*.filter') do
  begin
    SetInitParameter('key', 'Hello, World V3!');
  end;

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;
    CheckGETResponseEquals('example, Param key=Hello, World V3!', '/web/page.filter');
  finally
    Server.Free;
  end;
end;


procedure TAPIConfigTests.TestFilterInitCanReadContextConfiguration;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.filter');
  Context.Add(TFilterWithInitReadsContextConfiguration, '*.filter');
  Context.SetInitParameter('a', 'b');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;
    CheckGETResponseEquals('from init 1 2 3', '/web/page.filter');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestInvalidFilterUrlPatternRaisesException;
var
  Context: TdjWebAppContext;
begin
  Context := TdjWebAppContext.Create('web');
  try
    Context.Add(TExamplePage, '*.html');

    {$IFDEF FPC}
    ExpectException(EDarajaMappingException, '');
    {$ELSE}
    ExpectedException := EDarajaMappingException;
    {$ENDIF}
    Context.Add(TTestFilter, 'not-a-valid-pattern');
  finally
    Context.Free;
  end;
end;

procedure TAPIConfigTests.TestFilterInitCanReadFilterName;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.filter');
  Context.Add(TFilterReadsFilterName, '*.filter');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;
    CheckGETResponseEquals('filter name=TFilterReadsFilterName', '/web/page.filter');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestMapFilterTwiceToSameWebComponentRaisesException;
var
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.html');
  Context.Add(TTestFilter, '*.html');

  {$IFDEF FPC}
  ExpectException(EDarajaMappingException, '');
  {$ELSE}
  ExpectedException := EDarajaMappingException;
  {$ENDIF}
  try
    Context.Add(TTestFilter, '*.html');
  finally
    Context.Free;
  end;
end;

procedure TAPIConfigTests.TestCatchAllWebFilter;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.txt');
  Context.Add(TTestFilter, '/*');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example (filtered)', '/web/anypage.txt');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestExceptionInComponentInitWithWebFilter;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TTestFilter, '/*');
  Context.Add(TExceptionInInitComponent, '*.html');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    // Test the component: same as TestExceptionInInitStopsComponent, but
    // routed through a filter chain.
    CheckGETResponse500('/web/exception.html');
  finally
    Server.Free;
  end;
end;

procedure TAPIConfigTests.TestExceptionInComponentServiceWithWebFilter;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExceptionComponent, '*.html');
  Context.Add(TTestFilter, '/*');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    // Test the component
    CheckGETResponse500('/web/exception.html');
  finally
    Server.Free;
  end;
end;

// test exception in Get  -------------------------------------------------
type
  TExceptionInOnGetComponent = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TExceptionInOnGetComponent }

procedure TExceptionInOnGetComponent.OnGet(Request: TdjRequest;
  Response: TdjResponse);
begin
  raise Exception.Create('Exception in OnGet');
end;

procedure TAPIConfigTests.TestExceptionInComponentOnGetWithWebFilter;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExceptionInOnGetComponent, '*.html');
  Context.Add(TTestFilter, '/*');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    // Test the component
    CheckGETResponse500('/web/exception.html');
  finally
    Server.Free;
  end;
end;

// ----------------------------------------------------------------------------

{ TTestFilterWithDestroy }
type

  TTestFilterWithDestroy = class(TTestFilter)
  public
    procedure DestroyFilter; override;
  end;

procedure TTestFilterWithDestroy.DestroyFilter;
begin
    raise EUnitTestException.Create('error in destroy');
end;

procedure TAPIConfigTests.TestWebFilterDestroyFilter;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.html');
  Context.Add(TTestFilterWithDestroy, '/*');

  // run
  Server := TdjServer.Create;
  try
    Server.Add(Context);
    Server.Start;

    // Test the component
    CheckGETResponse200('/web/destroy.html');
  finally
    Server.Free;
  end;
end;

end.

