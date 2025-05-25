(*

    Daraja HTTP Framework
    Copyright (c) Michael Justin

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
    procedure AddTwoContextWithSameName;

    procedure ConfigTwoContexts;

    procedure AddWebComponentWithMultipleUrlPattern;

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
    procedure TestThreadPool;

    procedure TestBindErrorRaisesException;

    // test overriding the TdjWebComponent.OnGetLastModified method
    // (since 1.2.10)
    procedure TestCachedGetRequest;

    procedure TestOnlyAFilter;
    procedure TestFilter;
    procedure TestTwoFilters;
    procedure TestTwoFiltersReversed;
    procedure TestTwoFiltersAndTwoWebComponents;
    procedure TestFilterWithInit;
    procedure TestFilterV3WithInit;
    procedure TestFilterInitCanReadContextConfiguration;
    //procedure TestOneFilterAndTwoWebComponents;

    procedure TestMapFilterTwiceToSameWebComponentRaisesException;
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
  djHTTPConnector, djContextHandlerCollection, djHandlerList, djTypes,
  djAbstractHandler, djServerContext, djWebFilter, djWebFilterHolder,
  djWebFilterConfig,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdServerInterceptLogFile, IdSchedulerOfThreadPool, IdGlobal, IdException,
  IdResourceStrings,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils, Classes;

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

procedure TAPIConfigTests.AddTwoContextWithSameName;
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
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example', '/foo/bar');
    CheckGETResponseEquals('example', '/foo/bar2');

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
  private
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
  private
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

    // Test the component
    CheckGETResponse405('/ctx/exception');

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

procedure TAPIConfigTests.AddWebComponentWithMultipleUrlPattern;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('');
    Context.Add(TExamplePage, ['/baz', '/qux']);
    Server.Add(Context);
    Server.Start;

    CheckGETResponseEquals('example', '/baz');
    CheckGETResponseEquals('example', '/qux');
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

type

  { TTestFilter }

  TTestFilter = class(TdjWebFilter)
  public
    procedure DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

  { TTestFilterV3WithInit }

  TTestFilterV3WithInit = class(TdjWebFilter)
  private
    FInitParam: string;
  public
    procedure Init; override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TTestFilterWithInit }

  TTestFilterWithInit = class(TdjWebFilter)
  private
    FInitParam: string;
  public
    procedure Init(const Config: IWebFilterConfig); override;
    procedure DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

  { TFilterWithInitReadsContextConfiguration }

  TFilterWithInitReadsContextConfiguration = class(TdjWebFilter)
  private
    StaticContent: string;
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

procedure TAPIConfigTests.TestMapFilterTwiceToSameWebComponentRaisesException;
var
  Context: TdjWebAppContext;
begin
  // configure
  Context := TdjWebAppContext.Create('web');
  Context.Add(TExamplePage, '*.html');
  Context.Add(TTestFilter, '*.html');

  {$IFDEF FPC}
  ExpectException(EListError, '');
  {$ELSE}
  ExpectedException := EListError;
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

    // Test the component
    CheckGETResponse405('/web/exception.html');
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

