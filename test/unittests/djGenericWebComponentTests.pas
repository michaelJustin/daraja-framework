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

unit djGenericWebComponentTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjGenericWebComponentTests }

  TdjGenericWebComponentTests = class(TTestCase)
  published
    procedure TestConfigRaisesWhenNotInitialized;
    procedure TestInitAssignsConfigAndCallsParameterlessInit;
    procedure TestGetSessionReturnsExistingSessionWithoutCreating;
    procedure TestGetSessionReturnsNilWhenAbsentAndNotAskedToCreate;
    procedure TestGetSessionCreatesANewSessionWhenAbsentAndAskedTo;
  end;

implementation

uses
  djGenericWebComponent, djWebComponentConfig, djWebAppContext, djInterfaces,
  djServerContext, djHTTPServer, djTypes,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

type

  { TInitTrackingComponent }

  {*
   * Records whether the overridable, parameterless Init was invoked by the
   * inherited Init(Config) -- the convenience hook subclasses are meant to
   * override instead of Init(Config) itself.
   *}
  TInitTrackingComponent = class(TdjGenericWebComponent)
  public
    InitCalled: Boolean;
    procedure Init; override;
  end;

procedure TInitTrackingComponent.Init;
begin
  InitCalled := True;
end;

type

  { TTestRequestInfo }

  {*
   * Lets a test pre-assign a session, simulating a request that already
   * carries a valid session cookie -- TIdHTTPRequestInfo.Session has no
   * public setter of its own.
   *}
  TTestRequestInfo = class(TIdHTTPRequestInfo)
  public
    procedure SetSessionForTest(ASession: TIdHTTPSession);
  end;

procedure TTestRequestInfo.SetSessionForTest(ASession: TIdHTTPSession);
begin
  FSession := ASession;
end;

type

  { TTestServerContext }

  {*
   * TIdServerContext.Server has no public setter either -- a real context is
   * normally only ever built by Indy while accepting a connection. This
   * subclass points it at a specific HTTP server instance directly, without
   * any of that machinery, for direct unit testing.
   *}
  TTestServerContext = class(TdjServerContext)
  public
    constructor CreateWithServer(AServer: TdjHTTPServer);
  end;

constructor TTestServerContext.CreateWithServer(AServer: TdjHTTPServer);
begin
  inherited Create(nil, nil, nil);
  FServer := AServer;
end;

{ TdjGenericWebComponentTests }

procedure TdjGenericWebComponentTests.TestConfigRaisesWhenNotInitialized;
var
  Component: TdjGenericWebComponent;
begin
  Component := TdjGenericWebComponent.Create;
  try
    try
      Component.Config;
      Fail('Expected EDarajaLifecycleException before Init is called');
    except
      on E: EDarajaLifecycleException do
        ; // expected
    end;
  finally
    Component.Free;
  end;
end;

procedure TdjGenericWebComponentTests.TestInitAssignsConfigAndCallsParameterlessInit;
var
  Component: TInitTrackingComponent;
  Context: TdjWebAppContext;
  Config: IWebComponentConfig;
begin
  Context := TdjWebAppContext.Create('generic-wc-ctx');
  try
    Config := TdjWebComponentConfig.Create;
    (Config as IWriteableConfig).SetContext(Context.GetCurrentContext);

    Component := TInitTrackingComponent.Create;
    try
      Component.Init(Config);

      CheckTrue(Component.InitCalled,
        'the overridable parameterless Init must be called');
      CheckTrue(Component.Config = Config,
        'GetWebComponentConfig must return the config passed to Init');
    finally
      Component.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjGenericWebComponentTests.TestGetSessionReturnsExistingSessionWithoutCreating;
var
  Component: TdjGenericWebComponent;
  Request: TTestRequestInfo;
  ExistingSession: TIdHTTPSession;
  Session: TIdHTTPSession;
begin
  Component := TdjGenericWebComponent.Create;
  try
    ExistingSession := TIdHTTPSession.CreateInitialized(nil, 'existing-id', '127.0.0.1');
    try
      Request := TTestRequestInfo.Create(nil);
      try
        Request.SetSessionForTest(ExistingSession);

        // Context and Response are never dereferenced on this path -- an
        // already-assigned session short-circuits before either is touched.
        Session := Component.GetSession(nil, Request, nil, True);

        CheckTrue(Session = ExistingSession,
          'an existing session must be returned as-is, not replaced');
      finally
        Request.Free;
      end;
    finally
      ExistingSession.Free;
    end;
  finally
    Component.Free;
  end;
end;

procedure TdjGenericWebComponentTests.TestGetSessionReturnsNilWhenAbsentAndNotAskedToCreate;
var
  Component: TdjGenericWebComponent;
  Request: TIdHTTPRequestInfo;
  Session: TIdHTTPSession;
begin
  Component := TdjGenericWebComponent.Create;
  try
    Request := TIdHTTPRequestInfo.Create(nil);
    try
      Session := Component.GetSession(nil, Request, nil, False);

      CheckTrue(Session = nil, 'no session must be created when Create is False');
      CheckTrue(Request.Session = nil, 'the request must remain without a session');
    finally
      Request.Free;
    end;
  finally
    Component.Free;
  end;
end;

procedure TdjGenericWebComponentTests.TestGetSessionCreatesANewSessionWhenAbsentAndAskedTo;
var
  Component: TdjGenericWebComponent;
  Server: TdjHTTPServer;
  Context: TTestServerContext;
  Request: TIdHTTPRequestInfo;
  Response: TIdHTTPResponseInfo;
  Session: TIdHTTPSession;
begin
  Component := TdjGenericWebComponent.Create;
  try
    Server := TdjHTTPServer.Create;
    try
      Context := TTestServerContext.CreateWithServer(Server);
      try
        Request := TIdHTTPRequestInfo.Create(nil);
        try
          Response := TIdHTTPResponseInfo.Create(Server, Request, nil);
          try
            Session := Component.GetSession(Context, Request, Response, True);

            CheckTrue(Session <> nil, 'a new session must be created');
            CheckTrue(Session = Request.Session,
              'the new session must also be recorded on the request');
            CheckEquals(1, Response.Cookies.Count,
              'a session cookie must be added to the response');
          finally
            Response.Free;
          end;
        finally
          Request.Free;
        end;
      finally
        Context.Free;
      end;
    finally
      Server.Free;
    end;
  finally
    Component.Free;
  end;
end;

end.
