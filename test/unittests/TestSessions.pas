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

unit TestSessions;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  HTTPTestCase,
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF};

type
  TSessionTests = class(THTTPTestCase)
  published
    procedure TestCreateSession;

    // with web context -------------------------------------------------------
    procedure TestAutoCreateHandlerFirst;

    procedure TestAutoCreateContextFirst;

    // no web context ---------------------------------------------------------
    procedure TestAutoCreateNoContextHandlerFirst;

    // procedure TestAutoCreateNoContextConnectorFirst;

    // check POST has session -------------------------------------------------
    procedure TestPostHasSession;

  end;

implementation

uses
  djWebAppContext, djServerContext, djInterfaces, djWebComponent, djServer,
  djHandlerWrapper, djHTTPConnector, djTypes,
  Dialogs, SysUtils, Classes;

type
  TExamplePage = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse); override;
  end;

  TSessionDetector = class(TdjHandlerWrapper)
  public
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  end;

  TSessionComponent = class(TdjWebComponent)
  public
    procedure Service(Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  end;

  TSessionPOSTComponent = class(TdjWebComponent)
  public
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TExamplePage }

procedure TExamplePage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'example';
end;

{ TSessionDetector }

procedure TSessionDetector.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  inherited; // required to get a session (in "with context" mode)

  if Assigned(Request.Session) then
  begin
    Response.ContentText := 'success';
  end else
  begin
    Response.ContentText := 'no session';
  end;

  Response.ResponseNo := 200;
end;

{ TSessionTests }

 procedure TSessionTests.TestAutoCreateHandlerFirst;
var
  Server: TdjServer;
  Handler: IHandler;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // Handler
    Handler := TSessionDetector.Create;
    Server.AddHandler(Handler);

    // create component and register it
    Context := TdjWebAppContext.Create('session', True);
    Context.Add(TExamplePage, '/example');
    Server.Add(Context);

    Server.Start;

    CheckGETResponseEquals('success', '/session/example');

  finally
    Server.Free;
  end;
end;

procedure TSessionTests.TestAutoCreateContextFirst;
var
  Server: TdjServer;
  Handler: IHandler;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create component and register it
    Context := TdjWebAppContext.Create('session', True);
    Context.Add(TExamplePage, '/example');
    Server.Add(Context);

    // Handler
    Handler := TSessionDetector.Create;
    Server.AddHandler(Handler);

    Server.Start;

    CheckGETResponseEquals('success', '/session/example');

  finally
    Server.Free;
  end;
end;

procedure TSessionTests.TestAutoCreateNoContextHandlerFirst;
var
  Server: TdjServer;
  Connector: TdjHTTPConnector;
begin
  Server := TdjServer.Create;
  try
    // add test handler first!
    // TODO support AutoStartSession when Handler is added after connector
    Server.AddHandler(TSessionDetector.Create);

    // add a configured connector TODO DOC not (Server)!
    Connector := TdjHTTPConnector.Create(Server.Handler);
    Connector.Host := '127.0.0.1';
    Connector.Port := 8080;
    Connector.HTTPServer.AutoStartSession := True;
    Server.AddConnector(Connector);

    Server.Start;

    CheckGETResponseEquals('success', '/test');

  finally
    Server.Free;
  end;
end;

(*
procedure TSessionTests.TestAutoCreateNoContextConnectorFirst;
var
  Server: TdjServer;
  Connector: TdjHTTPConnector;
begin
  Server := TdjServer.Create;

  try
    // add a configured connector TODO DOC not (Server)!
    Connector := TdjHTTPConnector.Create(Server.Handler);
    Connector.Host := '127.0.0.1';
    Connector.Port := 80;
    Connector.HTTPServer.AutoStartSession := True;
    Server.AddConnector(Connector);

    // add test handler
    // TODO support AutoStartSession when Handler is added after connector
    Server.AddHandler(TSessionDetector.Create);

    Server.Start;

    CheckEquals('success', Get('/test'));

  finally
    Server.Free;
  end;

end;
*)

procedure TSessionTests.TestCreateSession;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create component and register it
    Context := TdjWebAppContext.Create('get');
    Context.Add(TSessionComponent, '/hello');

    Server.Add(Context);
    Server.Start;

    CheckGETResponseContains('200 OK', '/get/hello')

  finally
    Server.Free;
  end;
end;

procedure TSessionTests.TestPostHasSession;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create component and register it
    Context := TdjWebAppContext.Create('post', True);
    Context.Add(TSessionPOSTComponent, '/hello');

    Server.Add(Context);
    Server.Start;

    CheckPOSTResponseEquals('success', '/post/hello');

  finally
    Server.Free;
  end;
end;

{ TSessionComponent }

procedure TSessionComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  Assert(not Assigned(Request.Session));

  GetSession(Context, Request, Response);

  Assert(Assigned(Request.Session));

  Config.GetContext.Log('SessionID: ' + Request.Session.SessionID);
end;

{ TSessionPOSTComponent }

procedure TSessionPOSTComponent.OnPost(Request: TdjRequest; Response: TdjResponse);
begin
  if Assigned(Request.Session) then
  begin
    Response.ContentText := 'success';
  end else
  begin
    Response.ContentText := 'no session';
  end;

  Response.ResponseNo := 200;
end;

end.
