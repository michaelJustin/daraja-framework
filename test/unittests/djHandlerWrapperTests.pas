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

unit djHandlerWrapperTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjHandlerWrapperTests }

  TdjHandlerWrapperTests = class(TTestCase)
  published
    procedure TestGetSessionReturnsExistingSessionWithoutCreating;
    procedure TestGetSessionReturnsNilWhenAbsentAndNotAskedToCreate;
    procedure TestGetSessionCreatesANewSessionWhenAbsentAndAskedTo;
    procedure TestGetSessionDoesNotCreateASecondSessionOnRepeatedCalls;
  end;

implementation

uses
  djHandlerWrapper, djServerContext, djHTTPServer, djTypes,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

type

  { TTestHandlerWrapper }

  {*
   * Exposes the protected GetSession for direct testing.
   *}
  TTestHandlerWrapper = class(TdjHandlerWrapper)
  public
    function CallGetSession(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const CreateIt: Boolean): TIdHTTPSession;
  end;

function TTestHandlerWrapper.CallGetSession(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse;
  const CreateIt: Boolean): TIdHTTPSession;
begin
  Result := GetSession(Context, Request, Response, CreateIt);
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

{ TdjHandlerWrapperTests }

procedure TdjHandlerWrapperTests.TestGetSessionReturnsExistingSessionWithoutCreating;
var
  Wrapper: TTestHandlerWrapper;
  Request: TTestRequestInfo;
  ExistingSession: TIdHTTPSession;
  Session: TIdHTTPSession;
begin
  Wrapper := TTestHandlerWrapper.Create;
  try
    ExistingSession := TIdHTTPSession.CreateInitialized(nil, 'existing-id', '127.0.0.1');
    try
      Request := TTestRequestInfo.Create(nil);
      try
        Request.SetSessionForTest(ExistingSession);

        // Context and Response are never dereferenced on this path -- an
        // already-assigned session short-circuits before either is touched.
        Session := Wrapper.CallGetSession(nil, Request, nil, True);

        CheckTrue(Session = ExistingSession,
          'an existing session must be returned as-is, not replaced');
      finally
        Request.Free;
      end;
    finally
      ExistingSession.Free;
    end;
  finally
    Wrapper.Free;
  end;
end;

procedure TdjHandlerWrapperTests.TestGetSessionReturnsNilWhenAbsentAndNotAskedToCreate;
var
  Wrapper: TTestHandlerWrapper;
  Request: TIdHTTPRequestInfo;
  Session: TIdHTTPSession;
begin
  Wrapper := TTestHandlerWrapper.Create;
  try
    Request := TIdHTTPRequestInfo.Create(nil);
    try
      Session := Wrapper.CallGetSession(nil, Request, nil, False);

      CheckTrue(Session = nil, 'no session must be created when Create is False');
      CheckTrue(Request.Session = nil, 'the request must remain without a session');
    finally
      Request.Free;
    end;
  finally
    Wrapper.Free;
  end;
end;

procedure TdjHandlerWrapperTests.TestGetSessionCreatesANewSessionWhenAbsentAndAskedTo;
var
  Wrapper: TTestHandlerWrapper;
  Server: TdjHTTPServer;
  Context: TTestServerContext;
  Request: TIdHTTPRequestInfo;
  Response: TIdHTTPResponseInfo;
  Session: TIdHTTPSession;
begin
  Wrapper := TTestHandlerWrapper.Create;
  try
    Server := TdjHTTPServer.Create;
    try
      Context := TTestServerContext.CreateWithServer(Server);
      try
        Request := TIdHTTPRequestInfo.Create(nil);
        try
          Response := TIdHTTPResponseInfo.Create(Server, Request, nil);
          try
            Session := Wrapper.CallGetSession(Context, Request, Response, True);

            CheckTrue(Session <> nil, 'a new session must be created');
            CheckTrue(Session = Request.Session,
              'the new session must also be recorded on the request');
            CheckEquals(1, Response.Cookies.Count,
              'a session cookie must be added to the response');
            CheckEquals(Server.SessionIDCookieName, Response.Cookies[0].CookieName);
            CheckEquals(Session.SessionID, Response.Cookies[0].Value);
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
    Wrapper.Free;
  end;
end;

procedure TdjHandlerWrapperTests.TestGetSessionDoesNotCreateASecondSessionOnRepeatedCalls;
var
  Wrapper: TTestHandlerWrapper;
  Server: TdjHTTPServer;
  Context: TTestServerContext;
  Request: TIdHTTPRequestInfo;
  Response: TIdHTTPResponseInfo;
  First, Second: TIdHTTPSession;
begin
  Wrapper := TTestHandlerWrapper.Create;
  try
    Server := TdjHTTPServer.Create;
    try
      Context := TTestServerContext.CreateWithServer(Server);
      try
        Request := TIdHTTPRequestInfo.Create(nil);
        try
          Response := TIdHTTPResponseInfo.Create(Server, Request, nil);
          try
            First := Wrapper.CallGetSession(Context, Request, Response, True);
            // the second call finds Request.Session already set by the first
            Second := Wrapper.CallGetSession(Context, Request, Response, True);

            CheckTrue(First = Second,
              'a second call must return the same session, not create another');
            CheckEquals(1, Response.Cookies.Count,
              'no extra cookie must be added on the second call');
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
    Wrapper.Free;
  end;
end;

end.
