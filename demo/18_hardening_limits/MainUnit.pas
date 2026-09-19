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

unit MainUnit;

interface

procedure Demo;

implementation

uses
  djServer, djHTTPConnector, djWebComponent, djWebAppContext, djTypes,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdContext, IdCustomHTTPServer, IdIOHandler, IdHeaderList,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

const
  // Everything below is a starting point, not a recommendation for any
  // specific production value -- pick numbers that fit your deployment.
  MAX_CONNECTIONS = 200;
  LISTEN_QUEUE = 64;
  IDLE_READ_TIMEOUT_MS = 10 * 1000;      // Slowloris: a connection that sends
                                          // nothing for this long is dropped
  MAX_REQUEST_BODY_BYTES = 1 * 1024 * 1024; // reject a declared body over 1 MB
                                             // before Indy reads a single byte
                                             // of it
  SHORT_SESSION_TIMEOUT_MS = 2 * 60 * 1000; // see README: "Session growth" --
                                             // this narrows the DoS window,
                                             // it does not close it

type
  TEchoResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TEchoResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'This request made it past the connection, header, and body limits.';
  Response.ContentType := 'text/plain';
end;

procedure TEchoResource.OnPost(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'Body accepted, ' + IntToStr(Request.ContentLength) + ' bytes.';
  Response.ContentType := 'text/plain';
end;

type
  // Indy's server events are "of object" -- bound to a method, not a bare
  // procedure -- so the handlers live here rather than as free functions.
  // Nothing about this class is Daraja-specific; it only talks to the
  // TIdCustomHTTPServer that TdjHTTPConnector.HTTPServer exposes.
  TConnectionLimits = class
  public
    // Runs for every new TCP connection, before any HTTP parsing.
    //
    // Setting IOHandler.ReadTimeout here is what actually stops Slowloris: a
    // client that opens a connection and then sends bytes one at a time (or
    // not at all) would otherwise pin a worker thread indefinitely. Indy
    // aborts the read -- raising EIdReadTimeout on this connection -- once
    // nothing arrives within the timeout, freeing the thread. Daraja's own
    // connector already logs that exception at Debug rather than
    // Warn/Error (see issue #521), so this doesn't add log noise.
    procedure HandleConnect(AContext: TIdContext);

    // Fires once the request line and headers are parsed, but before Indy
    // reads the request body -- the only point where a body-size limit can
    // reject a request without first paying the cost of reading it.
    //
    // A chunked-encoded body has no Content-Length header at all, so this
    // check cannot catch it; that gap needs a library-level fix (tracked in
    // #531), not something reachable from application code.
    procedure HandleHeadersAvailable(AContext: TIdContext; const AUri: string;
      AHeaders: TIdHeaderList; var VContinueProcessing: Boolean);

    // Fires immediately after HandleHeadersAvailable sets
    // VContinueProcessing to False. Without this handler Indy sends a bare
    // 403; this turns the oversized-body case into the more accurate 413.
    procedure HandleHeadersBlocked(AContext: TIdContext; AHeaders: TIdHeaderList;
      var VResponseNo: Integer; var VResponseText, VContentText: String);
  end;

procedure TConnectionLimits.HandleConnect(AContext: TIdContext);
begin
  AContext.Connection.IOHandler.ReadTimeout := IDLE_READ_TIMEOUT_MS;
end;

procedure TConnectionLimits.HandleHeadersAvailable(AContext: TIdContext;
  const AUri: string; AHeaders: TIdHeaderList; var VContinueProcessing: Boolean);
var
  ContentLength: Int64;
begin
  ContentLength := StrToInt64Def(AHeaders.Values['Content-Length'], 0);
  if ContentLength > MAX_REQUEST_BODY_BYTES then
  begin
    VContinueProcessing := False;
  end;
end;

procedure TConnectionLimits.HandleHeadersBlocked(AContext: TIdContext;
  AHeaders: TIdHeaderList; var VResponseNo: Integer;
  var VResponseText, VContentText: String);
begin
  VResponseNo := 413;
  VResponseText := 'Payload Too Large';
  VContentText := 'Request body exceeds the ' +
    IntToStr(MAX_REQUEST_BODY_BYTES) + ' byte limit.';
end;

procedure Demo;
var
  Server: TdjServer;
  Connector: TdjHTTPConnector;
  Context: TdjWebAppContext;
  Limits: TConnectionLimits;
begin
  Server := TdjServer.Create;
  Limits := TConnectionLimits.Create;
  try
    // Build the connector explicitly (rather than the Server.Create(Port)
    // shortcut) so it exists, and can be configured, before Server.Start
    // ever accepts a connection.
    Connector := TdjHTTPConnector.Create(Server.Handler);
    Connector.Host := '127.0.0.1';
    Connector.Port := 8080;

    // Every property and event set below is public API on the Indy
    // TIdCustomHTTPServer that TdjHTTPConnector.HTTPServer already exposes
    // (see the "Configuration of internal Indy HTTP Server" section of the
    // getting-started guide for the same pattern with MaxConnections and a
    // thread pool). None of it requires touching the Daraja source.
    Connector.HTTPServer.MaxConnections := MAX_CONNECTIONS;
    Connector.HTTPServer.ListenQueue := LISTEN_QUEUE;
    Connector.HTTPServer.SessionTimeOut := SHORT_SESSION_TIMEOUT_MS;
    Connector.HTTPServer.OnConnect := Limits.HandleConnect;
    Connector.HTTPServer.OnHeadersAvailable := Limits.HandleHeadersAvailable;
    Connector.HTTPServer.OnHeadersBlocked := Limits.HandleHeadersBlocked;

    Server.AddConnector(Connector);

    Context := TdjWebAppContext.Create('demo');
    Context.Add(TEchoResource, '/echo');
    Server.Add(Context);

    Server.Start;
    WriteLn('Server is running, please open http://127.0.0.1:8080/demo/echo');
    WriteLn('Try: curl -X POST --data-binary @/some/2MB/file http://127.0.0.1:8080/demo/echo  (expect 413)');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
    Limits.Free;
  end;
end;

end.
