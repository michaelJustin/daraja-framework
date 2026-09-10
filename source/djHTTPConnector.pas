{***

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

***}

unit djHTTPConnector;

interface

uses
  djAbstractConnector, djHTTPServer, djInterfaces, djServerContext,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes,
  IdContext;

type
  { TdjHTTPConnector }

  {*
   * HTTP connector.
   *
   * Instances of this class wrap a TdjHTTPServer component.
   *}
  TdjHTTPConnector = class(TdjAbstractConnector)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FHTTPServer: TdjHTTPServer;
    FHostAndPort: string;
    procedure OnCommand(AContext: TIdContext;
      ARequestInfo: TdjRequest; AResponseInfo: TdjResponse);
  protected
    // TdjLifeCycle overrides
    /// \private
    procedure DoStart; override;
    /// \private
    procedure DoStop; override;
  public
    {*
     * Create a HTTP connector.
     *
     * The handler is a required argument. The connector will
     * call the "Handle" method for incoming requests.
     *
     * @param Handler the request handler
     *}
    constructor Create(const Handler: IHandler); virtual;
    {*
     * Destructor.
     *}
    destructor Destroy; override;

    // properties
    property HTTPServer: TdjHTTPServer read FHTTPServer;
  end;

implementation /// \cond

uses
  IdSocketHandle, IdIOHandler, IdGlobal, IdException,
  SysUtils, Classes;

{ TdjHTTPConnector }

constructor TdjHTTPConnector.Create(const Handler: IHandler);
begin
  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjHTTPConnector);
  {$ENDIF DARAJA_LOGGING}

  inherited Create(Handler);

  Assert(Assigned(Handler));

  FHTTPServer := TdjHTTPServer.Create;
end;

destructor TdjHTTPConnector.Destroy;
begin
  if IsStarted then
  begin
    Stop;
  end;

  HTTPServer.Free;

  inherited;
end;

procedure TdjHTTPConnector.DoStart;
var
  Binding: TIdSocketHandle;
begin
  CheckNotStarted;

  // create binding
  {$IFDEF DARAJA_LOGGING}
  Logger.Trace('Configuring HTTP server for %s:%d', [Host, Port]);
  {$ENDIF DARAJA_LOGGING}

  HTTPServer.Bindings.Clear;

  Binding := HTTPServer.Bindings.Add;
  Binding.IP := Host;
  Binding.Port := Port;
  Binding.IPVersion := Id_IPv4;

  // detect IPv6
  if Pos(':', Host) > 0 then
  begin
    Binding.IPVersion := Id_IPv6;
  end;

  {$IFDEF DARAJA_LOGGING}
  Logger.Trace('Starting Indy HTTP server');
  {$ENDIF DARAJA_LOGGING}

  FHostAndPort := 'http://' + Host + ':' + IntToStr(Port);

  try
    // command handler
    HTTPServer.OnCommandGet := OnCommand;
    HTTPServer.OnCommandOther := OnCommand;

    HTTPServer.Active := True;

    // TdjLifeCycle.Start sets the started flag once DoStart returns.

    {$IFDEF DARAJA_LOGGING}
    Logger.Info('Accepting requests at %s', [FHostAndPort]);
    {$ENDIF DARAJA_LOGGING}

  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Info('Could not start HTTP connector at %s', [FHostAndPort]);
      Logger.Error(E.Message, E);
      {$ENDIF DARAJA_LOGGING}
      raise;
    end;
  end;
end;

procedure TdjHTTPConnector.DoStop;
begin
  if IsStarted then
  begin
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Stopping HTTP connector at %s', [FHostAndPort]);
    {$ENDIF DARAJA_LOGGING}

    try
      HTTPServer.Active := False;
    except
      on E: Exception do
      begin
        {$IFDEF DARAJA_LOGGING}
        Logger.Error(E.Message, E);
        {$ENDIF DARAJA_LOGGING}
      end;
    end;
  end;
end;

procedure TdjHTTPConnector.OnCommand(AContext: TIdContext;
  ARequestInfo: TdjRequest; AResponseInfo: TdjResponse);
begin
  try
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('OnCommand %s', [ARequestInfo.Document]);
    {$ENDIF DARAJA_LOGGING}

    AResponseInfo.ResponseNo := -1;

    // Handle the request
    Handler.Handle(ARequestInfo.Document, AContext as TdjServerContext,
      ARequestInfo, AResponseInfo);

    // this tells TIdHTTPServer what encoding the ContentText is using
    // so it can be decoded to Unicode prior to then being charset-encoded
    // for output. If the input and output encodings are the same, the
    // Ansi string data gets transmitted as-is without decoding/reencoding...
    {$IFDEF FPC}
    if LowerCase(AResponseInfo.CharSet) = 'utf-8' then
    begin
      AContext.Connection.IOHandler.DefAnsiEncoding := IndyTextEncoding_UTF8;
    end;
    {$ENDIF FPC}

  except
    on E: EIdConnClosedGracefully do
    begin
      // The server side of this connection has disconnected normally but
      // the client has attempted to read or write to the connection.
    end;
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Error(ClassName + '.OnCommand: ' + E.ClassName + ' ' + E.Message);
      {$ENDIF DARAJA_LOGGING}
    end;
  end;
end;

end. /// \endcond

