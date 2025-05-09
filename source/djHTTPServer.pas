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

unit djHTTPServer;

interface

// {$i IdCompilerDefines.inc}

uses
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer, IdCustomTCPServer, IdIOHandler, IdContext,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils, Classes;

const
  DEFAULT_SESSION_TIMEOUT = 10 * 60 * 1000;

type
  { TdjHTTPServer }

  (**
   * HTTP Server.
   *
   * Inherits from Indy HTTP Server.
   *)
  TdjHTTPServer = class(TIdCustomHTTPServer)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    // Exceptions
    procedure MyOnException(AContext: TIdContext; AException: Exception);
    procedure MyOnListenException(AThread: TIdListenerThread; AException: Exception);

    // Sessions
    procedure MySessionStart(Sender: TIdHTTPSession);
    procedure MySessionEnd(Sender: TIdHTTPSession);

    procedure Trace(const S: string);

  protected
    (**
     * If the server has a connection limit (MaxConnections) set,
     * and a new request exceeds this limit, log as a warning message.
     *)
    procedure DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler); override;

  public
    (**
     * Create a HTTP Server.
     *)
    constructor Create;

    (**
     * Handler for HTTP requests.
     *)
    property OnCommandGet;
  end;

implementation

uses
  djServerContext,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdException, IdExceptionCore;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

{ TdjHTTPServer }

constructor TdjHTTPServer.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
{$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjHTTPServer.ClassName);
{$ENDIF DARAJA_LOGGING}

  Trace('Configuring HTTP server');

{$IFDEF DARAJA_LOGGING}
  Logger.Info('Indy version: ' + GetIndyVersion);
{$ENDIF DARAJA_LOGGING}

  // use HTTP 1.1 keep-alive by default
  KeepAlive := True;

  // session management
  // workaround for AV on port already open
  SessionList := TIdHTTPDefaultSessionList.Create(Self);
  SessionState := True;
  SessionTimeOut := DEFAULT_SESSION_TIMEOUT; // 10 minutes
  Trace('On-demand HTTP sessions enabled');

  OnException := MyOnException;
  OnListenException:= MyOnListenException;

  OnSessionStart := MySessionStart;
  OnSessionEnd := MySessionEnd;

  // register context class
  ContextClass := TdjServerContext;
  Trace('Context class: ' + TdjServerContext.ClassName);
end;

procedure TdjHTTPServer.Trace(const S: string);
begin
{$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
{$ENDIF DARAJA_LOGGING}
end;

procedure TdjHTTPServer.MyOnException(AContext: TIdContext;
  AException: Exception);
begin
  if AException is EIdSilentException then Exit;
  if AException is EIdNotConnected then Exit;

{$IFDEF DARAJA_LOGGING}
  Logger.Warn(ClassName + ' (OnException): ' + AException.ClassName + ' '
    + AException.Message);
{$ENDIF DARAJA_LOGGING}
end;

procedure TdjHTTPServer.MyOnListenException(AThread: TIdListenerThread;
  AException: Exception);
begin
{$IFDEF DARAJA_LOGGING}
  Logger.Warn(ClassName + ' (OnListenException): ' + AException.ClassName + ' '
    + AException.Message);
{$ENDIF DARAJA_LOGGING}
end;

procedure TdjHTTPServer.MySessionStart(Sender: TIdHTTPSession);
begin
  Trace('Session start ' + Sender.RemoteHost);
end;

procedure TdjHTTPServer.MySessionEnd(Sender: TIdHTTPSession);
begin
  Trace('Session end ' + Sender.RemoteHost);
end;

procedure TdjHTTPServer.DoMaxConnectionsExceeded(AIOHandler: TIdIOHandler);
begin
{$IFDEF DARAJA_LOGGING}
  Logger.Warn(ClassName + ': MaxConnections exceeded');
{$ENDIF DARAJA_LOGGING}
end;

end.

