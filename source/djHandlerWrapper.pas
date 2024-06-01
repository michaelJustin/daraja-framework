(*

    Daraja HTTP Framework
    Copyright (C) Michael Justin

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


    You can be released from the requirements of the license by purchasing
    a commercial license. Buying such a license is mandatory as soon as you
    develop commercial activities involving the Daraja framework without
    disclosing the source code of your own applications. These activities
    include: offering paid services to customers as an ASP, shipping Daraja 
    with a closed source product.

*)

unit djHandlerWrapper;

interface

{$i IdCompilerDefines.inc}

uses
  djAbstractHandlerContainer, djServerContext, djInterfaces,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

(**
 * A HandlerWrapper acts as a IHandler but delegates the handle method
 * and life cycle events to a delegate.
 * This is primarily used to implement the Decorator pattern.
 *)
type
  TdjHandlerWrapper = class(TdjAbstractHandlerContainer)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    FHandler: IHandler;

    // getter / setter
    function GetHandler: IHandler;
    procedure SetHandler(const Value: IHandler);

    procedure Trace(const S: string);

  protected
    (**
     * Get a HTTP session.
     *)
    function GetSession(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Create: Boolean): TIdHTTPSession;

  public
    constructor Create; override;

    // IHandlerContainer interface

    (**
     * Add a handler to the container.
     * This implementation of AddHandler calls SetHandler with the passed
     * handler. If this HandlerWrapper had a previous wrapped handler, then
     * it is passed to a call to AddHandler on the passed handler.
     *
     * \param Handler the handler to be added
     *)
    procedure AddHandler(const Handler: IHandler); override;

    (**
     * Remove a handler from the container.
     *
     * \param Handler the handler to be removed
     *)
    procedure RemoveHandler(const Handler: IHandler); override;

    // ILifeCycle interface

    (**
     * Start the handler.
     *)
    procedure DoStart; override;

    (**
     * Stop the handler.
     *)
    procedure DoStop; override;

    // IHandler interface

    (**
     * Handle a HTTP request.
     *
     * \param Target Request target
     * \param Context HTTP server context
     * \param Request HTTP request
     * \param Response HTTP response
     * \throws EWebComponentException if an exception occurs that interferes with the component's normal operation
     *
     * \sa IHandler
     *)
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;

    // properties
    property Handler: IHandler read GetHandler write SetHandler;

  end;

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomTCPServer,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

{ TdjHandlerWrapper }

// getter / setter

function TdjHandlerWrapper.GetHandler: IHandler;
begin
  Result := FHandler;
end;

function TdjHandlerWrapper.GetSession(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse;
  const Create: Boolean): TIdHTTPSession;
var
  C: TIdServerContext;
  S: TIdCustomHTTPServer;
begin
  Result := Request.Session;

  if not Assigned(Result) and Create then
  begin
    C := Context as TIdServerContext;
    S := C.Server as TIdCustomHTTPServer;

    Result := S.CreateSession(Context, Response, Request);

    Trace('Created a session');
  end;
end;

procedure TdjHandlerWrapper.SetHandler(const Value: IHandler);
begin
  FHandler := Value;
end;

// IHandlerContainer

procedure TdjHandlerWrapper.AddHandler(const Handler: IHandler);
var
  Old: IHandler;
  C: IHandlerContainer;
begin
  Old := FHandler;

  if Assigned(Old) then
  begin
    Supports(Handler, IHandlerContainer, C);
    if not Assigned(C) then
    begin
     raise Exception.Create('Can not add handler');
    end;
  end;

  SetHandler(Handler);

  if Assigned(C) then
  begin
    C.AddHandler(Old);
  end;
end;

procedure TdjHandlerWrapper.RemoveHandler(const Handler: IHandler);
var
  Old: IHandler;
  C: IHandlerContainer;
begin
  Old := FHandler;

  if Assigned(Old) then
  begin
    Supports(Handler, IHandlerContainer, C);
    if Assigned(C) then
    begin
      C.RemoveHandler(Handler);
    end;
  end else if Assigned(Old) and (Handler = Old) then
  begin
    SetHandler(nil)
  end
  else
  begin
    raise Exception.Create('Can not remove handler');
  end;

end;

// IHandler

procedure TdjHandlerWrapper.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  if (Assigned(Handler)) and Handler.IsStarted then
  begin
    Handler.Handle(Target, Context, Request, Response);
  end;
end;

// ILifeCycle

constructor TdjHandlerWrapper.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
{$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjHandlerWrapper.ClassName);
{$ENDIF DARAJA_LOGGING}

{$IFDEF LOG_CREATE}
  Trace('Created');
{$ENDIF}
end;

procedure TdjHandlerWrapper.Trace(const S: string);
begin
{$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
{$ENDIF DARAJA_LOGGING}
end;

procedure TdjHandlerWrapper.DoStart;
begin
  if Assigned(FHandler) then
  begin
    FHandler.Start;
  end;

  inherited;
end;

procedure TdjHandlerWrapper.DoStop;
begin
  inherited;

  if Assigned(Handler) then
  begin
    Handler.Stop;
  end;
end;

end.

