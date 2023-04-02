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

unit djGenericWebFilter;

interface

{$i IdCompilerDefines.inc}

uses
  djInterfaces, djWebComponentConfig, djServerContext, djTypes,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  IdCustomHTTPServer, Classes;

type
  (**
   * Defines a generic Web Filter.
   *)

  { TdjGenericWebFilter }

  TdjGenericWebFilter = class(TInterfacedObject, IWebFilter)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    procedure Trace(const S: string);
  public
    (**
     * Constructor.
     *)
    constructor Create;

    (**
     * Destructor.
     *)
    destructor Destroy; override;

    (**
     * The doFilter method of the Filter is called by the container each time a request/response pair is passed through the chain due to a client request for a resource at the end of the chain. The FilterChain passed in to this method allows the Filter to pass on the request and response to the next entity in the chain.
     *)
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse (*; Chain: IWebFilterChain *)); virtual;

    procedure DestroyFilter;
  end;

implementation

uses
  IdCustomTCPServer;

{ TdjGenericWebFilter }

constructor TdjGenericWebFilter.Create;
begin
  inherited;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjGenericWebFilter.ClassName);
  {$ENDIF DARAJA_LOGGING}

  {$IFDEF LOG_CREATE}
  Trace('Created');
  {$ENDIF}
end;

destructor TdjGenericWebFilter.Destroy;
begin
  {$IFDEF LOG_DESTROY}
  Trace('Destroy');
  {$ENDIF}
  inherited;
end;

procedure TdjGenericWebFilter.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

procedure TdjGenericWebFilter.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace('DoFilter');
  end;
  {$ENDIF DARAJA_LOGGING}
end;

procedure TdjGenericWebFilter.DestroyFilter;
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace('DestroyFilter');
  end;
  {$ENDIF DARAJA_LOGGING}
end;

end.

