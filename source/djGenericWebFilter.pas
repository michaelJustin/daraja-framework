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

unit djGenericWebFilter;

interface

// {$i IdCompilerDefines.inc}

uses
  djInterfaces, djWebComponentConfig, djWebFilterConfig,
  djServerContext, djTypes,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  Classes;

type
  { TdjGenericWebFilter }

  (**
   * A generic web filter class implementing the IWebFilter interface.
   * 
   * This class provides functionality to filter web requests or responses
   * as part of the Daraja framework.
   *)
  TdjGenericWebFilter = class(TInterfacedObject, IWebFilter)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FConfig: IWebFilterConfig;
    procedure Trace(const S: string);
  public
    // IWebFilter interface todo protected?
    procedure Init(const Config: IWebFilterConfig); overload; virtual;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain); virtual;
    procedure DestroyFilter; virtual;
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
     * A convenience method which can be overridden so that there is no need
     * to call inherited Init(config).
     *)
    procedure Init; overload; virtual;

    (**
     * Returns the configuration for this filter.
     *
     * @return The filter configuration object
     * @throws EWebComponentException if the filter is not initialized
     *)
    function GetWebFilterConfig: IWebFilterConfig;

    property Config: IWebFilterConfig read GetWebFilterConfig;
  end;

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomTCPServer;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

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

procedure TdjGenericWebFilter.Init;
begin
  Trace('Init');
  // this is a convenience method which can be overridden so that there is no need
  // to call inherited Init(config).
end;

procedure TdjGenericWebFilter.Init(const Config: IWebFilterConfig);
begin
  Trace('Init');

  Assert(Assigned(Config));
  Assert(Assigned(Config.GetContext));
  Assert(Assigned(Config.GetContext.GetContextConfig));
  Assert(not Assigned(FConfig));

  FConfig := Config;

  Init;
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
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace('DoFilter');
  end;
  {$ENDIF DARAJA_LOGGING}
end;

function TdjGenericWebFilter.GetWebFilterConfig: IWebFilterConfig;
begin
  if not Assigned(FConfig) then
  begin
    raise EWebComponentException.Create('Filter is not initialized.');
  end;

  Result := FConfig;
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

