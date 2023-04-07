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

unit djWebFilterHolder;

interface

{$i IdCompilerDefines.inc}

uses
  djWebFilter, djGenericHolder, djServerContext, djWebFilterConfig,
  djTypes, djInterfaces,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  Classes;

type
  (**
   * Holds a WebFilter (class reference) and configuration info.
   *
   * A WebComponent instance will be created 'on the fly'
   * when the WebComponent property is accessed.
   * (lazy instantiation).
   *)

  { TdjWebFilterHolder }

  TdjWebFilterHolder = class(TdjGenericHolder<TdjWebFilter>)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FConfig: TdjWebFilterConfig;
    FClass: TdjWebFilterClass;
    FWebFilter: TdjWebFilter;
    function GetClass: TdjWebFilterClass;
    procedure Trace(const S: string);
    function GetWebFilter: TdjWebFilter;
  public
    constructor Create(const WebFilterClass: TdjWebFilterClass);
    destructor Destroy; override;

    (**
     * Start the filter.
     *)
    procedure DoStart; override;
    (**
     * Stop the filter.
     *)
     procedure DoStop; override;
     procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain);
    // properties
    (**
     * The filter class.
     *)
    property WebFilterClass: TdjWebFilterClass read GetClass;
    (**
     * The instance of the filter.
     *)
    property WebFilter: TdjWebFilter read GetWebFilter;
  end;

implementation

uses

  SysUtils;

{ TdjWebFilterHolder }

constructor TdjWebFilterHolder.Create(const WebFilterClass: TdjWebFilterClass);
begin
  inherited Create(WebFilterClass);

  FConfig := TdjWebFilterConfig.Create;
  FClass := WebFilterClass;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjWebFilterHolder.ClassName);
  {$ENDIF DARAJA_LOGGING}

  {$IFDEF LOG_CREATE}Trace('Created');{$ENDIF}
end;

destructor TdjWebFilterHolder.Destroy;
begin
  {$IFDEF LOG_DESTROY}Trace('Destroy');{$ENDIF}

  FConfig.Free;

  inherited Destroy;
end;

procedure TdjWebFilterHolder.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
    if Logger.IsTraceEnabled then
    begin
      Logger.Trace(S);
    end;
  {$ENDIF DARAJA_LOGGING}
end;

function TdjWebFilterHolder.GetClass: TdjWebFilterClass;
begin
  Result := FClass;
end;

function TdjWebFilterHolder.GetWebFilter: TdjWebFilter;
begin
  Result := FWebFilter;
end;

procedure TdjWebFilterHolder.DoStart;
begin
  inherited DoStart;

  CheckStarted;

  // ... config

  Trace('Create instance of class ' + FClass.ClassName);
  FWebFilter := FClass.Create;

  try
    Trace('Init Web Filter "' + Name + '"');
    WebFilter.Init(FConfig);
  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Warn(
        Format('Could not start "%s". Init method raised %s with message "%s".', [
        FClass.ClassName, E.ClassName, E.Message]),
        E);
      {$ENDIF DARAJA_LOGGING}

      Trace('Free the Web Filter  "' + Name + '"');
      WebFilter.Free;
      raise;
    end;
  end;
end;

procedure TdjWebFilterHolder.DoStop;
begin
  Trace('Destroy instance of ' + FClass.ClassName);
  try
    WebFilter.Free;
  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Warn('TdjWebFilterHolder.Stop: ' + E.Message, E);
      {$ENDIF DARAJA_LOGGING}
      // TODO raise ?;
    end;
  end;

  inherited;
end;

procedure TdjWebFilterHolder.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  CheckStopped;

  WebFilter.DoFilter(Context, Request, Response, Chain);
end;

end.



