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
  Classes, Generics.Collections;

type
  (**
   * Holds a WebFilter and configuration data.
   *)

  { TdjWebFilterHolder }

  TdjWebFilterHolder = class(TdjGenericHolder<TdjWebFilter>)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FConfig: IWebFilterConfig;
    FClass: TdjWebFilterClass;
    FWebFilter: TdjWebFilter;
    function GetClass: TdjWebFilterClass;
    procedure Trace(const S: string);
  public
    constructor Create(WebFilterClass: TdjWebFilterClass);
    destructor Destroy; override;

    (**
     * Set the context.
     *
     * \param Context the Web Filter context
     *)
    procedure SetContext(const Context: IContext);

    (**
     * Set initialization parameter.
     *
     * \param Key init parameter name
     * \param Value init parameter value
     *)
    procedure SetInitParameter(const Key: string; const Value: string);

    (**
     * Start the filter.
     *)
    procedure DoStart; override;
    (**
     * Stop the filter.
     *)
     procedure DoStop; override;
     procedure DoFilter(Context: TdjServerContext;
       Request: TdjRequest; Response: TdjResponse;
       const Chain: IWebFilterChain);
    // properties
    (**
     * The filter class.
     *)
    property WebFilterClass: TdjWebFilterClass read GetClass;
    (**
     * The instance of the filter.
     *)
    property WebFilter: TdjWebFilter read FWebFilter;
  end;

  // note Delphi 2009 AVs if it is a TObjectList<>
  // see http://stackoverflow.com/questions/289825/why-is-tlist-remove-producing-an-eaccessviolation-error
  // for a workaround
  // use  TdjWeFilterHolders.Create(TComparer<TdjWebFilterHolder>.Default);
  TdjWebFilterHolders = class(TObjectList<TdjWebFilterHolder>);

implementation

uses
  SysUtils;

{ TdjWebFilterHolder }

constructor TdjWebFilterHolder.Create(WebFilterClass: TdjWebFilterClass);
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
  inherited;
end;

procedure TdjWebFilterHolder.SetContext(const Context: IContext);
begin
  Assert(Context <> nil);
  Assert(FConfig <> nil);

  (FConfig as IWriteableConfig).SetContext(Context);
end;

procedure TdjWebFilterHolder.SetInitParameter(const Key: string;
  const Value: string);
begin
  (FConfig as IWriteableConfig).Add(Key, Value);
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

procedure TdjWebFilterHolder.DoStart;
begin
  inherited;

  CheckStarted;

  Assert(FConfig <> nil);
  Assert(FConfig.GetContext <> nil);
  Assert(FConfig.GetContext.GetContextConfig <> nil);

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
    // Destroy (and ensure that Free will be called)
    try
      WebFilter.DestroyFilter;
    except
      on E: Exception do
      begin
        {$IFDEF DARAJA_LOGGING}
        Logger.Warn('TdjWebFilterHolder.Stop: ' + E.Message, E);
        {$ENDIF DARAJA_LOGGING}
      end;
    end;

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



