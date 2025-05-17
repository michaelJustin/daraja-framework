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

unit djWebComponentHolder;

interface

// {$i IdCompilerDefines.inc}

uses
  djWebComponent, djGenericHolder, djLifeCycle, djInterfaces,
  djWebComponentConfig, djServerContext, djTypes,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  Classes;

type
  { TdjWebComponentHolder }

  (**
   * Holds a WebComponent and configuration data.
   *)
  TdjWebComponentHolder = class(TdjGenericHolder<TdjWebComponent>)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FConfig: IWebComponentConfig;
    FClass: TdjWebComponentClass;
    FWebComponent: TdjWebComponent;

    procedure Trace(const S: string);
    function GetWebComponent: TdjWebComponent;
    function GetClass: TdjWebComponentClass;
  protected
    // TdjLifeCycle overrides
    procedure DoStart; override;
    procedure DoStop; override;
  public
    // IHandler interface
    procedure Handle(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse);
  public
    (**
     * Constructor.
     *
     * @param WebComponentClass the Web Component class
     *)
    constructor Create(WebComponentClass: TdjWebComponentClass); overload;

    (**
     * Destructor.
     *)
    destructor Destroy; override;

    (**
     * Get the context.
     *)
    function GetContext: IContext;

    (**
     * Set the context.
     *
     * @param Context the context
     *)
    procedure SetContext(const Context: IContext);

    (**
     * Set initialization parameter.
     *
     * @param Key init parameter name
     * @param Value init parameter value
     *)
    procedure SetInitParameter(const Key: string; const Value: string);

    // properties
    (**
     * The Web Component Class.
     *)
    property WebComponentClass: TdjWebComponentClass read GetClass;

    (**
     * The instance of the Web Component.
     *)
    property WebComponent: TdjWebComponent read GetWebComponent;
  end;

implementation

uses
  djWebComponentHandler,
  SysUtils;

{ TdjWebComponentHolder }

constructor TdjWebComponentHolder.Create(WebComponentClass: TdjWebComponentClass);
begin
  inherited Create(WebComponentClass);

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjWebComponentHolder.ClassName);
  {$ENDIF DARAJA_LOGGING}

  FConfig := TdjWebComponentConfig.Create;
  FClass := WebComponentClass;

  {$IFDEF LOG_CREATE}Trace('Created');{$ENDIF}
end;

destructor TdjWebComponentHolder.Destroy;
begin
  {$IFDEF LOG_DESTROY}Trace('Destroy');{$ENDIF}
  inherited;
end;

procedure TdjWebComponentHolder.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

function TdjWebComponentHolder.GetClass: TdjWebComponentClass;
begin
  Result := FClass;
end;

function TdjWebComponentHolder.GetContext: IContext;
begin
  Result := FConfig.GetContext;
end;

function TdjWebComponentHolder.GetWebComponent: TdjWebComponent;
begin
  Result := FWebComponent;
end;

procedure TdjWebComponentHolder.SetContext(const Context: IContext);
begin
  Assert(Context <> nil);
  Assert(Context.GetContextConfig <> nil); // TODO check this happens before Context init is called
  (FConfig as IWriteableConfig).SetContext(Context);
end;

procedure TdjWebComponentHolder.SetInitParameter(const Key: string;
  const Value: string);
begin
  (FConfig as IWriteableConfig).Add(Key, Value);
end;

procedure TdjWebComponentHolder.DoStart;
begin
  inherited;

  CheckStarted;

  Assert(FConfig <> nil);
  Assert(FConfig.GetContext <> nil);
  Assert(FConfig.GetContext.GetContextConfig <> nil);

  Trace('Create instance of class ' + FClass.ClassName);
  FWebComponent := FClass.Create;

  try
    Trace('Init Web Component "' + Name + '"');
    WebComponent.Init(FConfig);
  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Warn(
        Format('Could not start "%s". Init method raised %s with message "%s".', [
        FClass.ClassName, E.ClassName, E.Message]),
        E);
      {$ENDIF DARAJA_LOGGING}

      Trace('Stop the Web Component  "' + Name + '"');

      Self.Stop;
    end;
  end;
end;

procedure TdjWebComponentHolder.DoStop;
begin
  Trace('Destroy instance of ' + FClass.ClassName);
  try
    WebComponent.Free;
  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Warn('TdjWebComponentHolder.Stop: ' + E.Message, E);
      {$ENDIF DARAJA_LOGGING}
      // TODO raise ?;
    end;
  end;

  inherited;
end;

procedure TdjWebComponentHolder.Handle(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  TdjWebComponentHandler.InvokeService(WebComponent, Context, Request, Response);
end;

end.
