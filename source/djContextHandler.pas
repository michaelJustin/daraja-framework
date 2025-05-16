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

unit djContextHandler;

interface

// {$i IdCompilerDefines.inc}

uses
  djInterfaces, djServerContext, djContextConfig, djHandlerWrapper,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes,
  Classes;

const
  ROOT_CONTEXT = '';
  WEBAPPS = 'webapps';

type
  { TdjContext }

  (**
   * Handles requests within a specific context path.
   * Manages initialization parameters and context configuration.
   * @implements IContext
   * @implements IWriteableConfig
   *)
  TdjContext = class(TInterfacedObject, IContext, IWriteableConfig)
  private
    {$IFDEF DARAJA_LOGGING}
    ContextLogger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FConfig: IContextConfig;
    FContextPath: string;

    (**
     * a-z A-Z 0-9 . - _ ~ ! $ & ' ( ) * + , ; = : @
     * and percent-encoded characters
     *)
    procedure ValidateContextPath(const ContextPath: string);
  protected
    // IWriteableConfig interface
    procedure Add(const Key: string; const Value: string);
    procedure SetContext(const Context: IContext);
  protected
    // IContext interface
    procedure Init(const Config: IContextConfig);
    function GetContextConfig: IContextConfig;
    function GetContextPath: string;
    function GetInitParameter(const Key: string): string;
    function GetInitParameterNames: TdjStrings;
    procedure Log(const Msg: string);
  public
    (**
     * Initializes a new context with the specified path.
     *
     * @param ContextPath The path for this context.
     * @throws EWebComponentException If the context path contains invalid characters.
     *)
    constructor Create(const ContextPath: string);
  end;

  { TdjContextHandler }

  (**
   * Context handler.
   *)
  TdjContextHandler = class(TdjHandlerWrapper)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FContext: IContext;
    FConnectorNames: TStrings;
    FErrorHandler: IHandler;

    procedure Trace(const S: string);
    function GetContextPath: string;
    procedure SetErrorHandler(const Value: IHandler);
  protected
    // TdjLifeCycle overrides
    (**
     * Start the handler.
     * @sa TdjLifeCycle
     *)
    procedure DoStart; override;
    (**
     * Start the handler.
     * @sa TdjLifeCycle
     *)
    procedure DoStop; override;
  protected
    // IHandler interface
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  protected
    (**
     * Check if the Document matches this context.
     *
     * @param ConnectorName the connector name (like 'host:port'
     * @param Target the target URL document
     *
     * @return True if the context matches the connector name and target URL document
     *)
    function ContextMatches(const ConnectorName, Target: string): Boolean;

    (**
     * Creates connector name in the form 'host:port'
     *
     * @returns connector name
     *)
    function ToConnectorName(Context: TdjServerContext): string;
  public
    (**
     * Create a ContextHandler.
     *)
    constructor Create(const ContextPath: string); reintroduce;
    (**
     * Destructor.
     *)
    destructor Destroy; override;

    (**
     * The internal IContext field.
     *)
    function GetCurrentContext: IContext;

    (**
     * Set initialization parameter.
     *
     * @param Key init parameter name
     * @param Value init parameter value
     *)
    procedure SetInitParameter(const Key: string; const Value: string);

    // properties
    property ConnectorNames: TStrings read FConnectorNames;
    property ContextPath: string read GetContextPath;
    property ErrorHandler: IHandler read FErrorHandler write SetErrorHandler;
  end;

implementation

uses
  SysUtils;

{ TdjContext }

constructor TdjContext.Create(const ContextPath: string);
begin
  inherited Create;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  ContextLogger := TdjLoggerFactory.GetLogger(ContextPath);
  {$ENDIF DARAJA_LOGGING}

  ValidateContextPath(ContextPath);

  // TODO check why creation is needed here (actually it is accessed before init is called)
  FConfig := TdjContextConfig.Create;
  FContextPath := ContextPath;
end;

function TdjContext.GetContextConfig: IContextConfig;
begin
  Result := FConfig;
end;

function TdjContext.GetContextPath: string;
begin
  Result := FContextPath;
end;

function TdjContext.GetInitParameter(const Key: string): string;
begin
  Result := FConfig.GetInitParameter(Key);
end;

function TdjContext.GetInitParameterNames: TdjStrings;
begin
  Result := FConfig.GetInitParameterNames;
end;

procedure TdjContext.ValidateContextPath(const ContextPath: string);
var
  Ch: Char;
begin
  Assert(Pos('\', ContextPath) = 0);
  Assert(Pos('/', ContextPath) = 0);

  for Ch in ContextPath do
  begin
    case Ch of
      'a'..'z': ;
      'A'..'Z': ;
      '0'..'9': ;
      '.': ;
      '-': ;
      '_': ;
      '~': ;
      '!': ;
      '$': ;
      '&': ;
      '''': ;
      '(': ;
      ')': ;
      '*': ;
      '+': ;
      ',': ;
      ';': ;
      '=': ;
      ':': ;
      '@': ;
      '%': ;
    else
      raise EWebComponentException.CreateFmt('Invalid context name "%s"',
        [ContextPath]);
    end;
  end;
end;

procedure TdjContext.Init(const Config: IContextConfig);
begin
  // iow: does it decrease the reference count?
  FConfig := Config; // TODO check if it is ok to overwrite the field here with a new one
end;

procedure TdjContext.Add(const Key, Value: string);
begin
  (GetContextConfig as IWriteableConfig).Add(Key, Value);
end;

procedure TdjContext.SetContext(const Context: IContext);
begin
  // do nothing, we are in the context
end;

procedure TdjContext.Log(const Msg: string);
begin
  {$IFDEF DARAJA_LOGGING}
  ContextLogger.Info(Msg);
  {$ELSE}
  if System.IsConsole then
  begin
    WriteLn(Msg);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

{ TdjContextHandler }

constructor TdjContextHandler.Create(const ContextPath: string);
begin
  inherited Create;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjContextHandler.ClassName);
  {$ENDIF DARAJA_LOGGING}

  FContext := TdjContext.Create(ContextPath);
  FConnectorNames := TStringList.Create;
end;

destructor TdjContextHandler.Destroy;
begin
  FConnectorNames.Free;

  inherited;
end;

function TdjContextHandler.GetContextPath: string;
begin
  Result := GetCurrentContext.GetContextPath;
end;

function TdjContextHandler.GetCurrentContext: IContext;
begin
  Assert(FContext <> nil);
  Result := FContext;
end;

function TdjContextHandler.ToConnectorName(Context: TdjServerContext): string;
begin
  Result := Context.Binding.IP + ':' + IntToStr(Context.Binding.Port);
end;

procedure TdjContextHandler.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

function TdjContextHandler.ContextMatches(const ConnectorName, Target: string): Boolean;
begin
  Result := (Pos('/' + ContextPath + '/', Target) = 1)
    or ((ContextPath = ROOT_CONTEXT) and (Pos('/', Target) = 1));

  if Result and (ConnectorNames.Count > 0) then
  begin
    Result := ConnectorNames.IndexOf(ConnectorName) > -1;
  end;
end;

procedure TdjContextHandler.SetErrorHandler(const Value: IHandler);
begin
  FErrorHandler := Value;

  if Started then
  begin
    FErrorHandler.Start;
  end;
end;

procedure TdjContextHandler.SetInitParameter(const Key, Value: string);
begin
  CheckStarted;
  (FContext as IWriteableConfig).Add(Key, Value);
end;

procedure TdjContextHandler.DoStart;
begin
  inherited;

  {$IFDEF DARAJA_LOGGING}
  Logger.Info('Starting context ' + ContextPath);
  {$ENDIF DARAJA_LOGGING}
end;

procedure TdjContextHandler.DoStop;
begin
  {$IFDEF DARAJA_LOGGING}
  Logger.Info('Stopping context ' + ContextPath);
  {$ENDIF DARAJA_LOGGING}

  inherited;
end;

procedure TdjContextHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  Trace('Handle ' + Target);
end;

end.

