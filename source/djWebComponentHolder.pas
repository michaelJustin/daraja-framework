{***

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

***}

unit djWebComponentHolder;

interface

uses
  djWebComponent, djGenericHolder, djLifeCycle, djInterfaces,
  djWebComponentConfig, djServerContext, djTypes,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  Classes;

type
  { TdjWebComponentHolder }

  {*
   * Holds a WebComponent and configuration data.
   *}
  TdjWebComponentHolder = class(TdjGenericHolder<TdjWebComponent>)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FConfig: IWebComponentConfig;
    FClass: TdjWebComponentClass;
    FWebComponent: TdjWebComponent;
    function GetWebComponent: TdjWebComponent;
    function GetClass: TdjWebComponentClass;
  protected
    // TdjLifeCycle overrides
    /// \private
    procedure DoStart; override;
    /// \private
    procedure DoStop; override;
  public
    // IHandler interface
    /// \private
    procedure Handle(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse);
  public
    {*
     * Constructor.
     *
     * @param WebComponentClass the Web Component class
     *}
    constructor Create(WebComponentClass: TdjWebComponentClass); overload;

    {*
     * Destructor.
     *}
    destructor Destroy; override;

    {*
     * Get the context.
     *}
    function GetContext: IContext;

    {*
     * Set the context.
     *
     * @param Context the context
     *}
    procedure SetContext(const Context: IContext);

    {*
     * Set initialization parameter.
     *
     * @param Key init parameter name (case-sensitive)
     * @param Value init parameter value
     * @throws EWebComponentException if the key is already set
     *}
    procedure SetInitParameter(const Key: string; const Value: string);

    // properties
    {*
     * The Web Component Class.
     *}
    property WebComponentClass: TdjWebComponentClass read GetClass;

    {*
     * The instance of the Web Component.
     *}
    property WebComponent: TdjWebComponent read GetWebComponent;
  end;

implementation /// \cond

uses
  djWebComponentHandler,
  SysUtils;

{ TdjWebComponentHolder }

constructor TdjWebComponentHolder.Create(WebComponentClass: TdjWebComponentClass);
begin
  inherited Create(WebComponentClass);

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjWebComponentHolder);
  {$ENDIF DARAJA_LOGGING}

  FConfig := TdjWebComponentConfig.Create;
  FClass := WebComponentClass;


end;

destructor TdjWebComponentHolder.Destroy;
begin
  // Release the FConfig interface reference
  FConfig := nil;

  inherited;
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

  CheckNotStarted;

  Assert(FConfig <> nil);
  Assert(FConfig.GetContext <> nil);
  Assert(FConfig.GetContext.GetContextConfig <> nil);

  {$IFDEF DARAJA_LOGGING}
  Logger.Trace('Create instance of class %s', [FClass.ClassName]);
  {$ENDIF DARAJA_LOGGING}

  FWebComponent := FClass.Create;

  try
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Init Web Component "%s"', [Name]);
    {$ENDIF DARAJA_LOGGING}

    WebComponent.Init(FConfig);
  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Warn('Could not start "%s". Init method raised %s with message "%s".', [
        FClass.ClassName, E.ClassName, E.Message]);
      {$ENDIF DARAJA_LOGGING}

      {$IFDEF DARAJA_LOGGING}
      Logger.Trace('Stop the Web Component "%s"', [Name]);
      {$ENDIF DARAJA_LOGGING}

      Self.Stop;
    end;
  end;
end;

procedure TdjWebComponentHolder.DoStop;
begin
  try
    WebComponent.Free;
    FWebComponent := nil;
  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Warn('TdjWebComponentHolder.Stop: ' + E.Message, E);
      {$ENDIF DARAJA_LOGGING}
      // Swallowed on purpose: TdjLifeCycle.Stop logs and continues so a
      // failing stop cannot leave the component half-started.
    end;
  end;

  inherited;
end;

procedure TdjWebComponentHolder.Handle(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  TdjWebComponentHandler.InvokeService(WebComponent, Context, Request, Response);
end;

end. /// \endcond
