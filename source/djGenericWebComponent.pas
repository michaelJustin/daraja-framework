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

unit djGenericWebComponent;

interface

{$i IdCompilerDefines.inc}

uses
  djInterfaces, djWebComponentConfig, djServerContext, djTypes,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  Classes;

type
  (**
   * Defines a generic Web Component.
   *)
  TdjGenericWebComponent = class(TInterfacedObject, IWebComponent)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    FConfig: IWebComponentConfig;

    procedure Trace(const S: string);

  protected
    (**
     * Handle a HTTP request.
     *
     * The status code of the response always should be set for a component
     * that throws or sends an error.
     *
     * \note a custom Web Component should not override this method.
     *
     * \param Context HTTP server context
     * \param Request HTTP request
     * \param Response HTTP response
     * \throws EWebComponentException if an exception occurs that interferes with the component's normal operation
     *)
    procedure Service({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse); virtual;

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
      * Called by the container on startup.
      *
      * \note if this method is overridden, the overriding code
      * must also call inherited Init.
      *
      *
      * \param Config the configuration
      *
      * \throws EWebComponentException if initialization failed
      *)
    procedure Init(const Config: IWebComponentConfig); virtual;

    (**
     * Get or create a HTTP session.
     *
     * \note it requires the current TdjServerContext so calling it from one of the
     * HTTP method handlers is not possible. It can be called from
     * the Servive method.
     *
     * \note if the context was created with the Auto Session option,
     * this method will always return a session independent of the Create parameter
     *
     * \param Context HTTP server context
     * \param Request HTTP request
     * \param Response HTTP response
     * \param Create if True, create a session if no one exists
     *
     * \returns HTTP session
     *)
    function GetSession(Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse;
      const Create: Boolean = True): TIdHTTPSession;

    (**
     * Returns a IWebComponentConfig object,
     * which contains initialization parameters for this component.
     *
     * \throws EWebComponentException if the method is called before
     * the component has been initialized.
     *)
    function GetWebComponentConfig: IWebComponentConfig;
    // todo move to private ?

    // properties

    property Config: IWebComponentConfig read GetWebComponentConfig;

  end;

implementation

uses
  IdCustomTCPServer;

{ TdjGenericWebComponent }

constructor TdjGenericWebComponent.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
{$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjGenericWebComponent.ClassName);
{$ENDIF DARAJA_LOGGING}

{$IFDEF LOG_CREATE}
  Trace('Created');
{$ENDIF}
end;

destructor TdjGenericWebComponent.Destroy;
begin
{$IFDEF LOG_DESTROY}
  Trace('Destroy');
{$ENDIF}

  inherited;
end;

procedure TdjGenericWebComponent.Trace(const S: string);
begin
{$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
{$ENDIF DARAJA_LOGGING}
end;

function TdjGenericWebComponent.GetSession(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse;
  const Create: Boolean): TIdHTTPSession;
var
  C: TIdServerContext;
  S: TIdCustomHTTPServer;
begin
  Result := Request.Session;

  if not Assigned(Result) and Create then
  begin
    Trace('Create a new session');
    C := Context as TIdServerContext;
    S := C.Server as TIdCustomHTTPServer;
    Result := S.CreateSession(Context, Response, Request);
  end;
end;

// getter / setter -----------------------------------------------------------

function TdjGenericWebComponent.GetWebComponentConfig: IWebComponentConfig;
begin
  if not Assigned(FConfig) then
  begin
    raise EWebComponentException.Create('Component is not initialized.');
  end;

  Result := FConfig;
end;

procedure TdjGenericWebComponent.Init(const Config: IWebComponentConfig);
begin
  Trace('Init');

  Assert(Assigned(Config));
  Assert(Assigned(Config.GetContext));
  Assert(Assigned(Config.GetContext.GetContextConfig));
  Assert(not Assigned(FConfig));

  FConfig := Config;
end;

procedure TdjGenericWebComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  Trace('Service');
end;

end.

