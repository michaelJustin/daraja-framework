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

unit djWebComponentContextHandler;

interface



uses
  djContextHandler, djWebComponentHandler, djServerContext,
  djWebComponentHolder, djWebComponent, djWebFilterHolder, djWebFilter,
  djInterfaces,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes;

type
  { TdjWebComponentContextHandler }

  {*
   * Context Handler for Web Components (and Web Filters).
   *}
  TdjWebComponentContextHandler = class(TdjContextHandler)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    WebComponentHandler: TdjWebComponentHandler;
    AutoStartSession: Boolean;
  protected
    // IHandler interface
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  protected
    {*
     * @param Target Request target
     * @param Context HTTP server context
     * @param Request HTTP request
     * @param Response HTTP response
     *}
    procedure DoHandle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
  public
    {*
     * Constructor.
     *
     * @param ContextPath the context path
     * @param Sessions enable HTTP sessions
     *}
    constructor Create(const ContextPath: string; Sessions: Boolean = False); overload;

    {*
     * Destructor.
     *}
    destructor Destroy; override;

    {*
     * Add a Web Component.
     *
     * Ownership: the returned holder belongs to the context. Use it only for
     * further configuration (init parameters etc.); do not free it or keep it
     * past the lifetime of the context.
     *
     * @param ComponentClass WebComponent class
     * @param UrlPattern path specification
     * @return the Web Component holder, which can be used for further configuration.
     * @throws EWebComponentException if the Web Component can not be added
     *}
    function AddWebComponent(ComponentClass: TdjWebComponentClass;
      const UrlPattern: string): TdjWebComponentHolder; overload;

    {*
     * Add a Web Component.
     *
     * @param ComponentClass WebComponent class
     * @param UrlPattern path specification
     * @return the Web Component holder, which can be used for further configuration.
     * @throws EWebComponentException if the Web Component can not be added
     *}
    function Add(ComponentClass: TdjWebComponentClass;
      const UrlPattern: string): TdjWebComponentHolder; overload;

    {*
     * Add a Web Filter, specifying a WebFilter class
     *
     * Ownership: the returned holder belongs to the context. Use it only for
     * further configuration; do not free it or keep it past the lifetime of the
     * context. If the filter cannot be added the holder is freed before the
     * exception propagates.
     *
     * @param FilterClass WebFilter class
     * @param UrlPattern path specification
     * @return the Web Filter holder, which can be used for further configuration.
     * @throws Exception if the WebFilter can not be added
     *}
    function AddWebFilter(FilterClass: TdjWebFilterClass;
      const UrlPattern: string): TdjWebFilterHolder; overload;

    {*
     * Add a Web Filter, specifying a WebFilter class
     *
     * @param FilterClass WebFilter class
     * @param UrlPattern path specification
     * @return the Web Filter holder, which can be used for further configuration.
     * @throws Exception if the WebFilter can not be added
     *}
    function Add(FilterClass: TdjWebFilterClass;
      const UrlPattern: string): TdjWebFilterHolder; overload;
  end;

implementation

uses
  Classes, SysUtils;

{ TdjWebComponentContextHandler }

constructor TdjWebComponentContextHandler.Create(const ContextPath: string;
  Sessions: Boolean);
begin
  inherited Create(ContextPath);

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjWebComponentContextHandler);
  {$ENDIF DARAJA_LOGGING}

  Self.AutoStartSession := Sessions;

  WebComponentHandler := TdjWebComponentHandler.Create;

  WebComponentHandler.SetContext(Self.GetCurrentContext);

  inherited AddHandler(WebComponentHandler);

  {$IFDEF LOG_CREATE}
  Logger.Trace('Created');
  {$ENDIF}
end;

destructor TdjWebComponentContextHandler.Destroy;
begin
  {$IFDEF LOG_DESTROY}
  Logger.Trace('Destroy');
  {$ENDIF}

  inherited;
end;

function TdjWebComponentContextHandler.AddWebComponent(ComponentClass: TdjWebComponentClass;
  const UrlPattern: string): TdjWebComponentHolder;
var
  Holder: TdjWebComponentHolder;
begin
  Holder := WebComponentHandler.FindHolder(ComponentClass);

  if Holder = nil then
  begin
    // create new holder
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Add new holder for Web Component %s',
      [ComponentClass.ClassName]);
    {$ENDIF DARAJA_LOGGING}

    Holder := WebComponentHandler.AddWebComponent(ComponentClass, UrlPattern);
    // set context of Holder to propagate it to WebComponentConfig
    Holder.SetContext(GetCurrentContext);
  end
  else
  begin
    // add the URL pattern
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Holder found for Web Component %s, add URL pattern %s',
      [ComponentClass.ClassName, UrlPattern]);
    {$ENDIF DARAJA_LOGGING}

    WebComponentHandler.AddWithMapping(Holder, UrlPattern);
  end;

  Result := Holder;
end;

function TdjWebComponentContextHandler.Add(ComponentClass: TdjWebComponentClass;
  const UrlPattern: string): TdjWebComponentHolder;
begin
  Result := AddWebComponent(ComponentClass, UrlPattern);
end;

function TdjWebComponentContextHandler.AddWebFilter(
  FilterClass: TdjWebFilterClass; const UrlPattern: string): TdjWebFilterHolder;
var
  Holder: TdjWebFilterHolder;
begin
  Holder := TdjWebFilterHolder.Create(FilterClass);
  try
    WebComponentHandler.AddWebFilter(Holder, UrlPattern);
  except
    Holder.Free;
    raise;
  end;
  Result := Holder;
end;

function TdjWebComponentContextHandler.Add(FilterClass: TdjWebFilterClass;
  const UrlPattern: string): TdjWebFilterHolder;
begin
  Result := AddWebFilter(FilterClass, UrlPattern);
end;

procedure TdjWebComponentContextHandler.DoHandle(const Target: string;
  Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse);
begin
  {$IFDEF DARAJA_LOGGING}
  Logger.Trace('Context %s handles %s', [ContextPath, Target]);
  {$ENDIF DARAJA_LOGGING}

  (WebComponentHandler as IHandler).Handle(Target, Context, Request, Response);
end;

procedure TdjWebComponentContextHandler.Handle(const Target: string;
  Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse);
begin
  if not ContextMatches(ToConnectorName(Context), Target) then
  begin
    Exit;
  end;

  if AutoStartSession then
  begin
    GetSession(Context, Request, Response, True);
  end;

  DoHandle(Target, Context, Request, Response);
end;


end.

