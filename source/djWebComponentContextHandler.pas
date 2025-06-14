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

unit djWebComponentContextHandler;

interface

// {$i IdCompilerDefines.inc}

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

  (**
   * Context Handler for Web Components (and Web Filters).
   *)
  TdjWebComponentContextHandler = class(TdjContextHandler)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    WebComponentHandler: TdjWebComponentHandler;
    AutoStartSession: Boolean;

    procedure Trace(const S: string);
  protected
    // IHandler interface
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  protected
    (**
     * @param Target Request target
     * @param Context HTTP server context
     * @param Request HTTP request
     * @param Response HTTP response
     *)
    procedure DoHandle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
    (**
     * Add a Web Component.
     *
     * @param Holder holds information about the Web Component
     * @param UrlPattern path specification
     * @throws EWebComponentException if the Web Component can not be added
     * @deprecated for removal
     *)
    procedure AddWebComponent(Holder: TdjWebComponentHolder;
      const UrlPattern: string); overload;
    (**
     * Add a Web Filter Holder.
     *
     * @param Holder holds information about the Web Filter
     * @param UrlPattern path specification
     * @throws Exception if the Web Filter can not be added
     * @deprecated for removal
     *)
    procedure AddWebFilter(Holder: TdjWebFilterHolder;
      const UrlPattern: string); overload; deprecated;
  public
    (**
     * Constructor.
     *
     * @param ContextPath the context path
     * @param Sessions enable HTTP sessions
     *)
    constructor Create(const ContextPath: string; Sessions: Boolean = False); overload;

    (**
     * Destructor.
     *)
    destructor Destroy; override;

    (**
     * Add a Web Component.
     *
     * @param ComponentClass WebComponent class
     * @param UrlPattern path specification
     * @return the Web Component holder, which can be used for further configuration.
     * @throws EWebComponentException if the Web Component can not be added
     *)
    function AddWebComponent(ComponentClass: TdjWebComponentClass;
      const UrlPattern: string): TdjWebComponentHolder; overload;

    (**
     * Add a Web Component.
     *
     * @param ComponentClass WebComponent class
     * @param UrlPattern path specification
     * @return the Web Component holder, which can be used for further configuration.
     * @throws EWebComponentException if the Web Component can not be added
     *)
    function Add(ComponentClass: TdjWebComponentClass;
      const UrlPattern: string): TdjWebComponentHolder; overload;

    (**
     * Add a Web Component, specifying multiple URL patterns
     *
     * @param ComponentClass WebComponent class
     * @param UrlPattern path specifications
     * @return the Web Component holder, which can be used for further configuration.
     * @throws EWebComponentException if the Web Component can not be added
     *)
    function Add(ComponentClass: TdjWebComponentClass;
      const UrlPatterns: array of string): TdjWebComponentHolder; overload;

    (**
     * Add a Web Filter, specifying a WebFilter class
     *
     * @param FilterClass WebFilter class
     * @param UrlPattern path specification
     * @return the Web Filter holder, which can be used for further configuration.
     * @throws Exception if the WebFilter can not be added
     *)
    function AddWebFilter(FilterClass: TdjWebFilterClass;
      const UrlPattern: string): TdjWebFilterHolder; overload;

    (**
     * Add a Web Filter, specifying a WebFilter class
     *
     * @param FilterClass WebFilter class
     * @param UrlPattern path specification
     * @return the Web Filter holder, which can be used for further configuration.
     * @throws Exception if the WebFilter can not be added
     *)
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
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjWebComponentContextHandler.ClassName);
  {$ENDIF DARAJA_LOGGING}

  Self.AutoStartSession := Sessions;

  WebComponentHandler := TdjWebComponentHandler.Create;

  WebComponentHandler.SetContext(Self.GetCurrentContext);

  inherited AddHandler(WebComponentHandler);

{$IFDEF LOG_CREATE}
  Trace('Created');
{$ENDIF}
end;

destructor TdjWebComponentContextHandler.Destroy;
begin
{$IFDEF LOG_DESTROY}
  Trace('Destroy');
{$ENDIF}

  inherited;
end;

procedure TdjWebComponentContextHandler.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
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
    Trace(Format('Add new holder for Web Component %s',
      [ComponentClass.ClassName]));
    Holder := WebComponentHandler.AddWebComponent(ComponentClass, UrlPattern);
    // set context of Holder to propagate it to WebComponentConfig
    Holder.SetContext(GetCurrentContext);
  end
  else
  begin
    // add the URL pattern
    Trace(Format('Holder found for Web Component %s, add URL pattern %s',
      [ComponentClass.ClassName, UrlPattern]));
    WebComponentHandler.AddWithMapping(Holder, UrlPattern);
  end;

  Result := Holder;
end;

function TdjWebComponentContextHandler.Add(ComponentClass: TdjWebComponentClass;
  const UrlPattern: string): TdjWebComponentHolder;
begin
  Result := AddWebComponent(ComponentClass, UrlPattern);
end;

function TdjWebComponentContextHandler.Add(
  ComponentClass: TdjWebComponentClass; const UrlPatterns: array of string): TdjWebComponentHolder;
var
  UrlPattern: string;
begin
  if Length(UrlPatterns) = 0 then
    raise EWebComponentException.Create('No URL patterns given');

  Result := nil; // mtch

  for UrlPattern in UrlPatterns do
  begin
    Result := Add(ComponentClass, UrlPattern);
  end;
end;

procedure TdjWebComponentContextHandler.AddWebComponent(Holder: TdjWebComponentHolder;
  const UrlPattern: string);
begin
  // Holder can not be reused.
  // Create a new Holder if a Web Component should handle other UrlPatterns.
  if Holder.GetContext <> nil then
  begin
    raise EWebComponentException.CreateFmt(
      'Web Component %s is already installed in context %s',
      [Holder.WebComponentClass.ClassName, Holder.GetContext.GetContextPath]
      );
  end;

  // set context of Holder to propagate it to WebComponentConfig
  Holder.SetContext(Self.GetCurrentContext);

  WebComponentHandler.AddWithMapping(Holder, UrlPattern);
end;

procedure TdjWebComponentContextHandler.AddWebFilter(Holder: TdjWebFilterHolder;
  const UrlPattern: string);
begin
  // set context of Holder to propagate it to WebFilterConfig
  Holder.SetContext(Self.GetCurrentContext);

  WebComponentHandler.AddWebFilter(Holder, UrlPattern);
end;

function TdjWebComponentContextHandler.AddWebFilter(
  FilterClass: TdjWebFilterClass; const UrlPattern: string): TdjWebFilterHolder;
var
  Holder: TdjWebFilterHolder;
begin
  Holder := TdjWebFilterHolder.Create(FilterClass);
  WebComponentHandler.AddWebFilter(Holder, UrlPattern);
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
  Trace('Context ' + ContextPath + ' handles ' + Target);
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

