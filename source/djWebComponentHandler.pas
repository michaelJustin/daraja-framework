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

unit djWebComponentHandler;

interface

{$i IdCompilerDefines.inc}

uses
  djInterfaces, djAbstractHandler, djWebComponent, djServerContext,
  djWebComponentHolder, djWebComponentHolders,
  djWebComponentMapping, djWebComponentMappings, djPathMap,
{$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
{$ENDIF DARAJA_LOGGING}
  djTypes;

type
  (**
   * Web Component handler.
   *
   * An instance of this class is created per context.
   *
   * It holds a list of web components and their path mappings,
   * and passes incoming requests to the matching web component.
   *)
  TdjWebComponentHandler = class(TdjAbstractHandler)
  private
{$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
{$ENDIF DARAJA_LOGGING}

    FWebComponentContext: IContext;
    PathMap: TdjPathMap;
    FWebComponentHolders: TdjWebComponentHolders;
    FMappings: TdjWebComponentMappings;

    procedure Trace(const S: string);

    function StripContext(const Doc: string): string;

    procedure InvokeService(Comp: TdjWebComponent; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);

    procedure CheckStoreContext(const Context: IContext);

    procedure CheckUniqueName(const Holder: TdjWebComponentHolder);

    procedure CreateOrUpdateMapping(const PathSpec: string; const Holder:
      TdjWebComponentHolder);

    procedure ValidateMappingPathSpec(const PathSpec: string;
      const Holder: TdjWebComponentHolder);

    function FindMapping(const WebComponentName: string): TdjWebComponentMapping;

  protected
    (**
     * Find matching component.
     *
     * \param ATarget the URL document path.
     *)
    function FindComponent(const ATarget: string): TdjWebComponentHolder;

  public
    (**
     * Create a ContextHandler.
     *
     * Use a ContextConfig (required argument) to configure the
     * route mappings.
     *)
    constructor Create; override;

    (**
     * Destructor.
     *)
    destructor Destroy; override;

    (**
     * Add a mapping
     *)
    procedure AddMapping(const Mapping: TdjWebComponentMapping);

    (**
     * Add a Web Component holder with mapping.
     *
     * \param Holder a Web Component holder
     * \param PathSpec a path spec
     *)
    procedure AddWithMapping(const Holder: TdjWebComponentHolder;
      const PathSpec: string);

    (**
     * Create a TdjWebComponentHolder for a WebComponentClass.
     *
     * \param WebComponentClass the Web Component class
     * \return a TdjWebComponentHolder with the WebComponentClass.
     *)
    function CreateHolder(const WebComponentClass: TdjWebComponentClass):
      TdjWebComponentHolder;

    (**
     * Find a TdjWebComponentHolder for a WebComponentClass.
     *
     * \param WebComponentClass the Web Component class
     * \return a TdjWebComponentHolder with the WebComponentClass or nil
     *         if the WebComponentClass is not registered
     *)
    function FindHolder(const WebComponentClass: TdjWebComponentClass):
      TdjWebComponentHolder;

    // IHandler interface

    (**
     * Handle a HTTP request.
     *
     * \param Target Request target
     * \param Context HTTP server context
     * \param Request HTTP request
     * \param Response HTTP response
     * \throws EWebComponentException if an exception occurs that interferes with the component's normal operation
     * \sa IHandler
     *)
    procedure Handle(const Target: string; Context: TdjServerContext; Request:
      TdjRequest; Response: TdjResponse); override;

    // ILifeCycle interface

    (**
     * Start the handler.
     *)
    procedure DoStart; override;

    (**
     * Stop the handler.
     *)
    procedure DoStop; override;

    // properties

    property WebComponentContext: IContext read FWebComponentContext;

    property WebComponentMappings: TdjWebComponentMappings read FMappings;

    property WebComponents: TdjWebComponentHolders read FWebComponentHolders;

  end;

implementation

uses
  djContextHandler, djGlobal, djHTTPConstants,
{$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
{$IFDEF DARAJA_MADEXCEPT}
  djStacktrace, madStackTrace,
{$ENDIF}
{$IFDEF DARAJA_JCLDEBUG}
  djStacktrace, JclDebug,
{$ENDIF}
{$ENDIF}
  Generics.Defaults,
  SysUtils, Classes;

{ TdjWebComponentHandler }

constructor TdjWebComponentHandler.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
{$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjWebComponentHandler.ClassName);
{$ENDIF DARAJA_LOGGING}

  FWebComponentHolders := TdjWebComponentHolders.Create(TComparer<TdjWebComponentHolder>.Default);
  FMappings := TdjWebComponentMappings.Create(TComparer<TdjWebComponentMapping>.Default);

  PathMap := TdjPathMap.Create;

{$IFDEF LOG_CREATE}Trace('Created');{$ENDIF}
end;

destructor TdjWebComponentHandler.Destroy;
begin
{$IFDEF LOG_DESTROY}Trace('Destroy');{$ENDIF}

  if IsStarted then
  begin
    Stop;
  end;

  PathMap.Free;

  FWebComponentHolders.Free;

  FMappings.Free;

  inherited;
end;

function TdjWebComponentHandler.CreateHolder(
  const WebComponentClass: TdjWebComponentClass): TdjWebComponentHolder;
begin
  Result := TdjWebComponentHolder.Create(WebComponentClass);
end;

// ILifeCycle

procedure TdjWebComponentHandler.DoStart;
var
  H: TdjWebComponentHolder;
begin
  inherited;

  for H in WebComponents do
  begin
    H.Start;
  end;
end;

procedure TdjWebComponentHandler.DoStop;
var
  H: TdjWebComponentHolder;
begin
  for H in WebComponents do
  begin
    H.Stop;
  end;

  inherited;
end;

function TdjWebComponentHandler.FindMapping(const WebComponentName: string):
  TdjWebComponentMapping;
var
  Mapping: TdjWebComponentMapping;
begin
  Result := nil;
  for Mapping in WebComponentMappings do
  begin
    if Mapping.WebComponentName = WebComponentName then
    begin
      Result := Mapping;
      Break;
    end;
  end;
end;

procedure TdjWebComponentHandler.CreateOrUpdateMapping(const PathSpec: string; const
  Holder: TdjWebComponentHolder);
var
  Mapping: TdjWebComponentMapping;
  WebComponentName: string;
begin
  ValidateMappingPathSpec(PathSpec, Holder);

  // check if this Web Component is already mapped
  WebComponentName := Holder.Name;

  Mapping := FindMapping(WebComponentName);

  if Assigned(Mapping) then
  begin
    // already mapped
    Trace(Format('Update mapping for Web Component "%s" -> %s,%s',
      [WebComponentName, Trim(Mapping.PathSpecs.CommaText), PathSpec]));
  end
  else
  begin
    // not mapped, create new mapping
    Mapping := TdjWebComponentMapping.Create;
    Mapping.WebComponentName := WebComponentName;

    AddMapping(Mapping);

    Trace(Format('Create mapping for Web Component "%s" -> %s',
    [Mapping.WebComponentName,
    Trim(PathSpec)]));
  end;

  // in both cases, add PathSpec
  Mapping.PathSpecs.Add(PathSpec);
end;

procedure TdjWebComponentHandler.CheckUniqueName(const Holder: TdjWebComponentHolder);
var
  I: Integer;
  Msg: string;
begin
  // fail if there is a different Holder with the same name
  for I := 0 to WebComponents.Count - 1 do
  begin
    if (WebComponents[I].Name = Holder.Name) then
    begin
      Msg := Format(
        'The Web Component "%s" can not be added because '
        + 'class "%s" is already registered with the same name',
        [Holder.Name, WebComponents[I].WebComponentClass.ClassName]);
      Trace(Msg);

      raise EWebComponentException.Create(Msg);
    end;
  end;
end;

procedure TdjWebComponentHandler.CheckStoreContext(const Context: IContext);
var
  Msg: string;
begin
  if Context = nil then
  begin
    Msg := 'Context is not assigned';
    Trace(Msg);
    raise EWebComponentException.Create(Msg);
  end;

  // store when the first component is added
  if WebComponents.Count = 0 then
  begin
    FWebComponentContext := Context;
  end
  else
  begin
    // all components must be in the same context
    if WebComponentContext <> Context then
    begin
      Msg := 'Web Components must belong to the same context';
      Trace(Msg);

      raise EWebComponentException.Create(Msg);
    end;
  end;
end;

procedure TdjWebComponentHandler.AddMapping(const Mapping:
  TdjWebComponentMapping);
begin
  WebComponentMappings.Add(Mapping);
end;

procedure TdjWebComponentHandler.AddWithMapping(
  const Holder: TdjWebComponentHolder; const PathSpec: string);
begin
  try
    PathMap.CheckExists(PathSpec);
  except
    on E: EWebComponentException do
    begin
      Trace(E.Message);
      raise;
    end;
  end;

  // validate and store context
  CheckStoreContext(Holder.GetContext);

  // add the Web Component to list unless it is already there
  if WebComponents.IndexOf(Holder) = -1 then
  begin
    CheckUniqueName(Holder);
    WebComponents.Add(Holder);
  end;

  // create or update a mapping entry
  CreateOrUpdateMapping(PathSpec, Holder);

  // add the PathSpec to the PathMap
  PathMap.AddPathSpec(PathSpec, Holder);

  if Started and not Holder.IsStarted then
  begin
    Holder.Start;
  end;
end;

function TdjWebComponentHandler.StripContext(const Doc: string): string;
begin
  if WebComponentContext.GetContextPath = ROOT_CONTEXT then
    Result := Doc
  else
  begin
    // strip leading slash
    Result := Copy(Doc, Length(WebComponentContext.GetContextPath) + 2, MAXINT);
  end;
end;

procedure TdjWebComponentHandler.Trace(const S: string);
begin
{$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
{$ENDIF DARAJA_LOGGING}
end;

procedure TdjWebComponentHandler.ValidateMappingPathSpec(const PathSpec: string;
      const Holder: TdjWebComponentHolder);
begin
  if TdjPathMap.GetSpecType(PathSpec) = stUnknown then
  begin
    raise EWebComponentException.CreateFmt(
      'Invalid mapping "%s" for Web Component "%s"', [PathSpec,
      Holder.Name]);
  end;
end;

function TdjWebComponentHandler.FindComponent(const ATarget: string):
  TdjWebComponentHolder;
var
  Matches: TStrings;
  Path: string;
  I: Integer;
  Tmp: TdjWebComponentHolder;
begin
  Result := nil;
  Path := StripContext(ATarget);

  Matches := PathMap.GetMatches(Path);
  try
    if Matches.Count = 0 then
    begin
      Trace('No path map match found for ' + ATarget);
    end
    else
    begin
      // find first non-stopped Web Component
      for I := 0 to Matches.Count - 1 do
      begin
        Tmp := (Matches.Objects[I] as TdjWebComponentHolder);
        if Tmp.Started then
        begin
          Trace('Match found: Web Component "' + Tmp.Name + '"');
          Result := Tmp;
          Break;
        end;
      end;
    end;
  finally
    Matches.Free;
  end;
end;

function TdjWebComponentHandler.FindHolder(
  const WebComponentClass: TdjWebComponentClass): TdjWebComponentHolder;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to WebComponents.Count - 1 do
  begin
    if WebComponents[I].WebComponentClass = WebComponentClass then
    begin
      Result := WebComponents[I];
      Break;
    end;
  end;
end;

procedure TdjWebComponentHandler.InvokeService(Comp: TdjWebComponent; Context:
  TdjServerContext; Request: TdjRequest; Response: TdjResponse);
var
  Msg: string;
begin
  try
    Trace('Invoke ' + Comp.ClassName + '.Service');

    // invoke service method
    Comp.Service(Context, Request, Response);

  except
    // log exceptions
    on E: Exception do
    begin
      Msg :=
        Format('Execution of method %s.Service caused an exception '
        + 'of type "%s". '
        + 'The exception message was "%s".',
        [Comp.ClassName, E.ClassName, E.Message]);

{$IFDEF DARAJA_LOGGING}
    Logger.Warn(Msg, E);
{$ENDIF DARAJA_LOGGING}

{$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  {$IFDEF DARAJA_LOGGING}
    {$IFDEF DARAJA_MADEXCEPT}
      Logger.Warn(string(madStackTrace.StackTrace));
    {$ENDIF DARAJA_MADEXCEPT}
    {$IFDEF DARAJA_JCLDEBUG}
      Logger.Warn(djStackTrace.GetStackList);
    {$ENDIF DARAJA_JCLDEBUG}
  {$ENDIF DARAJA_LOGGING}
{$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}

      Response.ContentText := '<!DOCTYPE html>' + #10
        + '<html>' + #10
        + '  <head>' + #10
        + '    <title>500 Internal Error</title>' + #10
        + '  </head>' + #10
        + '  <body>' + #10
        + '    <h1>' + Comp.ClassName + ' caused ' + E.ClassName + '</h1>' + #10
        + '    <h2>Exception message: ' + E.Message + '</h2>' + #10
        + '    <p>' + Msg + '</p>' + #10
{$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  {$IFDEF DARAJA_MADEXCEPT}
        + '    <hr />' + #10
        + '    <h2>Stack trace:</h2>' + #10
        + '    <pre>' + #10
        + string(madStackTrace.StackTrace) + #10
        + '    </pre>' + #10
  {$ENDIF DARAJA_MADEXCEPT}
  {$IFDEF DARAJA_JCLDEBUG}
        + '    <hr />' + #10
        + '    <h2>Stack trace:</h2>' + #10
        + '    <pre>' + #10
        + djStackTrace.GetStackList + #10
        + '    </pre>' + #10
  {$ENDIF DARAJA_JCLDEBUG}
{$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}
        + '    <hr />' + #10
        + '    <p><small>' + DWF_SERVER_FULL_NAME + '</small></p>' + #10
        + '  </body>' + #10
        + '</html>';

      raise;
    end;
  end;
end;

procedure TdjWebComponentHandler.Handle(const Target: string; Context:
  TdjServerContext; Request: TdjRequest; Response: TdjResponse);
var
  Holder: TdjWebComponentHolder;
begin
  Holder := FindComponent(Target);

  if Assigned(Holder) then
  begin
    Response.ResponseNo := HTTP_OK;
    try
      InvokeService(Holder.WebComponent, Context, Request, Response);
    except
      on E: Exception do
      begin
        Response.ResponseNo := HTTP_INTERNAL_SERVER_ERROR;

{$IFDEF DARAJA_LOGGING}
        // InvokeService already logged the exception
{$ENDIF DARAJA_LOGGING}

      end;
    end;
  end;
end;

end.

