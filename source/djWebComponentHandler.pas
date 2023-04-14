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
  djWebComponentMapping, djPathMap,
  djWebFilterHolder, djWebFilterMapping, djMultiMap,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes,
  Generics.Collections;

type

  (**
   * Web Component handler.
   *
   * An instance of this class is created per context.
   *
   * It holds a list of web components and their path mappings,
   * and passes incoming requests to the matching web component.
   *)

  { TdjWebComponentHandler }

  TdjWebComponentHandler = class(TdjAbstractHandler)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    FWebComponentContext: IContext;
    PathMap: TdjPathMap;

    FWebComponentHolders: TdjWebComponentHolders;
    FMappings: TdjWebComponentMappings;

    FWebFilterHolders: TdjWebFilterHolders;
    FWebFilterMappings: TdjWebFilterMappings;
    FilterNameMap: TObjectDictionary<string, TdjWebFilterHolder>;
    FWebFilterNameMappings: TdjMultiMap<TdjWebFilterMapping>;

    procedure SetFilters(Holders: TdjWebFilterHolders);
    // procedure InitializeHolders(Holders: TdjWebFilterHolders);
    procedure Trace(const S: string);
    function StripContext(const Doc: string): string;
    procedure InvokeService(Comp: TdjWebComponent; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
    procedure CheckStoreContext(const Context: IContext);
    procedure CheckUniqueName(Holder: TdjWebComponentHolder);
    procedure CreateOrUpdateMapping(const PathSpec: string; Holder:
      TdjWebComponentHolder);
    procedure ValidateMappingPathSpec(const PathSpec: string;
      Holder: TdjWebComponentHolder);
    function FindMapping(const WebComponentName: string): TdjWebComponentMapping;
    function GetFilterChain(const PathInContext: string; Request: TdjRequest;
      Holder: TdjWebComponentHolder): IWebFilterChain;
    function NewFilterChain(Holder: TdjWebFilterHolder;
      Chain: IWebFilterChain): IWebFilterChain;
    procedure UpdateNameMappings;
    procedure UpdateMappings;

  protected
    function FindComponent(const ATarget: string): TdjWebComponentHolder;
    procedure AddMapping(Mapping: TdjWebComponentMapping);

    property WebComponents: TdjWebComponentHolders read FWebComponentHolders;
    property WebFilters: TdjWebFilterHolders read FWebFilterHolders;

  public
    constructor Create; override;
    destructor Destroy; override;

    (**
     * Add a Web Component holder with mapping.
     *
     * \param Holder a Web Component holder
     * \param PathSpec a path spec
     *)
    procedure AddWithMapping(Holder: TdjWebComponentHolder; const PathSpec: string);

    (**
     * Add a Web Filter, specifying a WebFilter holder instance
     * and the mapped WebComponent name.
     *
     * \param FilterClass WebFilter class
     * \param WebComponent name
     *
     * \throws Exception if the WebFilter can not be added
     *)
    procedure AddFilterWithNameMapping(WebFilterHolder: TdjWebFilterHolder;
      const ComponentName: string);

    (**
     * Create a TdjWebComponentHolder for a WebComponentClass.
     *
     * \param WebComponentClass the Web Component class
     * \return a TdjWebComponentHolder with the WebComponentClass.
     *)
    function CreateHolder(WebComponentClass: TdjWebComponentClass):
      TdjWebComponentHolder;

    (**
     * Find a TdjWebComponentHolder for a WebComponentClass.
     *
     * \param WebComponentClass the Web Component class
     * \return a TdjWebComponentHolder with the WebComponentClass or nil
     *         if the WebComponentClass is not registered
     *)
    function FindHolder(WebComponentClass: TdjWebComponentClass):
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
    procedure DoStart; override;
    procedure DoStop; override;

    // properties
    property WebComponentContext: IContext read FWebComponentContext;
    property WebComponentMappings: TdjWebComponentMappings read FMappings;

  end;

implementation

uses
  djContextHandler, djGlobal, djHTTPConstants, djWebFilterChain,
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

type

  { TChainEnd }

  TChainEnd = class(TInterfacedObject, IWebFilterChain)
  private
    FWebComponentHolder: TdjWebComponentHolder;
  public
    constructor Create(Holder: TdjWebComponentHolder);
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse);
  end;

{ TChainEnd }

constructor TChainEnd.Create(Holder: TdjWebComponentHolder);
begin
  inherited Create;

  FWebComponentHolder := Holder;
end;

procedure TChainEnd.DoFilter(Context: TdjServerContext; Request: TdjRequest;
  Response: TdjResponse);
begin
  FWebComponentHolder.Handle(Context, Request, Response);
end;

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

  FWebFilterHolders := TdjWebFilterHolders.Create(TComparer<TdjWebFilterHolder>.Default);
  FWebFilterMappings := TdjWebFilterMappings.Create(TComparer<TdjWebFilterMapping>.Default);

  FilterNameMap := TObjectDictionary<string, TdjWebFilterHolder>.Create;

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

  FWebFilterHolders.Free;
  FWebFilterMappings.Free;

  FilterNameMap.Free;
  FWebFilterNameMappings.Free;

  inherited;
end;

function TdjWebComponentHandler.CreateHolder(WebComponentClass:
  TdjWebComponentClass): TdjWebComponentHolder;
begin
  Result := TdjWebComponentHolder.Create(WebComponentClass);
end;

// ILifeCycle

procedure TdjWebComponentHandler.DoStart;
var
  FH: TdjWebFilterHolder;
  H: TdjWebComponentHolder;
begin
  inherited;

  UpdateNameMappings;
  UpdateMappings;

  for FH in WebFilters do
  begin
    FH.Start;
  end;

  for H in WebComponents do
  begin
    H.Start;
  end;
end;

procedure TdjWebComponentHandler.DoStop;
var
  F: TdjWebFilterHolder;
  H: TdjWebComponentHolder;
begin
  for F in WebFilters do
  begin
    F.Stop;
  end;

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

procedure TdjWebComponentHandler.CreateOrUpdateMapping(const PathSpec: string;
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

procedure TdjWebComponentHandler.CheckUniqueName(Holder: TdjWebComponentHolder);
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

procedure TdjWebComponentHandler.AddMapping(Mapping: TdjWebComponentMapping);
begin
  WebComponentMappings.Add(Mapping);
end;

procedure TdjWebComponentHandler.AddWithMapping(Holder: TdjWebComponentHolder;
  const PathSpec: string);
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

  // Assign name (if empty)
  //if Holder.Name = '' then
  //begin
  //  Holder.Name := Holder.WebComponentClass.ClassName;
  //end;

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

procedure TdjWebComponentHandler.AddFilterWithNameMapping(
  WebFilterHolder: TdjWebFilterHolder; const ComponentName: string);
var
  Mapping: TdjWebFilterMapping;
begin
  if not WebComponents.Contains(ComponentName) then
  begin
    raise EWebComponentException.CreateFmt(
      'Invalid Web Component name mapping "%s" for Web Filter "%s"',
      [ComponentName, WebFilterHolder.Name]);
  end;

  if not WebFilters.Contains(WebFilterHolder) then
  begin
    WebFilters.Add(WebFilterHolder);
    SetFilters(WebFilters);
  end;

  Mapping := TdjWebFilterMapping.Create;
  Mapping.WebFilterHolder := WebFilterHolder;
  Mapping.WebFilterName := WebFilterHolder.Name;
  Mapping.WebComponentNames.Add(ComponentName);

  FWebFilterMappings.Add(Mapping);
end;

procedure TdjWebComponentHandler.SetFilters(Holders: TdjWebFilterHolders);
begin
  // InitializeHolders(Holders);
  UpdateNameMappings;
end;

(*
procedure TdjWebComponentHandler.InitializeHolders(Holders: TdjWebFilterHolders);
var
  Holder: TdjWebFilterHolder;
begin
  for Holder in Holders do
  begin
    // Holder.SetContext(Self.WebComponentContext);
  end;
end;
*)

function TdjWebComponentHandler.StripContext(const Doc: string): string;
begin
  if WebComponentContext.GetContextPath = ROOT_CONTEXT then
    Result := Doc
  else
  begin
    // strip leading slash
    Result := Copy(Doc, Length(WebComponentContext.GetContextPath) + 2);
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
  Holder: TdjWebComponentHolder);
begin
  if TdjPathMap.GetSpecType(PathSpec) = stUnknown then
  begin
    raise EWebComponentException.CreateFmt(
      'Invalid mapping "%s" for Web Component "%s"', [PathSpec, Holder.Name]);
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

function TdjWebComponentHandler.FindHolder(WebComponentClass: TdjWebComponentClass): TdjWebComponentHolder;
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
  Chain: IWebFilterChain;
begin
  Holder := FindComponent(Target);
  Chain := nil;

  if (Holder <> nil) and (FWebFilterMappings.Count > 0) then
  begin
    Chain := GetFilterChain(Target, Request, Holder);
  end;

  if Holder <> nil then
  begin
    Response.ResponseNo := HTTP_OK;
    try
      if Chain <> nil then begin
        Chain.DoFilter(Context, Request, Response);
      end else begin
        InvokeService(Holder.WebComponent, Context, Request, Response);
      end;
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

function TdjWebComponentHandler.GetFilterChain(const PathInContext: string;
  Request: TdjRequest; Holder: TdjWebComponentHolder): IWebFilterChain;
var
  Chain: IWebFilterChain;
  FilterMapping: TdjWebFilterMapping;
  ChainEnd: TChainEnd;
  NameMappings: TdjWebFilterMappings;
begin
  Chain := nil;

  if (FWebFilterNameMappings <> nil) and (FWebFilterNameMappings.Count > 0) then
  begin
    NameMappings := FWebFilterNameMappings.GetValues(Holder.Name);

    for FilterMapping in NameMappings do
    begin
       if Chain = nil then
       begin
         ChainEnd := TChainEnd.Create(Holder);
         Chain := NewFilterChain(FilterMapping.WebFilterHolder, ChainEnd);
       end else begin
         Chain := NewFilterChain(FilterMapping.WebFilterHolder, Chain);
       end;
    end;
  end;

  Result := Chain;
end;

function TdjWebComponentHandler.NewFilterChain(Holder: TdjWebFilterHolder;
  Chain: IWebFilterChain): IWebFilterChain;
begin
  Result := TdjWebFilterChain.Create(Holder, Chain);
end;

procedure TdjWebComponentHandler.UpdateNameMappings;
var
  WebFilterHolder: TdjWebFilterHolder;
begin
  FilterNameMap.Clear;

  for WebFilterHolder in WebFilters do
  begin
    FilterNameMap.Add(WebFilterHolder.Name, WebFilterHolder);
  end;
end;

procedure TdjWebComponentHandler.UpdateMappings;
var
  FilterMapping: TdjWebFilterMapping;
  WebFilterHolder: TdjWebFilterHolder;
  WebComponentNames: TStrings;
  WebComponentName: string;
begin
  FWebFilterNameMappings.Free;
  FWebFilterNameMappings := TdjMultiMap<TdjWebFilterMapping>.Create;

  for FilterMapping in FWebFilterMappings do
  begin
    WebFilterHolder := FilterNameMap[FilterMapping.WebFilterName];
    // if = nil ...
    FilterMapping.WebFilterHolder := WebFilterHolder;

    WebComponentNames := FilterMapping.WebComponentNames;
    if WebComponentNames.Count > 0 then
    begin
      for WebComponentName in WebComponentNames do
      begin
        FWebFilterNameMappings.Add(WebComponentName, FilterMapping);
      end;
    end;
  end;
end;

end.

