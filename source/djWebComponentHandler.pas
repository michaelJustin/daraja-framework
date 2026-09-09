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

unit djWebComponentHandler;

interface

uses
  djInterfaces, djAbstractHandler, djWebComponent, djServerContext,
  djWebComponentHolder, djWebComponentHolders,
  djWebComponentMapping, djPathMap,
  djWebFilterHolder, djWebFilterMapping,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes;

type
  { TdjWebComponentHandler }

  {*
   * Web Component handler.
   *
   * An instance of this class is created per context.
   *
   * It holds a list of web components and their path mappings,
   * and passes incoming requests to the matching web component.
   *}
  TdjWebComponentHandler = class(TdjAbstractHandler)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    FWebComponentContext: IContext;
    FPathMap: TdjPathMap;

    FWebComponentHolders: TdjWebComponentHolders;
    FWebComponentMappings: TdjWebComponentMappings;

    FWebFilterHolders: TdjWebFilterHolders;
    FWebFilterMappings: TdjWebFilterMappings;
    // a non-owning, filtered view of FWebFilterMappings (URL-pattern mappings)
    FWebFilterPathMappings: TdjWebFilterMappings;

    function StripContext(const Doc: string): string;
    procedure CheckUniqueName(Holder: TdjWebComponentHolder);
    procedure CheckUniqueFilterName(Holder: TdjWebFilterHolder);
    procedure CreateOrUpdateMapping(const UrlPattern: string; Holder:
      TdjWebComponentHolder);
    procedure ValidateMappingUrlPattern(const UrlPattern: string;
      Holder: TdjWebComponentHolder);
    function FindMapping(const WebComponentName: string): TdjWebComponentMapping; // overload;
    function NewFilterChain(Holder: TdjWebFilterHolder;
      Chain: IWebFilterChain): IWebFilterChain;
    procedure UpdateMappings;

    // properties
    property WebComponentContext: IContext read FWebComponentContext;
    property WebComponentMappings: TdjWebComponentMappings read FWebComponentMappings;
  protected
    // TdjLifeCycle overrides
    {*
     * Starts the web component handler.
     * This method is called to initialize and start the handler.
     *}
    procedure DoStart; override;
    {*
     * Stops the web component handler.
     * This method is called to clean up and stop the handler.
     *}
    procedure DoStop; override;
  protected
    {*
     * Finds a web component holder by its target identifier.
     *
     * @param ATarget The identifier of the target component to find.
     * @return A TdjWebComponentHolder instance representing the found component, or nil if not found.
     *}
    function FindComponent(const ATarget: string): TdjWebComponentHolder;

    {*
     * Adds a new web component mapping to the handler.
     *
     * @param Mapping The TdjWebComponentMapping instance to be added.
     *}
    procedure AddMapping(Mapping: TdjWebComponentMapping);

    {*
     * Builds the filter chain that applies to a request path.
     *
     * @param PathInContext The request path relative to the context. An empty
     *        path yields no chain.
     * @param Request The HTTP request being processed.
     * @param Holder The web component holder at the end of the chain.
     * @return The filter chain, or nil if no filter is mapped to the path.
     *}
    function GetFilterChain(const PathInContext: string; Request: TdjRequest;
      Holder: TdjWebComponentHolder): IWebFilterChain;

    property WebComponents: TdjWebComponentHolders read FWebComponentHolders;
    property WebFilters: TdjWebFilterHolders read FWebFilterHolders;
  protected
    // IHandler interface
    procedure Handle(const Target: string; Context: TdjServerContext; Request:
      TdjRequest; Response: TdjResponse); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    {*
     * Sets the context for the component handler.
     *
     * @param Context The context to be set, implementing the IContext interface.
     *}
    procedure SetContext(const Context: IContext);

    {*
     * Add a Web Component.
     *
     * @param ComponentClass WebComponent class
     * @param UrlPattern path specification
     *
     * @throws EWebComponentException if the Web Component can not be added
     *}
    function AddWebComponent(ComponentClass: TdjWebComponentClass;
      const UrlPattern: string): TdjWebComponentHolder; overload;

    {*
     * Add a Web Component holder with mapping.
     *
     * @param Holder a Web Component holder
     * @param UrlPattern a path spec
     *}
    procedure AddWithMapping(Holder: TdjWebComponentHolder; const UrlPattern: string); overload;

    {*
     * Add a Web Filter, specifying a WebFilter class
     * and the mapped path.
     *
     * @param Holder WebFilter holder
     * @param UrlPattern mapped path
     *
     * @throws Exception if the WebFilter can not be added
     *}
    procedure AddWebFilter(Holder: TdjWebFilterHolder;
      const UrlPattern: string); overload;

    {*
     * Find a TdjWebComponentHolder for a WebComponentClass.
     *
     * @param WebComponentClass the Web Component class
     * @return a TdjWebComponentHolder with the WebComponentClass or nil
     *         if the WebComponentClass is not registered
     *}
    function FindHolder(WebComponentClass: TdjWebComponentClass):
      TdjWebComponentHolder;

    {*
     * Invokes a service for the specified web component.
     *
     * @param Comp The web component instance to handle.
     * @param Context The server context in which the service is invoked.
     * @param Request The incoming request to be processed.
     * @param Response The outgoing response to be sent.
     *}
    class procedure InvokeService(Comp: TdjWebComponent; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
  end;

implementation /// \cond

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
  IdHTTP,
  SysUtils, Classes;

resourcestring
  rsCreateMappingForWebComponent = 'Create mapping for Web Component "%s" ->'
    +' %s';
  rsExecutionOfMethodSServiceCausedAnExceptionOfTyp = 'Execution of method %s.'
    +'Service caused an exception of type "%s". The exception message was "%s".';
  rsInvalidMappingSForWebComponentS = 'Invalid mapping "%s" for Web Component '
    +'"%s"';
  rsNoPathMapMatchFoundFor = 'No path map match found for ';
  rsTheWebComponentSCanNotBeAddedBecauseClassSIsAlr = 'The Web Component "%s" '
    +'can not be added because class "%s" is already registered with the same '
    +'name';
  rsUpdateMappingForWebComponent = 'Update mapping for Web Component "%s" -'
    +'> %s,%s';
  rsTheWebFilterSCanNotBeAdded = 'The Web Filter "%s" can not be added because '
    +'a different filter with the same name is already registered';

type

  { TChainEnd }

  TChainEnd = class(TInterfacedObject, IWebFilterChain)
  strict private
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
  inherited;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjWebComponentHandler);
  {$ENDIF DARAJA_LOGGING}

  FWebComponentHolders := TdjWebComponentHolders.Create;
  FWebComponentMappings := TdjWebComponentMappings.Create;

  FWebFilterHolders := TdjWebFilterHolders.Create;
  FWebFilterMappings := TdjWebFilterMappings.Create;

  FPathMap := TdjPathMap.Create;
end;

destructor TdjWebComponentHandler.Destroy;
begin
  if IsStarted then
  begin
    Stop;
  end;

  FPathMap.Free;

  FWebComponentHolders.Free;
  FWebComponentMappings.Free;

  FWebFilterHolders.Free;
  FWebFilterMappings.Free;

  // FWebFilterPathMappings is a non-owning view; the mappings belong to
  // FWebFilterMappings, freed above
  FWebFilterPathMappings.Free;

  inherited;
end;

procedure TdjWebComponentHandler.SetContext(const Context: IContext);
begin
  Assert(Context <> nil) ;
  FWebComponentContext := Context;
end;

function TdjWebComponentHandler.AddWebComponent(ComponentClass: TdjWebComponentClass;
  const UrlPattern: string): TdjWebComponentHolder;
begin
  Result := TdjWebComponentHolder.Create(ComponentClass);
  try
    AddWithMapping(Result, UrlPattern);
  except
    on E: EWebComponentException do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Trace(E.Message);
      {$ENDIF DARAJA_LOGGING}

      Result.Free;
      raise;
    end;
  end;
end;

procedure TdjWebComponentHandler.DoStart;
var
  FH: TdjWebFilterHolder;
  CH: TdjWebComponentHolder;
begin
  inherited;

  UpdateMappings;

  for FH in WebFilters do
  begin
    FH.SetContext(WebComponentContext);
    FH.Start;
  end;

  for CH in WebComponents do
  begin
    CH.Start;
  end;
end;

procedure TdjWebComponentHandler.DoStop;
var
  FH: TdjWebFilterHolder;
  CH: TdjWebComponentHolder;
begin
  for FH in WebFilters do
  begin
    FH.Stop;
  end;

  for CH in WebComponents do
  begin
    CH.Stop;
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

procedure TdjWebComponentHandler.CreateOrUpdateMapping(const UrlPattern: string;
  Holder: TdjWebComponentHolder);
var
  Mapping: TdjWebComponentMapping;
  WebComponentName: string;
begin
  ValidateMappingUrlPattern(UrlPattern, Holder);

  // check if this Web Component is already mapped
  WebComponentName := Holder.Name;

  Mapping := FindMapping(WebComponentName);

  if Assigned(Mapping) then
  begin
    // already mapped
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace(rsUpdateMappingForWebComponent,
      [WebComponentName, Trim(Mapping.UrlPatterns.CommaText), UrlPattern]);
    {$ENDIF DARAJA_LOGGING}
  end
  else
  begin
    // not mapped, create new mapping
    Mapping := TdjWebComponentMapping.Create;
    Mapping.WebComponentName := WebComponentName;

    AddMapping(Mapping);

    {$IFDEF DARAJA_LOGGING}
    Logger.Trace(rsCreateMappingForWebComponent,
      [Mapping.WebComponentName, Trim(UrlPattern)]);
    {$ENDIF DARAJA_LOGGING}
  end;

  // in both cases, add URL pattern
  Mapping.UrlPatterns.Add(UrlPattern);
end;

procedure TdjWebComponentHandler.CheckUniqueName(Holder: TdjWebComponentHolder);
var
  CH: TdjWebComponentHolder;
  Msg: string;
begin
  // fail if there is a different Holder with the same name
  for CH in WebComponents do
  begin
    if (CH.Name = Holder.Name) then
    begin
      Msg := Format(
        rsTheWebComponentSCanNotBeAddedBecauseClassSIsAlr,
        [Holder.Name, CH.WebComponentClass.ClassName]);
      {$IFDEF DARAJA_LOGGING}
      Logger.Trace(Msg);
      {$ENDIF DARAJA_LOGGING}

      raise EWebComponentException.Create(Msg);
    end;
  end;
end;

procedure TdjWebComponentHandler.CheckUniqueFilterName(Holder: TdjWebFilterHolder);
var
  FH: TdjWebFilterHolder;
begin
  // fail if a different holder with the same name is already registered
  for FH in WebFilters do
  begin
    if (FH <> Holder) and (FH.Name = Holder.Name) then
    begin
      raise EWebComponentException.CreateFmt(rsTheWebFilterSCanNotBeAdded,
        [Holder.Name]);
    end;
  end;
end;

procedure TdjWebComponentHandler.AddMapping(Mapping: TdjWebComponentMapping);
begin
  WebComponentMappings.Add(Mapping);
end;

procedure TdjWebComponentHandler.AddWithMapping(Holder: TdjWebComponentHolder;
  const UrlPattern: string);
begin
  try
    FPathMap.CheckExists(UrlPattern);
  except
    on E: EWebComponentException do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Trace('AddWithMapping', E);
      {$ENDIF DARAJA_LOGGING}

      raise EWebComponentException.CreateFmt(
        'Web Component %s is already installed in context %s with URL pattern %s',
        [Holder.WebComponentClass.ClassName, Holder.GetContext.GetContextPath,
         UrlPattern]
        );
    end;
  end;

  Holder.SetContext(Self.WebComponentContext);

  Assert(Holder.GetContext <> nil);

  // add the Web Component to list unless it is already there
  if WebComponents.IndexOf(Holder) = -1 then
  begin
    CheckUniqueName(Holder);
    WebComponents.Add(Holder);
  end;

  // create or update a mapping entry
  CreateOrUpdateMapping(UrlPattern, Holder);

  // add the URL pattern to the FPathMap
  FPathMap.AddUrlPattern(UrlPattern, Holder);

  if Started and not Holder.IsStarted then
  begin
    Holder.Start;
  end;
end;

procedure TdjWebComponentHandler.AddWebFilter(
  Holder: TdjWebFilterHolder; const UrlPattern: string);
var
  Mapping: TdjWebFilterMapping;
begin
  // validate before touching any state, so a bad registration fails at
  // registration time instead of raising during request handling
  if TdjPathMap.GetSpecType(UrlPattern) = stUnknown then
  begin
    raise EWebComponentException.CreateFmt(
      rsInvalidMappingSForWebComponentS, [UrlPattern, Holder.Name]);
  end;

  CheckUniqueFilterName(Holder);

  if not WebFilters.Contains(Holder) then
  begin
    WebFilters.Add(Holder);
  end;

  Mapping := TdjWebFilterMapping.Create;
  Mapping.WebFilterHolder := Holder;
  Mapping.WebFilterName := Holder.Name;
  Mapping.UrlPatterns.Add(UrlPattern);

  FWebFilterMappings.Add(Mapping);
end;

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

procedure TdjWebComponentHandler.ValidateMappingUrlPattern(const UrlPattern: string;
  Holder: TdjWebComponentHolder);
begin
  if TdjPathMap.GetSpecType(UrlPattern) = stUnknown then
  begin
    raise EWebComponentException.CreateFmt(
      rsInvalidMappingSForWebComponentS, [UrlPattern, Holder.Name]);
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

  Matches := FPathMap.GetMatches(Path);
  try
    if Matches.Count = 0 then
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Trace(rsNoPathMapMatchFoundFor + ATarget);
      {$ENDIF DARAJA_LOGGING}
    end
    else
    begin
      // find first non-stopped Web Component
      for I := 0 to Matches.Count - 1 do
      begin
        Tmp := (Matches.Objects[I] as TdjWebComponentHolder);
        if Tmp.Started then
        begin
          {$IFDEF DARAJA_LOGGING}
          Logger.Trace('Match found: Web Component "%s"', [Tmp.Name]);
          {$ENDIF DARAJA_LOGGING}

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
  CH: TdjWebComponentHolder;
begin
  Result := nil;

  for CH in WebComponents do
  begin
    if CH.WebComponentClass = WebComponentClass then
    begin
      Result := CH;
      Break;
    end;
  end;
end;

class procedure TdjWebComponentHandler.InvokeService(Comp: TdjWebComponent; Context:
  TdjServerContext; Request: TdjRequest; Response: TdjResponse);
var
  ExceptionMessageHTML: string;
  Msg: string;
  Msg2: string;
begin
  try
    // invoke service method
    Comp.Service(Context, Request, Response);

  except
    // log exceptions
    on E: Exception do
    begin
      ExceptionMessageHTML := HTMLEncode(E.Message);

      Msg := Format(rsExecutionOfMethodSServiceCausedAnExceptionOfTyp,
               [Comp.ClassName, E.ClassName, ExceptionMessageHTML]);

      if E is EIdHTTPProtocolException
      then
      begin
        Msg2 := '<p>'
          + HTMLEncode(EIdHTTPProtocolException(E).ErrorMessage)
          + '</p>';
      end;

      Response.ContentText := '<!DOCTYPE html>' + #10
        + '<html>' + #10
        + '  <head>' + #10
        + '    <title>500 Internal Error</title>' + #10
        + '  </head>' + #10
        + '  <body>' + #10
        + '    <h1>' + Comp.ClassName + ' caused ' + E.ClassName + '</h1>' + #10
        + '    <h2>Exception message: ' + ExceptionMessageHTML + '</h2>' + #10
        + '    <p>' + Msg + '</p>' + #10
        + Msg2
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
begin
  Chain := nil;

  // an empty request path (e.g. the bare context root) never maps to a filter
  if (PathInContext <> '') and (FWebFilterPathMappings <> nil) then
  begin
    for FilterMapping in FWebFilterPathMappings do
    begin
      if FilterMapping.AppliesTo(PathInContext) then
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
  end;

  Result := Chain;
end;

function TdjWebComponentHandler.NewFilterChain(Holder: TdjWebFilterHolder;
  Chain: IWebFilterChain): IWebFilterChain;
begin
  Result := TdjWebFilterChain.Create(Holder, Chain);
end;

procedure TdjWebComponentHandler.UpdateMappings;
var
  FilterMapping: TdjWebFilterMapping;
begin
  // rebuild the non-owning URL-pattern view of FWebFilterMappings
  FWebFilterPathMappings.Free;
  FWebFilterPathMappings := TdjWebFilterMappings.Create;
  FWebFilterPathMappings.OwnsObjects := False;

  for FilterMapping in FWebFilterMappings do
  begin
    if FilterMapping.UrlPatterns.Count > 0 then
    begin
      FWebFilterPathMappings.Add(FilterMapping);
    end;
  end;
end;

end. /// \endcond

