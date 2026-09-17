{***

    Daraja HTTP Framework
    Copyright (c) 2016 Michael Justin

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

unit djWebComponent;

interface

uses
  djGenericWebComponent, djServerContext,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes;

type
  { TdjWebComponent }

  {*
   * A base class which can be subclassed to create a HTTP component
   * for a Web site.
   *
   * A subclass of TdjWebComponent must override at least one method, usually one of these:
   * @li OnGet, if the web component supports HTTP GET requests
   * @li OnPost, for HTTP POST requests
   * @li OnPut, for HTTP PUT requests
   * @li OnDelete, for HTTP DELETE requests
   *
   * @note Method handling notes and current limitations:
   * @li every On* handler that is not overridden responds with 405 Method Not
   *     Allowed, with one exception: HEAD is derived from OnGet (see OnHead).
   * @li a 405 response, whether from a not-overridden handler or set
   *     explicitly, always carries an Allow header listing the methods this
   *     component supports.
   * @li OPTIONS is answered by the framework itself unless OnOptions is
   *     overridden: the default response is 200 with an Allow header and no
   *     body (see OnOptions).
   * @li an unrecognised HTTP method responds with 501 Not Implemented.
   * @li conditional GET/HEAD is supported through OnGetLastModified
   *     (If-Modified-Since) and OnGetETag (If-None-Match). If a request
   *     carries If-None-Match, it alone decides the outcome, per RFC 7232
   *     Section 3.3; If-Modified-Since is only consulted when the request has
   *     no If-None-Match. A 304 response carries Date, and Last-Modified /
   *     ETag whenever the component supplies them.
   *}
  TdjWebComponent = class(TdjGenericWebComponent)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    procedure DoCachedGet(Request: TdjRequest; Response: TdjResponse);

    procedure SetHeadContentLength(Response: TdjResponse);

    function GetAllowedMethods: string;
  protected
    {*
     * Called by the server to handle a DELETE request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EDarajaException if an exception occurs
     *}
    procedure OnDelete(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server (via the service method) to allow a component to handle a GET request.
     *}
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server (via the service method) to allow a component to handle a HEAD request.
     *
     * The default implementation runs the same code path as a GET request,
     * including the OnGetLastModified / OnGetETag conditional handling, and
     * the response body is suppressed. A component which overrides OnGet
     * therefore answers HEAD requests with the GET headers and no content.
     * If OnGet is not overridden either, the response is 405 Method Not
     * Allowed.
     *
     * @note because the GET path runs in full, an expensive OnGet does its work
     * for a HEAD request too, and any side effect of OnGet is triggered by a
     * HEAD request as well. Override this method to handle HEAD separately.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EDarajaException if an exception occurs
     *}
    procedure OnHead(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server (via the service method) to allow a component to handle a OPTIONS request.
     *
     * The default implementation responds with 200 and an Allow header
     * listing the HTTP methods this component supports (i.e. the On*
     * handlers that are overridden), with no response body.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EDarajaException if an exception occurs
     *}
    procedure OnOptions(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a POST request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EDarajaException if an exception occurs
     *}
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a PUT request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EDarajaException if an exception occurs
     *}
    procedure OnPut(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a TRACE request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EDarajaException if an exception occurs
     *}
    procedure OnTrace(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a PATCH request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EDarajaException if an exception occurs
     * @sa http://tools.ietf.org/html/rfc5789
     *}
    procedure OnPatch(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Returns the time the WebComponent object was last modified.
     * If the time is unknown, this method returns 0 (the default).
     *
     * WebComponents that support HTTP GET requests and can quickly determine
     * their last modification time should override this method. This makes
     * browser and proxy caches work more effectively, reducing the load on
     * server and network resources.
     *
     * @param Request HTTP request
     * @return the last modified timestamp
     *}
    function OnGetLastModified(Request: TdjRequest): TDateTime; virtual;

    {*
     * Returns the entity tag (ETag) of the resource a GET/HEAD request would
     * return. If unknown, this method returns '' (the default), which means
     * no ETag is offered and no If-None-Match comparison is done.
     *
     * WebComponents that support HTTP GET requests and can cheaply compute a
     * stable identifier for their current representation should override
     * this method. The returned value is sent as the ETag header, quoted per
     * RFC 7232 if not already (e.g. '"abc123"'), and compared against a
     * request's If-None-Match header using a weak comparison (a leading
     * 'W/' is ignored on both sides).
     *
     * @param Request HTTP request
     * @return the entity tag, or '' if unknown
     *}
    function OnGetETag(Request: TdjRequest): string; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Service(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse); override;
  end;

  {*
   * Class reference to TdjWebComponent
   *}
  TdjWebComponentClass = class of TdjWebComponent;

implementation /// \cond

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer, IdGlobal, IdGlobalProtocols,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

const
  RESOURCE_LAST_MODIFIED_DEFAULT = 0;
  RESOURCE_ETAG_DEFAULT = '';
  HTTP_ERROR_METHOD_NOT_ALLOWED = 405;
  HTTP_ERROR_NOT_IMPLEMENTED = 501;

type
  TdjRequestHandlerMethod = procedure(Request: TdjRequest; Response: TdjResponse) of object;

// Strips a leading weak-comparison marker ('W/') from an ETag, so callers
// can compare the opaque tags regardless of strength (RFC 7232 Section 2.3).
function StripWeakPrefix(const ETag: string): string;
begin
  if (Length(ETag) >= 2) and (ETag[1] = 'W') and (ETag[2] = '/') then
    Result := Copy(ETag, 3, MaxInt)
  else
    Result := ETag;
end;

// True if ResourceETag matches one of the (possibly weak, comma-separated)
// entity tags in an If-None-Match header value, or that value is '*'.
//
// Split by hand rather than with TStringList.DelimitedText: its CSV-style
// quoting would strip the very double quotes that make each entry a valid
// RFC 7232 quoted entity tag.
function ETagMatchesIfNoneMatch(const IfNoneMatch, ResourceETag: string): Boolean;
var
  S, Target, Tag: string;
  I, Start: Integer;
  InQuotes: Boolean;
begin
  Result := False;
  if ResourceETag = '' then Exit;

  S := Trim(IfNoneMatch);
  if S = '*' then
  begin
    Result := True;
    Exit;
  end;

  Target := StripWeakPrefix(Trim(ResourceETag));

  InQuotes := False;
  Start := 1;
  for I := 1 to Length(S) + 1 do
  begin
    if (I <= Length(S)) and (S[I] = '"') then
      InQuotes := not InQuotes;

    if (not InQuotes) and ((I > Length(S)) or (S[I] = ',')) then
    begin
      Tag := Trim(Copy(S, Start, I - Start));
      if SameStr(StripWeakPrefix(Tag), Target) then
      begin
        Result := True;
        Exit;
      end;
      Start := I + 1;
    end;
  end;
end;

// True if Handler is not the TdjWebComponent base implementation, i.e. a
// subclass has overridden it. Self.<Handler> resolves through the VMT to the
// actual (possibly overridden) implementation, while the class-qualified
// @TdjWebComponent.<Handler> is always the base implementation's address.
function IsOverridden(Handler: TdjRequestHandlerMethod; BaseImplementation: Pointer): Boolean;
begin
  Result := TMethod(Handler).Code <> BaseImplementation;
end;

{ TdjWebComponent }

constructor TdjWebComponent.Create;
begin
  inherited Create;

  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjWebComponent);
  {$ENDIF DARAJA_LOGGING}

  
end;

destructor TdjWebComponent.Destroy;
begin
  

  inherited;
end;

procedure TdjWebComponent.OnDelete(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
end;

procedure TdjWebComponent.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
end;

procedure TdjWebComponent.OnHead(Request: TdjRequest; Response: TdjResponse);
begin
  // Derive HEAD from the GET path: the headers are identical, and the connector
  // suppresses the response body of a HEAD request (Service restores the
  // Content-Length afterwards). If OnGet is not overridden either, its default
  // 405 response is returned unchanged.
  DoCachedGet(Request, Response);
end;

procedure TdjWebComponent.OnOptions(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ResponseNo := 200;
  Response.CustomHeaders.Values['Allow'] := GetAllowedMethods;
  Response.ContentLength := 0;
end;

procedure TdjWebComponent.OnPost(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
end;

procedure TdjWebComponent.OnPut(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
end;

procedure TdjWebComponent.OnTrace(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
end;

procedure TdjWebComponent.OnPatch(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
end;

function TdjWebComponent.OnGetLastModified(Request: TdjRequest): TDateTime;
begin
  Result := RESOURCE_LAST_MODIFIED_DEFAULT;
end;

function TdjWebComponent.OnGetETag(Request: TdjRequest): string;
begin
  Result := RESOURCE_ETAG_DEFAULT;
end;

procedure TdjWebComponent.DoCachedGet(Request: TdjRequest;
  Response: TdjResponse);
var
  ResourceDate : TDateTime;
  ResourceETag : string;
  ReqDate : TDateTime;
  IfNoneMatch : string;
  NotModified : Boolean;
begin
  ResourceDate := OnGetLastModified(Request);
  ResourceETag := OnGetETag(Request);

  if (ResourceDate = RESOURCE_LAST_MODIFIED_DEFAULT)
    and (ResourceETag = RESOURCE_ETAG_DEFAULT) then
  begin
    OnGet(Request, Response);
    Exit;
  end;

  // RFC 7232 Section 3.3: a recipient MUST ignore If-Modified-Since if the
  // request contains an If-None-Match header, since the latter is more
  // accurate (e.g. it supports multiple representations / "*").
  IfNoneMatch := Request.RawHeaders.Values['If-None-Match'];
  if IfNoneMatch <> '' then
  begin
    NotModified := ETagMatchesIfNoneMatch(IfNoneMatch, ResourceETag);
  end else begin
    ReqDate := GMTToLocalDateTime(Request.RawHeaders.Values['If-Modified-Since']);
    // if the file date in the If-Modified-Since header is within 2 seconds of the
    // actual file, then we will send a 304.
    NotModified := (ResourceDate <> RESOURCE_LAST_MODIFIED_DEFAULT)
      and (ReqDate <> 0)
      and (Abs(ReqDate - ResourceDate) < 2 * (1 / (24 * 60 * 60)));
  end;

  // RFC 7232 Section 4.1: a 304 response carries the same Date, ETag and
  // Last-Modified header fields a 200 response to the same request would
  // have carried.
  Response.Date := Now;
  if ResourceDate <> RESOURCE_LAST_MODIFIED_DEFAULT then
    Response.LastModified := ResourceDate;
  if ResourceETag <> RESOURCE_ETAG_DEFAULT then
    Response.ETag := ResourceETag;

  if NotModified then
    Response.ResponseNo := 304
  else
    OnGet(Request, Response);
end;

// The connector suppresses the body of a HEAD response, and in doing so also
// skips its own Content-Length calculation, leaving "Content-Length: 0". This
// restores the length that the same response would have carried as a GET,
// mirroring the calculation in TIdHTTPResponseInfo.WriteHeader.
procedure TdjWebComponent.SetHeadContentLength(Response: TdjResponse);
var
  LCharSet: string;
begin
  // the handler has set a length itself
  if Response.ContentLength <> -1 then Exit;

  // a non-identity transfer coding must not be sent with a Content-Length
  if (Response.TransferEncoding <> '')
    and not SameText(Response.TransferEncoding, 'identity') then Exit;

  // responses which never carry a body
  if ((Response.ResponseNo div 100) = 1)
    or (Response.ResponseNo = 204)
    or (Response.ResponseNo = 304) then Exit;

  if Response.ContentText <> '' then
  begin
    LCharSet := Response.CharSet;
    if LCharSet = '' then
    begin
      // no content type was set, so the connector falls back to its default
      if SizeOf(Char) > 1 then
      begin
        LCharSet := 'utf-8';
      end else begin
        LCharSet := 'ISO-8859-1';
      end;
    end;
    Response.ContentLength :=
      CharsetToEncoding(LCharSet).GetByteCount(Response.ContentText);
  end
  else if Assigned(Response.ContentStream) then
  begin
    Response.ContentLength := Response.ContentStream.Size;
  end;
end;

function TdjWebComponent.GetAllowedMethods: string;

  procedure AddMethod(var Methods: string; const AName: string);
  begin
    if Methods <> '' then
      Methods := Methods + ', ';
    Methods := Methods + AName;
  end;

begin
  Result := '';

  if IsOverridden(OnGet, @TdjWebComponent.OnGet) then
  begin
    AddMethod(Result, 'GET');
    AddMethod(Result, 'HEAD');
  end
  else if IsOverridden(OnHead, @TdjWebComponent.OnHead) then
    AddMethod(Result, 'HEAD');

  if IsOverridden(OnPost, @TdjWebComponent.OnPost) then
    AddMethod(Result, 'POST');
  if IsOverridden(OnPut, @TdjWebComponent.OnPut) then
    AddMethod(Result, 'PUT');
  if IsOverridden(OnDelete, @TdjWebComponent.OnDelete) then
    AddMethod(Result, 'DELETE');
  if IsOverridden(OnPatch, @TdjWebComponent.OnPatch) then
    AddMethod(Result, 'PATCH');
  if IsOverridden(OnTrace, @TdjWebComponent.OnTrace) then
    AddMethod(Result, 'TRACE');

  // OPTIONS is always supported, whether by the default implementation or
  // an override.
  AddMethod(Result, 'OPTIONS');
end;

procedure TdjWebComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  case Request.CommandType of
    hcHEAD:
      begin
        OnHead(Request, Response);
        SetHeadContentLength(Response);
      end;
    hcGET:
      begin
        DoCachedGet(Request, Response);
      end;
    hcPOST:
      begin
        OnPost(Request, Response);
      end;
    hcDELETE:
      begin
        OnDelete(Request, Response);
      end;
    hcPATCH:
      begin
        OnPatch(Request, Response);
      end;
    hcPUT:
      begin
        OnPut(Request, Response);
      end;
    hcTRACE:
      begin
        OnTrace(Request, Response);
      end;
    hcOPTION:
      begin
        OnOptions(Request, Response);
      end;
  else
    begin
        {$IFDEF DARAJA_LOGGING}
        Logger.Error('Unknown HTTP method');
        {$ENDIF DARAJA_LOGGING}
        Response.ResponseNo := HTTP_ERROR_NOT_IMPLEMENTED;
    end;
  end;

  // RFC 7231 requires a 405 response to carry an Allow header listing the
  // methods this component does support, whether the 405 came from a
  // not-overridden On* handler above or was set explicitly by a handler.
  if Response.ResponseNo = HTTP_ERROR_METHOD_NOT_ALLOWED then
  begin
    Response.CustomHeaders.Values['Allow'] := GetAllowedMethods;
  end;
end;

end. /// \endcond

