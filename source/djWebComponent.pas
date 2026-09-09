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
   *     OPTIONS is not derived - override OnOptions explicitly if you need it.
   * @li an unrecognised HTTP method responds with 501 Not Implemented.
   * @li conditional GET is supported through OnGetLastModified only
   *     (If-Modified-Since); there is no ETag / If-None-Match handling.
   *}
  TdjWebComponent = class(TdjGenericWebComponent)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    procedure DoCachedGet(Request: TdjRequest; Response: TdjResponse); virtual;

    procedure SetHeadContentLength(Response: TdjResponse);
  protected
    {*
     * Called by the server to handle a DELETE request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
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
     * including the OnGetLastModified conditional handling, and the response
     * body is suppressed. A component which overrides OnGet therefore answers
     * HEAD requests with the GET headers and no content. If OnGet is not
     * overridden either, the response is 405 Method Not Allowed.
     *
     * @note because the GET path runs in full, an expensive OnGet does its work
     * for a HEAD request too, and any side effect of OnGet is triggered by a
     * HEAD request as well. Override this method to handle HEAD separately.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *}
    procedure OnHead(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server (via the service method) to allow a component to handle a OPTIONS request.
     *}
    procedure OnOptions(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a POST request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *}
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a PUT request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *}
    procedure OnPut(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a TRACE request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *}
    procedure OnTrace(Request: TdjRequest; Response: TdjResponse); virtual;

    {*
     * Called by the server to handle a PATCH request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
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
  HTTP_ERROR_METHOD_NOT_ALLOWED = 405;
  HTTP_ERROR_NOT_IMPLEMENTED = 501;

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
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
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

procedure TdjWebComponent.DoCachedGet(Request: TdjRequest;
  Response: TdjResponse);
var
  ResourceDate : TDateTime;
  ReqDate : TDateTime;
begin
  ResourceDate := OnGetLastModified(Request);

  if ResourceDate = RESOURCE_LAST_MODIFIED_DEFAULT then
  begin
    OnGet(Request, Response);
  end else begin
    ReqDate := GMTToLocalDateTime(Request.RawHeaders.Values['If-Modified-Since']);
    // if the file date in the If-Modified-Since header is within 2 seconds of the
    // actual file, then we will send a 304.
    if (ReqDate <> 0) and (Abs(ReqDate - ResourceDate) < 2 * (1 / (24 * 60 * 60))) then
    begin
      Response.ResponseNo := 304;
    end else begin
      Response.Date := Now;
      Response.LastModified := ResourceDate;

      OnGet(Request, Response);
    end;
  end;
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
end;

end. /// \endcond

