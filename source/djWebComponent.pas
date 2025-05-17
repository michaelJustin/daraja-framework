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

unit djWebComponent;

interface

// {$i IdCompilerDefines.inc}

uses
  djGenericWebComponent, djServerContext,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes;

type
  { TdjWebComponent }

  (**
   * A base class which can be subclassed to create a HTTP component
   * for a Web site.
   *
   * A subclass of TdjWebComponent must override at least one method, usually one of these:
   * @li OnGet, if the web component supports HTTP GET requests
   * @li OnPost, for HTTP POST requests
   * @li OnPut, for HTTP PUT requests
   * @li OnDelete, for HTTP DELETE requests
   *)
  TdjWebComponent = class(TdjGenericWebComponent)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    procedure DoCachedGet(Request: TdjRequest; Response: TdjResponse); virtual;
  protected
    (**
     * Called by the server to handle a DELETE request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *)
    procedure OnDelete(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
     * Called by the server (via the service method) to allow a component to handle a GET request.
     *)
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
     * Called by the server (via the service method) to allow a component to handle a HEAD request.
     *)
    procedure OnHead(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
     * Called by the server (via the service method) to allow a component to handle a OPTIONS request.
     *)
    procedure OnOptions(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
     * Called by the server to handle a POST request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *)
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
     * Called by the server to handle a PUT request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *)
    procedure OnPut(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
     * Called by the server to handle a TRACE request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     *)
    procedure OnTrace(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
     * Called by the server to handle a PATCH request.
     *
     * @param Request The HTTP request to process
     * @param Response The HTTP response to fill
     * @throws EWebComponentException if an exception occurs
     * @sa http://tools.ietf.org/html/rfc5789
     *)
    procedure OnPatch(Request: TdjRequest; Response: TdjResponse); virtual;

    (**
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
     *)
    function OnGetLastModified(Request: TdjRequest): TDateTime; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Service(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse); override;
  end;

  (**
   * Class reference to TdjWebComponent
   *)
  TdjWebComponentClass = class of TdjWebComponent;

{$IFNDEF DOXYGEN_SKIP}

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer, IdGlobalProtocols,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

const
  RESOURCE_LAST_MODIFIED_DEFAULT = 0;
  HTTP_ERROR_METHOD_NOT_ALLOWED = 405;

{ TdjWebComponent }

constructor TdjWebComponent.Create;
begin
  inherited Create;

  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjWebComponent.ClassName);
  {$ENDIF DARAJA_LOGGING}

  {$IFDEF LOG_CREATE}Trace('Created');{$ENDIF}
end;

destructor TdjWebComponent.Destroy;
begin
  {$IFDEF LOG_DESTROY}Trace('Destroy');{$ENDIF}

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
  Response.ResponseNo := HTTP_ERROR_METHOD_NOT_ALLOWED;
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

procedure TdjWebComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  case Request.CommandType of
    hcHEAD:
      begin
        OnHead(Request, Response);
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
    end;
  end;
end;

{$ENDIF DOXYGEN_SKIP}

end.

