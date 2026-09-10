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

unit djDefaultWebComponent;

interface

uses
  djWebComponent, djInterfaces, djServerContext,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes;

type
  { TdjDefaultWebComponent }
  
  {*
   * Web Component for static content.
   *
   * After registration in a context, this web component will serve
   * requests for resources which have not been resolved by any other
   * registered web component, but exists in the file system.
   *
   * For a request to a resource
   *   http://host:port/<context-root>/path/to/file.ext
   * the static file must exist at
   *   <server-root>/webapps/<context-root>/path/to/file.ext
   *
   * If the file does not exist, a HTTP 404 error will be returned.
   *
   * Requests that would resolve outside the static content directory (path
   * traversal, e.g. "../") are rejected with HTTP 404.
   *
   * @note This class is unsupported demonstration code. Review it carefully
   *       before serving untrusted content from disk.
   *
   * See TdjDefaultWebComponentTests for usage examples.
   *}
  TdjDefaultWebComponent = class(TdjWebComponent)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FContextPath: string;
    FStaticResourcePath: string;
    function BuildAbsolutePath: string;
    procedure Validate;
    {*
     * Called in Init to set the path to static content.
     *}
    procedure SetStaticResourcePath;
    {*
     * Remove the context from the resource URL
     *}
    function StripContext(const Doc: string): string;
  public
    procedure Init(const Config: IWebComponentConfig); override;
    // IHandler interface
    procedure Service(Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  djContextHandler, // to access ROOT_CONTEXT
  djHTTPConstants,
  StrUtils, SysUtils;

{ TdjDefaultWebComponent }

procedure TdjDefaultWebComponent.Init(const Config: IWebComponentConfig);
begin
  inherited;

  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjDefaultWebComponent);
  {$ENDIF DARAJA_LOGGING}

  // copy the context path
  FContextPath := Config.GetContext.GetContextPath;

  // calculate the static resource path
  SetStaticResourcePath;

  // raises EWebComponentException if static webapp folder is missing
  Validate;
end;

procedure TdjDefaultWebComponent.Validate;
begin
  if DirectoryExists(BuildAbsolutePath) then
  begin
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Static content directory found: %s', [FStaticResourcePath]);
    {$ENDIF DARAJA_LOGGING}
  end
  else
  begin
    {$IFDEF DARAJA_LOGGING}
    Logger.Warn('Static content directory not found: %s', [FStaticResourcePath]);
    {$ENDIF DARAJA_LOGGING}

    raise EWebComponentException.CreateFmt(
      'Static resource path not found (%s)',
      [FStaticResourcePath]);
  end;
end;

function TdjDefaultWebComponent.StripContext(const Doc: string): string;
begin
  if FContextPath = ROOT_CONTEXT then
    Result := Doc
  else
    Result := Copy(Doc, Length(FContextPath) + 2, MAXINT);
end;

procedure TdjDefaultWebComponent.SetStaticResourcePath;
begin
  if FContextPath = ROOT_CONTEXT then
  begin
    FStaticResourcePath := WEBAPPS + '/ROOT';
  end
  else
  begin
    FStaticResourcePath := WEBAPPS + '/' + FContextPath;
  end;
end;

procedure TdjDefaultWebComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
var
  RelFileName: string;
  FileName: string;
  BaseDir: string;
  InsideBase: Boolean;
begin
  RelFileName := StripContext(Request.Document);

  FileName := BuildAbsolutePath + RelFileName;

  if PathDelim = '\' then
  begin
    // on Winoid systems replace slash with backslash
    FileName := StringReplace(FileName, '/', PathDelim, [rfReplaceAll]);
  end;

  // resolve '..' / '.' and make sure the request cannot escape the static
  // content directory (path traversal protection)
  FileName := ExpandFileName(FileName);
  BaseDir := IncludeTrailingPathDelimiter(ExpandFileName(BuildAbsolutePath));
  {$IFDEF MSWINDOWS}
  InsideBase := StartsText(BaseDir, FileName);
  {$ELSE}
  InsideBase := StartsStr(BaseDir, FileName);
  {$ENDIF}
  if not InsideBase then
  begin
    Response.ResponseNo := 404;

    {$IFDEF DARAJA_LOGGING}
    Logger.Warn('Rejected path outside static content directory: %s',
      [Request.Document]);
    {$ENDIF DARAJA_LOGGING}

    Exit;
  end;

  if FileExists(FileName) then
  begin
    Response.ResponseNo := HTTP_OK;
    Response.ContentType :=
      Response.HTTPServer.MIMETable.GetFileMIMEType(FileName);

    // Serve every file type the same way. SmartServeFile adds conditional GET
    // (304 Not Modified via If-Modified-Since), sets Last-Modified, and uses
    // the operating system file transfer fast path. Set an explicit "inline"
    // Content-Disposition first: otherwise SmartServeFile falls back to
    // "attachment; filename=..." and the browser downloads index.html instead
    // of rendering it.
    Response.ContentDisposition := 'inline';
    Response.SmartServeFile(Context, Request, FileName);

    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Resource found: %s', [RelFileName]);
    {$ENDIF DARAJA_LOGGING}
  end
  else
  begin
    Response.ResponseNo := 404;

    {$IFDEF DARAJA_LOGGING}
    Logger.Warn('Resource not found: %s', [RelFileName]);
    {$ENDIF DARAJA_LOGGING}
  end;
end;

function TdjDefaultWebComponent.BuildAbsolutePath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + FStaticResourcePath;
end;

end.
