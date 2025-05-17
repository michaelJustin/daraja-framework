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
  
  (**
   * Web Component for static context.
   *
   * @note This class is unsupported demonstration code.
   *)
  TdjDefaultWebComponent = class(TdjWebComponent)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    ContextPath: string;
    StaticResourcePath: string;
    procedure Trace(const S: string);
    (**
     * Called in Init to set the path to static content.
     *)
    procedure SetStaticResourcePath;
    (**
     * Remove the context from the resource URL
     *)
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
  SysUtils, Classes;

{ TdjDefaultWebComponent }

procedure TdjDefaultWebComponent.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

procedure TdjDefaultWebComponent.Init(const Config: IWebComponentConfig);
begin
  inherited;

  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjDefaultWebComponent.ClassName);
  {$ENDIF DARAJA_LOGGING}

  // copy the context path
  ContextPath := Config.GetContext.GetContextPath;

  // calculate the static resource path
  SetStaticResourcePath;

  if DirectoryExists(ExtractFilePath(ParamStr(0)) + StaticResourcePath) then
  begin
    Trace('Static content directory found: ' + StaticResourcePath);
  end
  else
  begin

    {$IFDEF DARAJA_LOGGING}
    Logger.Warn('Static content directory not found: ' + StaticResourcePath);
    {$ELSE}
    Trace('Static content directory not found: ' + StaticResourcePath);
    {$ENDIF DARAJA_LOGGING}

    raise EWebComponentException.CreateFmt(
      'Static resource path not found (%s)',
      [StaticResourcePath]);
  end;

  Trace('Initialized');
end;

function TdjDefaultWebComponent.StripContext(const Doc: string): string;
begin
  if ContextPath = ROOT_CONTEXT then
    Result := Doc
  else
    Result := Copy(Doc, Length(ContextPath) + 2, MAXINT);
end;

procedure TdjDefaultWebComponent.SetStaticResourcePath;
begin
  if ContextPath = ROOT_CONTEXT then
  begin
    StaticResourcePath := WEBAPPS + '/ROOT';
  end
  else
  begin
    StaticResourcePath := WEBAPPS + '/' + ContextPath;
  end;
end;

procedure TdjDefaultWebComponent.Service(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
var
  RelFileName: string;
  FileName: string;
begin
  RelFileName := StripContext(Request.Document);

  FileName := ExtractFilePath(ParamStr(0)) + StaticResourcePath + RelFileName;

  if PathDelim = '\' then
  begin
    // on Winoid systems replace slash with backslash
    FileName := StringReplace(FileName, '/', PathDelim, [rfReplaceAll]);
  end;

  if FileExists(FileName) then
  begin

    Response.ResponseNo := HTTP_OK;
    Response.ContentType :=
      Response.HTTPServer.MIMETable.GetFileMIMEType(FileName);

    if Response.ContentType = 'text/html' then
    begin
      Response.ContentStream := TFileStream.Create(FileName, fmOpenRead or
        fmShareDenyNone);
    end
    else
    begin
      Response.SmartServeFile(Context, Request, FileName);
    end;
    Trace('Resource found: ' + RelFileName);
  end
  else
  begin
    Response.ResponseNo := 404;

    {$IFDEF DARAJA_LOGGING}
    Logger.Warn('Resource not found: ' + RelFileName);
    {$ELSE}
    Trace('Resource not found: ' + RelFileName);
    {$ENDIF DARAJA_LOGGING}
  end;
end;

end.
