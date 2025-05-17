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

unit djDefaultHandler;

interface

uses
  djAbstractHandler, djServerContext,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes,
  Classes;

type
  { TdjDefaultHandler }
  
  (**
   * Default Handler.
   *
   * This handler deals with unhandled requests in the server.
   * For requests for favicon.ico, the favicon.ico file is served.
   * For requests to '/' a welcome page is served.
   *
   * @note This class is unsupported demonstration code.
   *)
  TdjDefaultHandler = class(TdjAbstractHandler)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    procedure Trace(const S: string);
    function LoadRes: TStream;
    function HomePage: string;
  public
    (**
     * Create a DefaultHandler.
     *)
    constructor Create; override;
    (**
     * Destructor.
     *)
    destructor Destroy; override;

    // IHandler interface
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  djHTTPConstants,
  SysUtils;

{ TdjDefaultHandler }

constructor TdjDefaultHandler.Create;
begin
  inherited Create;

  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjDefaultHandler.ClassName);
  {$ENDIF DARAJA_LOGGING}

  {$IFDEF LOG_CREATE}Trace('Created');{$ENDIF}
end;

destructor TdjDefaultHandler.Destroy;
begin
  {$IFDEF LOG_DESTROY}Trace('Destroy');{$ENDIF}
  inherited;
end;

function TdjDefaultHandler.LoadRes: TStream;
var
  FileName: string;
begin
  Result := nil;
  FileName := ExtractFilePath(ParamStr(0)) + 'favicon.ico';
  if FileExists(FileName) then
  begin
    Trace('Load favicon.ico from file');
    Result := TFileStream.Create(FileName, fmOpenRead);
    Trace(IntToStr(Result.Size));
  end;
end;

procedure TdjDefaultHandler.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

function TdjDefaultHandler.HomePage: string;
begin
  Result := '<!DOCTYPE html>'
    + '<html>'
    + '<head><title>Daraja Framework</title></head>'
    + '<body>'
    + '  <h1>Welcome!</h1>'
    + '  <p>This is the default web page for this server.</p>'
    + '  <p>The web server software is running but no content for this page has been added, yet.</p>'
    + '</body>'
    + '</html>';
end;

procedure TdjDefaultHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  if (Response.ResponseNo = -1) then
  begin
    Trace('Unhandled request.');
    if Request.Document = '/' then
    begin
      // For requests to '/'
      Response.ContentText := HomePage;
      Response.ResponseNo := HTTP_OK
    end
    else if Request.Document = '/favicon.ico' then
    begin
      // For requests to /favicon.ico (note: to test, clear browser cache)
      Response.ContentStream := LoadRes;
      if Assigned(Response.ContentStream) then
      begin
        Response.ContentType := 'image/x-icon';
        Response.ResponseNo := HTTP_OK
      end
    end
  end
end;

end.

