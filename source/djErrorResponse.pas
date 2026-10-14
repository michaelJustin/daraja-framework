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

unit djErrorResponse;

interface

uses
  djTypes,
  SysUtils;

{*
 * Resets a response that a Web Component or Web Filter may already have
 * partly filled in, so that an error response can be built on a clean
 * state.
 *
 * Clears the content (ContentText and ContentStream), every entity and
 * response header field (ContentType, ContentLength, Location, ...) and
 * CustomHeaders. Keeps the server name, the connection's keep-alive setting,
 * and the cookies, because Indy stores a newly created session's cookie
 * there.
 *
 * @param Response the response to reset. Its headers must not have been
 *        sent yet.
 *}
procedure ResetResponse(Response: TdjResponse);

{*
 * Replaces whatever a failed Web Component, Web Filter or handler left on
 * the response with the framework's 500 error page.
 *
 * The page is generic and discloses nothing about the exception, unless
 * DARAJA_PROJECT_STAGE_DEVELOPMENT is defined: then it shows the exception
 * class and message, the component name, and a stack trace where available.
 *
 * If the response headers were already sent (for example by a component
 * that streams its output), the status can no longer change. The pending
 * content is dropped and the connection is closed instead, so the client
 * sees a truncated response rather than an error page appended to a
 * partial body.
 *
 * @param Response the response to replace
 * @param E the exception that caused the error
 * @param ComponentName the Web Component that was handling the request, if
 *        known; shown only on the development page
 *}
procedure SetErrorResponse(Response: TdjResponse; E: Exception;
  const ComponentName: string = '');

implementation /// \cond

uses
  djGlobal, djHTTPConstants
  {$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  , IdHTTP
  {$IFDEF DARAJA_MADEXCEPT}
  , madStackTrace
  {$ENDIF}
  {$IFDEF DARAJA_JCLDEBUG}
  , djStacktrace
  {$ENDIF}
  {$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  ;

procedure ReleaseContent(Response: TdjResponse);
begin
  Response.ContentText := '';
  if Response.FreeContentStream then
  begin
    Response.ContentStream.Free;
  end;
  Response.ContentStream := nil;
end;

procedure ResetResponse(Response: TdjResponse);
var
  ServerName: string;
  CloseConnection: Boolean;
begin
  // Indy's Clear resets every header field and the CustomHeaders, but also
  // the Server and Connection values it set up before the request reached
  // the framework -- put those two back.
  ServerName := Response.Server;
  CloseConnection := Response.CloseConnection;

  Response.Clear;

  Response.Server := ServerName;
  // the setter also restores the matching Connection header value
  Response.CloseConnection := CloseConnection;

  ReleaseContent(Response);
end;

function ErrorPageBody(E: Exception; const ComponentName: string): string;
begin
  {$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  // Development mode: disclose the exception class and message, the
  // component, and (if available) a stack trace. Exception messages
  // routinely carry file paths, SQL fragments, connection strings or
  // internal hostnames, so this detail must never be the default for a
  // deployed server.
  Result := '    <h1>500 Internal Server Error</h1>' + #10
    + '    <h2>' + HTMLEncode(E.ClassName) + ': ' + HTMLEncode(E.Message)
    + '</h2>' + #10;

  if ComponentName <> '' then
  begin
    Result := Result
      + '    <p>Web Component: ' + HTMLEncode(ComponentName) + '</p>' + #10;
  end;

  if E is EIdHTTPProtocolException then
  begin
    Result := Result
      + '    <p>' + HTMLEncode(EIdHTTPProtocolException(E).ErrorMessage)
      + '</p>' + #10;
  end;

  {$IFDEF DARAJA_MADEXCEPT}
  Result := Result
    + '    <hr />' + #10
    + '    <h2>Stack trace:</h2>' + #10
    + '    <pre>' + #10
    + string(madStackTrace.StackTrace) + #10
    + '    </pre>' + #10;
  {$ENDIF DARAJA_MADEXCEPT}
  {$IFDEF DARAJA_JCLDEBUG}
  Result := Result
    + '    <hr />' + #10
    + '    <h2>Stack trace:</h2>' + #10
    + '    <pre>' + #10
    + djStackTrace.GetStackList + #10
    + '    </pre>' + #10;
  {$ENDIF DARAJA_JCLDEBUG}
  {$ELSE}
  // Default (non-development) response: generic, discloses nothing about
  // the component, the exception, or its message.
  Result := '    <h1>500 Internal Server Error</h1>' + #10;
  {$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}
end;

procedure SetErrorResponse(Response: TdjResponse; E: Exception;
  const ComponentName: string);
begin
  if Response.HeaderHasBeenWritten then
  begin
    // Too late for a status line: anything still pending would be written
    // after the partial body. Drop it and make the client see a truncated
    // response instead.
    ReleaseContent(Response);
    Response.CloseConnection := True;
    Exit;
  end;

  ResetResponse(Response);

  Response.ResponseNo := HTTP_INTERNAL_SERVER_ERROR;
  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
  Response.ContentText := '<!DOCTYPE html>' + #10
    + '<html>' + #10
    + '  <head>' + #10
    + '    <title>500 Internal Error</title>' + #10
    + '  </head>' + #10
    + '  <body>' + #10
    + ErrorPageBody(E, ComponentName)
    + '    <hr />' + #10
    + '    <p><small>' + DWF_SERVER_FULL_NAME + '</small></p>' + #10
    + '  </body>' + #10
    + '</html>';
end;

end. /// \endcond
