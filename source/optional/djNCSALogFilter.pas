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

unit djNCSALogFilter;

interface

uses
  djWebFilter, djServerContext, djTypes, djInterfaces,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

type
  { TdjNCSALogFilter }

  {*
   * Implements NCSA logging.
   *
   * @sa https://en.wikipedia.org/wiki/Common_Log_Format
   * @note This class is unsupported demonstration code.
   *}
  TdjNCSALogFilter = class(TdjWebFilter)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FS: TFormatSettings;
  public
    procedure Init; override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain); override;
  end;

{*
 * Escapes a request-derived value for an access-log line, the way Apache
 * httpd escapes its access log: a double quote becomes \", a backslash
 * becomes \\, and a control character (#0..#31 and #127, including CR, LF
 * and ESC) becomes \xHH. Other characters are kept as they are.
 *
 * This keeps a client from forging log lines (with CR/LF), shifting the
 * quoted fields (with a double quote), or sending terminal escape
 * sequences to whoever reads the log.
 *
 * @param Value the value to escape
 * @return the escaped value
 *}
function NCSAEscape(const Value: string): string;

{*
 * Formats one Common Log Format line:
 * host - user [timestamp] "method uri version" status length
 *
 * The user name, method, URI and version come from the client and are
 * escaped with NCSAEscape. An empty user name is written as '-', and so is
 * a negative length.
 *
 * @param RemoteIP the client address
 * @param AuthUsername the user name from the Authorization header
 * @param Command the request method
 * @param URI the request URI
 * @param Version the HTTP version
 * @param Timestamp the time, already formatted
 * @param ResponseNo the response status code
 * @param ContentLength the response body length, or -1 if unknown
 * @return the log line, without a line break
 *}
function FormatNCSALogLine(const RemoteIP, AuthUsername, Command, URI,
  Version, Timestamp: string; ResponseNo: Integer;
  ContentLength: Int64): string;

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdGlobal
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  {$IFNDEF FPC}, Windows{$ENDIF};

function DateTimeToNCSATime(const AValue: TDateTime; const AFS:
  TFormatSettings): string;
const
  Neg: array[Boolean] of string = ('+', '-');
var
  AOffset: TDateTime;
  AHour, AMin, ASec, AMSec: Word;
  Bias: Integer;
begin
  Result := FormatDateTime('dd/mmm/yyyy:hh:nn:ss', AValue, AFS);

  AOffset := OffsetFromUTC;
  // DecodeTime does not handle a negative TDateTime, so decode the magnitude
  DecodeTime(Abs(AOffset), AHour, AMin, ASec, AMSec);

  Bias := MinsPerHour * AHour + AMin;

  if AOffset <> 0 then
  begin
    Result := Format('%s %s%.2d%.2d', [Result, Neg[AOffset < 0],
      Abs(Bias) div MinsPerHour,
        Abs(Bias) mod MinsPerHour]);
  end;
end;

function NCSAEscape(const Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    case Value[I] of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      #0..#31, #127: Result := Result + '\x' + IntToHex(Ord(Value[I]), 2);
    else
      Result := Result + Value[I];
    end;
  end;
end;

function FormatNCSALogLine(const RemoteIP, AuthUsername, Command, URI,
  Version, Timestamp: string; ResponseNo: Integer;
  ContentLength: Int64): string;
var
  User: string;
  Size: string;
begin
  if Trim(AuthUsername) = '' then
  begin
    User := '-';
  end
  else
  begin
    User := NCSAEscape(AuthUsername);
  end;

  if ContentLength < 0 then
  begin
    Size := '-';
  end
  else
  begin
    Size := IntToStr(ContentLength);
  end;

  Result := RemoteIP + ' '
    + '- '
    + User + ' '
    + '[' + Timestamp + '] '
    + '"' + NCSAEscape(Command) + ' ' + NCSAEscape(URI) + ' '
    + NCSAEscape(Version) + '" '
    + IntToStr(ResponseNo) + ' '
    + Size;
end;

{ TdjNCSALogFilter }

procedure TdjNCSALogFilter.Init;
begin
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjNCSALogFilter);
  {$ENDIF DARAJA_LOGGING}

  {$IFDEF FPC}
  FS := FormatSettings;
  {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, FS);
  {$ENDIF}

  FS.DateSeparator := '/';
  FS.TimeSeparator := ':';
end;

procedure TdjNCSALogFilter.DoFilter;
var
  LogMsg: string;
begin
  try
    Chain.DoFilter(Context, Request, Response);
  finally
    LogMsg := FormatNCSALogLine(Request.RemoteIP, Request.AuthUsername,
      Request.Command, Request.URI, Request.Version,
      DateTimeToNCSATime(Now, FS), Response.ResponseNo,
      Response.ContentLength);

    if IsConsole then
    begin
      System.Writeln(LogMsg);
    end;
  end;
end;

end.

