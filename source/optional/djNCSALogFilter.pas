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

unit djNCSALogFilter;

interface

{$i IdCompilerDefines.inc}

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
  (**
   * Implements NCSA logging.
   *
   * \sa https://en.wikipedia.org/wiki/Common_Log_Format
   *
   * \note This class is unsupported demonstration code.
   *)

  { TdjNCSALogFilter }

  TdjNCSALogFilter = class(TdjWebFilter)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FS: TFormatSettings;
  public
    procedure Init(const Config: IWebFilterConfig); override;

    (**
     * The doFilter method of the Filter is called by the container each time
     * a request/response pair is passed through the chain due to a client
     * request for a resource at the end of the chain.
     * The FilterChain passed in to this method allows the Filter to pass on
     * the request and response to the next entity in the chain.
     *)
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain); override;
  end;

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
  DecodeTime(AOffset, AHour, AMin, ASec, AMSec);

  Bias := MinsPerHour * AHour + AMin;

  if AOffset <> 0 then
  begin
    Result := Format('%s %s%.2d%.2d', [Result, Neg[AOffset < 0],
      Abs(Bias) div MinsPerHour,
        Abs(Bias) mod MinsPerHour]);
  end;
end;

{ TdjNCSALogFilter }

procedure TdjNCSALogFilter.Init(const Config: IWebFilterConfig);
begin
  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjNCSALogFilter.ClassName);
  {$ENDIF DARAJA_LOGGING}

  {$IFDEF FPC}
  FS := FormatSettings;
  {$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, FS);
  {$ENDIF}

  FS.DateSeparator := '/';
  FS.TimeSeparator := ':';
end;

{ TdjNCSALogFilter }

procedure TdjNCSALogFilter.DoFilter;
var
  LogMsg: string;

  function IfEmpty(const AParam: string): string;
  begin
    if Trim(AParam) = '' then
    begin
      Result := '-';
    end
    else
    begin
      Result := AParam;
    end;
  end;

  function IfNegative(const AParam: Int64): string;
  begin
    if AParam < 0 then
    begin
      Result := '-';
    end
    else
    begin
      Result := IntToStr(AParam);
    end;
  end;

begin
  try
    Chain.DoFilter(Context, Request, Response);
  finally
    LogMsg := Request.RemoteIP + ' '
      + '- '
      + IfEmpty(Request.AuthUsername) + ' '
      + '[' + DateTimeToNCSATime(Now, FS) + '] '
      + '"' + Request.Command + ' ' + Request.URI + ' ' + Request.Version + '" '
      + IntToStr(Response.ResponseNo) + ' '
      + IfNegative(Response.ContentLength);

    if IsConsole then
    begin
      System.Writeln(LogMsg);
    end;
  end;
end;

end.

