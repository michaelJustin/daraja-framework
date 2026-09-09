(*

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

*)

unit HTTPTestCase;

interface

{$I IdCompilerDefines.inc}

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF},
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdGlobal, IdHTTP;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

type

  { THTTPTestCase }

  THTTPTestCase = class(TTestCase)
  strict private
    {$IFDEF STRING_IS_ANSI}
    FDestEncoding: IIdTextEncoding;
    {$ENDIF}
    IdHTTP: TIdHTTP;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

    {$IFDEF STRING_IS_ANSI}
    property DestEncoding: IIdTextEncoding read FDestEncoding write FDestEncoding;
    {$ENDIF}
  public
    procedure CheckGETResponseEquals(Expected: string; URL: string = ''; msg: string = '');

    procedure CheckGETResponseContains(Expected: string; URL: string = ''; msg: string = '');

    procedure CheckGETResponse200(URL: string = ''; msg: string = '');

    procedure CheckGETResponse404(URL: string = ''; msg: string = '');

    procedure CheckGETResponse405(URL: string = ''; msg: string = '');

    procedure CheckGETResponse500(URL: string = ''; msg: string = '');

    procedure CheckPOSTResponseEquals(Expected: string; URL: string = ''; msg: string = '');

    // for tests overriding the TdjWebComponent.OnGetLastModified method
    // (since 1.2.10)
    procedure CheckCachedGETResponseEquals(IfModifiedSince: TDateTime; Expected: string; URL: string = ''; msg: string = '');
    procedure CheckCachedGETResponseIs304(IfModifiedSince: TDateTime; URL: string = ''; msg: string = '');

    // GET the URL, then GET it again echoing back the Last-Modified response
    // header as If-Modified-Since; checks that the second response is 304.
    procedure CheckConditionalGETIs304(URL: string = ''; msg: string = '');

    // GET the URL, then send a HEAD request for it; checks that HEAD answers
    // 200 with the same Content-Length and Content-Type as the GET, and that
    // no response body is sent.
    procedure CheckHEADMatchesGET(URL: string = ''; msg: string = '');

    procedure CheckHEADResponse405(URL: string = ''; msg: string = '');

    // send a HEAD request with an If-Modified-Since header; checks for 304.
    procedure CheckCachedHEADResponseIs304(IfModifiedSince: TDateTime; URL: string = ''; msg: string = '');

    procedure CheckContentTypeEquals(Expected: string; URL: string = ''; msg: string = '');

    // GET the URL and compare a single response header value.
    procedure CheckGETResponseHeaderEquals(const HeaderName, Expected: string; URL: string = ''; msg: string = '');

    procedure Upload(URL: string; const SourceFile: string);

  end;

implementation

uses
  Classes;

resourcestring
  StrHttp127001 = 'http://127.0.0.1:8080';

{ THTTPTestCase }

procedure THTTPTestCase.CheckGETResponseEquals(Expected: string; URL: string = ''; msg: string = '');
var
  Actual: string;
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  Actual := IdHTTP.Get(URL{$IFDEF STRING_IS_ANSI}, DestEncoding{$ENDIF});

  CheckEquals(Expected, Actual, msg);
end;

procedure THTTPTestCase.CheckCachedGETResponseEquals(IfModifiedSince: TDateTime; Expected: string; URL: string = ''; msg: string = '');
var
  Actual: string;
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Request.RawHeaders.Values['If-Modified-Since'] := LocalDateTimeToGMT(IfModifiedSince);
  Actual := IdHTTP.Get(URL{$IFDEF STRING_IS_ANSI}, DestEncoding{$ENDIF});

  CheckEquals(Expected, Actual, msg);
end;

procedure THTTPTestCase.CheckCachedGETResponseIs304(IfModifiedSince: TDateTime; URL: string = ''; msg: string = '');
var
  Actual: Integer;
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Request.LastModified := IfModifiedSince;
  IdHTTP.HTTPOptions := IdHTTP.HTTPOptions + [hoNoProtocolErrorException];

  IdHTTP.Get(URL{$IFDEF STRING_IS_ANSI}, DestEncoding{$ENDIF});
  Actual := IdHTTP.ResponseCode;

  CheckEquals(304, Actual, msg);
end;

procedure THTTPTestCase.CheckConditionalGETIs304(URL: string = ''; msg: string = '');
var
  LastMod: TDateTime;
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL);
  LastMod := IdHTTP.Response.LastModified;
  CheckTrue(LastMod > 0, 'server did not send a Last-Modified header');

  IdHTTP.Request.LastModified := LastMod;
  IdHTTP.HTTPOptions := IdHTTP.HTTPOptions + [hoNoProtocolErrorException];

  IdHTTP.Get(URL);
  CheckEquals(304, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckHEADMatchesGET(URL: string = ''; msg: string = '');
var
  Body: string;
  GetLength: Integer;
  GetContentType: string;
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  Body := IdHTTP.Get(URL);
  GetLength := IdHTTP.Response.ContentLength;
  GetContentType := IdHTTP.Response.ContentType;

  IdHTTP.Head(URL);

  CheckEquals(200, IdHTTP.ResponseCode, msg);
  CheckEquals(GetLength, Integer(IdHTTP.Response.ContentLength),
    'HEAD Content-Length differs from GET');
  CheckEquals(GetContentType, IdHTTP.Response.ContentType,
    'HEAD Content-Type differs from GET');

  // a body sent in response to HEAD would still be in the connection buffer
  // and would desynchronize this follow-up request on the same connection
  CheckEquals(Body, IdHTTP.Get(URL), 'a response body was sent for HEAD');
end;

procedure THTTPTestCase.CheckHEADResponse405(URL: string = ''; msg: string = '');
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  // TIdHTTP.Head has no "allowed response codes" parameter
  IdHTTP.HTTPOptions := IdHTTP.HTTPOptions + [hoNoProtocolErrorException];

  IdHTTP.Head(URL);
  CheckEquals(405, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckCachedHEADResponseIs304(IfModifiedSince: TDateTime;
  URL: string = ''; msg: string = '');
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Request.LastModified := IfModifiedSince;
  IdHTTP.HTTPOptions := IdHTTP.HTTPOptions + [hoNoProtocolErrorException];

  IdHTTP.Head(URL);
  CheckEquals(304, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckContentTypeEquals(Expected: string; URL: string;
  msg: string);
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL);
  CheckEquals(Expected, IdHTTP.Response.ContentType, msg);
end;

procedure THTTPTestCase.CheckGETResponseHeaderEquals(const HeaderName,
  Expected: string; URL: string = ''; msg: string = '');
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL);
  CheckEquals(Expected, IdHTTP.Response.RawHeaders.Values[HeaderName], msg);
end;

procedure THTTPTestCase.CheckGETResponse200(URL: string; msg: string);
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL);
  CheckEquals(200, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponse404(URL: string; msg: string);
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL, [404]);
  CheckEquals(404, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponse405(URL: string; msg: string);
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL, [405]);
  CheckEquals(405, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponse500(URL: string; msg: string);
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL, [500]);
  CheckEquals(500, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponseContains(Expected: string; URL: string = ''; msg: string = '');
var
  Actual: string;
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  Actual := IdHTTP.Get(URL);

  CheckTrue(Pos(Expected, Actual) > 0, msg);
end;

procedure THTTPTestCase.CheckPOSTResponseEquals(Expected: string; URL: string;
  msg: string);
var
  Strings: TStrings;
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  Strings := TStringList.Create;
  try
    Strings.Add('send=send');
    CheckEquals(Expected, IdHTTP.Post(URL, Strings), msg);
  finally
    Strings.Free;
  end;
end;

procedure THTTPTestCase.Upload(URL: string; const SourceFile: string);
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Post(URL, SourceFile)
end;

procedure THTTPTestCase.SetUp;
begin
  inherited;

  {$IFDEF FPC}
  CheckEquals(65001, DefaultSystemCodePage);
  {$ENDIF}

  IdHTTP := TIdHTTP.Create;
end;

procedure THTTPTestCase.TearDown;
begin
  IdHTTP.Free;

  inherited;
end;

end.
