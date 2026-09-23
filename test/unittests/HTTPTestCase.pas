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

    // like CheckGETResponse500, but also checks the response body -- for
    // GETs expected to return 500 with specific content (e.g. a custom
    // ErrorHandler's page).
    procedure CheckGETResponse500ContentEquals(Expected: string; URL: string = ''; msg: string = '');

    procedure CheckPOSTResponseEquals(Expected: string; URL: string = ''; msg: string = '');

    // for tests overriding the TdjWebComponent.OnGetLastModified method
    // (since 1.2.10)
    procedure CheckCachedGETResponseEquals(IfModifiedSince: TDateTime; Expected: string; URL: string = ''; msg: string = '');
    procedure CheckCachedGETResponseIs304(IfModifiedSince: TDateTime; URL: string = ''; msg: string = '');

    // GET the URL, then GET it again echoing back the Last-Modified response
    // header as If-Modified-Since; checks that the second response is 304.
    procedure CheckConditionalGETIs304(URL: string = ''; msg: string = '');

    // GET the URL, then GET it again echoing back the ETag response header
    // as If-None-Match; checks that the second response is 304.
    procedure CheckConditionalGETWithETagIs304(URL: string = ''; msg: string = '');

    // send a GET with the given If-None-Match header; checks for 304.
    procedure CheckIfNoneMatchGETResponseIs304(const IfNoneMatch: string; URL: string = ''; msg: string = '');

    // send a GET with the given If-None-Match header; checks the response body.
    procedure CheckIfNoneMatchGETResponseEquals(const IfNoneMatch, Expected: string; URL: string = ''; msg: string = '');

    // send a GET with both an If-Modified-Since and an If-None-Match header;
    // checks the response code (used to prove which one wins, issue #430).
    procedure CheckGETResponseCodeWithConditionalHeaders(IfModifiedSince: TDateTime;
      const IfNoneMatch: string; ExpectedCode: Integer; URL: string = ''; msg: string = '');

    // like CheckCachedGETResponseIs304, but also checks that the 304
    // response carries Date and Last-Modified headers (issue #430).
    procedure CheckCachedGETResponseIs304WithDateAndLastModified(IfModifiedSince: TDateTime; URL: string = ''; msg: string = '');

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

    // send an OPTIONS request; checks for 200, no response body, and the
    // given Allow header value.
    procedure CheckOPTIONSAllowHeaderEquals(const Expected: string; URL: string = ''; msg: string = '');

    // POST to the URL; checks for 405 and the given Allow header value.
    procedure CheckPOSTResponse405AllowHeaderEquals(const Expected: string; URL: string = ''; msg: string = '');

    procedure Upload(URL: string; const SourceFile: string);

  strict private
    // prefix a path-only URL with the default test server base URL
    function ResolveURL(const URL: string): string;

    // allow the IdHTTP instance to receive non-2xx responses without
    // raising an EIdHTTPProtocolException
    procedure AllowErrorResponseCodes;

  end;

implementation

uses
  Classes;

resourcestring
  StrHttp127001 = 'http://127.0.0.1:8080';

{ THTTPTestCase }

function THTTPTestCase.ResolveURL(const URL: string): string;
begin
  if Pos('http', URL) <> 1 then
    Result := StrHttp127001 + URL
  else
    Result := URL;
end;

procedure THTTPTestCase.AllowErrorResponseCodes;
begin
  IdHTTP.HTTPOptions := IdHTTP.HTTPOptions + [hoNoProtocolErrorException];
end;

procedure THTTPTestCase.CheckGETResponseEquals(Expected: string; URL: string = ''; msg: string = '');
var
  Actual: string;
begin
  URL := ResolveURL(URL);

  Actual := IdHTTP.Get(URL{$IFDEF STRING_IS_ANSI}, DestEncoding{$ENDIF});

  CheckEquals(Expected, Actual, msg);
end;

procedure THTTPTestCase.CheckCachedGETResponseEquals(IfModifiedSince: TDateTime; Expected: string; URL: string = ''; msg: string = '');
var
  Actual: string;
begin
  URL := ResolveURL(URL);

  IdHTTP.Request.RawHeaders.Values['If-Modified-Since'] := LocalDateTimeToGMT(IfModifiedSince);
  Actual := IdHTTP.Get(URL{$IFDEF STRING_IS_ANSI}, DestEncoding{$ENDIF});

  CheckEquals(Expected, Actual, msg);
end;

procedure THTTPTestCase.CheckCachedGETResponseIs304(IfModifiedSince: TDateTime; URL: string = ''; msg: string = '');
var
  Actual: Integer;
begin
  URL := ResolveURL(URL);

  IdHTTP.Request.LastModified := IfModifiedSince;
  AllowErrorResponseCodes;

  IdHTTP.Get(URL{$IFDEF STRING_IS_ANSI}, DestEncoding{$ENDIF});
  Actual := IdHTTP.ResponseCode;

  CheckEquals(304, Actual, msg);
end;

procedure THTTPTestCase.CheckCachedGETResponseIs304WithDateAndLastModified(IfModifiedSince: TDateTime; URL: string = ''; msg: string = '');
begin
  URL := ResolveURL(URL);

  IdHTTP.Request.LastModified := IfModifiedSince;
  AllowErrorResponseCodes;

  IdHTTP.Get(URL);
  CheckEquals(304, IdHTTP.ResponseCode, msg);
  CheckTrue(IdHTTP.Response.RawHeaders.Values['Date'] <> '', '304 response missing Date header');
  CheckTrue(IdHTTP.Response.RawHeaders.Values['Last-Modified'] <> '', '304 response missing Last-Modified header');
end;

procedure THTTPTestCase.CheckConditionalGETIs304(URL: string = ''; msg: string = '');
var
  LastMod: TDateTime;
begin
  URL := ResolveURL(URL);

  IdHTTP.Get(URL);
  LastMod := IdHTTP.Response.LastModified;
  CheckTrue(LastMod > 0, 'server did not send a Last-Modified header');

  IdHTTP.Request.LastModified := LastMod;
  AllowErrorResponseCodes;

  IdHTTP.Get(URL);
  CheckEquals(304, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckConditionalGETWithETagIs304(URL: string = ''; msg: string = '');
var
  ETag: string;
begin
  URL := ResolveURL(URL);

  // clear any If-None-Match left over from an earlier check on this IdHTTP
  // instance, so this first request is a plain, unconditional GET
  IdHTTP.Request.CustomHeaders.Values['If-None-Match'] := '';
  IdHTTP.Get(URL);
  ETag := IdHTTP.Response.ETag;
  CheckTrue(ETag <> '', 'server did not send an ETag header');

  // Custom headers are not typed properties, so they survive as-is across
  // SetHeaders (unlike RawHeaders, which SetHeaders rebuilds from scratch).
  IdHTTP.Request.CustomHeaders.Values['If-None-Match'] := ETag;
  AllowErrorResponseCodes;

  IdHTTP.Get(URL);
  CheckEquals(304, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckIfNoneMatchGETResponseIs304(const IfNoneMatch: string; URL: string = ''; msg: string = '');
begin
  URL := ResolveURL(URL);

  IdHTTP.Request.CustomHeaders.Values['If-None-Match'] := IfNoneMatch;
  AllowErrorResponseCodes;

  IdHTTP.Get(URL);
  CheckEquals(304, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckIfNoneMatchGETResponseEquals(const IfNoneMatch, Expected: string; URL: string = ''; msg: string = '');
var
  Actual: string;
begin
  URL := ResolveURL(URL);

  IdHTTP.Request.CustomHeaders.Values['If-None-Match'] := IfNoneMatch;
  Actual := IdHTTP.Get(URL);

  CheckEquals(Expected, Actual, msg);
end;

procedure THTTPTestCase.CheckGETResponseCodeWithConditionalHeaders(IfModifiedSince: TDateTime;
  const IfNoneMatch: string; ExpectedCode: Integer; URL: string = ''; msg: string = '');
begin
  URL := ResolveURL(URL);

  IdHTTP.Request.LastModified := IfModifiedSince;
  IdHTTP.Request.CustomHeaders.Values['If-None-Match'] := IfNoneMatch;
  AllowErrorResponseCodes;

  IdHTTP.Get(URL);
  CheckEquals(ExpectedCode, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckHEADMatchesGET(URL: string = ''; msg: string = '');
var
  Body: string;
  GetLength: Integer;
  GetContentType: string;
begin
  URL := ResolveURL(URL);

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
  URL := ResolveURL(URL);

  // TIdHTTP.Head has no "allowed response codes" parameter
  AllowErrorResponseCodes;

  IdHTTP.Head(URL);
  CheckEquals(405, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckCachedHEADResponseIs304(IfModifiedSince: TDateTime;
  URL: string = ''; msg: string = '');
begin
  URL := ResolveURL(URL);

  IdHTTP.Request.LastModified := IfModifiedSince;
  AllowErrorResponseCodes;

  IdHTTP.Head(URL);
  CheckEquals(304, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckContentTypeEquals(Expected: string; URL: string;
  msg: string);
begin
  URL := ResolveURL(URL);

  IdHTTP.Get(URL);
  CheckEquals(Expected, IdHTTP.Response.ContentType, msg);
end;

procedure THTTPTestCase.CheckGETResponseHeaderEquals(const HeaderName,
  Expected: string; URL: string = ''; msg: string = '');
begin
  URL := ResolveURL(URL);

  IdHTTP.Get(URL);
  CheckEquals(Expected, IdHTTP.Response.RawHeaders.Values[HeaderName], msg);
end;

procedure THTTPTestCase.CheckOPTIONSAllowHeaderEquals(const Expected: string;
  URL: string = ''; msg: string = '');
var
  Body: string;
begin
  URL := ResolveURL(URL);

  Body := IdHTTP.Options(URL);

  CheckEquals(200, IdHTTP.ResponseCode, msg);
  CheckEquals('', Body, 'OPTIONS response must not carry a body');
  CheckEquals(Expected, IdHTTP.Response.RawHeaders.Values['Allow'], msg);
end;

procedure THTTPTestCase.CheckPOSTResponse405AllowHeaderEquals(const Expected: string;
  URL: string = ''; msg: string = '');
var
  Strings: TStrings;
begin
  URL := ResolveURL(URL);

  AllowErrorResponseCodes;

  Strings := TStringList.Create;
  try
    Strings.Add('send=send');
    IdHTTP.Post(URL, Strings);
  finally
    Strings.Free;
  end;

  CheckEquals(405, IdHTTP.ResponseCode, msg);
  CheckEquals(Expected, IdHTTP.Response.RawHeaders.Values['Allow'], msg);
end;

procedure THTTPTestCase.CheckGETResponse200(URL: string; msg: string);
begin
  URL := ResolveURL(URL);

  IdHTTP.Get(URL);
  CheckEquals(200, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponse404(URL: string; msg: string);
begin
  URL := ResolveURL(URL);

  IdHTTP.Get(URL, [404]);
  CheckEquals(404, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponse405(URL: string; msg: string);
begin
  URL := ResolveURL(URL);

  IdHTTP.Get(URL, [405]);
  CheckEquals(405, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponse500(URL: string; msg: string);
begin
  URL := ResolveURL(URL);

  IdHTTP.Get(URL, [500]);
  CheckEquals(500, IdHTTP.ResponseCode, msg);
end;

procedure THTTPTestCase.CheckGETResponse500ContentEquals(Expected: string;
  URL: string; msg: string);
var
  Actual: string;
begin
  URL := ResolveURL(URL);

  // IdHTTP.Get(URL, [500]) alone suppresses the EIdHTTPProtocolException for
  // a 500 reply, but by design still discards the response body unless
  // hoWantProtocolErrorContent is also set (see TIdHTTPProtocol.ProcessResponse's
  // CheckException: LDiscardContent defaults to True for an ignored reply
  // code, independently of the exception-suppression mechanism used).
  IdHTTP.HTTPOptions := IdHTTP.HTTPOptions + [hoWantProtocolErrorContent];

  Actual := IdHTTP.Get(URL, [500]);
  CheckEquals(500, IdHTTP.ResponseCode, msg);
  CheckEquals(Expected, Actual, msg);
end;

procedure THTTPTestCase.CheckGETResponseContains(Expected: string; URL: string = ''; msg: string = '');
var
  Actual: string;
begin
  URL := ResolveURL(URL);

  Actual := IdHTTP.Get(URL);

  CheckTrue(Pos(Expected, Actual) > 0, msg);
end;

procedure THTTPTestCase.CheckPOSTResponseEquals(Expected: string; URL: string;
  msg: string);
var
  Strings: TStrings;
begin
  URL := ResolveURL(URL);

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
  URL := ResolveURL(URL);

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
