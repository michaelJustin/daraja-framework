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
  private
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

    procedure CheckContentTypeEquals(Expected: string; URL: string = ''; msg: string = '');

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

procedure THTTPTestCase.CheckContentTypeEquals(Expected: string; URL: string;
  msg: string);
begin
  if Pos('http', URL) <> 1 then URL := StrHttp127001 + URL;

  IdHTTP.Get(URL);
  CheckEquals(Expected, IdHTTP.Response.ContentType, msg);
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
