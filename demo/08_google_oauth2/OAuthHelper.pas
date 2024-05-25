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

unit OAuthHelper;

// note: this is unsupported example code

interface

{$i IdCompilerDefines.inc}

uses
  Classes;

const
  MY_HOST = 'http://localhost';
  MY_CALLBACK_URL = '/oauth2callback';

var
  // values from client_secret.json
  client_secret: string;
  client_id: string;
  auth_uri: string;
  token_uri: string;
  redirect_uri: string;

  // values from auth token response
type
  TCredentials = record
    access_token: string;
    expires_in: Integer;
  end;

procedure LoadClientSecrets(Filename: string);

function ToCredentials(const JSON: string): TCredentials;

implementation

uses
  SysUtils,
  {$IFDEF FPC}
  fpjson, jsonparser;
  {$ELSE}
  JsonDataObjects;
  {$ENDIF}

{$IFDEF FPC}
procedure LoadClientSecrets(Filename: string);
var
  S: TStream;
  Data: TJSONData;
  C, web : TJSONObject;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Data := GetJSON(S);
    C := TJSONObject(Data);

    web := C.Objects['web'];

    client_id := web.Get('client_id');
    client_secret := web.Get('client_secret');
    auth_uri := web.Get('auth_uri');
    token_uri := web.Get('token_uri');

    redirect_uri := web.Arrays['redirect_uris'].Strings[0];

    if redirect_uri <> MY_HOST + MY_CALLBACK_URL then
      raise Exception
        .CreateFmt('Please enter the redirect URI %s in the API console!',
          [MY_HOST + MY_CALLBACK_URL]);

  finally
    S.Free;
  end;
end;

function ToCredentials(const JSON: string): TCredentials;
var
  Data: TJSONData;
  C : TJSONObject;
begin
  Data := GetJSON(JSON);
  C := TJSONObject(Data);

  Result.access_token := C.Get('access_token');
  Result.expires_in := C.Get('expires_in');
end;

{$ELSE}

procedure LoadClientSecrets(Filename: string);
var
  C, web: TJsonObject;
begin
  C := TJsonObject.ParseFromFile(FileName) as TJsonObject;

  web := C.O['web'];

  client_id := web.S['client_id'];
  client_secret := web.S['client_secret'];
  auth_uri := web.S['auth_uri'];
  token_uri := web.S['token_uri'];

  redirect_uri := web.A['redirect_uris'].S[0];

  if redirect_uri <> MY_HOST + MY_CALLBACK_URL then
    raise Exception
      .CreateFmt('Please enter the redirect URI %s in the API console!',
        [MY_HOST + MY_CALLBACK_URL]);
end;

function ToCredentials(const JSON: string): TCredentials;
var
  C: TJsonObject;
begin
  C := TJsonObject.Parse(JSON) as TJsonObject;

  Result.access_token := C.S['access_token'];
  Result.expires_in := C.I['expires_in'];
end;
{$ENDIF}

end.
