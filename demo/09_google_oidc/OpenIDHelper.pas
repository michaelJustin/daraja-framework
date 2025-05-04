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

unit OpenIDHelper;

// note: this is unsupported example code

interface

{$i IdCompilerDefines.inc}

type
  TOpenIDParams = record
    client_id: string;
    client_secret: string;
    auth_uri: string;
    token_uri: string;
  end;

type
  TIdTokenResponse = record
    access_token: string;
    token_type: string;
    expires_in: Integer;
    id_token: string;
  end;

  (*
  "claims_supported": [
    "aud",
    "email",
    "email_verified",
    "exp",
    "family_name",
    "given_name",
    "iat",
    "iss",
    "locale",
    "name",
    "picture",
    "sub"
   ],
  *)

  // TODO should be a key-value list except iat / exp
  TIdTokenClaims = record
    iss: string;
    sub: string;
    aud: string;
    iat: Integer;
    exp: Integer;
    at_hash: string;
    name: string;
    email: string;
    email_verified: string;
  end;

function CreateState: string;
function LoadClientSecrets(const Filename: string): TOpenIDParams;
function ToIdTokenResponse(const JSON: string): TIdTokenResponse;
function ReadJWTParts(const JSON: string): string;
function ParseJWT(const JSON: string): TIdTokenClaims;

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCoderMIME, IdGlobal,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  {$IFDEF FPC}
  fpjson, jsonparser,
  {$ELSE}
  JsonDataObjects,
  {$ENDIF}
  Classes, SysUtils;

function CreateState: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
end;

{$IFDEF FPC}
function LoadClientSecrets(const Filename: string): TOpenIDParams;
var
  S: TStream;
  Data: TJSONData;
  C: TJSONObject;
  W: TJSONObject;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Data := GetJSON(S);
    C := TJSONObject(Data);
    W := C.Objects['web'];

    Result.client_id := W.Get('client_id');
    Result.client_secret := W.Get('client_secret');
    Result.auth_uri := W.Get('auth_uri');
    Result.token_uri := W.Get('token_uri');
  finally
    S.Free;
  end;
end;
{$ELSE}
function LoadClientSecrets(const Filename: string): TOpenIDParams;
var
  C, web: TJsonObject;
begin
  C := TJsonObject.ParseFromFile(FileName) as TJsonObject;

  web := C.O['web'];

  Result.client_id := web.S['client_id'];
  Result.client_secret := web.S['client_secret'];
  Result.auth_uri := web.S['auth_uri'];
  Result.token_uri := web.S['token_uri'];
end;
{$ENDIF}


{$IFDEF FPC}
function ToIdTokenResponse(const JSON: string): TIdTokenResponse;
var
  Data: TJSONData;
  C : TJSONObject;
begin
  Data := GetJSON(JSON);
  C := TJSONObject(Data);

  Result.access_token := C.Get('access_token');
  Result.id_token := C.Get('id_token', '');
  Result.expires_in := C.Get('expires_in', 0);

  // token_type = bearer
  // refresh_token only if access_type=offline
end;

function ParseJWT(const JSON: string): TIdTokenClaims;
var
  Data: TJSONData;
  C : TJSONObject;
begin
  Data := GetJSON(JSON);
  C := TJSONObject(Data);

  Result.iss := C.Get('iss');
  Result.sub := C.Get('sub');
  Result.aud := C.Get('aud');
  Result.iat := C.Get('iat');
  Result.exp := C.Get('exp');
  Result.at_hash := C.Get('at_hash');
  Result.email := C.Get('email');
  Result.email_verified := C.Get('email_verified');
  Result.name := C.Get('name');
end;

{$ELSE}
function ToIdTokenResponse(const JSON: string): TIdTokenResponse;
var
  C: TJsonObject;
begin
  C := TJsonObject.Parse(JSON) as TJsonObject;

  Result.access_token := C.S['access_token'];
  Result.id_token := C.S['id_token'];
  Result.expires_in := C.I['expires_in'];

  // token_type = bearer
  // refresh_token only if access_type=offline
end;

function ParseJWT(const JSON: string): TIdTokenClaims;
var
  C: TJsonObject;
begin
  C := TJsonObject.Parse(JSON) as TJsonObject;

  Result.iss := C.S['iss'];
  Result.sub := C.S['sub'];
  Result.aud := C.S['aud'];
  Result.iat := C.I['iat'];
  Result.exp := C.I['exp'];
  Result.at_hash := C.S['at_hash'];
  Result.email := C.S['email'];
  Result.email_verified := C.S['email_verified'];
  Result.name := C.S['name'];
end;
{$ENDIF}

// https://auth0.com/docs/tokens/id-token

function ReadJWTParts(const JSON: string): string;
var
  SL: TStrings;
  S: string;
begin
  // Assert('{"alg":"RS256","kid":"cf022a49e9786148ad0e379cc854844e36c3edc1","typ":"JWT"' =
  //  TIdDecoderMIME.DecodeString('eyJhbGciOiJSUzI1NiIsImtpZCI6ImNmMDIyYTQ5ZTk3ODYxNDhhZDBlMzc5Y2M4NTQ4NDRlMzZjM2VkYzEiLCJ0eXAiOiJKV1QifQ', IndyTextEncoding_UTF8));

  SL := TStringlist.Create;
  try
    SL.Delimiter := '.';
    SL.StrictDelimiter := True;
    SL.DelimitedText := JSON;

    // The body, also called the payload, contains identity claims about a user.
    S := SL[1];
    while Length(S) mod 4 <> 0 do S := S + '=';
    Result := TIdDecoderMIME.DecodeString(S, IndyTextEncoding_UTF8);
  finally
    SL.Free;
  end;
end;

end.
