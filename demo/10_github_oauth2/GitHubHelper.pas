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

unit GitHubHelper;

// note: this is unsupported example code

interface

{$i IdCompilerDefines.inc}

uses
  Classes;

const
  USER_SCOPE = 'read:user read:email';
  auth_uri = 'https://github.com/login/oauth/authorize';
  token_uri = 'https://github.com/login/oauth/access_token';

  client_id = '* your client id *';
  client_secret = '* your client secret *';

  MY_HOST = 'http://localhost';
  MY_CALLBACK_URL = '/oauth2callback';
  redirect_uri = MY_HOST + MY_CALLBACK_URL;

type
  TIdTokenResponse = record
    access_token: string;
    token_type: string;
    scope: string;
  end;

function CreateState: string;

function ToIdTokenResponse(const JSON: string): TIdTokenResponse;

function PrettyJson(JSON: string): string;

implementation

uses
  IdCoderMIME, IdGlobal,
  SysUtils,
  {$IFDEF FPC}
  fpjson, jsonparser;
  {$ELSE}
  JsonDataObjects;
  {$ENDIF}

function CreateState: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := Copy(GUIDToString(Guid), 2, 36);
end;

{$IFDEF FPC}

function ToIdTokenResponse(const JSON: string): TIdTokenResponse;
var
  Data: TJSONData;
  C : TJSONObject;
begin
  Data := GetJSON(JSON);
  C := TJSONObject(Data);

  Result.access_token := C.Get('access_token');
  Result.token_type := C.Get('token_type', '');
  Result.scope := C.Get('scope', '');
end;

function PrettyJson(JSON: string): string;
var
  Data: TJSONData;
  C : TJSONObject;
begin
  Data := GetJSON(JSON);
  C := TJSONObject(Data);

  Result := C.FormatJSON();
end;

{$ELSE}

function ToIdTokenResponse(const JSON: string): TIdTokenResponse;
var
  C: TJsonObject;
begin
  C := TJsonObject.Parse(JSON) as TJsonObject;

  Result.access_token := C.S['access_token'];
  Result.token_type := C.S['token_type'];
  Result.scope := C.S['scope'];
end;

function PrettyJson(JSON: string): string;
var
  C : TJsonObject;
begin
  C := TJsonObject.Parse(JSON) as TJsonObject;

  Result := C.ToJSON(False);
end;

{$ENDIF}

end.
