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

function LoadClientSecrets(const Filename: string): TOpenIDParams;

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

end.
