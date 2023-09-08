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

unit CallbackResource;

// note: this is unsupported example code

interface

uses
  djInterfaces, djWebComponent, djTypes;

type

  { TCallbackResource }

  TCallbackResource = class(TdjWebComponent)
  public
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TCallbackResource }

function Ellipsis(S: string): string;
begin
  Result := Copy(S, 1, 12) + string(' … ') + RightStr(S, 8);
end;

procedure TCallbackResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  Credentials: string;
begin
  if Request.Params.Values['state'] <> Request.Session.Content.Values['state'] then
  begin
    Response.ResponseNo := 401;
    WriteLn('Invalid state parameter.');
    Exit;
  end;

  Credentials := Format(
    'id_token: %s' + #10 +
    'access_token: %s ' + #10 +
    'token_type: %s '+ #10 +
    'scope: %s '+ #10 +
    'expires_in: %s',
    [
    Ellipsis(Request.Params.Values['id_token']),
    Ellipsis(Request.Params.Values['access_token']),
    Request.Params.Values['token_type'],
    Request.Params.Values['scope'],
    Request.Params.Values['expires_in']
    ]);

  Response.Session.Content.Values['credentials'] := Credentials;
  Response.Redirect('/index.html');
end;

end.

