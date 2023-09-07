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

unit RootResource;

// note: this is unsupported example code

interface

uses
  djWebComponent, djTypes;

type

  { TRootResource }

  TRootResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  SysUtils, StrUtils, Classes;

function Ellipsis(S: string): string;
begin
  Result := Copy(S, 1, 12) + ' … ' + RightStr(S, 8);
end;

{ TRootResource }

procedure TRootResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := Format('access_token: %s ' + #10 +
    'scope: %s '+ #10 +
    'token_type: %s '+ #10 +
    'expires_in: %s' + #10 +
    'id_token: %s',
    [
    Ellipsis(Request.Session.Content.Values['access_token']),
    Request.Session.Content.Values['scope'],
    Request.Session.Content.Values['token_type'],
    Request.Session.Content.Values['expires_in'],
    Ellipsis(Request.Session.Content.Values['id_token'])
    ]);

  Response.ContentType := 'text/plain';
  Response.CharSet := 'utf-8';
end;

end.
