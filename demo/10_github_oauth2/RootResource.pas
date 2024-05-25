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

interface

uses
  djWebComponent, djTypes;

type
  TRootResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  GitHubHelper, IndyHttpTransport,
  SysUtils, Classes;

{ TRootResource }

procedure TRootResource.OnGet(Request: TdjRequest; Response: TdjResponse);
var
  C: string;
  IdTokenResponse: TIdTokenResponse;
  IdHTTP: TIndyHttpTransport;
  JsonResponse: string;
begin
  C := Request.Session.Content.Values['credentials'];

  if C = '' then begin
    Response.Session.Content.Values['state'] := CreateState;
    Response.Redirect(MY_CALLBACK_URL)
  end else begin
    IdTokenResponse := ToIdTokenResponse(C);

    IdHTTP := TIndyHttpTransport.Create;
    try
      IdHTTP.Request.Accept := 'application/json';

      IdHTTP.Request.CustomHeaders.Values['Authorization'] :=
          'Bearer ' + IdTokenResponse.access_token;

      JsonResponse := IdHTTP.Get('https://api.github.com/user');
    finally
      IdHTTP.Free;
    end;

    Response.ContentText := PrettyJson(JsonResponse);
    Response.ContentType := 'text/plain';
    Response.CharSet := 'utf-8';
  end;
end;

end.
