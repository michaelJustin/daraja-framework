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

unit OAuth2CallbackResource;

interface

uses
  djWebComponent, djTypes;

type
  TOAuth2CallbackResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  GitHubHelper, IndyHttpTransport,
  SysUtils, Classes;

{ TOAuth2CallbackResource }

procedure TOAuth2CallbackResource.OnGet(Request: TdjRequest;
  Response: TdjResponse);
var
  AuthCode: string;
  State: string;
  IdHTTP: TIndyHttpTransport;
  Params: TStrings;
  ResponseText: string;
begin
  AuthCode := Request.Params.Values['code'];
  State := Request.Session.Content.Values['state'];

  if AuthCode = '' then begin
    Response.Redirect(auth_uri
     + '?client_id=' + client_id
     + '&redirect_uri=' + redirect_uri
     + '&scope=' + USER_SCOPE
     + '&state=' + State
     );
  end else begin
    // To access the OAuth provider and get the user information we need to
    // exchange the AUTHORIZATON_CODE for an ACCESS_TOKEN.
    Params := TStringList.Create;
    IdHTTP := TIndyHttpTransport.Create;
    try
      Params.Values['client_id'] := client_id;
      Params.Values['client_secret'] := client_secret;
      Params.Values['code'] := AuthCode;
      // Params.Values['redirect_uri'] := redirect_uri;
      Params.Values['state'] := State;
      // Params.Values['grant_type'] := 'authorization_code'; // see https://tools.ietf.org/html/rfc6749#section-4.1.3

      IdHTTP.Request.Accept := 'application/json';

      ResponseText := IdHTTP.Post(token_uri, Params);

      Response.Session.Content.Values['credentials'] := ResponseText;
      Response.Redirect('/index.html');
    finally
      IdHTTP.Free;
      Params.Free;
    end;
  end
end;

end.

