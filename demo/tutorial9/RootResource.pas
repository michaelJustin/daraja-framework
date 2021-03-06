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

  { TRootResource }

  TRootResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  OpenIDHelper, BindingHelper,
  IdHTTP, SysUtils, Classes;

{ TRootResource }

// https://developers.google.com/identity/protocols/OpenIDConnect
// https://developer.paypal.com/docs/integration/direct/identity/button-js-builder/

procedure TRootResource.OnGet(Request: TdjRequest; Response: TdjResponse);
var
  IdTokenResponse: TIdTokenResponse;
  S: string;
  Claims: TIdTokenClaims;
begin
  if Request.Session.Content.Values['credentials'] = '' then begin
    Response.Session.Content.Values['state'] := CreateState;
    Response.Redirect(OpenIDParams.redirect_uri)
  end else begin
    IdTokenResponse := ToIdTokenResponse(Request.Session.Content.Values['credentials']);
    if IdTokenResponse.expires_in <= 0 then begin // does this (<=0) happen?
      Response.Redirect(OpenIDParams.redirect_uri)
    end else begin
      S := ReadJWTParts(IdTokenResponse.id_token);
      // WriteLn(S);
      Claims := ParseJWT(S);
      // WriteLn('sub:' + Claims.sub); // Benutzer ID (stabil!)
      // WriteLn('email:' + Claims.email);
      // WriteLn('email_verified:' + Claims.email_verified);
      Request.Session.Content.Values['iss'] := Claims.iss;
      Request.Session.Content.Values['sub'] := Claims.sub;
      Request.Session.Content.Values['email'] := Claims.email;
      Request.Session.Content.Values['name'] := Claims.name;

      Response.ContentText := Bind(Config.GetContext.GetContextPath,
        'index.html', Request.Session.Content);
      Response.ContentType := 'text/html';
      Response.CharSet := 'utf-8';
    end;
  end;
end;

end.
