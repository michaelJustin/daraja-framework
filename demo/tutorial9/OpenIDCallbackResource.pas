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

unit OpenIDCallbackResource;

// note: this is unsupported example code

interface

uses
  djWebComponent, djTypes;

type

  { TOpenIDCallbackResource }

  TOpenIDCallbackResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  OpenIDHelper,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdHTTP,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils, Classes;

{ TOpenIDCallbackResource }

// https://developers.google.com/identity/protocols/OpenIDConnect

// https://openid.net/specs/openid-connect-core-1_0.html#AuthResponse

// "When using the Authorization Code Flow, the Client MUST validate the response according to RFC 6749, especially Sections 4.1.2 and 10.12."

procedure TOpenIDCallbackResource.OnGet(Request: TdjRequest;
  Response: TdjResponse);
var
  AuthCode: string;
  IdHTTP: TIdHTTP;
  Params: TStrings;
  ResponseText: string;
begin
  AuthCode := Request.Params.Values['code'];

  if AuthCode = '' then
  begin
    // get an auth code
    Response.Redirect(OpenIDParams.auth_uri
     + '?client_id=' + OpenIDParams.client_id
     + '&response_type=code'
     + '&scope=openid%20profile%20email'
     + '&redirect_uri=' + OpenIDParams.redirect_uri
     + '&state=' + Request.Session.Content.Values['state']
     );
  end
  else
  begin
    // auth code received, check state first
    if (Request.Params.Values['state'] <> Request.Session.Content.Values['state']) then
    begin
      Response.ResponseNo := 401;
      WriteLn('Invalid state parameter.');
      Exit;
    end;

    // exchange auth code for claims
    Params := TStringList.Create;
    IdHTTP := TIdHTTP.Create;
    try
      Params.Values['code'] := AuthCode;
      Params.Values['client_id'] := OpenIDParams.client_id;
      Params.Values['client_secret'] := OpenIDParams.client_secret;
      Params.Values['redirect_uri'] := OpenIDParams.redirect_uri;
      Params.Values['grant_type'] := 'authorization_code';

      ResponseText := IdHTTP.Post(OpenIDParams.token_uri, Params);

      Response.Session.Content.Values['credentials'] := ResponseText;

      Response.Redirect('/index.html');
    finally
      IdHTTP.Free;
      Params.Free;
    end;
  end
end;

end.

