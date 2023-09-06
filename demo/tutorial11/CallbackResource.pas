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
  OpenIDHelper,
  djInterfaces, djWebComponent, djTypes;

type

  { TCallbackResource }

  TCallbackResource = class(TdjWebComponent)
  private
    OpenIDParams: TOpenIDParams;
    RedirectURI: string;
  public
    procedure Init(const Config: IWebComponentConfig); override;
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  // IdHTTP,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils, Classes;

{ TCallbackResource }

procedure TCallbackResource.Init(const Config: IWebComponentConfig);
begin
  inherited Init(Config);

  RedirectURI := Config.GetInitParameter('RedirectURI');
  OpenIDParams := LoadClientSecrets(Config.GetInitParameter('secret.file'));
end;

procedure TCallbackResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  // get an ID token and an access token
  Response.Redirect(OpenIDParams.auth_uri
   + '?client_id=' + OpenIDParams.client_id // Your app registration's Application (client) ID
   + '&response_type=id_token%20token'  // Requests both an ID token and access token
   + '&redirect_uri=' + RedirectURI
   + '&scope=openid'
   + '&response_mode=form_post'         // 'form_post' or 'fragment'
   + '&state=' + Request.Session.Content.Values['state']
   + '&nonce=' + Request.Session.Content.Values['state']
   );
end;

procedure TCallbackResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  State: string;
  IdToken: string;
  AccessToken: string;
  TokenType: string;
var
  ResponseText: string;
begin
  State := Request.Params.Values['state'];
  if (State <> Request.Session.Content.Values['state']) then
  begin
    Response.ResponseNo := 401;
    WriteLn('Invalid state parameter.');
    Exit;
  end;

  IdToken := Request.Params.Values['id_token'];
  AccessToken := Request.Params.Values['access_token'];
  TokenType := Request.Params.Values['token_type'];

  Response.Session.Content.Values['id_token'] := IdToken;
  Response.Session.Content.Values['access_token'] := AccessToken;
  Response.Session.Content.Values['token_type'] := TokenType;
  Response.Session.Content.Values['expires_in'] := Request.Params.Values['expires_in'];
  Response.Session.Content.Values['scope'] := Request.Params.Values['scope'];

  // call UserInfo API
  (* IdHTTP := TIdHTTP.Create;
  try

    IdHTTP.HTTPOptions := [hoNoProtocolErrorException, hoWantProtocolErrorContent];
    IdHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' + AccessToken;
    // see https://login.microsoftonline.com/common/v2.0/.well-known/openid-configuration
    ResponseText := IdHTTP.Get('https://graph.microsoft.com/oidc/userinfo');
  finally
    IdHTTP.Free;
  end; *)

  Response.Session.Content.Values['credentials'] := 'valid';
  Response.Redirect('/index.html');
end;

end.

