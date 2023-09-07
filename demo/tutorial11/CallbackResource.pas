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
  SysUtils, StrUtils;

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
   + '&nonce=' + Request.Session.Content.Values['nonce']
   );
end;

procedure TCallbackResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  Credentials: string;

  function Ellipsis(S: string): string;
  begin
    Result := Copy(S, 1, 12) + string(' … ') + RightStr(S, 8);
  end;

begin
  if (Request.Params.Values['state'] <> Request.Session.Content.Values['state']) then
  begin
    Response.ResponseNo := 401;
    WriteLn('Invalid state parameter.');
    Exit;
  end;

  Credentials := Format(
    'access_token: %s ' + #10 +
    'scope: %s '+ #10 +
    'token_type: %s '+ #10 +
    'expires_in: %s' + #10 +
    'id_token: %s',
    [
    Ellipsis(Request.Params.Values['access_token']),
    Request.Params.Values['scope'],
    Request.Params.Values['token_type'],
    Request.Params.Values['expires_in'],
    Ellipsis(Request.Params.Values['id_token'])
    ]);

  Response.Session.Content.Values['credentials'] := Credentials;

  Response.Redirect('/index.html');
end;

end.

