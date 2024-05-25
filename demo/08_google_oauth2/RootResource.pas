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
  OAuthHelper,
  IdHTTP, SysUtils;

// see https://developers.google.com/identity/protocols/OAuth2WebServer

{ TRootResource }

procedure TRootResource.OnGet(Request: TdjRequest; Response: TdjResponse);
var
  Credentials: TCredentials;
  IdHTTP: TIdHTTP;
begin
  if Request.Session.Content.Values['credentials'] = '' then begin
    Response.Redirect(MY_CALLBACK_URL)
  end else begin
    Credentials := ToCredentials(Request.Session.Content.Values['credentials']);
    if Credentials.expires_in <= 0 then begin
      Response.Redirect(MY_CALLBACK_URL)
    end else begin
      IdHTTP := TIdHTTP.Create;
      try
        IdHTTP.Request.CustomHeaders.Values['Authorization'] :=
          'Bearer ' + Credentials.access_token;
        Response.ContentText := IdHTTP.Get('https://www.googleapis.com/drive/v2/files');
        Response.ContentType := 'text/plain';
        Response.CharSet := 'utf-8';
      finally
        IdHTTP.Free;
      end;
    end;
  end;
end;

end.
