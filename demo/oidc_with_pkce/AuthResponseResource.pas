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

unit AuthResponseResource;

// note: this is unsupported example code

interface

uses
  djWebComponent, djTypes;

type

  { TAuthResponseResource }

  TAuthResponseResource = class(TdjWebComponent)
  private
    function GetAuthToken(const AuthorizationCode: string;
      const CodeVerifier: string): string;
   function ParseResponse(const TokenResponse: string): string;
  public
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdLogDebug, IdGlobal,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  JsonDataObjects,
  SysUtils, Classes;

function CreateIdHTTPwithSSL12: TIdHTTP;
var
  IOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result := TIdHTTP.Create;

  IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Result);
  IOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
  Result.IOHandler := IOHandler;

  // Raw HTTP logging
  // Result.Intercept := TIdLogDebug.Create(Result);
  // TIdLogDebug(Result.Intercept).Active := True;
end;

{ TAuthResponseResource }

procedure TAuthResponseResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  AuthorizationCode: string;
  CodeVerifier: string;
  TokenResponse: string;
  AccessToken: string;
//  P: string;
begin
  if Request.Params.Values['state'] <> Request.Session.Content.Values['state'] then
  begin
    Response.ResponseNo := 401;
    WriteLn('Invalid state parameter.');
    Exit;
  end;

//  for P in Request.Params do
//  begin
//    WriteLn(P);
//  end;

  AuthorizationCode := Request.Params.Values['code'];

  if (AuthorizationCode <> '') then
  begin
    CodeVerifier := Request.Session.Content.Values['CodeVerifier'];
    TokenResponse := GetAuthToken(AuthorizationCode, CodeVerifier);
    AccessToken := ParseResponse(TokenResponse);
    Response.Session.Content.Values['access_token'] := AccessToken;
    Response.Redirect('/index.html');
  end;
  // ...
end;

function TAuthResponseResource.ParseResponse(const TokenResponse: string): string;
var
  Obj: TJsonObject;
  AccessToken: string;
begin
  Obj := TJsonObject.Parse(TokenResponse) as TJsonObject;

  AccessToken := Obj.S['access_token'];

  Result := AccessToken;
end;

function TAuthResponseResource.GetAuthToken(const AuthorizationCode: string;
  const CodeVerifier: string): string;
const
  TOKEN_ENDPOINT = 'https://login.microsoftonline.com/consumers/oauth2/v2.0/token';
var
  HTTP: TIdHTTP;
  RequestBody: TStrings;
begin
  HTTP := CreateIdHTTPwithSSL12;
  try
    try
      RequestBody := TStringList.Create;
      try
        HTTP.Request.ContentType := 'application/x-www-form-urlencoded';

        RequestBody.Add('grant_type=authorization_code');
        RequestBody.Add('client_id=ee5a0402-2861-44a2-b0e1-d79bfafbe56a');

        RequestBody.Add('redirect_uri=http://localhost/auth-response');
        RequestBody.Add('code=' + AuthorizationCode);
        RequestBody.Add('code_verifier=' + CodeVerifier);

        Result := HTTP.Post(TOKEN_ENDPOINT, RequestBody);

//        WriteLn(Result);

      finally
        RequestBody.Free;
      end;
    except
      on E: EIdHTTPProtocolException do
      begin
        WriteLn(E.Message);
        WriteLn(E.ErrorMessage);
        raise;
      end;
      on E: Exception do
      begin
        WriteLn(E.Message);
        raise;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

end.

