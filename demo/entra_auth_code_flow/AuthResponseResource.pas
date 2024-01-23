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
  djWebComponent, djTypes, djInterfaces;

type

  { TAuthResponseResource }

  TAuthResponseResource = class(TdjWebComponent)
  private
    ClientID: string;
    TokenEndpoint: string;
    RedirectURI: string;
    function GetAuthToken(const AuthorizationCode: string;
      const CodeVerifier: string): string;
   function ParseResponse(const TokenResponse: string): string;
  public
    procedure Init(const Config: IWebComponentConfig); override;
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders,
  {$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  IdLogDebug, IdGlobal,
  {$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  {$IFDEF FPC}
  fpjson, jsonparser,
  {$ELSE}
  JsonDataObjects,
  {$ENDIF}
  SysUtils, Classes;

function CreateIdHTTPwithSSL12: TIdHTTP;
var
  IOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result := TIdHTTP.Create;

  IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Result);
  IOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
  Result.IOHandler := IOHandler;

  {$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  // Raw HTTP logging
  Result.Intercept := TIdLogDebug.Create(Result);
  TIdLogDebug(Result.Intercept).Active := True;
  {$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}
end;

{ TAuthResponseResource }

procedure TAuthResponseResource.Init(const Config: IWebComponentConfig);
begin
  inherited;

  ClientID := Config.GetContext.GetInitParameter('ClientID');
  TokenEndpoint := Config.GetContext.GetInitParameter('TokenEndpoint');
  RedirectURI := Config.GetContext.GetInitParameter('RedirectURI');
end;

procedure TAuthResponseResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  AuthorizationCode: string;
  CodeVerifier: string;
  TokenResponse: string;
  AccessToken: string;
  {$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  P: string;
  {$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}
begin
  if Request.Params.Values['state'] <> Request.Session.Content.Values['state'] then
  begin
    Response.ResponseNo := 401;
    WriteLn('Invalid state parameter.');
    Exit;
  end;

  {$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}
  for P in Request.Params do
  begin
    WriteLn(P);
  end;
  {$ENDIF DARAJA_PROJECT_STAGE_DEVELOPMENT}

  AuthorizationCode := Request.Params.Values['code'];

  if AuthorizationCode <> '' then
  begin
    CodeVerifier := Request.Session.Content.Values['CodeVerifier'];
    TokenResponse := GetAuthToken(AuthorizationCode, CodeVerifier);
    AccessToken := ParseResponse(TokenResponse);
    Response.Session.Content.Values['access_token'] := AccessToken;
    Response.Redirect('/index.html');
  end;
end;

{$IFDEF FPC}
function TAuthResponseResource.ParseResponse(const TokenResponse: string): string;
var
  Data: TJSONData;
  Obj : TJSONObject;
begin
  Data := GetJSON(TokenResponse);
  Obj := TJSONObject(Obj);
  Result := Obj.Get('access_token');
end;
{$ELSE}
function TAuthResponseResource.ParseResponse(const TokenResponse: string): string;
var
  Obj: TJsonObject;
begin
  Obj := TJsonObject.Parse(TokenResponse) as TJsonObject;
  Result := Obj.S['access_token'];
end;
{$ENDIF}

function TAuthResponseResource.GetAuthToken(const AuthorizationCode: string;
  const CodeVerifier: string): string;
var
  HTTP: TIdHTTP;
  RequestBody: TStrings;
begin
  HTTP := CreateIdHTTPwithSSL12;
  try
    RequestBody := TStringList.Create;
    try
      HTTP.Request.ContentType := 'application/x-www-form-urlencoded';

      RequestBody.Add('grant_type=authorization_code');
      RequestBody.Add('client_id=' + ClientID);
      RequestBody.Add('redirect_uri=' + RedirectURI);
      RequestBody.Add('code=' + AuthorizationCode);
      RequestBody.Add('code_verifier=' + CodeVerifier);

      Result := HTTP.Post(TokenEndpoint, RequestBody);

    finally
      RequestBody.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

end.

