(*

    Daraja HTTP Framework
    Copyright (c) Michael Justin

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.


    You can be released from the requirements of the license by purchasing
    a commercial license. Buying such a license is mandatory as soon as you
    develop commercial activities involving the Daraja framework without
    disclosing the source code of your own applications. These activities
    include: offering paid services to customers as an ASP, shipping Daraja
    with a closed source product.

*)

unit MainUnit;

// note: this is unsupported example code

interface

uses
  djWebComponent, djWebFilter, djServerContext, djTypes, djInterfaces,
  Generics.Collections;

type

  { TRootResource }

  TRootResource = class(TdjWebComponent)
  private
    ClientID: string;
    TokenEndpoint: string;
    function GetAccessTokenByRefreshToken(const RefreshToken: string): string;
  public
    procedure Init; override;
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

  { TAuthFilter }

  TAuthFilter = class(TdjWebFilter)
  private
    AuthorizeEndpoint: string;
    ClientID: string;
    RedirectURI: string;
    function BuildCodeChallenge(const ACodeVerifier: string): string;
  public
    procedure Init; override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TAuthResponseResource }

  TAuthResponseResource = class(TdjWebComponent)
  private
    ClientID: string;
    TokenEndpoint: string;
    RedirectURI: string;
    function GetAccessToken(const AuthorizationCode: string;
      const CodeVerifier: string): string;
  public
    procedure Init(const Config: IWebComponentConfig); override;
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure Demo;

implementation

uses
  djServer, djWebAppContext, djNCSALogFilter,
  IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdCoderMIME, IdHashSHA, IdGlobal,
  JsonDataObjects, ShellAPI, SysUtils, Classes;

procedure Demo;
const
  AuthorizeEndpoint = 'https://accounts.google.com/o/oauth2/auth';
  TokenEndpoint = 'https://oauth2.googleapis.com/token';
  // Application (client) ID from App onfiguration
  ClientId = '170453537434-70q3hn1c8cd28ughsp3dagb934oq17h2.apps.googleusercontent.com';
  RedirectPath = '/auth-response';
  RedirectURI = 'http://127.0.0.1' + RedirectPath;
  RootResourcePath = '/index.html';
  StartURI = 'http://127.0.0.1' + RootResourcePath;
var
  Context: TdjWebAppContext;
  Server: TdjServer;
begin
  Context := TdjWebAppContext.Create('', True);

  Context.Add(TRootResource, RootResourcePath);
  Context.Add(TAuthResponseResource, RedirectPath);
  Context.Add(TAuthFilter, '*.html');
  Context.Add(TdjNCSALogFilter, '/*');

  Context.SetInitParameter('ClientID', ClientId);
  Context.SetInitParameter('RedirectURI', RedirectURI);
  Context.SetInitParameter('AuthorizeEndpoint', AuthorizeEndpoint);
  Context.SetInitParameter('TokenEndpoint', TokenEndpoint);

  Server := TdjServer.Create(80);
  try
    try
      Server.Add(Context);
      Server.Start;
      ShellExecute(0, 'open', PChar(StartURI), '', '', 0);
      WriteLn('Server is running, launching web browser ...');
      WriteLn('Hit enter to terminate.');
    except
      on E: Exception do WriteLn(E.Message);
    end;
    ReadLn;
  finally
    Server.Free;
  end;
end;

function CreateIdHTTPwithTLS12: TIdHTTP;
var
  IOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result := TIdHTTP.Create;
  IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Result);
  IOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
  Result.IOHandler := IOHandler;
end;

function ParseJsonResponseBody(const JsonResponseBody: string): TDictionary<string, string>;
var
  Obj: TJsonObject;
  JSONPair: TJsonNameValuePair;
begin
  Result := TDictionary<string, string>.Create;
  Obj := TJsonObject.Parse(JsonResponseBody) as TJsonObject;
  for JSONPair in Obj do begin
    Result.Add(JSONPair.Name, JSONPair.Value);
  end;
end;

{ TRootResource }

procedure TRootResource.Init;
begin
  ClientID := Config.GetContext.GetInitParameter('ClientID');
  TokenEndpoint := Config.GetContext.GetInitParameter('TokenEndpoint');
end;

procedure TRootResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := Format('<html><body><h1>Refresh token example</h1>'
    + '<p><b>Access token:</b> %s</p>'
    + '<p><b>Token type:</b> %s</p>'
    + '<p><b>Expires in:</b> %s seconds</p>'
    + '<p><b>Refresh token:</b> %s</p>'
    + '<form method="post"><input type="submit" value="Use refresh token"/></form>'
    + '</body></html>',
    [Request.Session.Content.Values['access_token'],
     Request.Session.Content.Values['token_type'],
     Request.Session.Content.Values['expires_in'],
     Request.Session.Content.Values['refresh_token']]);
  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

procedure TRootResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  RefreshToken: string;
  RefreshResponse: string;
  ResponseMap: TDictionary<string, string>;
  OldAccessToken: string;
  NewAccessToken: string;
begin
  OldAccessToken := Request.Session.Content.Values['access_token'];
  RefreshToken := Request.Session.Content.Values['refresh_token'];
  RefreshResponse := GetAccessTokenByRefreshToken(RefreshToken);
  ResponseMap := ParseJsonResponseBody(RefreshResponse);
  NewAccessToken := ResponseMap['access_token'];
  Response.ContentText := Format('<html><body><h1>Refresh token example</h1>'
    + '<p><b>Old access token:</b> %s</p>'
    + '<p><b>New access token:</b> %s</p>'
    + '</body></html>',
    [OldAccessToken, NewAccessToken]);
  Request.Session.Content.Values['access_token'] := NewAccessToken;
  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

function CreateGUIDString: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
end;

function TRootResource.GetAccessTokenByRefreshToken(const RefreshToken: string): string;
var
  HTTP: TIdHTTP;
  RequestBody: TStrings;
begin
  HTTP := CreateIdHTTPwithTLS12;
  try
    RequestBody := TStringList.Create;
    try
      HTTP.Request.ContentType := 'application/x-www-form-urlencoded';
      RequestBody.Add('client_id=' + ClientID);
      RequestBody.Add('scope=openid');
      RequestBody.Add('access_type=offline');
      RequestBody.Add('grant_type=refresh_token');
      RequestBody.Add('refresh_token=' + RefreshToken);
      Result := HTTP.Post(TokenEndpoint, RequestBody);
    finally
      RequestBody.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

{ TAuthFilter }

procedure TAuthFilter.Init;
begin
  AuthorizeEndpoint := Config.GetContext.GetInitParameter('AuthorizeEndpoint');
  ClientID := Config.GetContext.GetInitParameter('ClientID');
  RedirectURI := Config.GetContext.GetInitParameter('RedirectURI');
end;

procedure TAuthFilter.DoFilter(Context: TdjServerContext; Request: TdjRequest;
  Response: TdjResponse; const Chain: IWebFilterChain);
var
  CodeVerifier: string;
  CodeChallenge: string;
  AccessToken: string;
  State: string;
begin
  AccessToken := Request.Session.Content.Values['access_token'];
  if AccessToken = '' then
  begin
    CodeVerifier := Copy(CreateGUIDString, 2, 12);
    Response.Session.Content.Values['CodeVerifier'] := CodeVerifier;

    CodeChallenge := BuildCodeChallenge(CodeVerifier);

    State := CreateGUIDString;
    Response.Session.Content.Values['state'] := State;
    Response.Redirect(AuthorizeEndpoint
     + '?client_id=' + ClientID          // Your app registration's Application (client) ID
     + '&response_type=code'             // Request an auth code
     + '&redirect_uri=' + RedirectURI
     + '&scope=openid'
     + '&access_type=offline'            // Request offline access
     + '&response_mode=form_post'
     + '&state=' + State
     + '&code_challenge=' + CodeChallenge
     + '&code_challenge_method=S256');
  end else begin
    Chain.DoFilter(Context, Request, Response); // pass
  end;
end;

function TAuthFilter.BuildCodeChallenge(const ACodeVerifier: string): string;
var
  IdHashSHA256: TIdHashSHA256;
  SHA256Bytes: TIdBytes;
  SHA256: string;
begin
  if not IdSSLOpenSSLHeaders.Load then
  begin
    WriteLn(IdSSLOpenSSLHeaders.WhichFailedToLoad);
  end;

  IdHashSHA256 := TIdHashSHA256.Create;
  try
    SHA256Bytes := IdHashSHA256.HashString(ACodeVerifier, IndyTextEncoding_ASCII);
    SHA256 := TIdEncoderMIME.EncodeBytes(SHA256Bytes);
  finally
    IdHashSHA256.Free;
  end;

  Result := StringReplace(SHA256, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '',  [rfReplaceAll]);
end;

{ TAuthResponseResource }

procedure TAuthResponseResource.Init(const Config: IWebComponentConfig);
begin
  ClientID := Config.GetContext.GetInitParameter('ClientID');
  TokenEndpoint := Config.GetContext.GetInitParameter('TokenEndpoint');
  RedirectURI := Config.GetContext.GetInitParameter('RedirectURI');
end;

procedure TAuthResponseResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  AuthorizationCode: string;
  CodeVerifier: string;
  TokenResponse: string;
  ResponseMap: TDictionary<string, string>;
begin
  if Request.Params.Values['state'] <> Request.Session.Content.Values['state'] then
  begin
    Response.ResponseNo := 401;
    WriteLn('Invalid state parameter.');
    Exit;
  end;

  AuthorizationCode := Request.Params.Values['code'];
  if AuthorizationCode <> '' then
  begin
    CodeVerifier := Request.Session.Content.Values['CodeVerifier'];
    TokenResponse := GetAccessToken(AuthorizationCode, CodeVerifier);
    ResponseMap := ParseJsonResponseBody(TokenResponse);
    Response.Session.Content.Values['access_token'] := ResponseMap['access_token'];
    Response.Session.Content.Values['token_type'] := ResponseMap['token_type'];
    Response.Session.Content.Values['expires_in'] := ResponseMap['expires_in'];
    Response.Session.Content.Values['refresh_token'] := ResponseMap['refresh_token'];
    Response.Redirect('/index.html');
  end;
end;

function TAuthResponseResource.GetAccessToken(const AuthorizationCode: string;
  const CodeVerifier: string): string;
var
  HTTP: TIdHTTP;
  RequestBody: TStrings;
begin
  HTTP := CreateIdHTTPwithTLS12;
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
