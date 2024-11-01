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

unit MainUnit;

// note: this is unsupported example code

interface

uses
  djWebComponent, djWebFilter, djServerContext, djTypes, djInterfaces,
  Generics.Collections;

type

  { TRootResource }

  TRootResource = class(TdjWebComponent)
  public
    procedure Init(const Config: IWebComponentConfig); override;
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

  { TAuthFilter }

  TAuthFilter = class(TdjWebFilter)
  private
    AuthorizeEndpoint: string;
    ClientID: string;
    RedirectURI: string;
    function BuildCodeChallenge(const ACodeVerifier: string): string;
  public
    procedure Init(const Config: IWebFilterConfig); override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TAuthResponseResource }

  TAuthResponseResource = class(TdjWebComponent)
  private
    ClientID: string;
    TokenEndpoint: string;
    RedirectURI: string;
    function GetAuthToken(const AuthorizationCode: string;
      const CodeVerifier: string): string;
   function ParseResponse(const TokenResponse: string): TDictionary<string, string>;
  public
    procedure Init(const Config: IWebComponentConfig); override;
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure Demo;

implementation

uses
  djServer, djWebAppContext, djNCSALogFilter,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdCoderMIME, IdHashSHA, IdGlobal,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  {$IFDEF FPC}
  fpjson, jsonparser,
  {$ELSE}
  JsonDataObjects,
  {$ENDIF}
  ShellAPI, SysUtils, Classes;

procedure Demo;
const
  TokenEndpoint = 'https://login.microsoftonline.com/consumers/oauth2/v2.0/token';
  AuthorizeEndpoint = 'https://login.microsoftonline.com/consumers/oauth2/v2.0/authorize';
  // Application (client) ID from Entra configuration
  ClientId = '8e03cfb2-2cd2-48fd-b7b5-789ad1c13fd2';
  // Redirection URI to which the response will be sent.
  // MUST exactly match one of the Redirection URI values
  // for the Client pre-registered at the OpenID Provider.
  RedirectPath = '/auth-response';
  RedirectURI = 'http://127.0.0.1' + RedirectPath;
  RootResourcePath = '/index.html';
  StartURI = 'http://127.0.0.1' + RootResourcePath;
var
  Context: TdjWebAppContext;
  Server: TdjServer;
begin
  Context := TdjWebAppContext.Create('', True);

  Context.AddWebComponent(TRootResource, RootResourcePath);
  Context.AddWebComponent(TAuthResponseResource, RedirectPath);
  Context.AddFilterWithMapping(TAuthFilter, '*.html');
  Context.AddFilterWithMapping(TdjNCSALogFilter, '/*');

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

function CreateIdHTTPwithSSL12: TIdHTTP;
var
  IOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  Result := TIdHTTP.Create;
  IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Result);
  IOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
  Result.IOHandler := IOHandler;
end;

{ TRootResource }

procedure TRootResource.Init(const Config: IWebComponentConfig);
begin
  inherited;
end;

procedure TRootResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := Format('<html><body><h1>Refresh token example</h1>'
    + '<p><b>Access token:</b> %s</p>'
    + '<p><b>Token type:</b> %s</p>'
    + '<p><b>Expires in:</b> %s seconds</p>'
    + '<p><b>Refresh token:</b> %s</p>'
    + '</body></html>',
    [Request.Session.Content.Values['access_token'],
     Request.Session.Content.Values['token_type'],
     Request.Session.Content.Values['expires_in'],
     Request.Session.Content.Values['refresh_token']]);
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

{ TAuthFilter }

procedure TAuthFilter.Init(const Config: IWebFilterConfig);
begin
  inherited;
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
     + '&scope=openid offline_access'    // Request offline access
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
    TokenResponse := GetAuthToken(AuthorizationCode, CodeVerifier);
    ResponseMap := ParseResponse(TokenResponse);
    Response.Session.Content.Values['access_token'] := ResponseMap['access_token'];
    Response.Session.Content.Values['token_type'] := ResponseMap['token_type'];
    Response.Session.Content.Values['expires_in'] := ResponseMap['expires_in'];
    Response.Session.Content.Values['refresh_token'] := ResponseMap['refresh_token'];
    Response.Redirect('/index.html');
  end;
end;

function TAuthResponseResource.ParseResponse(const TokenResponse: string): TDictionary<string, string>;
{$IFDEF FPC}
var
  Data: TJSONData;
  Obj : TJSONObject;
begin
  Result := TDictionary<string, string>.Create;
  Data := GetJSON(TokenResponse);
  Obj := TJSONObject(Obj);
  Result['access_token'], Obj.Get('access_token')); // todo
end;
{$ELSE}
var
  Obj: TJsonObject;
  JSONPair: TJsonNameValuePair;
begin
  Result := TDictionary<string, string>.Create;
  Obj := TJsonObject.Parse(TokenResponse) as TJsonObject;
  for JSONPair in Obj do begin
    Result.Add(JSONPair.Name, JSONPair.Value);
  end;
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
