(*

    Daraja HTTP Framework
    Copyright (c) 2016 Michael Justin

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

unit AuthFilter;

// note: this is unsupported example code

interface

uses
  djWebFilter,
  djServerContext,
  djTypes,
  djInterfaces,
  Classes, SysUtils;

type

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

implementation

uses
  IdCoderMIME, IdGlobal, IdSSLOpenSSL, IdHashSHA, IdURI,
  IdSSLOpenSSLHeaders;

var
  OpenSSLLoaded: Boolean = False;

function SecureRandomBytes(Count: Integer): TIdBytes;
begin
  if not OpenSSLLoaded then
    raise Exception.Create('OpenSSL could not be loaded');

  SetLength(Result, Count);
  if RAND_bytes(PIdAnsiChar(@Result[0]), Count) <> 1 then
    raise Exception.Create('RAND_bytes failed');
end;

// ---------------------------------------------------------------------------
// Base64url without padding, built on Indy's MIME encoder
// ---------------------------------------------------------------------------
function Base64UrlEncode(const Data: TIdBytes): string;
var
  Enc: TIdEncoderMIME;
begin
  Enc := TIdEncoderMIME.Create(nil);
  try
    Result := Enc.EncodeBytes(Data);
  finally
    Enc.Free;
  end;
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '',  [rfReplaceAll]);
end;

// ---------------------------------------------------------------------------
// PKCE
// ---------------------------------------------------------------------------
function BuildCodeVerifier: string;
begin
  // 32 random bytes -> 43 URL-safe characters (RFC 7636 minimum)
  Result := Base64UrlEncode(SecureRandomBytes(32));
end;

{ TAuthFilter }

procedure TAuthFilter.Init;
begin
  AuthorizeEndpoint := Config.GetContext.GetInitParameter('AuthorizeEndpoint');
  ClientID := Config.GetContext.GetInitParameter('ClientID');
  RedirectURI := Config.GetContext.GetInitParameter('RedirectURI');
end;

function Q(const S: string): string;
begin
  Result := TIdURI.ParamsEncode(S);
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
    CodeVerifier := BuildCodeVerifier;
    Response.Session.Content.Values['CodeVerifier'] := CodeVerifier;

    CodeChallenge := BuildCodeChallenge(CodeVerifier);

    State := Base64UrlEncode(SecureRandomBytes(16));  // 128 bits, 22 URL-safe chars
    Response.Session.Content.Values['state'] := State;

    Response.Redirect(AuthorizeEndpoint
     + '?client_id=' + Q(ClientID)          // Your app registration's Application (client) ID
     + '&response_type=code'                // Request an auth code
     + '&redirect_uri=' + Q(RedirectURI)
     + '&scope=' + Q('User.Read Mail.Send') // Request read profile and send mail permission
     + '&response_mode=form_post'
     + '&state=' + State
     + '&code_challenge=' + CodeChallenge
     + '&code_challenge_method=S256'
     );
  end
  else
  begin
    Chain.DoFilter(Context, Request, Response); // pass
  end;
end;

function TAuthFilter.BuildCodeChallenge(const ACodeVerifier: string): string;
var
  IdHashSHA256: TIdHashSHA256;
  SHA256Bytes: TIdBytes;
  SHA256: string;
begin
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

  // Assert(Length(Result) = 43);
end;

initialization
  OpenSSLLoaded := IdSSLOpenSSL.LoadOpenSSLLibrary;

finalization
  IdSSLOpenSSL.UnLoadOpenSSLLibrary;

end.
