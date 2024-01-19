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
    function encode_SHA256_base64URL(const str_toEncode: string): string;
  public
    procedure Init(const Config: IWebFilterConfig); override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

implementation

uses
  IdCoderMIME, IdGlobal, IdSSLOpenSSL, IdHashSHA;

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
  AuthorizeEndpoint := Config.GetInitParameter('AuthorizeEndpoint');
  ClientID := Config.GetInitParameter('ClientID');
  RedirectURI := Config.GetInitParameter('RedirectURI');
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

    CodeChallenge := encode_SHA256_base64URL(CodeVerifier);

    State := CreateGUIDString;
    Response.Session.Content.Values['state'] := State;
    Response.Redirect(AuthorizeEndpoint
     + '?client_id=' + ClientID          // Your app registration's Application (client) ID
     + '&response_type=code'             // Request an auth code
     + '&redirect_uri=' + RedirectURI
     + '&scope=User.Read Mail.Send'      // Request read profile and send mail permission
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


function TAuthFilter.encode_SHA256_base64URL(const str_toEncode: string): string;
var
  hash_sha256: TIdHashSHA256;
  arr_sha256:  TIdBytes;
  str_b64:     string;
  str_b64URL:  string;
begin
  IdSSLOpenSSL.LoadOpenSSLLibrary;
  hash_sha256 := TIdHashSHA256.Create;
  try
    arr_sha256 := hash_sha256
      .HashString(str_toEncode, IndyTextEncoding_ASCII); // Hash SHA256
    str_b64 := TIdEncoderMIME.EncodeBytes(arr_sha256);
  finally
    hash_sha256.Free;
  end;
  IdSSLOpenSSL.UnLoadOpenSSLLibrary;

  // convert to Base64URL
  str_b64URL := str_b64;
  str_b64URL := StringReplace(str_b64URL, '+', '-', [rfReplaceAll]);    // Replace + with -
  str_b64URL := StringReplace(str_b64URL, '/', '_', [rfReplaceAll]);    // Replace / with _
  str_b64URL := StringReplace(str_b64URL, '=', '',  [rfReplaceAll]);    // Remove padding, character =

  Assert(Length(str_b64URL) = 43);
  result := str_b64URL;
end;

end.
