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
  IdCoderMIME, IdGlobal, IdSSLOpenSSL, IdHashSHA;

function CreateGUIDString: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := GUIDToString(Guid);
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

function TAuthFilter.BuildCodeChallenge(const ACodeVerifier: string): string;
var
  IdHashSHA256: TIdHashSHA256;
  SHA256Bytes: TIdBytes;
  SHA256: string;
begin
  IdSSLOpenSSL.LoadOpenSSLLibrary;
  IdHashSHA256 := TIdHashSHA256.Create;
  try
    SHA256Bytes := IdHashSHA256.HashString(ACodeVerifier, IndyTextEncoding_ASCII);
    SHA256 := TIdEncoderMIME.EncodeBytes(SHA256Bytes);
  finally
    IdHashSHA256.Free;
  end;
  IdSSLOpenSSL.UnLoadOpenSSLLibrary;

  Result := StringReplace(SHA256, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '=', '',  [rfReplaceAll]);

//  Assert(Length(Result) = 43);
end;

end.
