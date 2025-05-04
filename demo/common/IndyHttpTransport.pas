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

unit IndyHttpTransport;

// note: this is unsupported example code

interface

{$i IdCompilerDefines.inc}

uses
  IdHTTP;

type
  TIndyHttpTransport = class(TIdCustomHTTP)
  public
    constructor Create;
  end;

implementation

uses
  djGlobal,
  IdSSLOpenSSL;

{ TIndyHttpTransport }

constructor TIndyHttpTransport.Create;
var
  SSLIO: TIdSSLIOHandlerSocketOpenSSL;
begin
  inherited Create;

  HTTPOptions := HTTPOptions + [hoNoProtocolErrorException, hoWantProtocolErrorContent];

  SSLIO := TIdSSLIOHandlerSocketOpenSSL.Create(Self);
  SSLIO.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
  SSLIO.SSLOptions.Mode        := sslmClient;
  SSLIO.SSLOptions.VerifyMode  := [];
  SSLIO.SSLOptions.VerifyDepth := 0;

  Self.IOHandler := SSLIO;

  Request.UserAgent := djGlobal.DWF_SERVER_FULL_NAME;
end;

end.
