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

procedure Demo;

implementation

uses
  AuthFilter,
  AuthResponseResource,
  RootResource,
  djServer, djWebAppContext, djNCSALogFilter,
  ShellAPI, SysUtils;

procedure Demo;
const
  TOKEN_ENDPOINT = 'https://login.microsoftonline.com/consumers/oauth2/v2.0/token';
  AUTHORIZE_ENDPOINT = 'https://login.microsoftonline.com/consumers/oauth2/v2.0/authorize';
  // Application (client) ID from Entra configuration
  CLIENT_ID = '8e03cfb2-2cd2-48fd-b7b5-789ad1c13fd2';
  // Redirect URI must match Entra configuration
  REDIRECT_PATH = '/auth-response';
  REDIRECT_URI = 'http://127.0.0.1' + REDIRECT_PATH;
  GRAPH_API_ENDPOINT = 'https://graph.microsoft.com';
var
  Context: TdjWebAppContext;
  Server: TdjServer;
begin
  Context := TdjWebAppContext.Create('', True);

  Context.AddWebComponent(TRootResource, '/index.html');
  Context.AddWebComponent(TAuthResponseResource, REDIRECT_PATH);
  Context.AddFilterWithMapping(TAuthFilter, '*.html');
  Context.AddFilterWithMapping(TdjNCSALogFilter, '/*');

  Context.SetInitParameter('ClientID', CLIENT_ID);
  Context.SetInitParameter('RedirectURI', REDIRECT_URI);
  Context.SetInitParameter('AuthorizeEndpoint', AUTHORIZE_ENDPOINT);
  Context.SetInitParameter('TokenEndpoint', TOKEN_ENDPOINT);
  Context.SetInitParameter('GraphAPIEndpoint', GRAPH_API_ENDPOINT);

  Server := TdjServer.Create(80);
  try
    try
      Server.Add(Context);
      Server.Start;

      ShellExecute(0, 'open', PChar('http://127.0.0.1/index.html'), '', '', 0);

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

end.
