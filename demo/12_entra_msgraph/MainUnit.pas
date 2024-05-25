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
  djServer, djWebAppContext, djNCSALogFilter, djWebFilterHolder,
  ShellAPI, SysUtils;

procedure Demo;
const
  AUTHORIZE_ENDPOINT = 'https://login.microsoftonline.com/consumers/oauth2/v2.0/authorize';
  // Application (client) ID from Entra configuration
  CLIENT_ID = 'ee5a0402-2861-44a2-b0e1-d79bfafbe56a';
  // Redirect URI must match Entra configuration
  REDIRECT_PATH = '/auth-response';
  REDIRECT_URI = 'http://localhost' + REDIRECT_PATH;
var
  Context: TdjWebAppContext;
  FilterHolder: TdjWebFilterHolder;
  Server: TdjServer;
begin
  FilterHolder := TdjWebFilterHolder.Create(TAuthFilter);
  FilterHolder.SetInitParameter('AuthorizeEndpoint', AUTHORIZE_ENDPOINT);
  FilterHolder.SetInitParameter('ClientID', CLIENT_ID);
  FilterHolder.SetInitParameter('RedirectURI', REDIRECT_URI);

  Context := TdjWebAppContext.Create('', True);
  Context.AddWebComponent(TRootResource, '/index.html');
  Context.AddWebComponent(TAuthResponseResource, REDIRECT_PATH);
  Context.AddWebFilter(FilterHolder, '*.html');
  Context.AddFilterWithMapping(TdjNCSALogFilter, '/*');

  Server := TdjServer.Create(80);
  try
    try
      Server.Add(Context);
      Server.Start;

      ShellExecute(0, 'open', PChar('http://localhost/index.html'), '', '', 0);

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
