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

interface

procedure Demo;

implementation

uses
  djServer,
  djWebAppContext,
  djInterfaces,
  djNCSALogFilter,
  djWebFilterConfig,
  PublicResource,
  TokenSigninResource,
  LoginResource,
  DashboardResource,
  ShellAPI,
  SysUtils;

procedure Demo;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
  WebFilterConfig: IWebFilterConfig;
begin
  Server := TdjServer.Create(80);
  try
    try
      Context := TdjWebAppContext.Create('', True);

      Context.AddWebComponent(TPublicResource, '/index.html');
      Context.AddWebComponent(TLoginResource, '/login.html');
      Context.AddWebComponent(TTokenSigninResource, '/tokensignin');
      Context.AddWebComponent(TDashboardResource, '/dashboard.html');
      WebFilterConfig := TdjWebFilterConfig.Create;
      Context.AddFilterWithMapping(TdjNCSALogFilter, '/*', WebFilterConfig);

      Server.Add(Context);

      Server.Start;

      // launch browser
      ShellExecute(0, 'open', PChar('http://localhost/index.html'), '', '', 0);

      WriteLn('Server is running, launching http://localhost/index.html ...');
      WriteLn('Hit any key to terminate.');
    except
      on E: Exception do WriteLn(E.Message);
    end;
    ReadLn;
  finally
    Server.Free;
  end;
end;


end.
