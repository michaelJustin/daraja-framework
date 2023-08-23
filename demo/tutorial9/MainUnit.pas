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
  djServer,
  djWebAppContext,
  djNCSALogFilter,
  djWebComponentHolder,
  RootResource,
  OpenIDAuthFilter,
  OpenIDCallbackResource,
  ShellAPI,
  SysUtils;

procedure Demo;
const
  // URI must match OAuth 2.0 settings in Google Cloud project
  REDIRECT_URI = 'http://localhost/openidcallback';
  // Must point to file downloaded from Google Cloud project
  SECRET_FILE = 'client_secret.json';
var
  Context: TdjWebAppContext;
  Holder: TdjWebComponentHolder;
  Server: TdjServer;
begin
  Context := TdjWebAppContext.Create('', True);
  Context.AddWebComponent(TRootResource, '/index.html');

  Holder := TdjWebComponentHolder.Create(TOpenIDCallbackResource);

  Holder.SetInitParameter('RedirectURI', REDIRECT_URI);
  Holder.SetInitParameter('secret.file', SECRET_FILE);
  Context.AddWebComponent(Holder, '/openidcallback');

  Context.AddFilterWithMapping(TOpenIDAuthFilter, '*.html');
  Context.AddFilterWithMapping(TdjNCSALogFilter, '/*');

  Server := TdjServer.Create(80);
  try
    try
      Server.Add(Context);
      Server.Start;

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
