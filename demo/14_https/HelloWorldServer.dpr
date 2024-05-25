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

program HelloWorldServer;

{$APPTYPE CONSOLE}

uses
  djWebAppContext, djWebComponent, djWebComponentContextHandler, djServer,
  djHTTPConnector, djTypes,
  IdSSLOpenSSL,
  IdGlobal,
  SysUtils, Classes,
  HelloWorldResource in 'HelloWorldResource.pas';

procedure Demo;
var
  IOHandler: TIdServerIOHandlerSSLOpenSSL;
  Server: TdjServer;
  Connector: TdjHTTPConnector;
  Context: TdjWebAppContext;
begin
  IOHandler := TIdServerIOHandlerSSLOpenSSL.Create;
  IOHandler.SSLOptions.CertFile := 'cert.pem';
  IOHandler.SSLOptions.KeyFile := 'key.pem';
  IOHandler.SSLOptions.RootCertFile := 'cacert.pem';
  IOHandler.SSLOptions.Mode := sslmServer;

  Server := TdjServer.Create;
  try
    // add HTTPS connector
    Connector := TdjHTTPConnector.Create(Server.Handler);
    Connector.Host := '127.0.0.1';
    Connector.Port := 443;
    Connector.HTTPServer.IOHandler := IOHandler;
    Server.AddConnector(Connector);

    // add context
    Context := TdjWebAppContext.Create('tutorial');
    Context.Add(THelloWorldResource, '/hello');
    Server.Add(Context);
    Server.Start;

    WriteLn('Server is running, please open https://localhost/tutorial/hello');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
  end;
end;

begin
  Demo;
end.
