(*
    Daraja Framework
    Copyright (C) 2016 Michael Justin

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

unit djDefaultWebComponentTests;

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjDefaultWebComponentTests }

  TdjDefaultWebComponentTests = class(TTestCase)
  published
    procedure TestDefaultWebComponent;

    procedure TestDefaultWebComponentInRootContext;

    procedure DefaultWebComponentMissingResourcePath;

    procedure DefaultWebComponentResNotFound;
  end;

implementation

uses
  TestComponents, TestClient, djWebAppContext,
  djInterfaces, djWebComponentHolder, djServer, djDefaultWebComponent,
  IdHTTP,
  IdGlobal,
  SysUtils;

// helper functions ----------------------------------------------------------

{$IFDEF FPC}
function Get(Document: string; Host: string = 'http://127.0.0.1'; ADestEncoding: IIdTextEncoding = nil): string;
begin
  Result := TdjHTTPClient.Get(Document, Host, ADestEncoding);
end;
{$ELSE}
function Get(Document: string; Host: string = 'http://127.0.0.1'): string;
begin
  Result := TdjHTTPClient.Get(Document, Host);
end;
{$ENDIF}

function Get404(Document: string; Host: string = 'http://127.0.0.1'): string;
begin
  Result := TdjHTTPClient.Get(Host + Document, [404]);
end;

procedure TdjDefaultWebComponentTests.TestDefaultWebComponent;
var
  Server: TdjServer;
  Holder: TdjWebComponentHolder;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create the 'test' context
    Context := TdjWebAppContext.Create('test');

    // create example component and register it
    Holder := TdjWebComponentHolder.Create(TExamplePage);
    Context.AddWebComponent(Holder, '/index.html');

    Server.Add(Context);
    Server.Start;

    // test TExampleWebComponent
    CheckEquals('example', Get('/test/index.html'), '/test/index.html');

    // test static
    try
      CheckEquals('staticcontent', Get('/test/static.html'),
        '/test/static.html');
    except
      on E: EIdHTTPProtocolException do
      begin
        // expected
      end;
    end;
    (**)
    // create default web component and register it
    Holder := TdjWebComponentHolder.Create(TdjDefaultWebComponent);
    Context.AddWebComponent(Holder, '/');

    // test static
    CheckEquals('staticcontent', Get('/test/static.html'), '/test/static.html');

    // Expected Exception := EIdHTTPProtocolException;
    try
      Get('/test/missing.html')
    except
      on E: EIdHTTPProtocolException do
      begin
        // expected exception
      end;
      on E: Exception do
      begin
        Fail(E.Message);
      end;
    end;

  finally
    Server.Free;
  end;

end;

procedure TdjDefaultWebComponentTests.TestDefaultWebComponentInRootContext;
var
  Server: TdjServer;
  Holder: TdjWebComponentHolder;
  Context: TdjWebAppContext;
begin

  Server := TdjServer.Create;
  try
    // create the 'test' context
    Context := TdjWebAppContext.Create('');

    // create example component and register it
    Holder := TdjWebComponentHolder.Create(TExamplePage);
    Context.AddWebComponent(Holder, '/index.html');

    Server.Add(Context);

    Server.Start;

    CheckEquals('example', Get('/index.html'), '/index.html');

    // test static
    try
      CheckEquals('staticcontent', Get('/static.html'), '/static.html');
    except
      on E: EIdHTTPProtocolException do
      begin
        // expected
      end;
    end;

    // create default web component and register it
    Holder := TdjWebComponentHolder.Create(TdjDefaultWebComponent);
    Context.AddWebComponent(Holder, '/');

    // test static
    CheckEquals('staticcontent', Get('/static.html'), '/static.html');

    // Expected Exception := EIdHTTPProtocolException;
    try
      Get('/missing.html');
    except
      on E: EIdHTTPProtocolException do
      begin
        // expected exception
      end;
      on E: Exception do
      begin
        Fail(E.Message);
      end;
    end;

  finally
    Server.Free;
  end;

end;

procedure TdjDefaultWebComponentTests.DefaultWebComponentMissingResourcePath;
var
  Server: TdjServer;
  Holder: TdjWebComponentHolder;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create the 'missing' context (directory 'missing' does not exist)
    Context := TdjWebAppContext.Create('missing');
    Server.Add(Context);

    // create default web component and register it
    Holder := TdjWebComponentHolder.Create(TdjDefaultWebComponent);

    // this triggers a warning only
    // Expected Exception := EWebComponentException;
    try
      Context.AddWebComponent(Holder, '/');
    except
      on E: EWebComponentException do
      begin
        // expected exception
      end;
      on E: Exception do
      begin
        Fail(E.Message);
      end;
    end;

    Server.Start;

  finally

    Server.Free;
  end;
end;

procedure TdjDefaultWebComponentTests.DefaultWebComponentResNotFound;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('test');
    // add default web component
    Context.Add(TdjDefaultWebComponent, '/');

    Server.Add(Context);

    Server.Start;

    CheckEquals('', Get404('/notthere.html'));

  finally
    Server.Free;
  end;
end;

end.

