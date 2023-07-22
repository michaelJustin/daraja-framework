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

unit djDefaultWebComponentTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  HTTPTestCase,
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF};

type
  { TdjDefaultWebComponentTests }

  TdjDefaultWebComponentTests = class(THTTPTestCase)
  published
    procedure TestDefaultWebComponent;

    procedure TestDefaultWebComponentInRootContext;

    // procedure DefaultWebComponentMissingResourcePath;

    procedure DefaultWebComponentResNotFound;
  end;

implementation

uses
  djWebAppContext, djWebComponentHolder, djServer, djTypes, djWebComponent,
  djDefaultWebComponent,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdHTTP, IdGlobal,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

type
  TExamplePage = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse);
      override;
  end;

{ TExamplePage }

procedure TExamplePage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'example';
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
    CheckGETResponseEquals('example', '/test/index.html', '/test/index.html');

    // test static
    try
      CheckGETResponseEquals('staticcontent', '/test/static.html',
        '/test/static.html');
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
    CheckGETResponseEquals('staticcontent', '/test/static.html', '/test/static.html');

    CheckGETResponse404('/test/missing.html');
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

    CheckGETResponseEquals('example', '/index.html', '/index.html');

    // test static
    try
      CheckGETResponseEquals('staticcontent', '/static.html', '/static.html');
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
    CheckGETResponseEquals('staticcontent', '/static.html', '/static.html');

    CheckGETResponse404('/test/missing.html');

  finally
    Server.Free;
  end;
end;

(*
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

    // todo this triggers a warning only ok for dynamic environments

    {$IFDEF FPC}
    // ExpectException(EWebComponentException);
    {$ELSE}
    // ExpectedException := EWebComponentException;
    {$ENDIF}
    Context.AddWebComponent(Holder, '/');

    Server.Start;

  finally

    Server.Free;
  end;
end;
*)

procedure TdjDefaultWebComponentTests.DefaultWebComponentResNotFound;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('test');
    // add default web component
    Context.AddWebComponent(TdjDefaultWebComponent, '/');

    Server.Add(Context);

    Server.Start;

    CheckGETResponse404('/notthere.html');

  finally
    Server.Free;
  end;
end;

end.

