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
    procedure TestMissingFolderDetectedInInit;
  end;

implementation

uses
  djWebAppContext, djServer, djTypes, djWebComponent,
  djDefaultWebComponent,
  IdHTTP, IdGlobal,
  SysUtils;

type
  { TExamplePage }

  TExamplePage = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

{ TExamplePage }

procedure TExamplePage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'example';
end;

{ TdjDefaultWebComponentTests }

procedure TdjDefaultWebComponentTests.TestDefaultWebComponent;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create 'test' context
    Context := TdjWebAppContext.Create('test');
    // register TExamplePage at /index.html
    Context.Add(TExamplePage, '/index.html');

    Server.Add(Context);
    Server.Start;

    // GET /index.html should return the String 'example'
    CheckGETResponseEquals('example', '/test/index.html');
    // GET /test/static.html should return 404
    CheckGETResponse404('/test/static.html');

    // now register TdjDefaultWebComponent to fix it
    Context.Add(TdjDefaultWebComponent, '/');

    // GET /test/static.html should now return the String 'staticcontent'
    CheckGETResponseEquals('staticcontent', '/test/static.html');
    // GET /index.html should still return the String 'example'
    CheckGETResponseEquals('example', '/test/index.html');
  finally
    Server.Free;
  end;
end;

procedure TdjDefaultWebComponentTests.TestDefaultWebComponentInRootContext;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    // create ROOT context
    Context := TdjWebAppContext.Create('');
    // register TExamplePage at /index.html
    Context.Add(TExamplePage, '/index.html');

    Server.Add(Context);
    Server.Start;

    // GET /index.html should return the String 'example'
    CheckGETResponseEquals('example', '/index.html', '/index.html');
    // GET /static.html should return 404
    CheckGETResponse404('/static.html');

    // register TdjDefaultWebComponent to fix it
    Context.Add(TdjDefaultWebComponent, '/');

    // GET /test/static.html should now return the String 'staticcontent'
    CheckGETResponseEquals('staticcontent', '/static.html', '/static.html');
    // GET /index.html should still return the String 'example'
    CheckGETResponseEquals('example', '/index.html', '/index.html');
  finally
    Server.Free;
  end;
end;

procedure TdjDefaultWebComponentTests.TestMissingFolderDetectedInInit;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('folder_missing');
    Context.Add(TdjDefaultWebComponent, '/');
    Server.Add(Context);
    // ExpectedException := EWebComponentException; there is none
    Server.Start;
  finally
    Server.Free;
  end;
end;


end.

