(*

    Daraja HTTP Framework
    Copyright (c) 2016 Michael Justin

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

unit MainUnit;

interface

procedure Demo;

implementation

uses
  djServer, djWebAppContext, djWebComponent, djAbstractHandler,
  djServerContext, djTypes, SysUtils;

type
  TFailingResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TFailingResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  raise Exception.Create('Something went wrong building this page.');
end;

type
  // A minimal custom IHandler, built directly on TdjAbstractHandler -- the
  // same base TdjContextHandler/TdjServerBase use internally for anything
  // that is an IHandler but not a Web Component or Web Filter. Registered
  // as Context.ErrorHandler, it replaces the framework's generic 500 page
  // for exceptions raised anywhere in that context (a component's Service
  // method, or a filter).
  TCustomErrorPage = class(TdjAbstractHandler)
  protected
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TCustomErrorPage.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  // Context (a TdjServerContext) carries the failure detail: the status
  // code the framework was about to send, and the exception's class name
  // and message. Response.ResponseNo already defaults to that status code,
  // but this handler is free to change it.
  Response.ContentType := 'text/html';
  Response.ContentText := '<!DOCTYPE html>' + #10
    + '<html>' + #10
    + '<head><title>Oops</title></head>' + #10
    + '<body>' + #10
    + '  <h1>Something went wrong</h1>' + #10
    + '  <p>' + Context.LastErrorExceptionClass + ': '
    + Context.LastErrorExceptionMessage + '</p>' + #10
    + '  <p><small>Custom ErrorHandler for ' + Target + '</small></p>' + #10
    + '</body>' + #10
    + '</html>';
end;

procedure Demo;
var
  Server: TdjServer;
  PlainContext, CustomContext: TdjWebAppContext;
begin
  Server := TdjServer.Create(8080);
  try
    // No ErrorHandler set: the framework's default, generic 500 page.
    PlainContext := TdjWebAppContext.Create('plain');
    PlainContext.Add(TFailingResource, '/fail');
    Server.Add(PlainContext);

    // ErrorHandler set: TCustomErrorPage takes over instead.
    CustomContext := TdjWebAppContext.Create('custom');
    CustomContext.Add(TFailingResource, '/fail');
    CustomContext.ErrorHandler := TCustomErrorPage.Create;
    Server.Add(CustomContext);

    Server.Start;
    WriteLn('Server is running.');
    WriteLn('http://127.0.0.1:8080/plain/fail  -- default generic 500 page');
    WriteLn('http://127.0.0.1:8080/custom/fail -- custom ErrorHandler page');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
  end;
end;

end.
