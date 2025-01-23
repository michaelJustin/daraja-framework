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
  djWebComponent, djWebFilter, djServerContext, djServer, djWebAppContext,
  djTypes, djInterfaces, djPlatform,
  IdGlobal, // GetTickDiff64
  SysUtils;

type
  TAnyDocumentResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

  TResponseTimeFilter = class(TdjWebFilter)
  public
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain); override;
  end;

procedure TAnyDocumentResource.OnGet;
begin
  Response.ContentText := Request.Document;
  Response.ContentType := 'text/plain';
end;

procedure TResponseTimeFilter.DoFilter;
var
  Started: UInt64;
begin
  Started := djPlatform.GetTickCount;

  Chain.DoFilter(Context, Request, Response);

  Response.ContentText := Response.ContentText + Format(' processed in %d ms',
    [GetTickDiff64(Started, djPlatform.GetTickCount)])
end;

procedure Demo;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create;
  try
    Context := TdjWebAppContext.Create('echo');
    Context.Add(TAnyDocumentResource, '/*');
    Context.Add(TResponseTimeFilter, '/*');
    Server.Add(Context);
    Server.Start;
    WriteLn('Server is running, please open http://127.0.0.1:8080/echo/anypage');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
  end;
end;

end.
