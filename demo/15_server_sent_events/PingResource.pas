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

unit PingResource;

// note: this is unsupported example code

interface

uses djWebComponent, djServerContext, djTypes;

type

  { TPingResource }

  TPingResource = class(TdjWebComponent)
  private
    function BuildContentText(AContext: TdjServerContext): string;
  public
    procedure Service(Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  SysUtils, DateUtils;

function TPingResource.BuildContentText(AContext: TdjServerContext): string;
begin
  Result := 'event: ping' + #13
    + Format('data: {"time": "%s", "peer": "%s:%d"}',
    [DateToISO8601(Now, False),
    AContext.Binding.PeerIP,
    AContext.Binding.PeerPort]) + #13#13;
end;

procedure TPingResource.Service(Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse);
var
   Data: string;
begin
  Response.ContentType := 'text/event-stream';
  Response.CacheControl := 'no-store';
  Response.ContentLength := -2;
  Response.WriteHeader;
  repeat
    Data := BuildContentText(Context);
    Context.Connection.IOHandler.Write(Data);
    Sleep(Random(100));
  until False;

  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

end.

