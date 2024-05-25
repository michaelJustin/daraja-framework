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

unit HomeResource;

// note: this is unsupported example code

interface

uses djWebComponent, djTypes;

type
  THomeResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  SysUtils;

procedure THomeResource.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
    Response.ContentText := '<!DOCTYPE html>'
                            + #10 + '<html>'
                            + #10 + '    <head>'
                            + #10 + '        <title>SSE example</title>'
                            + #10 + '    </head>'
                            + #10 + '    <body>'
                            + #10 + '        <script>'
                            + #10 + '        const evtSource = new EventSource("ping");'
                            + #10 + '         '
                            + #10 + '        evtSource.addEventListener("ping", (event) => {'
                            + #10 + '  const newElement = document.createElement("li");'
                            + #10 + '  const eventList = document.getElementById("list");'
                            + #10 + '  const time = JSON.parse(event.data).time;'
                            + #10 + '  const peer = JSON.parse(event.data).peer;'
                            + #10 + '  newElement.textContent = `ping at ${time} from ${peer}`;'
                            + #10 + '  eventList.appendChild(newElement);'
                            + #10 + '});'
                            + #10 + ' '
                            + #10 + '        </script>'
                            + #10 + '        <ul id="list">'
                            + #10 + ' '
                            + #10 + '        </ul>'
                            + #10 + '    </body>'
                            + #10 + '</html>';

  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

end.

