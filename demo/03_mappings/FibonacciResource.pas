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

unit FibonacciResource;

interface

uses djWebComponent, djTypes;

type
  TFibonacciResource = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses StrUtils, SysUtils;

function fib(n: Integer): Integer;
begin
  if n=0 then begin Result := 0; Exit; end;
  if n=1 then begin Result := 1; Exit; end;
  Result := fib(n-1) + fib(n-2);
end;

{$IFDEF FPC}
function EndsText(const ASubText: string; const AText: string): Boolean;
begin
  Result := AText.EndsText(ASubText, AText);
end;
{$ENDIF}

procedure TFibonacciResource.OnGet(Request: TdjRequest; Response: TdjResponse);
const
  INVALID_ARGUMENT_VALUE = -1;
var
  InputParam: Integer;
begin
  InputParam := StrToIntDef(Request.Params.Values['n'], INVALID_ARGUMENT_VALUE);
  if InputParam <= INVALID_ARGUMENT_VALUE then begin
    Response.ResponseNo := 500;
    Response.ContentText := 'Internal server error: missing or invalid value';
    Response.ContentType := 'text/plain';
  end else if EndsText('.txt', Request.Document) then begin
    Response.ContentText := IntToStr(fib(InputParam));
    Response.ContentType := 'text/plain';
  end else if EndsText('.html', Request.Document) then begin
    Response.ContentText := Format('<html><body>Result: <b>%d</b></body></html>', [fib(InputParam)]);
    Response.ContentType := 'text/html';
  end;
end;

end.

