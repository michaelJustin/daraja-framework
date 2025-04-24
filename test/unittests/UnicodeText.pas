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

unit UnicodeText;

interface

const
  am = '&#x12A0;&#x121B;&#x122D;&#x129B;';
  ar = '&#x0627;&#x0644;&#x0639;&#x0631;&#x0628;&#x064A;&#x0629;';
  gu = '&#x0A97;&#x0AC1;&#x0A9C;&#x0AB0;&#x0ABE;&#x0AA4;&#x0AC0;';
  he = '&#x05E2;&#x05D1;&#x05E8;&#x05D9;&#x05EA;';
  hi = '&#x0939;&#x093F;&#x0928;&#x094D;&#x0926;&#x0940;';
  kn = '&#x0C95;&#x0CA8;&#x0CCD;&#x0CA8;&#x0CA1;';
  mr = '&#x092E;&#x0930;&#x093E;&#x0920;&#x0940;';
  ja = '&#x65E5;&#x672C;&#x8A9E;';
  ta = '&#x0BA4;&#x0BAE;&#x0BBF;&#x0BB4;&#x0BCD;';
  te = '&#x0C24;&#x0C46;&#x0C32;&#x0C41;&#x0C17;&#x0C41;';
  ur = '&#x0627;&#x0631;&#x062F;&#x0648;';
  zh = '&#x4E2D;&#x6587;';

function Decode(S: string): WideString;

implementation

uses
  SysUtils;

function Decode(S: string): WideString;
var
  I: Integer;
  Sub: string;
begin
  for I := 0 to Trunc(Length(S) / 8) - 1 do
  begin
    Sub := Copy(S, 8 * i + 4, 4);
    Result := Result + WideChar(StrToInt('$' + Sub));
  end;
end;

end.
