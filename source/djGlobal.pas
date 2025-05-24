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

unit djGlobal;

interface

{$IFNDEF DOXYGEN_SKIP}

// {$i IdCompilerDefines.inc}

const
  DWF_SERVER_VERSION = '3.0.5-SNAPSHOT';
  DWF_SERVER_FULL_NAME = 'Daraja HTTP Framework ' + DWF_SERVER_VERSION;
  DWF_SERVER_COPYRIGHT = 'Copyright (c) Michael Justin';

function HTMLEncode(const AData: string): string;

implementation

// http://stackoverflow.com/a/2971923/80901
function HTMLEncode(const AData: string): string;
var
  Pos, I: Integer;

  procedure Encode(const AStr: string);
  begin
    Move(AStr[1], Result[Pos], Length(AStr) * SizeOf(Char));
    Inc(Pos, Length(AStr));
  end;

begin
  SetLength(Result{%H-}, Length(AData) * 6);
  Pos := 1;
  for I := 1 to length(AData) do
  begin
    case AData[I] of
      '<': Encode('&lt;');
      '>': Encode('&gt;');
      '&': Encode('&amp;');
      '"': Encode('&quot;');
    else
      Result[Pos] := AData[I];
      Inc(Pos);
    end;
  end;
  SetLength(Result, Pos - 1);
end;

{$ENDIF DOXYGEN_SKIP}

end.
