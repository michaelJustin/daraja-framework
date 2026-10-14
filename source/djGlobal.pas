{***

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

***}

unit djGlobal;

interface

const
  DWF_SERVER_VERSION = '3.5.0-SNAPSHOT';
  DWF_SERVER_FULL_NAME = 'Daraja HTTP Framework ' + DWF_SERVER_VERSION;
  DWF_SERVER_COPYRIGHT = 'Copyright (c) 2016 Michael Justin';

{*
 * Returns the start-up banner: the word "daraja" as ASCII art, followed by
 * a line with the framework description and DWF_SERVER_VERSION.
 *
 * Lines are separated by sLineBreak; the result ends with a line break.
 *
 * @return the banner text
 *}
function GetServerBanner: string;

{*
 * Escapes text for HTML: '<', '>', '&', '"' and ''' become &lt;, &gt;,
 * &amp;, &quot; and &#39;.
 *
 * The result is safe in HTML element text and in a quoted attribute value,
 * single- or double-quoted. It is not safe in an unquoted attribute, or in
 * a JavaScript, CSS or URL context; those need an encoder made for them.
 *
 * @param AData the text to escape
 * @return the escaped text; AData itself if nothing needed escaping
 *}
function HTMLEncode(const AData: string): string;

implementation /// \cond

const
  // "daraja" in the FIGlet "standard" font. Note the doubled quote on the
  // third line: it is a single apostrophe in the output.
  BANNER_ART: array[0..5] of string = (
    '     _                  _       ',
    '  __| | __ _ _ __ __ _ (_) __ _ ',
    ' / _` |/ _` | ''__/ _` || |/ _` |',
    '| (_| | (_| | | | (_| || | (_| |',
    ' \__,_|\__,_|_|  \__,_|/ |\__,_|',
    '                     |__/       '
  );

function GetServerBanner: string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(BANNER_ART) to High(BANNER_ART) do
  begin
    Result := Result + BANNER_ART[I] + sLineBreak;
  end;
  Result := Result + 'HTTP server framework ' + DWF_SERVER_VERSION + sLineBreak;
end;

// based on http://stackoverflow.com/a/2971923/80901
function HTMLEncode(const AData: string): string;
var
  P, I, Len: Integer;

  procedure Encode(const AStr: string);
  begin
    Move(AStr[1], Result[P], Length(AStr) * SizeOf(Char));
    Inc(P, Length(AStr));
  end;

begin
  // first pass: the exact length of the result
  Len := 0;
  for I := 1 to Length(AData) do
  begin
    case AData[I] of
      '<', '>': Inc(Len, 4); // &lt; &gt;
      '&', '''': Inc(Len, 5); // &amp; &#39;
      '"': Inc(Len, 6); // &quot;
    else
      Inc(Len);
    end;
  end;

  if Len = Length(AData) then
  begin
    // nothing to escape
    Result := AData;
    Exit;
  end;

  SetLength(Result{%H-}, Len);
  P := 1;
  for I := 1 to Length(AData) do
  begin
    case AData[I] of
      '<': Encode('&lt;');
      '>': Encode('&gt;');
      '&': Encode('&amp;');
      '"': Encode('&quot;');
      '''': Encode('&#39;');
    else
      Result[P] := AData[I];
      Inc(P);
    end;
  end;
end;

end. /// \endcond
