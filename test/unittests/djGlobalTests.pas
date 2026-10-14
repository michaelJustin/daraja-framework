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

unit djGlobalTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjGlobalTests }

  {*
   * djGlobal.pas's only non-trivial code is HTMLEncode, a hand-rolled
   * buffer-based escaper (see the Move/SetLength arithmetic in its
   * implementation) -- exactly the kind of code where an off-by-one in the
   * buffer sizing or a missed character would silently corrupt output.
   *}
  TdjGlobalTests = class(TTestCase)
  published
    procedure TestEmptyStringIsUnchanged;
    procedure TestPlainStringIsUnchanged;
    procedure TestEncodesLessThan;
    procedure TestEncodesGreaterThan;
    procedure TestEncodesAmpersand;
    procedure TestEncodesDoubleQuote;
    procedure TestEncodesMixedContent;
    procedure TestEncodesStringOfOnlyTheWidestEscape;
    procedure TestConsecutiveSpecialCharactersAreAllEncoded;
    procedure TestEncodesSingleQuote;
    procedure TestSingleQuotedAttributeValueCannotEndEarly;
    procedure TestTextWithoutSpecialCharactersIsUnchanged;
    procedure TestServerBannerEndsWithVersionLine;
  end;

implementation

uses
  djGlobal, SysUtils;

{ TdjGlobalTests }

procedure TdjGlobalTests.TestEmptyStringIsUnchanged;
begin
  CheckEquals('', HTMLEncode(''));
end;

procedure TdjGlobalTests.TestPlainStringIsUnchanged;
begin
  CheckEquals('Hello, World! 123', HTMLEncode('Hello, World! 123'));
end;

procedure TdjGlobalTests.TestEncodesLessThan;
begin
  CheckEquals('&lt;', HTMLEncode('<'));
end;

procedure TdjGlobalTests.TestEncodesGreaterThan;
begin
  CheckEquals('&gt;', HTMLEncode('>'));
end;

procedure TdjGlobalTests.TestEncodesAmpersand;
begin
  CheckEquals('&amp;', HTMLEncode('&'));
end;

procedure TdjGlobalTests.TestEncodesDoubleQuote;
begin
  CheckEquals('&quot;', HTMLEncode('"'));
end;

procedure TdjGlobalTests.TestEncodesMixedContent;
begin
  CheckEquals('&lt;a href=&quot;x&quot;&gt;A &amp; B&lt;/a&gt;',
    HTMLEncode('<a href="x">A & B</a>'));
end;

procedure TdjGlobalTests.TestEncodesStringOfOnlyTheWidestEscape;
begin
  // '&quot;' (6 characters) is the longest replacement HTMLEncode produces;
  // the implementation sizes its result exactly in a first pass, so a
  // string of nothing but quotes checks that arithmetic at its maximum.
  CheckEquals('&quot;&quot;&quot;', HTMLEncode('"""'));
end;

procedure TdjGlobalTests.TestConsecutiveSpecialCharactersAreAllEncoded;
begin
  CheckEquals('&lt;&gt;&amp;&quot;&#39;', HTMLEncode('<>&"'''));
end;

procedure TdjGlobalTests.TestEncodesSingleQuote;
begin
  CheckEquals('&#39;', HTMLEncode(''''));
end;

procedure TdjGlobalTests.TestSingleQuotedAttributeValueCannotEndEarly;
begin
  // the attack from issue #562: without escaping ', this value closes the
  // single-quoted attribute and adds an event handler
  CheckEquals('&#39; autofocus onfocus=&#39;alert(1)',
    HTMLEncode(''' autofocus onfocus=''alert(1)'));
end;

procedure TdjGlobalTests.TestTextWithoutSpecialCharactersIsUnchanged;
const
  // includes a character outside ASCII, written as a character code so the
  // test does not depend on the source file's encoding
  Text = 'J' + #$00FC + 'rgen, 100% (ok)';
begin
  CheckEquals(Text, HTMLEncode(Text));
end;

procedure TdjGlobalTests.TestServerBannerEndsWithVersionLine;
var
  Banner, Expected: string;
begin
  Banner := GetServerBanner;
  Expected := 'HTTP server framework ' + DWF_SERVER_VERSION + sLineBreak;
  CheckEquals(Expected,
    Copy(Banner, Length(Banner) - Length(Expected) + 1, Length(Expected)));
end;

end.
