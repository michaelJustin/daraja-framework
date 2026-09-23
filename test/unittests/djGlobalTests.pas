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
  end;

implementation

uses
  djGlobal;

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
  // the implementation sizes its buffer as Length(AData) * 6, so a string of
  // nothing but quotes is the tightest possible fit for that arithmetic.
  CheckEquals('&quot;&quot;&quot;', HTMLEncode('"""'));
end;

procedure TdjGlobalTests.TestConsecutiveSpecialCharactersAreAllEncoded;
begin
  CheckEquals('&lt;&gt;&amp;&quot;', HTMLEncode('<>&"'));
end;

end.
