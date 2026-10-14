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

unit djNCSALogFilterTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjNCSALogFilterTests }

  {*
   * The access-log line is built from values the client controls: the user
   * name from the Authorization header, and the method, URI and version
   * from the request line. These tests pin the escaping that keeps such a
   * value from forging a log line or shifting the line's fields (#563).
   *}
  TdjNCSALogFilterTests = class(TTestCase)
  published
    procedure TestPlainValueIsUnchanged;
    procedure TestEscapesDoubleQuote;
    procedure TestEscapesBackslash;
    procedure TestEscapesLineBreaks;
    procedure TestEscapesOtherControlCharacters;
    procedure TestKeepsNonASCIICharacters;
    procedure TestLogLineFormat;
    procedure TestLogLineWithEmptyUserAndUnknownLength;
    procedure TestForgedUserNameStaysOnOneLine;
    procedure TestQuoteInURIDoesNotEndTheRequestField;
  end;

implementation

uses
  djNCSALogFilter;

{ TdjNCSALogFilterTests }

procedure TdjNCSALogFilterTests.TestPlainValueIsUnchanged;
begin
  CheckEquals('/index.html?a=1&b=2', NCSAEscape('/index.html?a=1&b=2'));
end;

procedure TdjNCSALogFilterTests.TestEscapesDoubleQuote;
begin
  CheckEquals('a\"b', NCSAEscape('a"b'));
end;

procedure TdjNCSALogFilterTests.TestEscapesBackslash;
begin
  // otherwise a value ending in a backslash would escape the closing quote
  // of its field
  CheckEquals('a\\b\\', NCSAEscape('a\b\'));
end;

procedure TdjNCSALogFilterTests.TestEscapesLineBreaks;
begin
  CheckEquals('a\x0D\x0Ab', NCSAEscape('a'#13#10'b'));
end;

procedure TdjNCSALogFilterTests.TestEscapesOtherControlCharacters;
begin
  // NUL, TAB, ESC (the start of a terminal escape sequence) and DEL
  CheckEquals('\x00\x09\x1B[31m\x7F', NCSAEscape(#0#9#27'[31m'#127));
end;

procedure TdjNCSALogFilterTests.TestKeepsNonASCIICharacters;
const
  // a name with a character outside ASCII (an u-umlaut), written as a
  // character code so the test does not depend on the source file's encoding
  Name = 'J' + #$00FC + 'rgen';
begin
  CheckEquals(Name, NCSAEscape(Name));
end;

procedure TdjNCSALogFilterTests.TestLogLineFormat;
begin
  CheckEquals(
    '127.0.0.1 - alice [10/Oct/2026:13:55:36 +0200] "GET /index.html HTTP/1.1" 200 2326',
    FormatNCSALogLine('127.0.0.1', 'alice', 'GET', '/index.html', 'HTTP/1.1',
      '10/Oct/2026:13:55:36 +0200', 200, 2326));
end;

procedure TdjNCSALogFilterTests.TestLogLineWithEmptyUserAndUnknownLength;
begin
  CheckEquals(
    '127.0.0.1 - - [10/Oct/2026:13:55:36 +0200] "GET / HTTP/1.1" 304 -',
    FormatNCSALogLine('127.0.0.1', '', 'GET', '/', 'HTTP/1.1',
      '10/Oct/2026:13:55:36 +0200', 304, -1));
end;

procedure TdjNCSALogFilterTests.TestForgedUserNameStaysOnOneLine;
var
  Line: string;
begin
  // a user name that tries to append a second, fake log entry
  Line := FormatNCSALogLine('10.0.0.1',
    'x'#13#10'127.0.0.1 - admin [10/Oct/2026:13:55:36 +0200] "GET /admin HTTP/1.1" 200 0',
    'GET', '/', 'HTTP/1.1', '10/Oct/2026:13:55:36 +0200', 401, 0);

  CheckEquals(0, Pos(#13, Line), 'CR in log line');
  CheckEquals(0, Pos(#10, Line), 'LF in log line');
  CheckEquals(
    '10.0.0.1 - x\x0D\x0A127.0.0.1 - admin [10/Oct/2026:13:55:36 +0200] '
    + '\"GET /admin HTTP/1.1\" 200 0 [10/Oct/2026:13:55:36 +0200] '
    + '"GET / HTTP/1.1" 401 0',
    Line);
end;

procedure TdjNCSALogFilterTests.TestQuoteInURIDoesNotEndTheRequestField;
begin
  CheckEquals(
    '127.0.0.1 - - [10/Oct/2026:13:55:36 +0200] "GET /a\" 200 9 \"x HTTP/1.1" 404 0',
    FormatNCSALogLine('127.0.0.1', '', 'GET', '/a" 200 9 "x', 'HTTP/1.1',
      '10/Oct/2026:13:55:36 +0200', 404, 0));
end;

end.
