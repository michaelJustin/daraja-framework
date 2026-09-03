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

unit TestHelper;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

const
  LOG_LEVEL = 'trace';

function UseConsoleTestRunner: Boolean;

procedure ConfigureLogging;

procedure RegisterUnitTests;

implementation

uses
  ConfigAPITests,
  {$IFDEF DARAJA_TEST_HTTPS}
  HttpsTests,
  {$ENDIF DARAJA_TEST_HTTPS}
  djDefaultWebComponentTests,
  djPathMapTests,
  djWebAppContextTests,
  djWebComponentHandlerTests,
  djWebComponentHolderTests,
  djWebFilterTests,
  djGlobal,
  TestSessions,
  djLogOverSimpleLogger,
  SimpleLogger,
  {$IFDEF FPC}testregistry,fpcunit{$ELSE}TestFramework{$ENDIF};

function UseConsoleTestRunner: Boolean;
begin
  Result := ParamCount > 0;
end;

procedure ConfigureLogging;
begin
  {$IFDEF DARAJA_LOGGING}
  SimpleLogger.Configure('defaultLogLevel', LOG_LEVEL);
  SimpleLogger.Configure('showDateTime', 'true');
  {$ENDIF DARAJA_LOGGING}
end;

{$IFDEF FPC}
procedure RegisterUnitTests;
var
  Tests: TTestSuite;
begin
  Tests := TTestSuite.Create(DWF_SERVER_FULL_NAME);
  Tests.AddTest(TTestSuite.Create(TdjPathMapTests));
  Tests.AddTest(TTestSuite.Create(TdjWebComponentHolderTests));
  Tests.AddTest(TTestSuite.Create(TdjWebComponentHandlerTests));
  Tests.AddTest(TTestSuite.Create(TdjWebAppContextTests));
  Tests.AddTest(TTestSuite.Create(TdjWebFilterTests));
  // optional feature
  Tests.AddTest(TTestSuite.Create(TdjDefaultWebComponentTests));

  // Integration suites: these start a real HTTP server on the loopback
  // interface. They run in every runner (console and GUI). Set the
  // DARAJA_SKIP_SERVER_TESTS define to exclude them on environments where
  // binding a listening socket is not possible.
  {$IFNDEF DARAJA_SKIP_SERVER_TESTS}
  Tests.AddTest(TTestSuite.Create(TSessionTests));
  Tests.AddTest(TTestSuite.Create(TAPIConfigTests));
  {$IFDEF DARAJA_TEST_HTTPS}
  Tests.AddTest(TTestSuite.Create(THttpsTests));
  {$ENDIF DARAJA_TEST_HTTPS}
  {$ENDIF DARAJA_SKIP_SERVER_TESTS}

  RegisterTest('', Tests);
end;
{$ELSE}
procedure RegisterUnitTests;
begin
  RegisterTests('', [TdjPathMapTests.Suite]);
  RegisterTests('', [TdjWebComponentHolderTests.Suite]);
  RegisterTests('', [TdjWebComponentHandlerTests.Suite]);
  RegisterTests('', [TdjWebAppContextTests.Suite]);
  RegisterTests('', [TdjWebFilterTests.Suite]);
  // optional feature
  RegisterTests('', [TdjDefaultWebComponentTests.Suite]);

  // Integration suites: these start a real HTTP server on the loopback
  // interface. They run in every runner (text and GUI). Set the
  // DARAJA_SKIP_SERVER_TESTS define to exclude them on environments where
  // binding a listening socket is not possible.
  {$IFNDEF DARAJA_SKIP_SERVER_TESTS}
  RegisterTests('', [TSessionTests.Suite]);
  RegisterTests('', [TAPIConfigTests.Suite]);
  {$IFDEF DARAJA_TEST_HTTPS}
  RegisterTests('', [THttpsTests.Suite]);
  {$ENDIF DARAJA_TEST_HTTPS}
  {$ENDIF DARAJA_SKIP_SERVER_TESTS}
end;
{$ENDIF}

end.
