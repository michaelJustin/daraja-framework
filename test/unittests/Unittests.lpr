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

program Unittests;

uses
{$IFDEF LINUX}
  cthreads,
{$ENDIF}
  SysUtils,
  LazUTF8,
  IdGlobal,
  djLogAPI, djLogOverSimpleLogger, SimpleLogger,
  Forms,
  Interfaces,
  djGlobal, djInterfaces, djDefaultWebComponent,
  djLifeCycleTests,
  djPathMapTests,
  djWebAppContextTests,
  djWebComponentHolderTests,
  djWebComponentHandlerTests,
  djDefaultWebComponentTests,
  djWebFilterTests,
  ConfigAPITests,
  HttpsTests,
  TestHelper,
  TestSessions,
  testregistry,
  fpcunit,
  GuiTestRunner,
  consoletestrunner;

{$R *.res}

begin
  // When built with heap trace (Console build mode, -gh), write the leak
  // report to heaptrace.log next to the executable. Must run before any
  // allocation. See UNIT-TESTS.md.
  {$IF DECLARED(SetHeapTraceOutput)}
  SetHeapTraceOutput('heaptrace.log');
  {$ENDIF}

  // On Unix, Indy transcodes through iconv; allow transliteration so headers
  // with non-representable characters degrade instead of raising. The global
  // only exists when Indy is built with iconv support.
  {$IF DEFINED(UNIX) AND DECLARED(GIdIconvUseTransliteration)}
  GIdIconvUseTransliteration := True;
  {$ENDIF}

  // The upload tests read and write .\resources\ relative to the working
  // directory. Pin it to the executable location (which is test\unittests\,
  // next to resources\ and webapps\) so the runner can be started from anywhere.
  SetCurrentDir(ExtractFileDir(ParamStr(0)));

  ConfigureLogging;

  RegisterUnitTests;

  if UseConsoleTestRunner then
  begin
    // Launch console Test Runner --------------------------------------------
    // Exit code: bit 0 set on failures, bit 1 set on errors (see FPCUnit
    // TProgressWriter.GetExitCode), so scripts and CI can detect a red run.
    consoletestrunner.TTestRunner.Create(nil).Run;
  end else begin
    // Launch GUI Test Runner ------------------------------------------------
    Application.Initialize;
    Application.CreateForm(TGuiTestRunner, TestRunner);
    TestRunner.Caption := DWF_SERVER_FULL_NAME + ' FPCUnit tests';
    Application.Run;
  end;
end.
