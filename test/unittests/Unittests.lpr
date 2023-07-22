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

program Unittests;

uses
{$IFDEF LINUX}
  cthreads,
{$ENDIF}
  LazUTF8,
  djLogAPI, djLogOverSimpleLogger, SimpleLogger,
  Forms,
  Interfaces,
  djGlobal,
  djPathMapTests,
  djWebAppContextTests,
  djWebComponentHolderTests,
  djWebComponentHandlerTests,
  djDefaultWebComponentTests,
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
  {$IFDEF LINUX}
  GIdIconvUseTransliteration := True;
  {$ENDIF}

  ConfigureLogging;

  RegisterUnitTests;

  if UseConsoleTestRunner then
  begin
    // Launch console Test Runner --------------------------------------------
    consoletestrunner.TTestRunner.Create(nil).Run;

    {$IFNDEF LINUX}
    ReadLn;
    {$ENDIF}
  end else begin
    // Launch GUI Test Runner ------------------------------------------------
    Application.Initialize;
    Application.CreateForm(TGuiTestRunner, TestRunner);
    TestRunner.Caption := DWF_SERVER_FULL_NAME + ' FPCUnit tests';
    Application.Run;
  end;

  {$IFNDEF LINUX}
  // SetHeapTraceOutput('heaptrace.log');
  {$ENDIF}
end.
