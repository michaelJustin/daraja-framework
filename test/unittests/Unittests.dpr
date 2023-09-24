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

{$APPTYPE CONSOLE}

uses
  djAbstractConfig in '..\..\source\djAbstractConfig.pas',
  djAbstractConnector in '..\..\source\djAbstractConnector.pas',
  djAbstractHandler in '..\..\source\djAbstractHandler.pas',
  djAbstractHandlerContainer in '..\..\source\djAbstractHandlerContainer.pas',
  djContextConfig in '..\..\source\djContextConfig.pas',
  djContextHandler in '..\..\source\djContextHandler.pas',
  djContextHandlerCollection in '..\..\source\djContextHandlerCollection.pas',
  djContextMap in '..\..\source\djContextMap.pas',
  djDefaultHandler in '..\..\source\optional\djDefaultHandler.pas',
  djDefaultWebComponent in '..\..\source\optional\djDefaultWebComponent.pas',
  djGenericHolder in '..\..\source\djGenericHolder.pas',
  djGenericWebComponent in '..\..\source\djGenericWebComponent.pas',
  djGlobal in '..\..\source\djGlobal.pas',
  djHandlerCollection in '..\..\source\djHandlerCollection.pas',
  djHandlerList in '..\..\source\djHandlerList.pas',
  djHandlerWrapper in '..\..\source\djHandlerWrapper.pas',
  djHTTPConnector in '..\..\source\djHTTPConnector.pas',
  djHTTPConstants in '..\..\source\djHTTPConstants.pas',
  djHTTPServer in '..\..\source\djHTTPServer.pas',
  djInitParameters in '..\..\source\djInitParameters.pas',
  djInterfaces in '..\..\source\djInterfaces.pas',
  djLifeCycle in '..\..\source\djLifeCycle.pas',
  djPathMap in '..\..\source\djPathMap.pas',
  djPlatform in '..\..\source\djPlatform.pas',
  djServer in '..\..\source\djServer.pas',
  djServerBase in '..\..\source\djServerBase.pas',
  djServerContext in '..\..\source\djServerContext.pas',
  djServerInterfaces in '..\..\source\djServerInterfaces.pas',
  djStacktrace in '..\..\source\optional\djStacktrace.pas',
  djTypes in '..\..\source\djTypes.pas',
  djWebAppContext in '..\..\source\djWebAppContext.pas',
  djWebComponent in '..\..\source\djWebComponent.pas',
  djWebComponentConfig in '..\..\source\djWebComponentConfig.pas',
  djWebComponentContextHandler in '..\..\source\djWebComponentContextHandler.pas',
  djWebComponentHandler in '..\..\source\djWebComponentHandler.pas',
  djWebComponentHolder in '..\..\source\djWebComponentHolder.pas',
  djWebComponentHolders in '..\..\source\djWebComponentHolders.pas',
  djWebComponentMapping in '..\..\source\djWebComponentMapping.pas',
  ConfigAPITests in 'ConfigAPITests.pas',
  HttpsTests in 'HttpsTests.pas',
  djDefaultWebComponentTests in 'djDefaultWebComponentTests.pas',
  djPathMapTests in 'djPathMapTests.pas',
  djWebAppContextTests in 'djWebAppContextTests.pas',
  djWebComponentHandlerTests in 'djWebComponentHandlerTests.pas',
  djWebComponentHolderTests in 'djWebComponentHolderTests.pas',
  HTTPTestCase in 'HTTPTestCase.pas',
  TestSessions in 'TestSessions.pas',
  UnicodeText in 'UnicodeText.pas',
  TestHelper in 'TestHelper.pas',
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  SysUtils;

begin
  ConfigureLogging;

  RegisterUnitTests;

  if FindCmdLineSwitch('text-mode', ['-', '/'], true) then
    TextTestRunner.RunRegisteredTests(rxbContinue)
  else
  begin
    ReportMemoryLeaksOnShutDown := True;
    TGUITestRunner.RunRegisteredTests;
  end;
end.

