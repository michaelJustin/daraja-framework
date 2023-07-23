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

// note: this is unsupported example code

unit BootstrapDemoMain;

interface

procedure Demo;

implementation

uses
  {$IFDEF DARAJA_LOGGING}
  djLogOverSimpleLogger,
  SimpleLogger,
  {$ENDIF DARAJA_LOGGING}
  AjaxStatsCmp,
  djDefaultHandler,
  djDefaultWebComponent,
  djHandlerList,
  djInterfaces,
  djNCSALogFilter,
  djWebFilterConfig,
  djServer,
  djStatisticsHandler,
  djWebAppContext,
  FileUploadCmp,
  FormCmp,
  IdGlobal,
  IndexCmp,
  ShellAPI,
  ShutdownHelper,
  SourceCmp,
  ThankYouCmp;

procedure ConfigureLogging;
begin
  {$IFDEF DARAJA_LOGGING}
  SimpleLogger.Configure('defaultLogLevel', 'info');
  SimpleLogger.Configure('showDateTime', 'true');
  {$ENDIF DARAJA_LOGGING}
end;

procedure Demo;
var
  Server: TdjServer;
  HandlerList: IHandlerContainer;
  DefaultHandler: IHandler;
  Context: TdjWebAppContext;
  WebFilterConfig: IWebFilterConfig;
begin
  ConfigureLogging;

  WebFilterConfig := TdjWebFilterConfig.Create;

  Server := TdjServer.Create(8080);
  try
    // add statistics handler
    StatsWrapper := TdjStatisticsHandler.Create;
    Server.AddHandler(StatsWrapper);

    // add a handlerlist with a TdjDefaultHandler
    DefaultHandler := TdjDefaultHandler.Create;
    HandlerList := TdjHandlerList.Create;
    HandlerList.AddHandler(DefaultHandler);
    Server.AddHandler(HandlerList);

    // get a context handler for the root context, with session support
    Context := TdjWebAppContext.Create('demo', True);

    // -----------------------------------------------------------------------
    // register the Web Components
    Context.AddWebComponent(TdjDefaultWebComponent, '/'); // for static contant
    Context.AddWebComponent(TIndexPage, '/index.html'); // home page
    Context.AddWebComponent(TFormPage, '/form.html');  // form demo
    Context.AddWebComponent(TThankYouPage, '/thankyou.html'); // form demo
    Context.AddWebComponent(TUploadPage, '/upload.html'); // file upload demo
    Context.AddWebComponent(TAjaxStatsJson, '/ajaxstats.json'); // live statistics demo
    Context.AddWebComponent(TSourcePage, '/source.html'); // view source
    // -----------------------------------------------------------------------
    // register the Web Filters
    Context.AddFilterWithMapping(TdjNCSALogFilter, '/*', WebFilterConfig);

    // add the "demo" context
    Server.Add(Context);

    // allow Ctrl+C
    SetShutdownHook(Server);

    // start the server
    Server.Start;

    // launch browser
    ShellExecute(0, 'open', PChar('http://127.0.0.1:8080/demo/index.html'), '',
      '', 0);

    // terminate
    WriteLn('Hit any key to terminate.');
    ReadLn;

  finally
    Server.Free;
  end;
end;

end.
