{***

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

***}

unit djServer;

interface

uses
  djInterfaces, djTypes, djHTTPConnector, djServerBase, djServerInterfaces,
  djWebComponentContextHandler, djContextHandlerCollection,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  {$IFDEF FPC}
  LazUTF8,
  {$ENDIF}
  Classes, Generics.Collections;

const
  DEFAULT_BINDING_PORT = 8080;
  DEFAULT_BINDING_IP = '127.0.0.1'; // instead of '0.0.0.0';

  {*
   * @mainpage Welcome to Daraja HTTP Framework!
   *
   * @section intro Introduction
   *
   * Daraja is a flexible HTTP server framework for Object Pascal, based on the stand-alone HTTP server in the free open source library Internet Direct (Indy).
   * Daraja provides the core foundation for serving HTTP resources of all content-types such as HTML pages, images, scripts, web service responses etc. by mapping resource paths to your own code. Your code then can create the response content, or let the framework serve a static file.
   *
   * It allows to compose web applications with these building blocks:
   *
   * @li a \link TdjWebComponent Web Component base class \endlink which provides HTTP method handling (OnGet, OnPost, OnPut etc.)
   * @li a \link TdjWebFilter Web Filter base class \endlink for request interception and modification (pre- and postprocessing)
   * @li a HTTP server run time environment, based on <a target="_blank" href="http://www.indyproject.org/">Internet Direct (Indy)</a>
   *
   * Copyright (c) Michael Justin
   * https://www.habarisoft.com/
   * Mail: mailto:info@habarisoft.com
   *
   * @section trademarks Trademarks
   *
   * Habari is a registered trademark of Michael Justin and is protected by the
   * laws of Germany and other countries.
   * Embarcadero, the Embarcadero Technologies logos and all other Embarcadero
   * Technologies product or service names are trademarks, servicemarks, and/or
   * registered trademarks of Embarcadero Technologies, Inc.
   * and are protected by the laws of the United States and other countries.
   * Other brands and their products are trademarks of their respective holders.
   *}

type
  { TdjServer }

  {*
   * Basic server class for the Web Component framework.
   *}
  TdjServer = class(TdjServerBase)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    FDefaultHost: string;
    FDefaultPort: Integer;
    ConnectorMap: TObjectDictionary<string, IConnector>;
    ConnectorList: TdjStrings;
    ContextHandlers: IHandlerContainer;
    ContextNames: TStrings;
    procedure StartConnectors;
    procedure StopConnectors;
    procedure StopContextHandlers;
  protected
    // TdjLifeCycle overrides
    /// \private
    procedure DoStart; override;
    /// \private
    procedure DoStop; override;
  public
    {*
     * Create a TdjServer using the default host and port.
     *}
    constructor Create; overload; override;

    {*
     * Create a TdjServer, using the specified port and the default host.
     *
     * @param APort the port to be used.
     *}
    constructor Create(const APort: Integer); reintroduce; overload;

    {*
     * Create a TdjServer, using the specfied host and port.
     *
     * @param AHost the host to be used.
     * @param APort the port to be used.
     *}
    constructor Create(const AHost: string;
      const APort: Integer = DEFAULT_BINDING_PORT); reintroduce; overload;

    {*
     * Destructor.
     *}
    destructor Destroy; override;

    {*
     * Add a preconfigured connector.
     *
     * Ownership: the connector is held by an interface reference and is
     * released when the server is destroyed. Do not free it yourself.
     *
     * @param Connector the connector
     *}
    procedure AddConnector(const Connector: IConnector); overload;

    {*
     * Create and add a connector for a host and port.
     *
     * @param Host the connector host name
     * @param Port the connector port number
     *}
    procedure AddConnector(const Host: string; Port: Integer = DEFAULT_BINDING_PORT); overload;

    {*
     * Add a new context.
     *
     * Ownership: the server takes ownership of the context. It is destroyed
     * together with the server; do not free it yourself. If a context with the
     * same path is already registered, the passed context is freed and an
     * EWebComponentException is raised.
     *
     * @param Context the context handler.
     * @throws EWebComponentException if the context path is already registered.
     *}
    procedure Add(Context: TdjWebComponentContextHandler);

    {*
     * The number of connectors.
     *
     * @returns number of connectors
     *}
    function ConnectorCount: Integer;

  end;

implementation /// \cond

uses
  Generics.Defaults, SysUtils;

{ TdjServer }

constructor TdjServer.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjServer);
  {$ENDIF DARAJA_LOGGING}

  FDefaultHost := DEFAULT_BINDING_IP;
  FDefaultPort := DEFAULT_BINDING_PORT;

  ConnectorMap := TObjectDictionary<string, IConnector>.Create;

  ConnectorList := TdjStrings.Create;

  ContextHandlers := TdjContextHandlerCollection.Create;
  ContextNames := TStringList.Create;

  AddHandler(ContextHandlers);
end;

constructor TdjServer.Create(const AHost: string;
  const APort: Integer = DEFAULT_BINDING_PORT);
begin
  Create;

  FDefaultHost := AHost;
  FDefaultPort := APort;
end;

constructor TdjServer.Create(const APort: Integer);
begin
  Create;

  FDefaultPort := APort;
end;

destructor TdjServer.Destroy;
begin
  if IsStarted then
  begin
    Stop;
  end;

  ConnectorMap.Free;
  ConnectorList.Free;

  ContextNames.Free;

  inherited;
end;

function TdjServer.ConnectorCount: Integer;
begin
  Result := ConnectorList.Count;
end;

procedure TdjServer.AddConnector(const Connector: IConnector);
var
  ConnectorName: string;
begin
  ConnectorName := '[' + Connector.Host + ']:' + IntToStr(Connector.Port);

  ConnectorMap.Add(ConnectorName, Connector);
  ConnectorList.Add(ConnectorName);

  if IsStarted then
  begin
    Connector.Start;
  end;
end;

procedure TdjServer.AddConnector(const Host: string; Port: Integer =
  DEFAULT_BINDING_PORT);
var
  Connector: IConnector;
begin
  Connector := TdjHTTPConnector.Create(Self.Handler);

  Connector.Host := Host;
  Connector.Port := Port;

  AddConnector(Connector);
end;

procedure TdjServer.Add(Context: TdjWebComponentContextHandler);
var
  ContextPath: string;
begin
  {$IFDEF DARAJA_LOGGING}
  Logger.Trace('Add context %s', [Context.ContextPath]);
  {$ENDIF DARAJA_LOGGING}

  if ContextNames.IndexOf(Context.ContextPath) < 0 then
  begin
    ContextNames.Add(Context.ContextPath);
  end else begin
    ContextPath := Context.ContextPath; // needed for exception message
    Context.Free; // avoid leak
    raise EWebComponentException.CreateFmt('Context path "%s" is already registered.',
      [ContextPath]);
  end;

  ContextHandlers.AddHandler(Context);
end;

procedure TdjServer.StartConnectors;
var
  ConnectorName: string;
  Connector: IConnector;
begin
  for ConnectorName in ConnectorList do
  begin
    Connector := ConnectorMap[ConnectorName];
    Connector.Start;

    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Connector %s started', [ConnectorName]);
    {$ENDIF DARAJA_LOGGING}
  end;
end;

procedure TdjServer.StopConnectors;
var
  ConnectorName: string;
  Connector: IConnector;
  Keys: TdjStrings;
begin
  Keys := TdjStrings.Create(ConnectorList);

  try
    Keys.Reverse;

    for ConnectorName in Keys do
    begin
      Connector := ConnectorMap[ConnectorName];
      Connector.Stop;

      {$IFDEF DARAJA_LOGGING}
      Logger.Trace('Connector %s stopped', [ConnectorName]);
      {$ENDIF DARAJA_LOGGING}
    end;

  finally
    Keys.Free
  end;
end;

procedure TdjServer.StopContextHandlers;
begin
  ContextHandlers.Stop;
end;

procedure TdjServer.DoStart;
begin
  CheckNotStarted;

  // add default connector
  if ConnectorList.Count = 0 then
  begin
    AddConnector(FDefaultHost, FDefaultPort);
  end;

  try
    // start HTTP connectors
    StartConnectors;
  except
    on E: Exception do
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Error('Could not start connectors.');
      {$ENDIF DARAJA_LOGGING}
      raise;
    end;
  end;

  inherited;
end;

procedure TdjServer.DoStop;
begin
  CheckNotStopped;
  StopContextHandlers;
  StopConnectors;

  inherited;
end;

end. /// \endcond

