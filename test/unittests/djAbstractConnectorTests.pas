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

unit djAbstractConnectorTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjAbstractConnectorTests }

  {*
   * TdjAbstractConnector (Host/Port, DoStart/DoStop contract) only does
   * anything observable once paired with a real connector implementation,
   * so these tests exercise it through TdjHTTPConnector / TdjHTTPServer --
   * the concrete pairing every connector in this framework actually uses.
   *}
  TdjAbstractConnectorTests = class(TTestCase)
  published
    procedure TestHostAndPortRoundTrip;
    procedure TestHTTPServerCreateSetsExpectedDefaults;
    procedure TestStartBindsAndActivatesHTTPServer;
    procedure TestStopDeactivatesHTTPServer;
    procedure TestDestroyReleasesTheSocketOfAStartedConnector;
    procedure TestDoubleStartIsANoOp;
    procedure TestStopWhenNeverStartedDoesNothing;
    procedure TestMutatingPortWhileRunningDoesNotMoveTheLiveBinding;
  end;

implementation

uses
  djHTTPConnector, djHTTPServer, djServerContext, djInterfaces, djTypes,
  SysUtils;

type

  { TNullHandler }

  {*
   * A minimal IHandler that is never actually invoked in these tests --
   * TdjAbstractConnector only requires one to be assigned at construction.
   *}
  TNullHandler = class(TInterfacedObject, IHandler)
  strict private
    FStarted: Boolean;
  public
    procedure Start;
    procedure Stop;
    function IsStarted: Boolean;
    function IsStopped: Boolean;
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
  end;

procedure TNullHandler.Start;
begin
  FStarted := True;
end;

procedure TNullHandler.Stop;
begin
  FStarted := False;
end;

function TNullHandler.IsStarted: Boolean;
begin
  Result := FStarted;
end;

function TNullHandler.IsStopped: Boolean;
begin
  Result := not FStarted;
end;

procedure TNullHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  //
end;

{ TdjAbstractConnectorTests }

procedure TdjAbstractConnectorTests.TestHostAndPortRoundTrip;
var
  Connector: TdjHTTPConnector;
begin
  Connector := TdjHTTPConnector.Create(TNullHandler.Create);
  try
    Connector.Host := '127.0.0.1';
    Connector.Port := 12345;

    CheckEquals('127.0.0.1', Connector.Host);
    CheckEquals(12345, Connector.Port);
  finally
    Connector.Free;
  end;
end;

procedure TdjAbstractConnectorTests.TestHTTPServerCreateSetsExpectedDefaults;
var
  Server: TdjHTTPServer;
begin
  Server := TdjHTTPServer.Create;
  try
    CheckTrue(Server.KeepAlive, 'HTTP 1.1 keep-alive is on by default');
    CheckTrue(Server.SessionState, 'on-demand sessions are enabled by default');
    CheckEquals(DEFAULT_SESSION_TIMEOUT, Server.SessionTimeOut,
      'default session timeout');
    CheckTrue(Server.ContextClass = TdjServerContext,
      'the Daraja server context class is registered');
  finally
    Server.Free;
  end;
end;

procedure TdjAbstractConnectorTests.TestStartBindsAndActivatesHTTPServer;
var
  Connector: TdjHTTPConnector;
begin
  Connector := TdjHTTPConnector.Create(TNullHandler.Create);
  try
    Connector.Host := '127.0.0.1';
    Connector.Port := 0; // ask the OS for a free ephemeral port

    Connector.Start;
    try
      CheckTrue(Connector.IsStarted, 'connector reports started');
      CheckTrue(Connector.HTTPServer.Active, 'the wrapped HTTP server is active');
      CheckEquals(1, Connector.HTTPServer.Bindings.Count, 'one binding created');
      CheckTrue(Connector.HTTPServer.Bindings[0].Port > 0,
        'the OS assigned a real port for the ephemeral binding');
    finally
      Connector.Stop;
    end;
  finally
    Connector.Free;
  end;
end;

procedure TdjAbstractConnectorTests.TestStopDeactivatesHTTPServer;
var
  Connector: TdjHTTPConnector;
begin
  Connector := TdjHTTPConnector.Create(TNullHandler.Create);
  try
    Connector.Host := '127.0.0.1';
    Connector.Port := 0;
    Connector.Start;

    Connector.Stop;

    CheckFalse(Connector.IsStarted, 'connector reports stopped');
    CheckFalse(Connector.HTTPServer.Active, 'the wrapped HTTP server is deactivated');
  finally
    Connector.Free;
  end;
end;

procedure TdjAbstractConnectorTests.TestDestroyReleasesTheSocketOfAStartedConnector;
const
  TestPort = 18453;
var
  Connector, SecondConnector: TdjHTTPConnector;
begin
  Connector := TdjHTTPConnector.Create(TNullHandler.Create);
  Connector.Host := '127.0.0.1';
  Connector.Port := TestPort;
  Connector.Start;

  // Free without an explicit Stop call: TdjHTTPConnector.Destroy's
  // "if IsStarted then Stop" must release the listening socket, or the
  // rebind attempt below will fail with EIdCouldNotBindSocket.
  Connector.Free;

  SecondConnector := TdjHTTPConnector.Create(TNullHandler.Create);
  try
    SecondConnector.Host := '127.0.0.1';
    SecondConnector.Port := TestPort;
    try
      SecondConnector.Start;
    except
      on E: Exception do
        Fail('Destroy must release the port before returning: '
          + E.ClassName + ': ' + E.Message);
    end;

    CheckTrue(SecondConnector.IsStarted);
  finally
    SecondConnector.Stop;
    SecondConnector.Free;
  end;
end;

procedure TdjAbstractConnectorTests.TestDoubleStartIsANoOp;
var
  Connector: TdjHTTPConnector;
  BoundPort: Integer;
begin
  Connector := TdjHTTPConnector.Create(TNullHandler.Create);
  try
    Connector.Host := '127.0.0.1';
    Connector.Port := 0;
    Connector.Start;
    BoundPort := Connector.HTTPServer.Bindings[0].Port;

    // TdjLifeCycle.Start exits immediately once IsStarted is true, so a
    // second Start must not attempt to rebind.
    Connector.Start;

    CheckTrue(Connector.IsStarted, 'still started after a second Start');
    CheckEquals(BoundPort, Connector.HTTPServer.Bindings[0].Port,
      'the original binding is untouched by the redundant Start');
  finally
    Connector.Stop;
    Connector.Free;
  end;
end;

procedure TdjAbstractConnectorTests.TestStopWhenNeverStartedDoesNothing;
var
  Connector: TdjHTTPConnector;
begin
  Connector := TdjHTTPConnector.Create(TNullHandler.Create);
  try
    Connector.Host := '127.0.0.1';
    Connector.Port := 0;

    // TdjLifeCycle.Stop exits immediately when already stopped (the default
    // state), so this must be a no-op rather than raising or touching the
    // never-created binding.
    Connector.Stop;

    CheckFalse(Connector.IsStarted);
    CheckFalse(Connector.HTTPServer.Active,
      'a connector that was never started must not become active');
  finally
    Connector.Free;
  end;
end;

procedure TdjAbstractConnectorTests.TestMutatingPortWhileRunningDoesNotMoveTheLiveBinding;
var
  Connector: TdjHTTPConnector;
  OriginalPort: Integer;
begin
  Connector := TdjHTTPConnector.Create(TNullHandler.Create);
  try
    Connector.Host := '127.0.0.1';
    Connector.Port := 0;
    Connector.Start;

    OriginalPort := Connector.HTTPServer.Bindings[0].Port;

    // TdjAbstractConnector.SetPort has no guard on the running state: it
    // just writes the property. The live socket -- bound during DoStart --
    // is unaffected until the connector is stopped and started again.
    Connector.Port := OriginalPort + 1;

    CheckEquals(OriginalPort + 1, Connector.Port,
      'the property itself accepts the change unconditionally');
    CheckEquals(OriginalPort, Connector.HTTPServer.Bindings[0].Port,
      'the already-bound socket must not move just because the property changed');
    CheckTrue(Connector.HTTPServer.Active,
      'the server keeps running on the original binding');
  finally
    Connector.Stop;
    Connector.Free;
  end;
end;

end.
