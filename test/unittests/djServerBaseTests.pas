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

unit djServerBaseTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjServerBaseTests }

  TdjServerBaseTests = class(TTestCase)
  published
    procedure TestHandleForwardsWhenWrappedHandlerIsStarted;
    procedure TestHandleSkipsWhenNoHandlerAssigned;
    procedure TestHandleSkipsWhenWrappedHandlerNotStarted;
    procedure TestStartStartsTheWrappedHandler;
    procedure TestStopStopsTheWrappedHandler;
    procedure TestStopWhenNeverStartedDoesNotStopWrappedHandler;
  end;

implementation

uses
  djServerBase, djInterfaces, djServerContext, djTypes;

type
  { TFakeHandler: records calls; mirrors the one in djHandlerCollectionTests }
  TFakeHandler = class(TInterfacedObject, IHandler)
  public
    StartCalled, StopCalled, HandleCalled: Boolean;
    FStarted: Boolean;
    procedure Start;
    procedure Stop;
    function IsStarted: Boolean;
    function IsStopped: Boolean;
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
  end;

procedure TFakeHandler.Start;
begin
  StartCalled := True;
  FStarted := True;
end;

procedure TFakeHandler.Stop;
begin
  StopCalled := True;
  FStarted := False;
end;

function TFakeHandler.IsStarted: Boolean;
begin
  Result := FStarted;
end;

function TFakeHandler.IsStopped: Boolean;
begin
  Result := not FStarted;
end;

procedure TFakeHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  HandleCalled := True;
end;

{ TdjServerBaseTests }

// TdjServerBase descends from TInterfacedObject (via TdjLifeCycle), and its
// Handle/AddHandler are only reachable through the IHandler/IHandlerContainer
// interfaces (Handle is redeclared protected on the class itself). Held as
// an interface variable for its whole lifetime here, for the same reason
// explained in djHandlerCollectionTests.pas: a transient interface cast on a
// manually-managed instance would bounce the refcount 0->1->0 and
// self-destruct the object mid-statement.

procedure TdjServerBaseTests.TestHandleForwardsWhenWrappedHandlerIsStarted;
var
  ServerBase: IHandlerContainer;
  Fake: TFakeHandler;
begin
  ServerBase := TdjServerBase.Create;
  Fake := TFakeHandler.Create;
  ServerBase.AddHandler(Fake);
  ServerBase.Start;

  ServerBase.Handle('/x', nil, nil, nil);

  CheckTrue(Fake.HandleCalled, 'the wrapped handler must receive the request');
end;

procedure TdjServerBaseTests.TestHandleSkipsWhenNoHandlerAssigned;
var
  ServerBase: IHandlerContainer;
begin
  ServerBase := TdjServerBase.Create;
  ServerBase.Start;

  // must not raise just because nothing was ever wired up
  ServerBase.Handle('/x', nil, nil, nil);
end;

procedure TdjServerBaseTests.TestHandleSkipsWhenWrappedHandlerNotStarted;
var
  ServerBase: IHandlerContainer;
  Fake: TFakeHandler;
begin
  ServerBase := TdjServerBase.Create;
  Fake := TFakeHandler.Create;
  ServerBase.AddHandler(Fake);
  // deliberately not calling ServerBase.Start

  ServerBase.Handle('/x', nil, nil, nil);

  CheckFalse(Fake.HandleCalled,
    'Handle is guarded by the wrapped handler''s own IsStarted, not merely '
    + 'by it being assigned');
end;

procedure TdjServerBaseTests.TestStartStartsTheWrappedHandler;
var
  ServerBase: IHandlerContainer;
  Fake: TFakeHandler;
begin
  ServerBase := TdjServerBase.Create;
  Fake := TFakeHandler.Create;
  ServerBase.AddHandler(Fake);

  ServerBase.Start;

  CheckTrue(Fake.StartCalled, 'starting the wrapper must start the wrapped handler');
end;

procedure TdjServerBaseTests.TestStopStopsTheWrappedHandler;
var
  ServerBase: IHandlerContainer;
  Fake: TFakeHandler;
begin
  ServerBase := TdjServerBase.Create;
  Fake := TFakeHandler.Create;
  ServerBase.AddHandler(Fake);
  ServerBase.Start;

  ServerBase.Stop;

  CheckTrue(Fake.StopCalled, 'stopping the wrapper must stop the wrapped handler');
end;

procedure TdjServerBaseTests.TestStopWhenNeverStartedDoesNotStopWrappedHandler;
var
  ServerBase: IHandlerContainer;
  Fake: TFakeHandler;
begin
  ServerBase := TdjServerBase.Create;
  Fake := TFakeHandler.Create;
  ServerBase.AddHandler(Fake);

  // TdjLifeCycle.Stop exits immediately when already stopped (the default
  // state for a never-started component), so DoStop -- and therefore the
  // wrapped handler's own Stop -- must never run.
  ServerBase.Stop;

  CheckFalse(Fake.StopCalled,
    'stopping a component that was never started must be a no-op');
end;

end.
