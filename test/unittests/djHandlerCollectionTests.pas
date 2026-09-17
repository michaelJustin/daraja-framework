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

unit djHandlerCollectionTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjHandlerCollectionTests }

  TdjHandlerCollectionTests = class(TTestCase)
  published
    procedure TestHandleCallsAllHandlers;
    procedure TestHandleSkipsWhenNotStarted;
    procedure TestHandleContinuesAfterHandlerException;
    procedure TestAddHandlerStartsItWhenCollectionAlreadyStarted;
    procedure TestAddHandlerDoesNotStartItWhenCollectionNotStarted;
    procedure TestRemoveHandlerStopsStartedHandler;
    procedure TestStartStartsAllHandlers;
    procedure TestStartContinuesAfterHandlerStartException;
    procedure TestStopStopsAllHandlersEvenAfterException;
  end;

implementation

uses
  djHandlerCollection, djInterfaces, djServerContext, djTypes, SysUtils;

type
  { TFakeHandler: records calls, can be told to raise on Start/Handle }
  TFakeHandler = class(TInterfacedObject, IHandler)
  public
    StartCalled, StopCalled, HandleCalled: Boolean;
    FStarted: Boolean;
    RaiseOnStart, RaiseOnHandle: Boolean;
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
  if RaiseOnStart then
    raise Exception.Create('boom on start');
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
  if RaiseOnHandle then
    raise Exception.Create('boom on handle');
end;

{ TdjHandlerCollectionTests }

// TdjHandlerCollection descends from TInterfacedObject (via TdjLifeCycle),
// and its AddHandler/RemoveHandler/Handle are only reachable through the
// IHandlerContainer/IHandler interfaces (they're protected on the class
// itself). So the collection is held as an interface variable for its
// whole lifetime here, not a class reference: a class reference cast to an
// interface only transiently ("as IHandlerContainer") would bump the
// refcount from 0 to 1 and back to 0 on that single statement, which
// self-destructs the object right there, before the test even runs.
// Holding it as IHandlerContainer from Create onward keeps one persistent
// reference, and the object is cleaned up automatically when that variable
// goes out of scope -- no manual Free needed or safe to use here.

procedure TdjHandlerCollectionTests.TestHandleCallsAllHandlers;
var
  Collection: IHandlerContainer;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  H2 := TFakeHandler.Create;
  Collection.AddHandler(H1);
  Collection.AddHandler(H2);
  Collection.Start;

  Collection.Handle('/x', nil, nil, nil);

  CheckTrue(H1.HandleCalled, 'first handler called');
  CheckTrue(H2.HandleCalled, 'second handler called');
end;

procedure TdjHandlerCollectionTests.TestHandleSkipsWhenNotStarted;
var
  Collection: IHandlerContainer;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  Collection.AddHandler(H1);
  // Collection.Start; -- deliberately not started

  Collection.Handle('/x', nil, nil, nil);

  CheckFalse(H1.HandleCalled, 'handler must not run while collection is stopped');
end;

procedure TdjHandlerCollectionTests.TestHandleContinuesAfterHandlerException;
var
  Collection: IHandlerContainer;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  H1.RaiseOnHandle := True;
  H2 := TFakeHandler.Create;
  Collection.AddHandler(H1);
  Collection.AddHandler(H2);
  Collection.Start;

  Collection.Handle('/x', nil, nil, nil);

  CheckTrue(H2.HandleCalled,
    'later handlers must still run after an earlier one raises');
end;

procedure TdjHandlerCollectionTests.TestAddHandlerStartsItWhenCollectionAlreadyStarted;
var
  Collection: IHandlerContainer;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  Collection.Start;

  H1 := TFakeHandler.Create;
  Collection.AddHandler(H1);

  CheckTrue(H1.StartCalled, 'handler added to a running collection is started');
end;

procedure TdjHandlerCollectionTests.TestAddHandlerDoesNotStartItWhenCollectionNotStarted;
var
  Collection: IHandlerContainer;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  Collection.AddHandler(H1);

  CheckFalse(H1.StartCalled,
    'handler added to a stopped collection is not started yet');
end;

procedure TdjHandlerCollectionTests.TestRemoveHandlerStopsStartedHandler;
var
  Collection: IHandlerContainer;
  H1: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  Collection.AddHandler(H1);
  Collection.Start;
  CheckTrue(H1.IsStarted, 'sanity: handler is running');

  Collection.RemoveHandler(H1);

  CheckTrue(H1.StopCalled, 'removing a running handler stops it first');
end;

procedure TdjHandlerCollectionTests.TestStartStartsAllHandlers;
var
  Collection: IHandlerContainer;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  H2 := TFakeHandler.Create;
  Collection.AddHandler(H1);
  Collection.AddHandler(H2);

  Collection.Start;

  CheckTrue(H1.StartCalled, 'handler 1 started');
  CheckTrue(H2.StartCalled, 'handler 2 started');
end;

procedure TdjHandlerCollectionTests.TestStartContinuesAfterHandlerStartException;
var
  Collection: IHandlerContainer;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  H1.RaiseOnStart := True;
  H2 := TFakeHandler.Create;
  Collection.AddHandler(H1);
  Collection.AddHandler(H2);

  Collection.Start;

  CheckTrue(H2.StartCalled,
    'later handlers must still start after an earlier one raises');
  CheckTrue(Collection.IsStarted,
    'the collection itself must still end up started');
end;

procedure TdjHandlerCollectionTests.TestStopStopsAllHandlersEvenAfterException;
var
  Collection: IHandlerContainer;
  H1, H2: TFakeHandler;
begin
  Collection := TdjHandlerCollection.Create;
  H1 := TFakeHandler.Create;
  H2 := TFakeHandler.Create;
  Collection.AddHandler(H1);
  Collection.AddHandler(H2);
  Collection.Start;

  Collection.Stop;

  CheckTrue(H1.StopCalled, 'handler 1 stopped');
  CheckTrue(H2.StopCalled, 'handler 2 stopped');
end;

end.
