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

unit djLifeCycleTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type
  TdjLifeCycleTests = class(TTestCase)
  published
    procedure TestStartsStopped;
    procedure TestStartThenStop;
    procedure TestFailedStopStillCountsAsStopped;
  end;

implementation

uses
  djLifeCycle, SysUtils;

type
  { TFailingLifeCycle - a lifecycle whose custom stop code always fails }
  TFailingLifeCycle = class(TdjLifeCycle)
  protected
    procedure DoStop; override;
  end;

procedure TFailingLifeCycle.DoStop;
begin
  raise Exception.Create('stop failure for test');
end;

{ TdjLifeCycleTests }

procedure TdjLifeCycleTests.TestStartsStopped;
var
  LC: TdjLifeCycle;
begin
  LC := TdjLifeCycle.Create;
  try
    CheckFalse(LC.IsStarted, 'a fresh lifecycle is not started');
    CheckTrue(LC.IsStopped, 'a fresh lifecycle is stopped');
  finally
    LC.Free;
  end;
end;

procedure TdjLifeCycleTests.TestStartThenStop;
var
  LC: TdjLifeCycle;
begin
  LC := TdjLifeCycle.Create;
  try
    LC.Start;
    CheckTrue(LC.IsStarted, 'started after Start');
    CheckFalse(LC.IsStopped, 'not stopped after Start');

    LC.Stop;
    CheckFalse(LC.IsStarted, 'not started after Stop');
    CheckTrue(LC.IsStopped, 'stopped after Stop');
  finally
    LC.Free;
  end;
end;

procedure TdjLifeCycleTests.TestFailedStopStillCountsAsStopped;
var
  LC: TFailingLifeCycle;
begin
  LC := TFailingLifeCycle.Create;
  try
    LC.Start;
    CheckTrue(LC.IsStarted, 'started after Start');

    // DoStop raises; Stop logs and swallows it, but the component must
    // not be left in a half-started state.
    LC.Stop;
    CheckFalse(LC.IsStarted, 'not started after a failed Stop');
    CheckTrue(LC.IsStopped, 'stopped after a failed Stop');
  finally
    LC.Free;
  end;
end;

end.
