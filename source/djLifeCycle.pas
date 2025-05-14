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

unit djLifeCycle;

interface

// {$i IdCompilerDefines.inc}

uses
  djInterfaces,
{$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
{$ENDIF DARAJA_LOGGING}
  SyncObjs;

type
  { TdjLifeCycle }

  (**
   * Abstract LifeCycle implementation.
   *)
  TdjLifeCycle = class(TInterfacedObject, ILifeCycle)
  private
    FStarted: Boolean;
    FStopped: Boolean;
    
    CS: TCriticalSection;

    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    procedure Trace(const S: string);

    procedure SetStarted(const Value: Boolean);
    procedure SetStopped(const Value: Boolean);

  protected
    (**
     * Execute the custom start code.
     *)
    procedure DoStart; virtual;
    
    (**
     * Execute the custom stop code.
     *)
    procedure DoStop; virtual;

    (**
     * Raises an exception if the lifecycle is in "started" state
     *)
    procedure CheckStarted;

    (**
     * Raises an exception if the lifecycle is in "stopped" state
     *)
    procedure CheckStopped;

  public
    // ILifeCycle interface
    procedure Start;
    procedure Stop;
    function IsStarted: Boolean;
    function IsStopped: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // properties
    property Started: Boolean read FStarted write SetStarted;
    property Stopped: Boolean read FStopped write SetStopped;
  end;

implementation

uses
  SysUtils;

{ TdjLifeCycle }

procedure TdjLifeCycle.CheckStarted;
begin
  if Started then
    raise Exception.Create('Component started already!');
end;

procedure TdjLifeCycle.CheckStopped;
begin
  if Stopped then
    raise Exception.Create('Component stopped already!');
end;

constructor TdjLifeCycle.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjLifeCycle.ClassName);
  {$ENDIF DARAJA_LOGGING}

  CS := TCriticalSection.Create;

  FStopped := True;

  {$IFDEF LOG_CREATE}Trace('Created');
  {$ENDIF}
end;

destructor TdjLifeCycle.Destroy;
begin
  {$IFDEF LOG_DESTROY}Trace('Destroy');
  {$ENDIF}

  CS.Free;

  inherited;
end;

// getter / setter

function TdjLifeCycle.IsStarted: Boolean;
begin
  Result := FStarted;
end;

function TdjLifeCycle.IsStopped: Boolean;
begin
  Result := FStopped;
end;

procedure TdjLifeCycle.SetStarted(const Value: Boolean);
begin
  FStarted := Value;
  FStopped := not Value;
end;

procedure TdjLifeCycle.SetStopped(const Value: Boolean);
begin
  FStopped := Value;
  FStarted := not Value;
end;

// logging

procedure TdjLifeCycle.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

// methods

procedure TdjLifeCycle.DoStart;
begin

end;

procedure TdjLifeCycle.DoStop;
begin

end;

procedure TdjLifeCycle.Start;
begin
  if IsStarted then
    Exit;

  CS.Enter;
  try
    try
      Trace('Starting ...');
      DoStart;
      Trace('Started');
      Started := True;
    except
      on E: Exception do
      begin
        {$IFDEF DARAJA_LOGGING}
        Logger.Error('Start failed: %s %s in %s',
          [E.Message, E.ClassName, Self.ClassName]);
        {$ENDIF DARAJA_LOGGING}
        raise;
      end;
    end;
  finally
    CS.Leave;
  end;
end;

procedure TdjLifeCycle.Stop;
begin
  if Stopped then
    Exit;

  CS.Enter;
  try
    try
      Trace('Stopping ...');
      DoStop;
      Trace('Stopped');
      Stopped := True;
    except
      on E: Exception do
      begin
        {$IFDEF DARAJA_LOGGING}
        Logger.Error('Stop failed', E);
        {$ENDIF DARAJA_LOGGING}
      end;
    end;
  finally
    CS.Leave;
  end;
end;

end.
