{***

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

***}

unit djLifeCycle;

interface

uses
  djInterfaces,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  SyncObjs;

type
  { TdjLifeCycle }

  {*
   * Abstract LifeCycle implementation.
   *}
  TdjLifeCycle = class(TInterfacedObject, ILifeCycle)
  strict private
    FStarted: Boolean;
    FStopped: Boolean;
    FCS: TCriticalSection;
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
  protected
    {*
     * Execute the custom start code.
     *}
    procedure DoStart; virtual;

    {*
     * Execute the custom stop code.
     *}
    procedure DoStop; virtual;

    {*
     * Raises an exception if the lifecycle is already in "started" state.
     *}
    procedure CheckNotStarted;

    {*
     * Raises an exception if the lifecycle is already in "stopped" state.
     *}
    procedure CheckNotStopped;

  public
    // ILifeCycle interface
    procedure Start;
    procedure Stop;
    function IsStarted: Boolean;
    function IsStopped: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // properties (read-only; use Start / Stop to change lifecycle state)
    property Started: Boolean read FStarted;
    property Stopped: Boolean read FStopped;
  end;

implementation /// \cond

uses
  SysUtils;

{ TdjLifeCycle }

procedure TdjLifeCycle.CheckNotStarted;
begin
  if Started then
    raise Exception.Create('Component started already!');
end;

procedure TdjLifeCycle.CheckNotStopped;
begin
  if Stopped then
    raise Exception.Create('Component stopped already!');
end;

constructor TdjLifeCycle.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjLifeCycle);
  {$ENDIF DARAJA_LOGGING}

  FCS := TCriticalSection.Create;

  FStopped := True;
end;

destructor TdjLifeCycle.Destroy;
begin
  FCS.Free;

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

  FCS.Enter;
  try
    // re-check: another thread may have finished starting this instance
    // while we were waiting for the lock (e.g. two requests racing to
    // first-touch a lazily-loaded web component)
    if IsStarted then
      Exit;

    try
      DoStart;
      FStarted := True;
      FStopped := False;
    except
      on E: Exception do
      begin
        {$IFDEF DARAJA_LOGGING}
        Logger.Error('Start failed: %s %s in %s',
          [E.Message, E.ClassName, Self.ClassName]);
        {$ENDIF DARAJA_LOGGING}

        // Best-effort rollback: DoStart may have partially started
        // sub-resources (e.g. connectors) before failing. FStarted is
        // still False, so Destroy's "if IsStarted then Stop" would never
        // reach them, leaking whatever did start. DoStop's CheckNotStopped
        // requires FStopped = False, so flip it temporarily; any exception
        // from the rollback itself is swallowed, since the original
        // DoStart failure is what the caller needs to see.
        FStopped := False;
        try
          DoStop;
        except
          on E2: Exception do
          begin
            {$IFDEF DARAJA_LOGGING}
            Logger.Error('Rollback after failed Start also failed', E2);
            {$ENDIF DARAJA_LOGGING}
          end;
        end;
        FStopped := True;

        raise;
      end;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TdjLifeCycle.Stop;
begin
  if Stopped then
    Exit;

  FCS.Enter;
  try
    try
      DoStop;
    except
      on E: Exception do
      begin
        // Unlike Start, a failed DoStop is logged and swallowed, not
        // re-raised. The component is still marked stopped below so it
        // cannot be left in a half-started state.
        {$IFDEF DARAJA_LOGGING}
        Logger.Error('Stop failed', E);
        {$ENDIF DARAJA_LOGGING}
      end;
    end;
    FStopped := True;
    FStarted := False;
  finally
    FCS.Leave;
  end;
end;

end. /// \endcond
