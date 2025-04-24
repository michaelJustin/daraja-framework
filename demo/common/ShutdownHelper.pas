(*
   Copyright (c) Michael Justin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

unit ShutdownHelper;

// note: this is unsupported example code

interface

uses
  djServer;

procedure SetShutdownHook(const AServer: TdjServer);

implementation

uses
  Windows;

var
  Server: TdjServer;

procedure Log(Msg: string);
begin
  if IsConsole then
    WriteLn(Msg);
end;

function ConsoleHandler(CtrlType: DWORD): BOOL; stdcall;
begin
  case CtrlType of
    CTRL_C_EVENT,
    CTRL_BREAK_EVENT,
    CTRL_LOGOFF_EVENT,
    CTRL_SHUTDOWN_EVENT,
    CTRL_CLOSE_EVENT:
    begin
      Log('Shutting down.');

      if Assigned(Server) then
      begin
        Log('Stopping server.');
        Server.Stop;
      end;

      Result := True;
    end
    else
      Result := False;
  end;
end;

procedure SetShutdownHook(const AServer: TdjServer);
begin
  // intercept control events
  SetConsoleCtrlHandler(@ConsoleHandler, True);

  Server := AServer;

  {$IFDEF FPC}
  // CTRL+C causes SIGINT error
  Log('Shutdown handler enabled');
  {$ELSE}
  Log('Shutdown with Ctrl-C and Ctrl-Close enabled');
  {$ENDIF}
end;

end.
