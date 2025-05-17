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

unit djContextHandlerCollection;

interface

// {$i IdCompilerDefines.inc}

uses
  djHandlerList, djContextMap
  {$IFDEF DARAJA_LOGGING},djLogAPI, djLoggerFactory{$ENDIF DARAJA_LOGGING}
  ;

type
  { TdjContextHandlerCollection }

  (**
   * Multiple contexts may have the same context path and they are
   * called in order until one handles the request.
   *)
  TdjContextHandlerCollection = class(TdjHandlerList)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}

    ContextMap: TdjContextMap;

    procedure Trace(const S: string);

  public
    (**
     * Create a collection of context handlers.
     *)
    constructor Create; override;

    (**
     * Destructor.
     *)
    destructor Destroy; override;

  end;

implementation

{ TdjContextHandlerCollection }

constructor TdjContextHandlerCollection.Create;
begin
  inherited Create;

  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjContextHandlerCollection.ClassName);
  {$ENDIF DARAJA_LOGGING}

  ContextMap := TdjContextMap.Create;

  Trace('Created');
end;

destructor TdjContextHandlerCollection.Destroy;
begin
  {$IFDEF LOG_DESTROY}Trace('Destroy');{$ENDIF}

  ContextMap.Free;

  inherited;
end;

procedure TdjContextHandlerCollection.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

end.
