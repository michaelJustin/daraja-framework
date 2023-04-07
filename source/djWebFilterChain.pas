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

unit djWebFilterChain;

interface

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses
  djInterfaces, djWebFilterHolder, djServerContext, djTypes,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  Classes;

type

  { TdjWebFilterChain }

  TdjWebFilterChain = class(TInterfacedObject, IWebFilterChain)
  private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
    procedure Trace(const S: string);
  private
    FChain: IWebFilterChain;
    FHolder: TdjWebFilterHolder;
  public
    constructor Create(Holder: TdjWebFilterHolder; const FilterChain: IWebFilterChain);

    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse);
  end;

implementation

{ TdjWebFilterChain }

constructor TdjWebFilterChain.Create(Holder: TdjWebFilterHolder;
  const FilterChain: IWebFilterChain);
begin
  inherited Create;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger('dj.' + TdjWebFilterChain.ClassName);
  {$ENDIF DARAJA_LOGGING}

  FHolder := Holder;
  FChain := FilterChain;

  {$IFDEF LOG_CREATE}
  Trace('Created');
  {$ENDIF}
end;

procedure TdjWebFilterChain.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  Trace('DoFilter');
  FHolder.DoFilter(Context, Request, Response, FChain);
end;

procedure TdjWebFilterChain.Trace(const S: string);
begin
  {$IFDEF DARAJA_LOGGING}
  if Logger.IsTraceEnabled then
  begin
    Logger.Trace(S);
  end;
  {$ENDIF DARAJA_LOGGING}
end;

end.

