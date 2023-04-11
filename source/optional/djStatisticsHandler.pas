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

unit djStatisticsHandler;

interface

{$i IdCompilerDefines.inc}

uses
  djHandlerWrapper, djServerContext, djTypes,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdThreadSafe;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

type
  (**
   * Collects HTTP request statistics.
   *
   * \note This class is unsupported demonstration code.
   *
   *)
  TdjStatisticsHandler = class(TdjHandlerWrapper)
  private

    FResponses2xx: TIdThreadSafeInt64;
    FResponses3xx: TIdThreadSafeInt64;
    FResponses1xx: TIdThreadSafeInt64;
    FResponses4xx: TIdThreadSafeInt64;
    FResponses5xx: TIdThreadSafeInt64;

    FRequestsActive: TIdThreadSafeInt64;
    FRequests: TIdThreadSafeInt64;

    FRequestsDurationTotal: TIdThreadSafeInt64;
    FRequestsDurationMax: TIdThreadSafeCardinal;
    FRequestsDurationMin: TIdThreadSafeCardinal;

    function GetRequestsDurationAve: Integer;
    function GetRequests: Int64;
    function GetRequestsActive: Integer;
    function GetRequestsDurationMax: Cardinal;
    function GetRequestsDurationMin: Cardinal;
    function GetRequestsDurationTotal: Int64;
    function GetResponses1xx: Int64;
    function GetResponses2xx: Int64;
    function GetResponses3xx: Int64;
    function GetResponses4xx: Int64;
    function GetResponses5xx: Int64;

  public
    constructor Create; override;
    destructor Destroy; override;

    (**
     * Handle a HTTP request.
     *
     * \param Target Request target
     * \param Context HTTP server context
     * \param Request HTTP request
     * \param Response HTTP response
     * \throws EWebComponentException if an exception occurs that interferes with the component's normal operation
     *
     * \sa IHandler
     *
     *)
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;

    // properties
    property Requests: Int64 read GetRequests;
    property RequestsActive: Integer read GetRequestsActive;

    property RequestsDurationAve: Integer read GetRequestsDurationAve;
    property RequestsDurationTotal: Int64 read GetRequestsDurationTotal;
    property RequestsDurationMin: Cardinal read GetRequestsDurationMin;
    property RequestsDurationMax: Cardinal read GetRequestsDurationMax;

    property Responses1xx: Int64 read GetResponses1xx;
    property Responses2xx: Int64 read GetResponses2xx;
    property Responses3xx: Int64 read GetResponses3xx;
    property Responses4xx: Int64 read GetResponses4xx;
    property Responses5xx: Int64 read GetResponses5xx;

  end;

implementation

uses
  djPlatform,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdGlobal, // GetTickDiff64
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

{ TdjStatisticsHandler }

constructor TdjStatisticsHandler.Create;
begin
  inherited Create;

  FResponses2xx := TIdThreadSafeInt64.Create;
  FResponses3xx := TIdThreadSafeInt64.Create;
  FResponses1xx := TIdThreadSafeInt64.Create;
  FResponses4xx := TIdThreadSafeInt64.Create;
  FResponses5xx := TIdThreadSafeInt64.Create;

  FRequestsActive := TIdThreadSafeInt64.Create;
  FRequests := TIdThreadSafeInt64.Create;

  FRequestsDurationTotal := TIdThreadSafeInt64.Create;
  FRequestsDurationMax := TIdThreadSafeCardinal.Create;
  FRequestsDurationMin := TIdThreadSafeCardinal.Create;

end;

destructor TdjStatisticsHandler.Destroy;
begin

  FResponses2xx.Free;
  FResponses3xx.Free;
  FResponses1xx.Free;
  FResponses4xx.Free;
  FResponses5xx.Free;

  FRequestsActive.Free;
  FRequests.Free;

  FRequestsDurationTotal.Free;
  FRequestsDurationMax.Free;
  FRequestsDurationMin.Free;

  inherited;
end;

function TdjStatisticsHandler.GetRequests: Int64;
begin
  Result := FRequests.Value;
end;

function TdjStatisticsHandler.GetRequestsActive: Integer;
begin
 Result := FRequestsActive.Value;
end;

function TdjStatisticsHandler.GetRequestsDurationAve: Integer;
begin
  if Requests = 0 then
  begin
    Result := 0;
  end
  else
  begin
    Result := Trunc(RequestsDurationTotal / Requests);
  end;
end;

function TdjStatisticsHandler.GetRequestsDurationMax: Cardinal;
begin
  Result := FRequestsDurationMax.Value;
end;

function TdjStatisticsHandler.GetRequestsDurationMin: Cardinal;
begin
  Result := FRequestsDurationMin.Value;
end;

function TdjStatisticsHandler.GetRequestsDurationTotal: Int64;
begin
  Result := FRequestsDurationTotal.Value;
end;

function TdjStatisticsHandler.GetResponses1xx: Int64;
begin
  Result := FResponses1xx.Value;
end;

function TdjStatisticsHandler.GetResponses2xx: Int64;
begin
  Result := FResponses2xx.Value;
end;

function TdjStatisticsHandler.GetResponses3xx: Int64;
begin
  Result := FResponses3xx.Value;
end;

function TdjStatisticsHandler.GetResponses4xx: Int64;
begin
  Result := FResponses4xx.Value;
end;

function TdjStatisticsHandler.GetResponses5xx: Int64;
begin
  Result := FResponses5xx.Value;
end;

procedure TdjStatisticsHandler.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
var
  Started: Cardinal;
  Elapsed: Cardinal;
begin
  Started := djPlatform.GetTickCount;

  try

    FRequests.Increment;
    FRequestsActive.Increment;

    inherited;

  finally
    FRequestsActive.Decrement;

    Elapsed := GetTickDiff64(Started, djPlatform.GetTickCount);

    FRequestsDurationTotal.Value := FRequestsDurationTotal.Value + Elapsed;

    if Elapsed > FRequestsDurationMax.Value then
      FRequestsDurationMax.Value := Elapsed;
    if (FRequestsDurationMin.Value = 0) or (Elapsed < FRequestsDurationMin.Value) then
      FRequestsDurationMin.Value := Elapsed;

    case (Trunc(Response.ResponseNo / 100)) of
      1: FResponses1xx.Increment;
      2: FResponses2xx.Increment;
      3: FResponses3xx.Increment;
      4: FResponses4xx.Increment;
      5: FResponses5xx.Increment;
    else
      begin
{$IFDEF DEBUG}
        Assert(False, 'Bad HTTP response ' + IntToStr(Response.ResponseNo)
          + ' - target: ' + Target);
{$ENDIF}
      end;
    end;

  end;
end;

end.

