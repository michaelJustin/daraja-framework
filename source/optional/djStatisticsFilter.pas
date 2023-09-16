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

unit djStatisticsFilter;

interface

{$i IdCompilerDefines.inc}

uses
  djWebFilter, djHandlerWrapper, djServerContext, djTypes, djInterfaces,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdThreadSafe;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

type
  (**
   * Collects HTTP request statistics.
   *
   * \note This class is unsupported demonstration code.
   *)
  TdjStatisticsFilter = class(TdjWebFilter)
  private
    FResponses2xx: TIdThreadSafeInt64;
    FResponses3xx: TIdThreadSafeInt64;
    FResponses1xx: TIdThreadSafeInt64;
    FResponses4xx: TIdThreadSafeInt64;
    FResponses5xx: TIdThreadSafeInt64;

    FRequestsActive: TIdThreadSafeInt64;
    FRequests: TIdThreadSafeInt64;

    //FRequestsDurationTotal: TIdThreadSafeInt64;
    //FRequestsDurationMax: TIdThreadSafeCardinal;
    //FRequestsDurationMin: TIdThreadSafeCardinal;

    //function GetRequestsDurationAve: Integer;
    //function GetRequestsDurationMax: Cardinal;
    //function GetRequestsDurationMin: Cardinal;
    //function GetRequestsDurationTotal: Int64;

    function GetRequests: Int64;
    function GetRequestsActive: Integer;
    function GetResponses1xx: Int64;
    function GetResponses2xx: Int64;
    function GetResponses3xx: Int64;
    function GetResponses4xx: Int64;
    function GetResponses5xx: Int64;
  public
    destructor Destroy; override;

    procedure Init(const Config: IWebFilterConfig); override;
    (**
     * The doFilter method of the Filter is called by the container each time
     * a request/response pair is passed through the chain due to a client
     * request for a resource at the end of the chain.
     * The FilterChain passed in to this method allows the Filter to pass on
     * the request and response to the next entity in the chain.
     *)
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain); override;

    //property RequestsDurationAve: Integer read GetRequestsDurationAve;
    //property RequestsDurationTotal: Int64 read GetRequestsDurationTotal;
    //property RequestsDurationMin: Cardinal read GetRequestsDurationMin;
    //property RequestsDurationMax: Cardinal read GetRequestsDurationMax;

    property Requests: Int64 read GetRequests;
    property RequestsActive: Integer read GetRequestsActive;
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

{ TdjStatisticsFilter }

procedure TdjStatisticsFilter.Init;
begin
  inherited;

  FResponses1xx := TIdThreadSafeInt64.Create;
  FResponses2xx := TIdThreadSafeInt64.Create;
  FResponses3xx := TIdThreadSafeInt64.Create;
  FResponses4xx := TIdThreadSafeInt64.Create;
  FResponses5xx := TIdThreadSafeInt64.Create;

  FRequestsActive := TIdThreadSafeInt64.Create;
  FRequests := TIdThreadSafeInt64.Create;

  //FRequestsDurationTotal := TIdThreadSafeInt64.Create;
  //FRequestsDurationMax := TIdThreadSafeCardinal.Create;
  //FRequestsDurationMin := TIdThreadSafeCardinal.Create;
end;

destructor TdjStatisticsFilter.Destroy;
begin
  FResponses1xx.Free;
  FResponses2xx.Free;
  FResponses3xx.Free;
  FResponses4xx.Free;
  FResponses5xx.Free;

  FRequestsActive.Free;
  FRequests.Free;

  //FRequestsDurationTotal.Free;
  //FRequestsDurationMax.Free;
  //FRequestsDurationMin.Free;

  inherited;
end;

function TdjStatisticsFilter.GetRequests: Int64;
begin
  Result := FRequests.Value;
end;

function TdjStatisticsFilter.GetRequestsActive: Integer;
begin
  Result := FRequestsActive.Value;
end;

//function TdjStatisticsFilter.GetRequestsDurationAve: Integer;
//begin
//  if Requests = 0 then
//  begin
//    Result := 0;
//  end
//  else
//  begin
//    Result := Trunc(RequestsDurationTotal / Requests);
//  end;
//end;

//function TdjStatisticsFilter.GetRequestsDurationMax: Cardinal;
//begin
//  Result := FRequestsDurationMax.Value;
//end;
//
//function TdjStatisticsFilter.GetRequestsDurationMin: Cardinal;
//begin
//  Result := FRequestsDurationMin.Value;
//end;
//
//function TdjStatisticsFilter.GetRequestsDurationTotal: Int64;
//begin
//  Result := FRequestsDurationTotal.Value;
//end;

function TdjStatisticsFilter.GetResponses1xx: Int64;
begin
  Result := FResponses1xx.Value;
end;

function TdjStatisticsFilter.GetResponses2xx: Int64;
begin
  Result := FResponses2xx.Value;
end;

function TdjStatisticsFilter.GetResponses3xx: Int64;
begin
  Result := FResponses3xx.Value;
end;

function TdjStatisticsFilter.GetResponses4xx: Int64;
begin
  Result := FResponses4xx.Value;
end;

function TdjStatisticsFilter.GetResponses5xx: Int64;
begin
  Result := FResponses5xx.Value;
end;

procedure TdjStatisticsFilter.DoFilter;
//var
//  Started: Cardinal;
//  Elapsed: Cardinal;
  procedure SetSessionValue(const AKey: string; AValue: Integer);
  begin
    Request.Session.Content.Values['stats:' + AKey] := IntToStr(AValue);
  end;
begin
  //Started := djPlatform.GetTickCount;

  try
    SetSessionValue('requests', FRequests.Increment);
    SetSessionValue('requestsactive', FRequestsActive.Increment);

    Chain.DoFilter(Context, Request, Response);

  finally
    SetSessionValue('requestsactive', FRequestsActive.Decrement);

    //Elapsed := GetTickDiff64(Started, djPlatform.GetTickCount);
    //
    //FRequestsDurationTotal.Value := FRequestsDurationTotal.Value + Elapsed;
    //
    //if Elapsed > FRequestsDurationMax.Value then
    //  FRequestsDurationMax.Value := Elapsed;
    //if (FRequestsDurationMin.Value = 0) or (Elapsed < FRequestsDurationMin.Value) then
    //  FRequestsDurationMin.Value := Elapsed;

    case Trunc(Response.ResponseNo / 100) of
      1: SetSessionValue('responses1xx', FResponses1xx.Increment);
      2: SetSessionValue('responses2xx', FResponses2xx.Increment);
      3: SetSessionValue('responses3xx', FResponses3xx.Increment);
      4: SetSessionValue('responses4xx', FResponses4xx.Increment);
      5: SetSessionValue('responses5xx', FResponses5xx.Increment);
    else
      begin
        {$IFDEF DEBUG}
        Assert(False, 'Bad HTTP response ' + IntToStr(Response.ResponseNo));
        {$ENDIF}
      end;
    end;
  end;
end;

end.

