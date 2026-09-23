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

unit djWebFilterChainTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjWebFilterChainTests }

  TdjWebFilterChainTests = class(TTestCase)
  published
    procedure TestFilterRunsBeforeAndAfterTerminal;
    procedure TestFilterShortCircuitsChain;
    procedure TestExceptionMidChainSkipsTerminal;
    procedure TestTwoFiltersRunInWrappingOrder;
    procedure TestZeroFilterChainInvokesTerminalDirectly;
  end;

implementation

uses
  djWebFilterChain, djWebFilterHolder, djWebFilter, djWebAppContext,
  djInterfaces, djServerContext, djTypes,
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomHTTPServer,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

type

  { TChainRecordingFilter }

  {*
   * Appends its (init-parameter supplied) name to the response before and
   * after delegating to the next link, so tests can observe both call order
   * and that mutations made after Chain.DoFilter returns are preserved.
   *}
  TChainRecordingFilter = class(TdjWebFilter)
  strict private
    FName: string;
  public
    procedure Init(const Config: IWebFilterConfig); override;
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TShortCircuitFilter }

  TShortCircuitFilter = class(TdjWebFilter)
  public
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TThrowingFilter }

  TThrowingFilter = class(TdjWebFilter)
  public
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain); override;
  end;

  { TTerminalChainLink }

  {*
   * Stands in for the wrapped handler at the end of a real filter chain
   * (TChainEnd in djWebComponentHandler.pas).
   *}
  TTerminalChainLink = class(TInterfacedObject, IWebFilterChain)
  public
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse);
  end;

{ TChainRecordingFilter }

procedure TChainRecordingFilter.Init(const Config: IWebFilterConfig);
begin
  FName := Config.GetInitParameter('name');
end;

procedure TChainRecordingFilter.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  Response.ContentText := Response.ContentText + '>' + FName;
  Chain.DoFilter(Context, Request, Response);
  Response.ContentText := Response.ContentText + '<' + FName;
end;

{ TShortCircuitFilter }

procedure TShortCircuitFilter.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  // deliberately does not call Chain.DoFilter
  Response.ContentText := Response.ContentText + 'stopped';
end;

{ TThrowingFilter }

procedure TThrowingFilter.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
begin
  raise Exception.Create('boom');
end;

{ TTerminalChainLink }

procedure TTerminalChainLink.DoFilter(Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := Response.ContentText + '>TERMINAL';
end;

{ helpers }

function NewStartedHolder(FilterClass: TdjWebFilterClass;
  Context: TdjWebAppContext; const InitName: string): TdjWebFilterHolder;
begin
  Result := TdjWebFilterHolder.Create(FilterClass);
  Result.SetContext(Context.GetCurrentContext);
  if InitName <> '' then
    Result.SetInitParameter('name', InitName);
  Result.Start;
end;

procedure StopAndFree(var Holder: TdjWebFilterHolder);
begin
  if Holder = nil then
    Exit;
  // releases the TdjWebFilter instance created by Start; TdjWebFilterHolder's
  // destructor does not do this itself (see TdjWebComponentHandler.Destroy for
  // the same "if IsStarted then Stop" idiom applied by the owning container)
  if Holder.IsStarted then
    Holder.Stop;
  FreeAndNil(Holder);
end;

{ TdjWebFilterChainTests }

procedure TdjWebFilterChainTests.TestFilterRunsBeforeAndAfterTerminal;
var
  Context: TdjWebAppContext;
  Holder: TdjWebFilterHolder;
  Chain: IWebFilterChain;
  Request: TdjRequest;
  Response: TdjResponse;
begin
  Context := TdjWebAppContext.Create('chain-ctx');
  try
    Holder := NewStartedHolder(TChainRecordingFilter, Context, 'A');
    try
      Chain := TdjWebFilterChain.Create(Holder, TTerminalChainLink.Create);

      Request := TIdHTTPRequestInfo.Create(nil);
      try
        Response := TIdHTTPResponseInfo.Create(nil, Request, nil);
        try
          Chain.DoFilter(nil, Request, Response);

          CheckEquals('>A>TERMINAL<A', Response.ContentText,
            'filter must run before delegating and again after the chain returns');
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      StopAndFree(Holder);
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebFilterChainTests.TestFilterShortCircuitsChain;
var
  Context: TdjWebAppContext;
  Holder: TdjWebFilterHolder;
  Chain: IWebFilterChain;
  Request: TdjRequest;
  Response: TdjResponse;
begin
  Context := TdjWebAppContext.Create('chain-ctx');
  try
    Holder := NewStartedHolder(TShortCircuitFilter, Context, '');
    try
      Chain := TdjWebFilterChain.Create(Holder, TTerminalChainLink.Create);

      Request := TIdHTTPRequestInfo.Create(nil);
      try
        Response := TIdHTTPResponseInfo.Create(nil, Request, nil);
        try
          Chain.DoFilter(nil, Request, Response);

          CheckEquals('stopped', Response.ContentText,
            'a filter that never calls Chain.DoFilter must prevent the '
            + 'wrapped handler from running');
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      StopAndFree(Holder);
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebFilterChainTests.TestExceptionMidChainSkipsTerminal;
var
  Context: TdjWebAppContext;
  Holder: TdjWebFilterHolder;
  Chain: IWebFilterChain;
  Request: TdjRequest;
  Response: TdjResponse;
begin
  Context := TdjWebAppContext.Create('chain-ctx');
  try
    Holder := NewStartedHolder(TThrowingFilter, Context, '');
    try
      Chain := TdjWebFilterChain.Create(Holder, TTerminalChainLink.Create);

      Request := TIdHTTPRequestInfo.Create(nil);
      try
        Response := TIdHTTPResponseInfo.Create(nil, Request, nil);
        try
          try
            Chain.DoFilter(nil, Request, Response);
            Fail('expected exception raised by the filter to propagate');
          except
            on E: Exception do
              CheckEquals('boom', E.Message,
                'the filter''s exception must propagate to the caller');
          end;

          CheckEquals('', Response.ContentText,
            'the wrapped handler must not run once a filter raises');
        finally
          Response.Free;
        end;
      finally
        Request.Free;
      end;
    finally
      StopAndFree(Holder);
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebFilterChainTests.TestTwoFiltersRunInWrappingOrder;
var
  Context: TdjWebAppContext;
  OuterHolder, InnerHolder: TdjWebFilterHolder;
  Chain: IWebFilterChain;
  Request: TdjRequest;
  Response: TdjResponse;
begin
  Context := TdjWebAppContext.Create('chain-ctx');
  try
    OuterHolder := NewStartedHolder(TChainRecordingFilter, Context, 'outer');
    try
      InnerHolder := NewStartedHolder(TChainRecordingFilter, Context, 'inner');
      try
        // OuterHolder wraps a chain that itself wraps InnerHolder, which
        // wraps the terminal link -- mirrors how
        // TdjWebComponentHandler.GetFilterChain nests TdjWebFilterChain
        // instances around each other.
        Chain := TdjWebFilterChain.Create(OuterHolder,
          TdjWebFilterChain.Create(InnerHolder, TTerminalChainLink.Create));

        Request := TIdHTTPRequestInfo.Create(nil);
        try
          Response := TIdHTTPResponseInfo.Create(nil, Request, nil);
          try
            Chain.DoFilter(nil, Request, Response);

            CheckEquals('>outer>inner>TERMINAL<inner<outer', Response.ContentText,
              'the outer filter must run first and last, the inner filter '
              + 'in between, around the terminal handler');
          finally
            Response.Free;
          end;
        finally
          Request.Free;
        end;
      finally
        StopAndFree(InnerHolder);
      end;
    finally
      StopAndFree(OuterHolder);
    end;
  finally
    Context.Free;
  end;
end;

procedure TdjWebFilterChainTests.TestZeroFilterChainInvokesTerminalDirectly;
var
  Chain: IWebFilterChain;
  Request: TdjRequest;
  Response: TdjResponse;
begin
  // with no filter mapped to a path, TdjWebComponentHandler.GetFilterChain
  // returns the terminal link itself, with no TdjWebFilterChain wrapping it
  // at all -- exercise that boundary case directly.
  Chain := TTerminalChainLink.Create;

  Request := TIdHTTPRequestInfo.Create(nil);
  try
    Response := TIdHTTPResponseInfo.Create(nil, Request, nil);
    try
      Chain.DoFilter(nil, Request, Response);

      CheckEquals('>TERMINAL', Response.ContentText,
        'a zero-filter chain must reach the wrapped handler directly');
    finally
      Response.Free;
    end;
  finally
    Request.Free;
  end;
end;

end.
