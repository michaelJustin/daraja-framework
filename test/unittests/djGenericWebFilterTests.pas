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

unit djGenericWebFilterTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjGenericWebFilterTests }

  TdjGenericWebFilterTests = class(TTestCase)
  published
    procedure TestConfigRaisesWhenNotInitialized;
    procedure TestInitAssignsConfigAndCallsParameterlessInit;
  end;

implementation

uses
  djGenericWebFilter, djWebFilterConfig, djWebAppContext, djInterfaces,
  djTypes, SysUtils;

type

  { TInitTrackingFilter }

  {*
   * Records whether the overridable, parameterless Init was invoked by the
   * inherited Init(Config) -- the convenience hook subclasses are meant to
   * override instead of Init(Config) itself.
   *}
  TInitTrackingFilter = class(TdjGenericWebFilter)
  public
    InitCalled: Boolean;
    procedure Init; override;
  end;

procedure TInitTrackingFilter.Init;
begin
  InitCalled := True;
end;

{ TdjGenericWebFilterTests }

procedure TdjGenericWebFilterTests.TestConfigRaisesWhenNotInitialized;
var
  Filter: TdjGenericWebFilter;
begin
  Filter := TdjGenericWebFilter.Create;
  try
    try
      Filter.Config;
      Fail('Expected EDarajaLifecycleException before Init is called');
    except
      on E: EDarajaLifecycleException do
        ; // expected
    end;
  finally
    Filter.Free;
  end;
end;

procedure TdjGenericWebFilterTests.TestInitAssignsConfigAndCallsParameterlessInit;
var
  Filter: TInitTrackingFilter;
  Context: TdjWebAppContext;
  Config: IWebFilterConfig;
begin
  Context := TdjWebAppContext.Create('generic-wf-ctx');
  try
    Config := TdjWebFilterConfig.Create;
    (Config as IWriteableConfig).SetContext(Context.GetCurrentContext);

    Filter := TInitTrackingFilter.Create;
    try
      Filter.Init(Config);

      CheckTrue(Filter.InitCalled,
        'the overridable parameterless Init must be called');
      CheckTrue(Filter.Config = Config,
        'GetWebFilterConfig must return the config passed to Init');
    finally
      Filter.Free;
    end;
  finally
    Context.Free;
  end;
end;

end.
