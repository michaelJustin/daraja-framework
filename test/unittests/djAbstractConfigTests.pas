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

unit djAbstractConfigTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjAbstractConfigTests }

  TdjAbstractConfigTests = class(TTestCase)
  published
    procedure TestGetInitParameterForMissingKeyReturnsEmptyString;
    procedure TestAddAndGetInitParameterRoundTrip;
    procedure TestParameterNamesAreCaseSensitive;
    procedure TestAddDuplicateKeyRaises;
    procedure TestGetInitParameterNamesReturnsAllAddedKeys;
    procedure TestGetContextDefaultsToNilBeforeSetContext;
    procedure TestSetContextRejectsNil;
    procedure TestSetContextIsIdempotentForTheSameContext;
    procedure TestSetContextRejectsChangingToADifferentContext;
    procedure TestNameDefaultsEmptyAndRoundTrips;
  end;

implementation

uses
  djAbstractConfig, djInterfaces, djTypes, djWebAppContext, SysUtils, Classes;

{ TdjAbstractConfigTests }

// TdjAbstractConfig descends from TInterfacedObject, and Add/SetContext/
// SetName/GetInitParameter/GetInitParameterNames are only reachable through
// the IWriteableConfig/IContextConfig interfaces (they're protected on the
// class itself; GetContext is public, and GetName is protected with no
// interface of its own -- see TTestConfig below). Each test keeps a
// persistent IWriteableConfig reference for the object's whole lifetime --
// same reasoning as djHandlerCollectionTests.pas -- and casts to
// IContextConfig only while that reference is still held, so the transient
// cast can never drop the refcount to zero.

type
  { TTestConfig }

  {*
   * GetName has no interface of its own (unlike SetName, part of
   * IWriteableConfig) -- normally it's only reachable via a leaf class such
   * as TdjWebFilterConfig.GetFilterName. This exposes it directly, the same
   * way TTestPathMap exposes TdjPathMap.GetSpecType in djPathMapTests.pas.
   *}
  TTestConfig = class(TdjAbstractConfig)
  public
    function GetName: string;
  end;

function TTestConfig.GetName: string;
begin
  Result := inherited;
end;

procedure TdjAbstractConfigTests.TestGetInitParameterForMissingKeyReturnsEmptyString;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjAbstractConfig.Create;
  Config := ConfigObj;

  CheckEquals('', (Config as IContextConfig).GetInitParameter('missing'),
    'a key that was never added must return an empty string, not raise');
end;

procedure TdjAbstractConfigTests.TestAddAndGetInitParameterRoundTrip;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjAbstractConfig.Create;
  Config := ConfigObj;

  Config.Add('key', 'value');

  CheckEquals('value', (Config as IContextConfig).GetInitParameter('key'));
end;

procedure TdjAbstractConfigTests.TestParameterNamesAreCaseSensitive;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjAbstractConfig.Create;
  Config := ConfigObj;

  Config.Add('Key', 'upper');
  Config.Add('key', 'lower');

  CheckEquals('upper', (Config as IContextConfig).GetInitParameter('Key'));
  CheckEquals('lower', (Config as IContextConfig).GetInitParameter('key'));
  CheckEquals('', (Config as IContextConfig).GetInitParameter('KEY'),
    'a differently-cased key must not match either of the above');
end;

procedure TdjAbstractConfigTests.TestAddDuplicateKeyRaises;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjAbstractConfig.Create;
  Config := ConfigObj;

  Config.Add('key', 'first');

  try
    Config.Add('key', 'second');
    Fail('Expected EDarajaConfigException for a duplicate key');
  except
    on E: EDarajaConfigException do
      ; // expected
  end;

  CheckEquals('first', (Config as IContextConfig).GetInitParameter('key'),
    'the original value must survive a rejected duplicate Add');
end;

procedure TdjAbstractConfigTests.TestGetInitParameterNamesReturnsAllAddedKeys;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
  Names: TdjStringArray;
begin
  ConfigObj := TdjAbstractConfig.Create;
  Config := ConfigObj;

  Config.Add('alpha', '1');
  Config.Add('beta', '2');

  Names := (Config as IContextConfig).GetInitParameterNames;

  // order is unspecified (see djInitParameters.pas's documented contract)
  CheckEquals(2, Length(Names), 'parameter count');
  CheckTrue((Names[0] = 'alpha') or (Names[1] = 'alpha'), 'alpha missing');
  CheckTrue((Names[0] = 'beta') or (Names[1] = 'beta'), 'beta missing');
end;

procedure TdjAbstractConfigTests.TestGetContextDefaultsToNilBeforeSetContext;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjAbstractConfig.Create;
  Config := ConfigObj;

  CheckTrue(ConfigObj.GetContext = nil,
    'a freshly created config must have no context yet');
end;

procedure TdjAbstractConfigTests.TestSetContextRejectsNil;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjAbstractConfig.Create;
  Config := ConfigObj;

  try
    Config.SetContext(nil);
    Fail('Expected EDarajaConfigException when setting the context to nil');
  except
    on E: EDarajaConfigException do
      ; // expected
  end;
end;

procedure TdjAbstractConfigTests.TestSetContextIsIdempotentForTheSameContext;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
  WebAppContext: TdjWebAppContext;
  Ctx: IContext;
begin
  WebAppContext := TdjWebAppContext.Create('abstract-config-ctx');
  try
    Ctx := WebAppContext.GetCurrentContext;

    ConfigObj := TdjAbstractConfig.Create;
    Config := ConfigObj;

    Config.SetContext(Ctx);
    Config.SetContext(Ctx); // setting the same context again must not raise

    CheckTrue(ConfigObj.GetContext = Ctx);
  finally
    WebAppContext.Free;
  end;
end;

procedure TdjAbstractConfigTests.TestSetContextRejectsChangingToADifferentContext;
var
  ConfigObj: TdjAbstractConfig;
  Config: IWriteableConfig;
  FirstContext, SecondContext: TdjWebAppContext;
begin
  FirstContext := TdjWebAppContext.Create('abstract-config-ctx-1');
  try
    SecondContext := TdjWebAppContext.Create('abstract-config-ctx-2');
    try
      ConfigObj := TdjAbstractConfig.Create;
      Config := ConfigObj;

      Config.SetContext(FirstContext.GetCurrentContext);

      try
        Config.SetContext(SecondContext.GetCurrentContext);
        Fail('Expected EDarajaConfigException when changing to a different context');
      except
        on E: EDarajaConfigException do
          ; // expected
      end;

      CheckTrue(ConfigObj.GetContext = FirstContext.GetCurrentContext,
        'the original context must survive a rejected change');
    finally
      SecondContext.Free;
    end;
  finally
    FirstContext.Free;
  end;
end;

procedure TdjAbstractConfigTests.TestNameDefaultsEmptyAndRoundTrips;
var
  ConfigObj: TTestConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TTestConfig.Create;
  Config := ConfigObj;

  CheckEquals('', ConfigObj.GetName, 'no name set yet');

  Config.SetName('my-filter');

  CheckEquals('my-filter', ConfigObj.GetName);
end;

end.
