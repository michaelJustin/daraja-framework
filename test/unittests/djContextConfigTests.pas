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

unit djContextConfigTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjContextConfigTests }

  {*
   * TdjContextConfig adds no code of its own over TdjAbstractConfig (see
   * djContextConfig.pas) -- the bulk of the behavioral contract is covered
   * directly by djAbstractConfigTests.pas. These tests just confirm the
   * subclass actually surfaces that inherited behavior and satisfies
   * IContextConfig.
   *}
  TdjContextConfigTests = class(TTestCase)
  published
    procedure TestImplementsIContextConfig;
    procedure TestAddAndGetInitParameterRoundTrip;
    procedure TestGetInitParameterForMissingKeyReturnsEmptyString;
  end;

implementation

uses
  djContextConfig, djInterfaces, SysUtils;

{ TdjContextConfigTests }

procedure TdjContextConfigTests.TestImplementsIContextConfig;
var
  ConfigObj: TdjContextConfig;
  Config: IWriteableConfig;
  AsContextConfig: IContextConfig;
begin
  ConfigObj := TdjContextConfig.Create;
  Config := ConfigObj;

  CheckTrue(Supports(Config, IContextConfig, AsContextConfig),
    'TdjContextConfig must implement IContextConfig');
end;

procedure TdjContextConfigTests.TestAddAndGetInitParameterRoundTrip;
var
  ConfigObj: TdjContextConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjContextConfig.Create;
  Config := ConfigObj;

  Config.Add('key', 'value');

  CheckEquals('value', (Config as IContextConfig).GetInitParameter('key'));
end;

procedure TdjContextConfigTests.TestGetInitParameterForMissingKeyReturnsEmptyString;
var
  ConfigObj: TdjContextConfig;
  Config: IWriteableConfig;
begin
  ConfigObj := TdjContextConfig.Create;
  Config := ConfigObj;

  CheckEquals('', (Config as IContextConfig).GetInitParameter('missing'));
end;

end.
