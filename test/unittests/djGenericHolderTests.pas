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

unit djGenericHolderTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjGenericHolderTests }

  TdjGenericHolderTests = class(TTestCase)
  published
    procedure TestNameDefaultsToTheHeldClassName;
    procedure TestNameCanBeOverridden;
  end;

implementation

uses
  djGenericHolder;

type
  { a minimal class satisfying the TInterfacedObject constraint on T }
  TDummyHeldComponent = class(TInterfacedObject)
  end;

{ TdjGenericHolderTests }

procedure TdjGenericHolderTests.TestNameDefaultsToTheHeldClassName;
var
  Holder: TdjGenericHolder<TDummyHeldComponent>;
begin
  Holder := TdjGenericHolder<TDummyHeldComponent>.Create(TDummyHeldComponent);
  try
    CheckEquals('TDummyHeldComponent', Holder.Name,
      'the default name is the held class''s own class name');
  finally
    Holder.Free;
  end;
end;

procedure TdjGenericHolderTests.TestNameCanBeOverridden;
var
  Holder: TdjGenericHolder<TDummyHeldComponent>;
begin
  Holder := TdjGenericHolder<TDummyHeldComponent>.Create(TDummyHeldComponent);
  try
    Holder.Name := 'custom-name';

    CheckEquals('custom-name', Holder.Name);
  finally
    Holder.Free;
  end;
end;

end.
