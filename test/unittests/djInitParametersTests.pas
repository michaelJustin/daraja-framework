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

unit djInitParametersTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjInitParametersTests }

  {*
   * TdjInitParameters is a plain TDictionary<string, string> alias -- all of
   * its own behavior is the documented contract on that alias (case-sensitive
   * keys, unspecified iteration order). These tests pin that contract down
   * across both the FPC and Delphi RTLs, and exercise the "missing parameter"
   * lookup path the issue calls out.
   *}
  TdjInitParametersTests = class(TTestCase)
  published
    procedure TestAddedParameterIsRetrievable;
    procedure TestMissingParameterIsNotFound;
    procedure TestKeysAreComparedCaseSensitively;
    procedure TestRemoveDeletesOnlyTheGivenKey;
    procedure TestValueCanBeUpdatedInPlace;
  end;

implementation

uses
  djInitParameters;

{ TdjInitParametersTests }

procedure TdjInitParametersTests.TestAddedParameterIsRetrievable;
var
  Params: TdjInitParameters;
  Value: string;
begin
  Params := TdjInitParameters.Create;
  try
    Params.Add('key', 'value');

    CheckTrue(Params.ContainsKey('key'));
    CheckTrue(Params.TryGetValue('key', Value));
    CheckEquals('value', Value);
  finally
    Params.Free;
  end;
end;

procedure TdjInitParametersTests.TestMissingParameterIsNotFound;
var
  Params: TdjInitParameters;
  Value: string;
begin
  Params := TdjInitParameters.Create;
  try
    CheckFalse(Params.ContainsKey('missing'),
      'a key that was never added must not be reported as present');
    CheckFalse(Params.TryGetValue('missing', Value),
      'looking up a missing key must fail rather than returning a default');
  finally
    Params.Free;
  end;
end;

procedure TdjInitParametersTests.TestKeysAreComparedCaseSensitively;
var
  Params: TdjInitParameters;
begin
  Params := TdjInitParameters.Create;
  try
    Params.Add('Key', 'upper');
    Params.Add('key', 'lower');

    // per the documented contract on TdjInitParameters, 'Key' and 'key' are
    // distinct parameter names, not the same one overwritten
    CheckEquals(2, Params.Count);
    CheckEquals('upper', Params['Key']);
    CheckEquals('lower', Params['key']);
    CheckFalse(Params.ContainsKey('KEY'), 'a differently-cased key must not match');
  finally
    Params.Free;
  end;
end;

procedure TdjInitParametersTests.TestRemoveDeletesOnlyTheGivenKey;
var
  Params: TdjInitParameters;
begin
  Params := TdjInitParameters.Create;
  try
    Params.Add('a', '1');
    Params.Add('b', '2');

    Params.Remove('a');

    CheckFalse(Params.ContainsKey('a'), 'the removed key must be gone');
    CheckTrue(Params.ContainsKey('b'), 'other keys must be unaffected');
  finally
    Params.Free;
  end;
end;

procedure TdjInitParametersTests.TestValueCanBeUpdatedInPlace;
var
  Params: TdjInitParameters;
begin
  Params := TdjInitParameters.Create;
  try
    Params.Add('key', 'first');
    Params['key'] := 'second';

    CheckEquals('second', Params['key']);
    CheckEquals(1, Params.Count, 'updating a value must not add a second entry');
  finally
    Params.Free;
  end;
end;

end.
