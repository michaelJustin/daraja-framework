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

unit djWebFilterMapping;

interface

{$i IdCompilerDefines.inc}

uses
  djWebFilterHolder,
  Classes, Generics.Collections;

type
  (**
   * Web Filter Mapping.
   *)

  { TdjWebFilterMapping }

  TdjWebFilterMapping = class(TObject)
  private
    FName: string;
    FPathSpecs: TStrings;
    FWebComponentNames: TStrings;
    FHolder: TdjWebFilterHolder;
  public
    constructor Create;
    destructor Destroy; override;

    function AppliesTo(const PathInContext: string): Boolean;

    // properties
    property WebFilterHolder: TdjWebFilterHolder read FHolder write FHolder;
    property WebFilterName: string read FName write FName;
    property WebComponentNames: TStrings read FWebComponentNames;
    property PathSpecs: TStrings read FPathSpecs;
  end;

  (**
   * Web Filter Mappings.
   *)
  TdjWebFilterMappings = TObjectList<TdjWebFilterMapping>;

implementation

uses
  djPathMap,
  Generics.Defaults;

{ TdjWebFilterMapping }

constructor TdjWebFilterMapping.Create;
begin
  inherited;

  FPathSpecs := TStringList.Create;
  FWebComponentNames := TStringList.Create;
end;

destructor TdjWebFilterMapping.Destroy;
begin
  FWebComponentNames.Free;
  FPathSpecs.Free;

  inherited;
end;

function TdjWebFilterMapping.AppliesTo(const PathInContext: string): Boolean;
var
  PathSpec: string;
begin
  Result := False;
  if FPathSpecs.Count = 0 then
    Exit;
  for PathSpec in PathSpecs do
  begin
    if TdjPathMap.Matches(PathInContext, PathSpec) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.

