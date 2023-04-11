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

unit djWebComponentMapping;

interface

{$i IdCompilerDefines.inc}

uses
  Classes,
  Generics.Collections;

type
  (**
   * Web Component Mapping.
   *)
  TdjWebComponentMapping = class(TObject)
  private
    FName: string;
    FPathSpecs: TStrings;
  public
    (**
     * Constructor.
     *)
    constructor Create;
    (**
     * Destructor.
     *)
    destructor Destroy; override;

    // properties
    property WebComponentName: string read FName write FName;
    property PathSpecs: TStrings read FPathSpecs;
  end;

  (**
   * Web Component Mappings.
   *)
  // note Delphi 2009 AVs if it is a TObjectList<>
  // see http://stackoverflow.com/questions/289825/why-is-tlist-remove-producing-an-eaccessviolation-error
  // for a workaround
  // use TdjWebComponentMappings.Create(TComparer<TdjWebComponentMapping>.Default);
  TdjWebComponentMappings = TObjectList<TdjWebComponentMapping>;

implementation

{ TdjWebComponentMapping }

constructor TdjWebComponentMapping.Create;
begin
  FPathSpecs := TStringList.Create;
end;

destructor TdjWebComponentMapping.Destroy;
begin
  FPathSpecs.Free;

  inherited;
end;

end.

