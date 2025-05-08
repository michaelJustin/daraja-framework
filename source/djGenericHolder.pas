(*

    Daraja HTTP Framework
    Copyright (c) Michael Justin

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

unit djGenericHolder;

interface

// {$i IdCompilerDefines.inc}

uses
  djLifeCycle,
  Classes;

type
  { TdjGenericHolder }

  (**
   * A generic holder class that manages objects of type T.
   * 
   * @tparam T A type parameter constrained to TInterfacedObject.
   * @note This class inherits from TdjLifeCycle.
   *)
  TdjGenericHolder<T: TInterfacedObject> = class(TdjLifeCycle)
  private
    FName: string;
  public
    (**
     * Constructor for creating an instance of the class.
     *
     * @param AClass The class type to be used for initialization.
     *)
    constructor Create(AClass: TInterfacedClass); reintroduce;
    // properties
    property Name: string read FName write FName;
  end;

implementation

constructor TdjGenericHolder<T>.Create(AClass: TInterfacedClass);
begin
  inherited Create;

  Assert(Assigned(AClass));

  // default value for name property
  FName := AClass.ClassName;
end;


end.
