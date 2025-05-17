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

unit djWebComponentHolders;

interface

// {$i IdCompilerDefines.inc}

uses
  djWebComponentHolder,
  Generics.Collections;

type
  // note Delphi 2009 AVs if it is a TObjectList<>
  // see http://stackoverflow.com/questions/289825/why-is-tlist-remove-producing-an-eaccessviolation-error
  // for a workaround
  // use  TdjWebComponentHolders.Create(TComparer<TdjWebComponentHolder>.Default);

  { TdjWebComponentHolders }

  (**
   * TdjWebComponentHolders is a class that manages a list of TdjWebComponentHolder objects.
   * It inherits from TObjectList to provide functionality for handling collections of components.
   *)
  TdjWebComponentHolders = class(TObjectList<TdjWebComponentHolder>)
  public
    (**
     * Checks if the specified web component name exists.
     *
     * @param WebComponentName The name of the web component to check.
     * @return True if the web component name exists, otherwise False.
     *)
    function Contains(const WebComponentName: string): Boolean;
  end;

implementation

{ TdjWebComponentHolders }

function TdjWebComponentHolders.Contains(const WebComponentName: string): Boolean;
var
  Holder: TdjWebComponentHolder;
begin
  Result := False;
  for Holder in Self do
  begin
    if Holder.Name = WebComponentName then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
