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

unit djMultiMap;

interface

uses
  Generics.Collections;

type
  { TdjMultiMap }

  TdjMultiMap<T: class> = class(TObjectDictionary<string, TObjectList<T>>)
  public
    destructor Destroy; override;
    procedure Add(const Key: string; Value: T);
    function GetValues(const Key: string): TObjectList<T>;
  end;

implementation

uses
  Generics.Defaults;

{ TdjMultiMap }

destructor TdjMultiMap<T>.Destroy;
var
  Key: String;
  L: TObjectList<T>;
begin
  (* for L in Self.Values do
  begin
     L.Free;
  end; *)
  for Key in Self.Keys do
  begin
     if TryGetValue(Key, L) then
     begin
       // L.OwnsObjects := False;
       L.Free;
     end;
  end;

  inherited;
end;

procedure TdjMultiMap<T>.Add(const Key: string; Value: T);
var
  L: TObjectList<T>;
begin
  if not TryGetValue(Key, L) then
  begin
    L := TObjectList<T>.Create(TComparer<T>.Default, False);
    inherited Add(Key, L);
  end;
  L.Add(Value);
end;

function TdjMultiMap<T>.GetValues(const Key: string): TObjectList<T>;
var
  Found: Boolean;
begin
  Found := TryGetValue(Key, Result);

  if (not Found) or (Result.Count = 0) then
  begin
    Result := nil;
  end;
end;

end.

