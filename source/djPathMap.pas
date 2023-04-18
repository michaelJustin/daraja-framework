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

unit djPathMap;

interface

{$i IdCompilerDefines.inc}

uses
  Classes;

type
  TSpecType = (stUnknown, stExact, stPrefix, stSuffix, stDefault);

  (**
   * Holds all known web component mappings for a context.
   *
   * Matching is performed in the following order
   * Exact match
   * Longest prefix match
   * Longest suffix match
   *)

  { TdjPathMap }

  TdjPathMap = class(TStringList)
  protected
    (**
     *
     * \param Path the URL document path
     * \param Spec the path specification (for example, '/*')
     * \param SpecType the path specification type
     * \return True if the Path mathes the Spec (with known SpecType)
     *)
    class function Matches(const Path, Spec: string; SpecType: TSpecType): Boolean; overload;

  public
    (**
     * \param Spec the path specification (for example, '/*')
     * \return the path specification type
     *)
    class function GetSpecType(const Spec: string): TSpecType;

    class function Matches(const Path, Spec: string): Boolean; overload;

    (**
     * Check if a mapping path exists.
     * This procedure throws a EWebComponentException if the PathSpec is already registered for this context.
     *
     * \param PathSpec a single component mapping path (for example, '*.html' or '/*')
     * \throws EWebComponentException
     *)
    procedure CheckExists(const PathSpec: string);

    (**
     * Add a web component mapping.
     *
     * \param PathSpec a single component mapping path (for example, '*.html' or '/*')
     * \param Value the mapped web component
     * \throws EWebComponentException
     *)
    procedure AddPathSpec(const PathSpec: string; Value: TObject); overload;

    (**
     * Return all matching mappings for the given path.
     * The best match will be the first entry.
     *
     * \param Path the URL path (without context), for example 'test.html'
     * \result list of matching mappings
     *)
    function GetMatches(const Path: string): TStrings;
  end;

implementation

uses
  djInterfaces,
  SysUtils;

{ TdjPathMap }

procedure TdjPathMap.AddPathSpec(const PathSpec: string; Value: TObject);
begin
  CheckExists(PathSpec);

  AddObject(PathSpec, Value);
end;

procedure TdjPathMap.CheckExists(const PathSpec: string);
begin
  if IndexOf(PathSpec) > -1 then
  begin
    raise EWebComponentException.Create('Mapping key exists');
  end;
end;

class function TdjPathMap.Matches(const Path, Spec: string): Boolean;
begin
  Result := Matches(Path, Spec, GetSpecType(Spec));
end;

class function TdjPathMap.GetSpecType(const Spec: string): TSpecType;
begin
  if (Pos('/', Spec) = 1) and (Pos('/*', Spec) = Length(Spec) - 1)
    and (Length(Spec) > 1) then
  begin
    Result := stPrefix;
  end
  else if (Pos('*.', Spec) = 1) and (Pos('/', Spec) = 0) then
  begin
    Result := stSuffix;
  end
  else if (Spec = '/') then
  begin
    Result := stDefault
  end
  else if (Pos('/', Spec) = 1) and (Pos('*', Spec) = 0) then
  begin
    Result := stExact;
  end
  else
  begin
    Result := stUnknown;
  end;
end;

class function TdjPathMap.Matches(const Path: string; const Spec: string;
  SpecType: TSpecType): Boolean;
var
  Tmp: string;
begin
  case
    SpecType of
    stPrefix:
      begin
        Tmp := StringReplace(Spec, '/*', '/', []);
        Result := Pos(Tmp, Path) = 1;
      end;
    stSuffix:
      begin
        Result := '*' + ExtractFileExt(Path) = Spec
      end;
    stExact:
      begin
        Result := Path = Spec;
      end;
    stDefault:
      begin
        Result := True;
      end
  else
    raise Exception.CreateFmt('Unknown match %s %s', [Path, Spec]);
  end;
end;

function TdjPathMap.GetMatches(const Path: string): TStrings;

  procedure FindExactMatch;
  var
    I: Integer;
    Spec: string;
    SpecType: TSpecType;
  begin
    for I := 0 to Count - 1 do
    begin
      Spec := Strings[I];
      SpecType := GetSpecType(Spec);
      if SpecType = stExact then
      begin
        if Matches(Path, Spec, SpecType) then
        begin
          Result.AddObject(Spec, Objects[I]);
          Break;
        end;
      end;
    end;
  end;

  procedure AddPrefixMatch;
  var
    I: Integer;
    Spec: string;
    SpecType: TSpecType;
  begin
    for I := 0 to Count - 1 do
    begin
      Spec := Strings[I];
      SpecType := GetSpecType(Spec);
      if SpecType = stPrefix then
      begin
        if Matches(Path, Spec, SpecType) then
        begin
          Result.InsertObject(0, Spec, Objects[I]);
        end;
      end;
    end;
  end;

  procedure AddSuffixMatch;
  var
    I: Integer;
    Spec: string;
    SpecType: TSpecType;
  begin
    for I := 0 to Count - 1 do
    begin
      Spec := Strings[I];
      SpecType := GetSpecType(Spec);
      if SpecType = stSuffix then
      begin
        if Matches(Path, Spec, SpecType) then
        begin
          Result.InsertObject(0, Spec, Objects[I]);
        end;
      end;
    end;
  end;

  procedure AddDefaultMatch;
  var
    I: Integer;
    Spec: string;
    SpecType: TSpecType;
  begin
    for I := 0 to Count - 1 do
    begin
      Spec := Strings[I];
      SpecType := GetSpecType(Spec);
      if SpecType = stDefault then
      begin
        if Matches(Path, Spec, SpecType) then
        begin
          Result.AddObject(Spec, Objects[I]);
          Break;
        end;
      end;
    end;
  end;

begin
  Self.Sorted := True; // ascending order to have longest matches first

  Result := TStringList.Create;

  {
   Matching is performed in the following order
   Exact match.
   Longest prefix match.
   Longest suffix match.
   default. }

  FindExactMatch;
  if Result.Count = 0 then
  begin
    AddPrefixMatch;
  end;
  if Result.Count = 0 then
  begin
    AddSuffixMatch;
  end;
  if Result.Count = 0 then
  begin
    AddDefaultMatch;
  end;
end;

end.

