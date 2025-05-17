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

unit djPathMap;

interface

// {$i IdCompilerDefines.inc}

uses
  Classes;

type
  (**
   * @enum TSpecType
   * Declares the path specification types.
   *)
  TSpecType = (stUnknown,  //!< Unknown type
               stExact,    //!< Exact match
               stPrefix,   //!< Longest prefix match
               stSuffix,   //!< Longest suffix match
               stDefault   //!< Default
              );

  { TdjPathMap }

  (**
   * Holds all known web component mappings for a context.
   *
   * Matching is performed in the following order
   * Exact match
   * Longest prefix match
   * Longest suffix match
   *)
  TdjPathMap = class(TStringList)
  protected
    (**
     * Matches a given URL document path against a specified path pattern.
     * @param Path the URL document path
     * @param Spec the path specification
     * @param SpecType the path specification type
     * @return True if the Path matches the Spec (with known SpecType)
     *)
    class function Matches(const Path, Spec: string; SpecType: TSpecType): Boolean; overload;

  public
    (**
     * @param Spec the path specification
     * @return the path specification type
     *)
    class function GetSpecType(const Spec: string): TSpecType;

    (**
     * Matches the given path against a specified pattern.
     *
     * @param Path The file or directory path to be checked.
     * @param Spec The pattern or specification to match against.
     * @return True if the path matches the specification, otherwise False.
     *)
    class function Matches(const Path, Spec: string): Boolean; overload;

    (**
     * Check if a mapping path exists.
     * This procedure throws a EWebComponentException if the URL pattern is already registered for this context.
     *
     * @param UrlPattern a single component mapping path
     * @throws EWebComponentException
     *)
    procedure CheckExists(const UrlPattern: string);

    (**
     * Add a web component mapping.
     *
     * @param UrlPattern a single component mapping path
     * @param Value the mapped web component
     * @throws EWebComponentException
     *)
    procedure AddUrlPattern(const UrlPattern: string; Value: TObject); overload;

    (**
     * Return all matching mappings for the given path.
     * The best match will be the first entry.
     *
     * @param Path the URL path (without context
     * @result list of matching mappings
     *)
    function GetMatches(const Path: string): TStrings;
  end;

{$IFNDEF DOXYGEN_SKIP}

implementation

uses
  djTypes,
  SysUtils;

{ TdjPathMap }

procedure TdjPathMap.AddUrlPattern(const UrlPattern: string; Value: TObject);
begin
  CheckExists(UrlPattern);

  AddObject(UrlPattern, Value);
end;

procedure TdjPathMap.CheckExists(const UrlPattern: string);
begin
  if IndexOf(UrlPattern) > -1 then
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

{$ENDIF DOXYGEN_SKIP}

end.

