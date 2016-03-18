(*
    Daraja Framework
    Copyright (C) 2016 Michael Justin

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

unit djPathMapTests;

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjPathMapTests }

  TdjPathMapTests = class(TTestCase)
  published
    procedure TestGetSpecType;

    procedure TestPathSpec;

    procedure TestMorePathSpec;

    procedure TestDirPathSpec;
  end;

implementation

uses
  djInterfaces, djPathMap, djWebComponentHandler, djWebAppContext, djServer,
  SysUtils, Classes;

type
  TTestPathMap = class(TdjPathMap)
  public
    class function GetSpecType(Spec: string): TSpecType;
  end;

{ TTestPathMap }

class function TTestPathMap.GetSpecType(Spec: string): TSpecType;
begin
  Result := inherited;
end;

{ TdjPathMapTests }

procedure TdjPathMapTests.TestGetSpecType;
begin
  CheckTrue(stDefault = TTestPathMap.GetSpecType('/'), '/');

  CheckTrue(stExact = TTestPathMap.GetSpecType('/index.html'), '/index.html');

  CheckTrue(stExact = TTestPathMap.GetSpecType('/dir/index.html'),
    '/dir/index.html');

  CheckTrue(stSuffix = TTestPathMap.GetSpecType('*.html'), '*.html');

  CheckTrue(stUnknown = TTestPathMap.GetSpecType('*.html/a'), '*.html/a');

  CheckTrue(stPrefix = TTestPathMap.GetSpecType('/*'), '/*');

  CheckTrue(stUnknown = TTestPathMap.GetSpecType('/**'), '/**');

  CheckTrue(stUnknown = TTestPathMap.GetSpecType('/*.html'), '/*.html');
end;

procedure TdjPathMapTests.TestPathSpec;
var
  PS: TdjPathMap;

  MatchList: TStrings;
begin
  PS := TdjPathMap.Create;

  try
    (*
    Matching is performed in the following order
    Exact match.
    Longest prefix match.
    Longest suffix match.
    default.
    *)

    PS.AddPathSpec('/', nil);
    PS.AddPathSpec('/foo/*', nil);
    PS.AddPathSpec('/prefix/*', nil);
    PS.AddPathSpec('/prefix/more/*', nil);
    PS.AddPathSpec('/absolute.html', nil);
    PS.AddPathSpec('/absolute2.html', nil);
    PS.AddPathSpec('*.suf', nil);
    PS.AddPathSpec('*.suffix', nil);

    MatchList := PS.GetMatches('/prefix/absolute.html');
    try
      CheckEquals('/prefix/*', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

    MatchList := PS.GetMatches('/foobar');
    try
      CheckEquals('/', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

    MatchList := PS.GetMatches('/absolute.html');
    try
      CheckEquals('/absolute.html', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

    MatchList := PS.GetMatches('/absolute.suf');
    try
      CheckEquals('*.suf', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

    MatchList := PS.GetMatches('/absolute.suffix');
    try
      CheckEquals('*.suffix', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

    MatchList := PS.GetMatches('/prefix/more/absolute.html');
    try
      CheckEquals('/prefix/more/*', MatchList[0]);
      CheckEquals('/prefix/*', MatchList[1]);
    finally
      MatchList.Free;
    end;

    MatchList := PS.GetMatches('/nomatch');
    try
      CheckEquals('/', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

  finally
    PS.Free;
  end;

end;

procedure TdjPathMapTests.TestMorePathSpec;
var
  PS: TdjPathMap;
  MatchList: TStrings;
begin
  PS := TdjPathMap.Create;
  try
     PS.AddPathSpec('/*', nil);

     MatchList := PS.GetMatches('/something');
    try
      CheckEquals('/*', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;
  finally
    PS.Free;
  end;
end;

procedure TdjPathMapTests.TestDirPathSpec;
var
  PS: TdjPathMap;
  MatchList: TStrings;
begin
  PS := TdjPathMap.Create;
  try
     PS.AddPathSpec('/*', nil);

     MatchList := PS.GetMatches('/dir');
    try
      CheckEquals('/*', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

     MatchList := PS.GetMatches('/dir/');
    try
      CheckEquals('/*', Trim(MatchList.Text));
    finally
      MatchList.Free;
    end;

  finally
    PS.Free;
  end;
end;

end.

