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

unit djWebFilterMappingTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjWebFilterMappingTests }

  TdjWebFilterMappingTests = class(TTestCase)
  published
    procedure TestAppliesToWithNoPatterns;
    procedure TestAppliesToExactPattern;
    procedure TestAppliesToPrefixPattern;
    procedure TestAppliesToSuffixPattern;
    procedure TestAppliesToDefaultPattern;
    procedure TestAppliesToNoMatchingPattern;
    procedure TestAppliesToFirstOfSeveralPatterns;
    procedure TestAppliesToSecondOfSeveralPatterns;
  end;

implementation

uses
  djWebFilterMapping;

{ TdjWebFilterMappingTests }

procedure TdjWebFilterMappingTests.TestAppliesToWithNoPatterns;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    CheckFalse(Mapping.AppliesTo('/anything'),
      'a mapping with no URL patterns must never apply');
  finally
    Mapping.Free;
  end;
end;

procedure TdjWebFilterMappingTests.TestAppliesToExactPattern;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    Mapping.UrlPatterns.Add('/admin/index.html');

    CheckTrue(Mapping.AppliesTo('/admin/index.html'), 'exact match');
    CheckFalse(Mapping.AppliesTo('/admin/other.html'), 'different exact path');
  finally
    Mapping.Free;
  end;
end;

procedure TdjWebFilterMappingTests.TestAppliesToPrefixPattern;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    Mapping.UrlPatterns.Add('/admin/*');

    CheckTrue(Mapping.AppliesTo('/admin/'), 'bare prefix with trailing slash');
    CheckTrue(Mapping.AppliesTo('/admin'), 'bare prefix without trailing slash');
    CheckTrue(Mapping.AppliesTo('/admin/reports'), 'path below the prefix');
    CheckFalse(Mapping.AppliesTo('/other'), 'path outside the prefix');
  finally
    Mapping.Free;
  end;
end;

procedure TdjWebFilterMappingTests.TestAppliesToSuffixPattern;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    Mapping.UrlPatterns.Add('*.html');

    CheckTrue(Mapping.AppliesTo('/page.html'), 'matching suffix');
    CheckFalse(Mapping.AppliesTo('/page.txt'), 'different suffix');
  finally
    Mapping.Free;
  end;
end;

procedure TdjWebFilterMappingTests.TestAppliesToDefaultPattern;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    Mapping.UrlPatterns.Add('/');

    CheckTrue(Mapping.AppliesTo('/'), 'the default pattern catches the root');
    CheckTrue(Mapping.AppliesTo('/anything/at/all'),
      'the default pattern catches every path');
  finally
    Mapping.Free;
  end;
end;

procedure TdjWebFilterMappingTests.TestAppliesToNoMatchingPattern;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    Mapping.UrlPatterns.Add('/admin/*');
    Mapping.UrlPatterns.Add('*.html');

    CheckFalse(Mapping.AppliesTo('/other/data.txt'),
      'a path matching neither registered pattern must not apply');
  finally
    Mapping.Free;
  end;
end;

procedure TdjWebFilterMappingTests.TestAppliesToFirstOfSeveralPatterns;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    Mapping.UrlPatterns.Add('/admin/*');
    Mapping.UrlPatterns.Add('*.html');

    CheckTrue(Mapping.AppliesTo('/admin/reports'),
      'must match on the first pattern without needing to check the rest');
  finally
    Mapping.Free;
  end;
end;

procedure TdjWebFilterMappingTests.TestAppliesToSecondOfSeveralPatterns;
var
  Mapping: TdjWebFilterMapping;
begin
  Mapping := TdjWebFilterMapping.Create;
  try
    Mapping.UrlPatterns.Add('/admin/*');
    Mapping.UrlPatterns.Add('*.html');

    CheckTrue(Mapping.AppliesTo('/public/page.html'),
      'must fall through to a later pattern when earlier ones do not match');
  finally
    Mapping.Free;
  end;
end;

end.
