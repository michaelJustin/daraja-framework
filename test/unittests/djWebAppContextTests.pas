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

unit djWebAppContextTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type

  { TdjWebAppContextTests }

  TdjWebAppContextTests = class(TTestCase)
  published
    procedure TestTwoEqualComponentsSucceeds;

    procedure Test_Add_ClassWithSameUrlPatternTwice_RaisesException;

    // todo: Test AddHandler / RemoveHandler

  end;

implementation

uses
  djWebAppContext, djTypes, djWebComponent,
  SysUtils, Classes;

type
  TExamplePage = class(TdjWebComponent)
  public
    procedure OnGet({%H-}Request: TdjRequest; Response: TdjResponse);
      override;
  end;

{ TExamplePage }

procedure TExamplePage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText := 'example';
end;

{ TdjWebAppContextTests }

procedure TdjWebAppContextTests.TestTwoEqualComponentsSucceeds;
var
  Context: TdjWebAppContext;
begin
  Context := TdjWebAppContext.Create('');
  try
    Context.Add(TExamplePage, '/a');
    Context.Add(TExamplePage, '/b');
  finally
    Context.Free;
  end;
end;

procedure TdjWebAppContextTests.Test_Add_ClassWithSameUrlPatternTwice_RaisesException;
var
  Context: TdjWebAppContext;
begin
  Context := TdjWebAppContext.Create('foo');

  try
    Context.Add(TExamplePage, '/qux');

    {$IFDEF FPC}
    ExpectException(EWebComponentException, 'Web Component TExamplePage is already installed in context foo with URL pattern /qux');
    {$ELSE}
    ExpectedException := EWebComponentException;
    {$ENDIF}

    // same path -> error
    Context.Add(TExamplePage, '/qux');

  finally
    Context.Free;
  end;
end;

end.

