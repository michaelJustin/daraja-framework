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

unit djWebFilterTests;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  {$IFDEF FPC}fpcunit,testregistry{$ELSE}TestFramework{$ENDIF};

type
  TdjWebFilterTests = class(TTestCase)
  published
     procedure TestCreate;

  end;

implementation

uses
  djWebFilter, djWebAppContext, djServerContext, djTypes, djInterfaces;

type

  { TTestFilter }

  TTestFilter = class(TdjWebFilter)
  public
    procedure {%H-}DoFilter({%H-}Context: TdjServerContext; {%H-}Request: TdjRequest;
      {%H-}Response: TdjResponse; const {%H-}Chain: IWebFilterChain); override;
  end;

{ TTestFilter }

procedure TTestFilter.DoFilter(Context: TdjServerContext; Request: TdjRequest;
  Response: TdjResponse; const Chain: IWebFilterChain);
begin
   //
end;

procedure TdjWebFilterTests.TestCreate;
var
  Context: TdjWebAppContext;
  // Filter: IWebFilter;
begin
  Context := TdjWebAppContext.Create('x-ctx');
  try
    (* Filter := *) TTestFilter.Create;
  finally
    Context.Free;
  end;
end;

end.

