{***

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

***}

unit djWebComponentMapping;

interface



uses
  Classes,
  Generics.Collections;

type
  { TdjWebComponentMapping }

  {*
   * Web Component Mapping.
   *}
  TdjWebComponentMapping = class(TObject)
  private
    FName: string;
    FUrlPatterns: TStrings;
  public
    {*
     * Constructor.
     *}
    constructor Create;
    {*
     * Destructor.
     *}
    destructor Destroy; override;

    // properties
    property WebComponentName: string read FName write FName;
    property UrlPatterns: TStrings read FUrlPatterns;
  end;

  {*
   * Web Component Mappings.
   *}
  TdjWebComponentMappings = class(TObjectList<TdjWebComponentMapping>)
  public
    {*
     * Creates an owning list. Passes an explicit comparer to the base
     * constructor: on Delphi 2009 TObjectList&lt;T&gt;.Remove raises an AV
     * with the implicitly created one.
     *}
    constructor Create;
  end;

implementation /// \cond

uses
  Generics.Defaults;

{ TdjWebComponentMappings }

constructor TdjWebComponentMappings.Create;
begin
  inherited Create(TComparer<TdjWebComponentMapping>.Default);
end;

{ TdjWebComponentMapping }

constructor TdjWebComponentMapping.Create;
begin
  FUrlPatterns := TStringList.Create;
end;

destructor TdjWebComponentMapping.Destroy;
begin
  FUrlPatterns.Free;

  inherited;
end;

end. /// \endcond

