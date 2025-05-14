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

unit djAbstractConfig;

interface

// {$i IdCompilerDefines.inc}

uses
  djInterfaces, djTypes, djInitParameters;

type
  { TdjAbstractConfig }

  (**
   * Generic configuration class for managing initialization parameters and context.
   * This class implements the IWriteableConfig interface and provides methods
   * to add parameters, retrieve them, and manage the application context.
   * @implements IWriteableConfig
   *)
  TdjAbstractConfig = class(TInterfacedObject, IWriteableConfig, IContextConfig)
  private
    (**
     * Initialization parameters.
     *)
    FParams: TdjInitParameters;
    (**
     * The context.
     *)
    FContext: IContext;
  protected
    // IWriteableConfig interface
    procedure Add(const Key: string; const Value: string);
    procedure SetContext(const Context: IContext);
  protected
    // IContextConfig interface todo more generic.
    function GetInitParameter(const Key: string): string;
    function GetInitParameterNames: TdjStrings;
  public
    (**
     * Constructor.
     * Initializes the configuration object and allocates resources for parameters.
     *)
    constructor Create;

    (**
     * Destructor.
     * Frees allocated resources and cleans up the configuration object.
     *)
    destructor Destroy; override;

    (**
     * Get the current context.
     *
     * @return The context associated with this configuration.
     *)
    function GetContext: IContext;

  end;

implementation

{ TdjAbstractConfig }

constructor TdjAbstractConfig.Create;
begin
  inherited;

  FParams := TdjInitParameters.Create;
end;

destructor TdjAbstractConfig.Destroy;
begin
  FParams.Free;

  inherited;
end;

function TdjAbstractConfig.GetContext: IContext;
begin
  Result := FContext;
end;

procedure TdjAbstractConfig.Add(const Key: string; const Value: string);
begin
  if FParams.ContainsKey(Key) then
    raise EWebComponentException.
      CreateFmt('Duplicate key %s in configuration', [Key]);

  FParams.Add(Key, Value);
end;

function TdjAbstractConfig.GetInitParameter(const Key: string): string;
begin
  FParams.TryGetValue(Key, Result);
end;

function TdjAbstractConfig.GetInitParameterNames: TdjStrings;
var
  S: string;
begin
  Result := TdjStrings.Create;

  for S in FParams.Keys do
  begin
    Result.Add(S);
  end;
end;

procedure TdjAbstractConfig.SetContext(const Context: IContext);
begin
  if Context = nil then
    raise EWebComponentException.Create('Context can not be set to nil');

  if (FContext <> nil) and (Context <> FContext) then
    raise EWebComponentException.Create('Context must not be changed');

  FContext := Context;
end;

end.

