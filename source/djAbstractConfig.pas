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
  djInterfaces, djInitParameters;

type
  { TdjAbstractConfig }

  (**
   * Generic configuration class for managing initialization parameters and context.
   * This class implements the IWriteableConfig interface and provides methods
   * to add parameters, retrieve them, and manage the application context.
   *)
  TdjAbstractConfig = class(TInterfacedObject, IWriteableConfig)
  private
    (**
     * Initialization parameters.
     *)
    FParams: TdjInitParameters;

    (**
     * The context.
     *)
    FContext: IContext;

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
     * Add a configuration parameter.
     *
     * @param Key The key of the parameter to add.
     * @param Value The value of the parameter to add.
     * @throws EWebComponentException if the key already exists in the configuration.
     *)
    procedure Add(const Key: string; const Value: string);

    (**
     * Set the context.
     *
     * @param Context The context to set.
     * @throws EWebComponentException if the context is nil or if the context is already set and differs from the new context.
     *)
    procedure SetContext(const Context: IContext);

    // IConfig interface

    (**
     * Get the value of an initialization parameter.
     *
     * @param Key The key of the parameter to retrieve.
     * @return The value of the parameter, or an empty string if the key does not exist.
     *)
    function GetInitParameter(const Key: string): string;

    (**
     * Get the names of all initialization parameters.
     *
     * @return A list of all parameter keys as a TdjStrings object.
     *)
    function GetInitParameterNames: TdjStrings;

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

