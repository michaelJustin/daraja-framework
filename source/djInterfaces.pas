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

unit djInterfaces;

interface

{$i IdCompilerDefines.inc}

uses
  djServerContext, djTypes,
  SysUtils, Generics.Collections;

type
  (**
   * Base exception.
   *)
  EWebComponentException = class(Exception);

  (**
   * \interface ILifeCycle
   *
   * ILifeCycle interface.
   *)
  ILifeCycle = interface
    ['{9DE150B0-004A-4522-9581-DD47B1CFD87C}']
    procedure Start;
    procedure Stop;

    function IsStarted: Boolean;
    function IsStopped: Boolean;
  end;

  (**
   * \interface IHandler
   *
   * IHandler interface.
   *)
  IHandler = interface(ILifeCycle)
    ['{670E1E72-7EAA-4655-B40C-DD273110B9B7}']
    procedure Handle(const Target: string; Context: TdjServerContext; Request:
      TdjRequest; Response: TdjResponse);
  end;

  TdjHandlers = TList<IHandler>;

  (**
   * \interface IHandlerContainer
   *
   * IHandlerContainer interface.
   *)
  IHandlerContainer = interface(IHandler)
    ['{394BA545-CFB8-450D-8B87-E54645E25624}']
    procedure AddHandler(const Handler: IHandler);
    procedure RemoveHandler(const Handler: IHandler);
  end;

  // forward declaration

  IContextConfig = interface;

  TdjStrings = TList<string>;

  (**
   * \interface IContext
   *
   * Context interface.
   *)
  IContext = interface
    ['{19E32FEB-0348-42B2-8977-F03A0032473C}']

    procedure Init(const Config: IContextConfig);
    function GetContextConfig: IContextConfig;

    function GetContextPath: string;

    function GetInitParameter(const Key: string): string;
    function GetInitParameterNames: TdjStrings;

    procedure Log(const Msg: string);
  end;

  (**
   * \interface IWebComponentConfig
   *
   * Web Component configuration interface.
   *)
  IWebComponentConfig = interface
    ['{2F61659D-1EF3-4C7A-BDEF-7349A1B4E690}']
    function GetInitParameterNames: TdjStrings;
    function GetInitParameter(const Key: string): string;
    function GetContext: IContext;
  end;

  (**
   * \interface IContextConfig
   *
   * Context configuration interface.
   *)
  IContextConfig = interface
    ['{5304AF56-8180-4B71-9EEF-A50CDB97E67F}']
    function GetInitParameterNames: TdjStrings;
    function GetInitParameter(const Key: string): string;
  end;

  (**
   * \interface IWebComponent
   *
   * Web Component interface.
   *)
  IWebComponent = interface
    ['{22F7C5D3-36AD-4BCA-BE06-E4FAA03A7A72}']
    procedure Init(const Config: IWebComponentConfig);
    procedure Service(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse);
    function GetWebComponentConfig: IWebComponentConfig;

    property Config: IWebComponentConfig read GetWebComponentConfig;
  end;

  (**
   * \interface IWebFilterChain
   *
   * Web Filter Chain interface.
   *)
  IWebFilterChain = interface
    ['{56337AF8-89D6-45AC-B0BD-94977BB2CE40}']
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse);
  end;

  (**
   * \interface IWebFilterConfig
   *
   * Web Filter configuration interface.
   *)
  IWebFilterConfig = interface
    ['{86823762-EE7A-4523-80FE-32DD714C11DF}']
    function GetFilterName: string;
    function GetInitParameterNames: TdjStrings;
    function GetInitParameter(const Key: string): string;
    function GetContext: IContext;
  end;

  (**
   * \interface IWebFilter
   *
   * Web Filter interface.
   * A filter is an object that performs filtering tasks on either the request
   * to a resource (a servlet or static content), or on the response from a resource, or both.
   *)
  IWebFilter = interface
    ['{F1039636-3E60-48CD-BD7F-0050AB644C29}']
    procedure Init(const Config: IWebFilterConfig);
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain);
    // Called by the container to indicate to a filter that it is being taken out of service.
    procedure DestroyFilter;
  end;

  IWriteableConfig = interface
    ['{A3074743-C2EF-44C6-BD28-27E62F82E598}']
    procedure Add(const Key: string; const Value: string);
    procedure SetContext(const Context: IContext);
  end;


implementation

end.

