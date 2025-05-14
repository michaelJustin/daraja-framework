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

unit djInterfaces;

interface

// {$i IdCompilerDefines.inc}

uses
  djServerContext, djTypes,
  SysUtils, Generics.Collections;

type
  (**
   * Interface for components with a lifecycle (start/stop capabilities).
   * @interface ILifeCycle
   *)
  ILifeCycle = interface(IInterface)
    ['{9DE150B0-004A-4522-9581-DD47B1CFD87C}']
    (**
     * Starts the component.
     *)
    procedure Start;

    (**
     * Stops the component.
     *)
    procedure Stop;

    (**
     * @return True if component is started
     *)
    function IsStarted: boolean;

    (**
     * @return True if component is stopped
     *)
    function IsStopped: boolean;
  end;

  (**
   * Interface for HTTP request handlers.
   * @interface IHandler
   *)
  IHandler = interface(ILifeCycle)
    ['{670E1E72-7EAA-4655-B40C-DD273110B9B7}']
    (**
     * Handles an HTTP request.
     * @param Target Target path or resource identifier
     * @param Context Server context for this request
     * @param Request HTTP request object
     * @param Response HTTP response object to populate
     * @throws EWebComponentException if an exception occurs that interferes with the component's normal operation.
     *)
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse);
  end;

  (**
   * Interface for components that can contain multiple handlers.
   * @interface IHandlerContainer
   *)
  IHandlerContainer = interface(IHandler)
    ['{394BA545-CFB8-450D-8B87-E54645E25624}']
    (**
     * Adds a handler to this container.
     * @param Handler Handler to add
     *)
    procedure AddHandler(const Handler: IHandler);

    (**
     * Removes a handler from this container.
     * @param Handler Handler to remove
     *)
    procedure RemoveHandler(const Handler: IHandler);
  end;

  // forward declaration

  IContextConfig = interface;

  (**
   * Interface for server contexts that provide access to configuration and resources.
   * @interface IContext
   *)
  IContext = interface(IInterface)
    ['{19E32FEB-0348-42B2-8977-F03A0032473C}']
    (**
     * Initializes the context with the given configuration.
     *
     * @param Config The context configuration to use.
     *)
    procedure Init(const Config: IContextConfig);

    (**
     * Get the context configuration.
     * @return the context configuration
     *)
    function GetContextConfig: IContextConfig;

    (**
     * Get the context path.
     * @return the context path.
     *)
    function GetContextPath: string;

    (**
     * Gets the value of an initialization parameter.
     *
     * @param Key The parameter name.
     * @return The parameter value or empty string if not found.
     *)
    function GetInitParameter(const Key: string): string;

    (**
     * Gets all initialization parameter names.
     *
     * @return A list containing all parameter names.
     *)
    function GetInitParameterNames: TdjStrings;

    (**
     * Logs a message to the configured logging system.
     *
     * @param Msg The message to log.
     *)
     procedure Log(const Msg: string);
  end;

  (**
   * Interface for web component configuration, providing access to initialization parameters.
   * @interface IWebComponentConfig
   *)
  IWebComponentConfig = interface(IInterface)
    ['{2F61659D-1EF3-4C7A-BDEF-7349A1B4E690}']
    (**
     * @return Names of all initialization parameters
     *)
    function GetInitParameterNames: TdjStrings;

    (**
     * Gets an initialization parameter.
     * @param Key Parameter name
     * @return Parameter value or empty string if not found
     *)
    function GetInitParameter(const Key: string): string;

    (**
     * @return Context this component belongs to
     *)
    function GetContext: IContext;
  end;

  (**
   * Interface for context configuration, providing access to initialization parameters.
   * @interface IContextConfig
   *)
  IContextConfig = interface(IInterface)
    ['{5304AF56-8180-4B71-9EEF-A50CDB97E67F}']
    (**
     * @return Names of all initialization parameters
     *)
    function GetInitParameterNames: TdjStrings;

    (**
     * Gets an initialization parameter.
     * @param Key Parameter name
     * @return Parameter value or empty string if not found
     *)
    function GetInitParameter(const Key: string): string;
  end;

  (**
   * Interface for web components that process HTTP requests and generate responses.
   * @interface IWebComponent
   *)
  IWebComponent = interface(IInterface)
    ['{22F7C5D3-36AD-4BCA-BE06-E4FAA03A7A72}']
    (**
     * Called by the container on startup.
     *
     * @note if this method is overridden, the overriding code
     * must also call inherited Init.
     *
     * @param Config the configuration
     * @throws EWebComponentException if initialization failed
     *)
    procedure Init(const Config: IWebComponentConfig);

    (**
     * Handle a HTTP request.
     *
     * The status code of the response always should be set for a component
     * that throws or sends an error.
     *
     * @note a custom Web Component should not override this method.
     *
     * @param Context HTTP server context
     * @param Request HTTP request
     * @param Response HTTP response
     * @throws EWebComponentException if an exception occurs that interferes with the component's normal operation
     *)
    procedure Service(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse);

    (**
     * Returns a IWebComponentConfig object,
     * which contains initialization parameters for this component.
     *
     * @throws EWebComponentException if the method is called before
     * the component has been initialized.
     *)
    function GetWebComponentConfig: IWebComponentConfig;

    (**
     * Component configuration property.
     *)
    property Config: IWebComponentConfig read GetWebComponentConfig;
  end;

  (**
   * Interface for filter chains that allow sequential processing of requests through multiple filters.
   * @interface IWebFilterChain
   *)
  IWebFilterChain = interface(IInterface)
    ['{56337AF8-89D6-45AC-B0BD-94977BB2CE40}']
    (**
     * Executes the next filter in chain.
     * @param Context Server context
     * @param Request HTTP request
     * @param Response HTTP response to populate
     *)
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse);
  end;

  (**
   * Interface for web filter configuration, providing access to filter settings and parameters.
   * @interface IWebFilterConfig
   *)
  IWebFilterConfig = interface(IInterface)
    ['{86823762-EE7A-4523-80FE-32DD714C11DF}']
    (**
     * Retrieves the name of the filter.
     *
     * @return A string representing the name of the filter.
     *)
    function GetFilterName: string;

    (**
     * @return Names of all initialization parameters
     *)
    function GetInitParameterNames: TdjStrings;

    (**
     * Gets an initialization parameter.
     * @param Key Parameter name
     * @return Parameter value or empty string if not found
     *)
    function GetInitParameter(const Key: string): string;

    (**
     * @return Context this filter belongs to
     *)
    function GetContext: IContext;
  end;

  (**
   * Interface for web filters that can intercept and modify HTTP requests and responses.
   * @interface IWebFilter
   *)
  IWebFilter = interface(IInterface)
    ['{F1039636-3E60-48CD-BD7F-0050AB644C29}']
    (**
     * Called by the container on startup.
     *
     * @note if this method is overridden, the overriding code
     * must also call inherited Init.
     *
     * @param Config the configuration
     * @throws EWebComponentException if initialization failed
     *)
    procedure Init(const Config: IWebFilterConfig);

    (**
     * The DoFilter method of the Filter is called by the container each time
     * a request/response pair is passed through the chain due to a client request
     * for a resource at the end of the chain.
     *
     * @param Context The server context for this request
     * @param Request The HTTP request being processed
     * @param Response The HTTP response being generated
     * @param Chain The filter chain to pass the request/response to
     *)
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest;
      Response: TdjResponse; const Chain: IWebFilterChain);

    (**
     * Called when filter is being removed from service.
     * @note This method can be overridden in descendant classes to implement
     * custom cleanup logic for the filter.
     *)
    procedure DestroyFilter;
  end;

  (**
   * Interface for writable configuration objects that can be modified at runtime.
   * @interface IWriteableConfig
   *)
  IWriteableConfig = interface(IInterface)
    ['{A3074743-C2EF-44C6-BD28-27E62F82E598}']
    (**
     * Adds a key-value pair to configuration.
     * @param Key Parameter name
     * @param Value Parameter value
     *)
    procedure Add(const Key: string; const Value: string);

    (**
     * Sets the context for this configuration.
     * @param Context Context to set
     * @throws EWebComponentException if the context is nil or if the context is already set and differs from the new context.
     *)
    procedure SetContext(const Context: IContext);
  end;

  {$IFNDEF DOXYGEN_SKIP}
  // todo move
  TdjHandlers = TList<IHandler>;
  {$ENDIF DOXYGEN_SKIP}

implementation

end.
