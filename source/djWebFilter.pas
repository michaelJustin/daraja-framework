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

unit djWebFilter;

interface



uses
  djGenericWebFilter
  {$IFDEF DARAJA_LOGGING}
  , djLogAPI, djLoggerFactory
  {$ENDIF DARAJA_LOGGING}
  ;

type
  {*
   * A base class which can be subclassed to create a HTTP filter component
   * for a Web site.
   *
   * @note Threading: as with TdjWebComponent, the framework creates exactly
   * one instance of each registered filter, and every concurrent request
   * runs through that same instance's DoFilter method on its own thread. Do
   * not keep per-request state in instance fields — two requests handled at
   * the same time will read and write the same field, corrupting each
   * other's data. Use local variables for anything request-specific, and if
   * state truly must be shared across requests, synchronize access to it
   * explicitly (e.g. with a critical section).
   *
   * @note URL patterns are not authorization. A filter mapped to a prefix
   * or suffix pattern (e.g. everything under '/admin/') is a routing rule,
   * matched after the framework normalizes the request path (collapsing
   * '.', '..' and repeated '/', and stripping ';'-parameters) -- but it says
   * nothing about who the caller is. An authentication/authorization filter
   * must still check the caller's identity itself (e.g. the session) before
   * allowing a request through, rather than relying on the URL pattern alone to gate
   * access.
   *}
  TdjWebFilter = class(TdjGenericWebFilter)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
  public
    constructor Create;
  end;

  {*
   * Class reference to TdjWebFilter
   *}
  TdjWebFilterClass = class of TdjWebFilter;

implementation /// \cond

{ TdjWebFilter }

constructor TdjWebFilter.Create;
begin
  inherited Create;

  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjWebFilter);
  {$ENDIF DARAJA_LOGGING}
end;

end. /// \endcond

