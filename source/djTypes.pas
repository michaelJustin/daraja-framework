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

unit djTypes;

interface



uses
  IdCustomHTTPServer,
  SysUtils, Generics.Collections;

type
  {*
   * @class TdjRequest
   *
   * HTTP request information.
   * Type alias for Indy class TIdHTTPRequestInfo.
   *}
  TdjRequest = TIdHTTPRequestInfo;

  {*
   * @class TdjResponse
   *
   * HTTP response information.
   * Type alias for Indy class TIdHTTPResponseInfo.
   *}
  TdjResponse = TIdHTTPResponseInfo;

  {*
   * @class EDarajaException
   *
   * Common base class for all exceptions raised by the Daraja framework.
   *}
  EDarajaException = class(Exception);

  {*
   * @class EDarajaConfigException
   *
   * Raised for invalid or inconsistent configuration, e.g. a context that
   * is nil, already set, or a static resource path that does not exist.
   *}
  EDarajaConfigException = class(EDarajaException);

  {*
   * @class EDarajaMappingException
   *
   * Raised for invalid or conflicting URL pattern / component / filter /
   * connector registrations, and for lookups that fail to find a match.
   *}
  EDarajaMappingException = class(EDarajaException);

  {*
   * @class EDarajaLifecycleException
   *
   * Raised when a component or filter is used before it has been
   * initialized, or otherwise used outside of its expected lifecycle state.
   *}
  EDarajaLifecycleException = class(EDarajaException);

  {*
   * @class TdjStrings
   *
   * This class holds a list of strings.
   *}
  TdjStrings = TList<string>;

  {*
   * @class TdjStringArray
   *
   * A plain dynamic array of strings, returned by value.
   *}
  TdjStringArray = array of string;

implementation /// \cond

end. /// \endcond

