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

unit djHTTPConstants;

interface

const
  HTTP_OK = 200;
  HTTP_BAD_REQUEST = 400;
  HTTP_INTERNAL_SERVER_ERROR = 500;

  {*
   * Sentinel ResponseNo meaning "no handler has produced a response yet".
   *
   * This is not an Indy value: TdjHTTPConnector.OnCommand sets
   * Response.ResponseNo to it before dispatching a request, so that
   * TdjHandlerList and TdjDefaultHandler can tell whether an earlier handler
   * already handled the request.
   *}
  HTTP_NOT_HANDLED = -1;

implementation /// \cond

end. /// \endcond
