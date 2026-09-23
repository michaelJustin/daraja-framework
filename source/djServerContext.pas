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

unit djServerContext;

interface



uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdCustomTCPServer;
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}

type
  {*
   * A subclass of the Indy context class is used for
   * future extensions.
   *}
  TdjServerContext = class(TIdServerContext)
  strict private
    FLastErrorStatusCode: Integer;
    FLastErrorExceptionClass: string;
    FLastErrorExceptionMessage: string;
  public
    {*
     * The HTTP status code set for the exception described by
     * LastErrorExceptionClass/LastErrorExceptionMessage.
     *
     * Only meaningful while an TdjContextHandler.ErrorHandler is being
     * invoked for the current request; not cleared afterwards, so do not
     * read it outside of that call.
     *}
    property LastErrorStatusCode: Integer
      read FLastErrorStatusCode write FLastErrorStatusCode;

    {*
     * The class name of the exception that triggered the current
     * TdjContextHandler.ErrorHandler invocation.
     *
     * @sa LastErrorStatusCode
     *}
    property LastErrorExceptionClass: string
      read FLastErrorExceptionClass write FLastErrorExceptionClass;

    {*
     * The message of the exception that triggered the current
     * TdjContextHandler.ErrorHandler invocation.
     *
     * @sa LastErrorStatusCode
     *}
    property LastErrorExceptionMessage: string
      read FLastErrorExceptionMessage write FLastErrorExceptionMessage;
  end;

  TdjServerContextClass = class of TdjServerContext;

implementation /// \cond

end. /// \endcond
