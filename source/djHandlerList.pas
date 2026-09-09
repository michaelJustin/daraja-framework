{***

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

***}

unit djHandlerList;

interface



uses
  djServerContext, djHandlerCollection,
  {$IFDEF DARAJA_LOGGING}
  djLogAPI, djLoggerFactory,
  {$ENDIF DARAJA_LOGGING}
  djTypes;

type
  { TdjHandlerList }

  {*
   * Iterates handler list and exits when the response code is set.
   * If the response code is still -1, it returns 404
   *
   * This extension of TdjHandlerCollection will call
   * each contained handler in turn until either an
   * exception is thrown or a positive response status is set.
   *}
  TdjHandlerList = class(TdjHandlerCollection)
  strict private
    {$IFDEF DARAJA_LOGGING}
    Logger: ILogger;
    {$ENDIF DARAJA_LOGGING}
  protected
    // IHandler interface
    procedure Handle(const Target: string; Context: TdjServerContext;
      Request: TdjRequest; Response: TdjResponse); override;
  public
    {*
     * Constructor.
     *}
    constructor Create; override;
  end;

implementation /// \cond

uses
  djInterfaces, djGlobal,
  SysUtils;

{ TdjHandlerList }

constructor TdjHandlerList.Create;
begin
  inherited Create;

  // logging -----------------------------------------------------------------
  {$IFDEF DARAJA_LOGGING}
  Logger := TdjLoggerFactory.GetLogger(TdjHandlerList);
  {$ENDIF DARAJA_LOGGING}
end;

procedure TdjHandlerList.Handle(const Target: string; Context: TdjServerContext;
  Request: TdjRequest; Response: TdjResponse);
var
  H: IHandler;
begin
  {$IFDEF DARAJA_LOGGING}
  Logger.Trace('Handle %s', [Target]);
  {$ENDIF DARAJA_LOGGING}

  for H in FHandlers do
  begin
    H.Handle(Target, Context, Request, Response);

    if (Response.ResponseNo > 0) then
    begin
      {$IFDEF DARAJA_LOGGING}
      Logger.Trace('Handled %s', [Target]);
      {$ENDIF DARAJA_LOGGING}

      Break;
    end;
  end;

  // 404 if no context matches
  if Response.ResponseNo < 0 then
  begin
    {$IFDEF DARAJA_LOGGING}
    Logger.Trace('Not handled %s. Set ResponseNo to 404', [Target]);
    {$ENDIF DARAJA_LOGGING}

    Response.ResponseNo := 404;
    Response.ContentType := 'text/html';
    Response.ContentText :=
      '<!DOCTYPE html>' + #10
      + '<html>' + #10
      + '<head><title>404 Not Found</title></head>' + #10
      + '<body>' + #10
      + '  <h1>404 Not Found</h1>' + #10
      + '  <p>No resource is mapped to ' + HTMLEncode(Target) + '</p>' + #10
      + '</body>' + #10
      + '</html>';
  end;
end;

end. /// \endcond
