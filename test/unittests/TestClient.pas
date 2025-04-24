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

unit TestClient;

interface

uses
  {$IFDEF FPC}{$NOTES OFF}{$ENDIF}{$HINTS OFF}{$WARNINGS OFF}
  IdHTTP, IdGlobal,
  {$IFDEF FPC}{$ELSE}{$HINTS ON}{$WARNINGS ON}{$ENDIF}
  SysUtils;

type
  TdjHTTPClient = class(TIdHTTP)
  public
    {$IFDEF FPC}
    class function Get(Document: string; Host: string = 'http://127.0.0.1'; ADestEncoding: IIdTextEncoding = nil):
      string; overload;
    {$ELSE}
    class function Get(Document: string; Host: string = 'http://127.0.0.1'):
      string; overload;
    {$ENDIF}

    class function Get(URL: string; AIgnoreReplies: array of SmallInt): string;
      overload;

    class function GetStatus(Document: string; Host: string =
      'http://127.0.0.1'): Integer;

    class function GetHeader(HeaderKey: string; Document: string; Host: string =
      'http://127.0.0.1'): string;

  end;

implementation

{ TdjHTTPClient }

class function TdjHTTPClient.GetHeader(HeaderKey: string; Document: string;
  Host: string): string;
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create;
  try
    Result := HTTP.Get(Host + Document);
    Result := HTTP.Response.RawHeaders.Values[HeaderKey];
    Result := HTTP.Response.Server;
  finally
    HTTP.Free;
  end;
end;

{$IFDEF FPC}
class function TdjHTTPClient.Get(Document: string; Host: string = 'http://127.0.0.1'; ADestEncoding: IIdTextEncoding = nil): string;
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create;
  try
    Result := HTTP.Get(Host + Document, ADestEncoding);

  finally
    HTTP.Free;
  end;
end;
{$ELSE}
class function TdjHTTPClient.Get(Document: string; Host: string = 'http://127.0.0.1'): string;
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create;
  try
    Result := HTTP.Get(Host + Document);

  finally
    HTTP.Free;
  end;
end;
{$ENDIF}

class function TdjHTTPClient.Get(URL: string; AIgnoreReplies: array of
  SmallInt): string;
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create;
  try
    Result := HTTP.Get(URL, AIgnoreReplies);

    Assert(HTTP.ResponseCode = 404);
  finally
    HTTP.Free;
  end;
end;

class function TdjHTTPClient.GetStatus(Document, Host: string): Integer;
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create;
  try
    HTTP.Get(Host + Document);
    Result := HTTP.ResponseCode;
  finally
    HTTP.Free;
  end;
end;

end.

