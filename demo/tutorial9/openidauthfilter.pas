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

unit OpenIDAuthFilter;

// note: this is unsupported example code

interface

uses
  djWebFilter,
  djServerContext,
  djTypes,
  djInterfaces,
  Classes, SysUtils;

type

  { TFormAuthFilter }

  TFormAuthFilter = class(TdjWebFilter)
  public
  (**
   * The doFilter method of the Filter is called by the container each time
   * a request/response pair is passed through the chain due to a client
   * request for a resource at the end of the chain.
   * The FilterChain passed in to this method allows the Filter to pass on
   * the request and response to the next entity in the chain.
   *)
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response: TdjResponse;
      const Chain: IWebFilterChain); override;
  end;

implementation

uses
  OpenIDHelper;

{ TFormAuthFilter }

procedure TFormAuthFilter.DoFilter(Context: TdjServerContext; Request: TdjRequest;
  Response: TdjResponse; const Chain: IWebFilterChain);
var
  Credentials: string;
  IdTokenResponse: TIdTokenResponse;
  Jwt: string;
  Claims: TIdTokenClaims;
begin
  Credentials := Request.Session.Content.Values['credentials'];
  if Credentials = '' then
  begin
    Response.Session.Content.Values['state'] := CreateState;
    Response.Redirect(OpenIDParams.redirect_uri);
  end
  else
  begin
    IdTokenResponse := ToIdTokenResponse(Credentials);
    if IdTokenResponse.expires_in <= 0 then
    begin // does this (<=0) happen?
      Response.Redirect(OpenIDParams.redirect_uri);
    end
    else
    begin
      Jwt := ReadJWTParts(IdTokenResponse.id_token);;
      Claims := ParseJWT(Jwt);

      // Request.Session.Content.Objects['claims'] := Claims; // todo to class
      Request.Session.Content.Values['iss'] := Claims.iss;
      Request.Session.Content.Values['sub'] := Claims.sub;
      Request.Session.Content.Values['email'] := Claims.email;
      Request.Session.Content.Values['name'] := Claims.name;

      Chain.DoFilter(Context, Request, Response); // pass
    end;
  end;
end;

end.
