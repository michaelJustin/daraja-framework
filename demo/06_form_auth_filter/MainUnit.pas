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

unit MainUnit;

interface

procedure Demo;

implementation

uses
  djServer,
  djWebAppContext,
  djServerContext,
  djTypes,
  djInterfaces,
  djWebFilter,
  djNCSALogFilter,
  PublicResource,
  SecuredResource,
  LoginResource,
  LoginErrorResource,
  LogoutResource;

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
    procedure DoFilter(Context: TdjServerContext; Request: TdjRequest; Response:
      TdjResponse; const Chain: IWebFilterChain); override;
  end;


  { TFormAuthFilter }

  procedure TFormAuthFilter.DoFilter(Context: TdjServerContext;
    Request: TdjRequest; Response: TdjResponse; const Chain: IWebFilterChain);
  var
    IsLoggedIn: Boolean;
  begin
    IsLoggedIn := Request.Session.Content.Values['auth:username'] <> '';
    if IsLoggedIn then
    begin
      Chain.DoFilter(Context, Request, Response); // pass
    end
    else
    begin
      Request.Session.Content.Values['auth:target'] := Request.Document;
      Response.Redirect('/login');
    end;
  end;

procedure Demo;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create(80);
  try
    Context := TdjWebAppContext.Create('', True);
    Context.AddWebComponent(TPublicResource, '/index.html');
    Context.AddWebComponent(TSecuredResource, '/admin');
    Context.AddWebComponent(TLoginResource, '/login');
    Context.AddWebComponent(TLoginErrorResource, '/loginError');
    Context.AddWebComponent(TLogoutResource, '/logout');

    Context.AddFilterWithMapping(TFormAuthFilter, '/admin');
    Context.AddFilterWithMapping(TdjNCSALogFilter, '/*');

    Server.Add(Context);

    Server.Start;
    WriteLn('Server is running, please open http://localhost/index.html');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
  end;
end;

end.
