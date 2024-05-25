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

unit LoginResource;

// note: this is unsupported example code

interface

uses djWebComponent, djTypes;

type
  TLoginResource = class(TdjWebComponent)
  private
    function CheckPwd(const Username, Password: string): Boolean;
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  // to support UTF-8 form parameters, a patched version
  // of Indy TIdCustomHTTPServer.DecodeAndSetParams is required
  utf8helper,
  SysUtils;

procedure TLoginResource.OnGet(Request: TdjRequest; Response: TdjResponse);
var
  User: string;
begin
  User := Request.Session.Content.Values['auth:username'];
  if User = '' then
  begin
    // respond with login form
    Response.ContentText :=
      '<!DOCTYPE html>' + #13
    + '<html lang="en">' + #13
    + '  <head>' + #13
    + '    <meta charset="utf-8">' + #13
    + '    <title>Form based login example</title>' + #13
    + '  </head>' + #13
    + '  <body>' + #13
    + '    <form method="post">' + #13
    + '     <input type="text" name="username" required>' +#13
    + '     <input type="password" name="password" required>' + #13
    + '     <input type="submit" name="submit" value="Login">' + #13
    + '    </form>' + #13
    + '  </body>' + #13
    + '</html>';
  end
  else
  begin
    // respond with logout form
    Response.ContentText := Format(
      '<!DOCTYPE html>' + #13
    + '<html lang="en">' + #13
    + '  <head>' + #13
    + '    <meta charset="utf-8">' + #13
    + '    <title>Form based login example</title>' + #13
    + '  </head>' + #13
    + '  <body>' + #13
    + '    <p>Hello %s, you are logged in</p>' + #13
    + '    <form method="post" action="logout">' + #13
    + '     <input type="submit" value="Logout">' + #13
    + '    </form>' + #13
    + '  </body>' + #13
    + '</html>', [User]);
  end;

  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

procedure TLoginResource.OnPost(Request: TdjRequest; Response: TdjResponse);
var
  Username: string;
  Password: string;
begin
  MyDecodeAndSetParams(Request);

  // read form data
  Username := Request.Params.Values['username'];
  Password := Request.Params.Values['password'];

  if CheckPwd(Username, Password) then
  begin
    // store username in session
    Request.Session.Content.Values['auth:username'] := Username;
    // success: redirect to home page
    Response.Redirect(Request.Document);
  end else begin
    // bad user/password: return authentication error
    Response.ResponseNo := 401;
  end;
end;

function TLoginResource.CheckPwd(const Username, Password: string): Boolean;
begin
  Result := False;

  if Username = '汉语' then
  begin
    Result := Password = 'hello';
  end;
end;

end.

