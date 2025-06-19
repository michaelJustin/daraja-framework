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

unit MainUnit;

interface

procedure Demo;

implementation

uses
  djServer, djWebAppContext, djWebComponent, djTypes, djInterfaces,
  djNCSALogFilter, djFileUploadHelper,
  IdMessageCoder, IdGlobalProtocols,
  Classes, SysUtils;

type
  THomePage = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure THomePage.OnGet(Request: TdjRequest; Response: TdjResponse);
begin
  Response.ContentText :=
      '<!DOCTYPE html>'
    + '<html>'
    + '<body>'
    + '<form action="upload" method="post" enctype="multipart/form-data">'
    + '  Select file(s) to upload:'
    + '  <input type="text" name="fname" id="fname" value="Svifnökkvinn"><br>'
    + '  <input type="file" multiple name="fileToUpload" id="fileToUpload">'
    + '  <input type="submit" value="Upload File(s)" name="submit">'
    + '</form>'
    + '</body>'
    + '</html>';
  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

type
  TUploadPage = class(TdjWebComponent)
  private
    procedure ProcessMimePart(const Decoder: TIdMessageDecoder;
      const Dest: TMemoryStream; const Response: TdjResponse);
  public
    procedure OnPost(Request: TdjRequest; Response: TdjResponse); override;
  end;

procedure TUploadPage.OnPost(Request: TdjRequest; Response: TdjResponse);
begin
  HandleMultipartUpload(Request, Response, ProcessMimePart);
end;

procedure TUploadPage.ProcessMimePart(const Decoder: TIdMessageDecoder;
  const Dest: TMemoryStream; const Response: TdjResponse);
var
  UploadFile: string;
  ContentDisposition: string;
  ParamName: string;
  ParamValue: string;
  StringStream: TStringStream;
begin
  if Decoder.Filename <> '' then
  begin

    Response.ContentText := Response.ContentText
      + Format('<p>%s %d bytes</p>' + #13#10,
        [Decoder.Filename, Dest.Size]);

    Dest.Position := 0;
    UploadFile := '.\' + Decoder.Filename;
    Dest.SaveToFile(UploadFile);

    Response.ContentText := Response.ContentText
      + '<p>' + UploadFile + ' written</p>';

  end else begin
    ContentDisposition := Decoder.Headers.Values['Content-Disposition']; {Do not Localize}
    ParamName := ExtractHeaderSubItem(ContentDisposition, 'name', QuoteMIME); {do not localize}
    StringStream := TStringStream.Create('', TEncoding.UTF8);
    try
      StringStream.LoadFromStream(Dest);
      ParamValue := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  end;

  Response.ContentText := Response.ContentText
      + '<p>' + ParamValue + '</p>';

  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

procedure Demo;
var
  Server: TdjServer;
  Context: TdjWebAppContext;
begin
  Server := TdjServer.Create(80);
  try
    Context := TdjWebAppContext.Create('', True);
    Context.Add(THomePage, '/index.html');
    Context.Add(TUploadPage, '/upload');
    Context.Add(TdjNCSALogFilter, '/*');

    Server.Add(Context);

    Server.Start;
    WriteLn('Server is running, please open http://127.0.0.1/index.html');
    WriteLn('Hit enter to terminate.');
    ReadLn;
  finally
    Server.Free;
  end;
end;

end.
