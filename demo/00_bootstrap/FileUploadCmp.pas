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

unit FileUploadCmp;

// note: this is unsupported example code

interface

uses
  djWebComponent, djTypes,
  IdMessageCoder;

type
  TUploadPage = class(TdjWebComponent)
  private
    procedure ProcessMimePart(var ADecoder: TIdMessageDecoder;
      var AMsgEnd: Boolean; const Response: TdjResponse);
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse);
      override;

    procedure OnPost(Request: TdjRequest; Response:
      TdjResponse); override;
  end;

implementation

uses
  BindingFramework, djFileUploadHelper,
  IdGlobalProtocols,
  Classes, SysUtils;

{ TUploadPage }

procedure TUploadPage.OnGet(Request: TdjRequest; Response:
  TdjResponse);
begin
  Response.ContentText := Bind(Config.GetContext.GetContextPath, 'upload.html');
  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

function GetUploadFolder: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'upload\';
  ForceDirectories(Result);
end;

procedure TUploadPage.ProcessMimePart(var ADecoder: TIdMessageDecoder;
  var AMsgEnd: Boolean; const Response: TdjResponse);
var
  Stream: TMemoryStream;
  NewDecoder: TIdMessageDecoder;
  UploadFile: string;
begin
  Stream := TMemoryStream.Create;
  try
    NewDecoder := ADecoder.ReadBody(Stream, AMsgEnd);
    if ADecoder.Filename <> '' then
    begin

      Config.GetContext.Log(
        Format('Received %s (%d bytes)', [ADecoder.Filename, Stream.Size]));

      try
        Stream.Position := 0;
        Response.ContentText := Response.ContentText
          + Format('<p>%s %d bytes</p>' + #13#10,
            [ADecoder.Filename, Stream.Size]);

        // write stream to upload folder
        UploadFile := GetUploadFolder + ADecoder.Filename;
        Stream.SaveToFile(UploadFile);
        Response.ContentText := Response.ContentText
          + '<p>' + UploadFile + ' written</p>';

        Config.GetContext.Log(Format('Saved %s', [UploadFile]));

      except
        NewDecoder.Free;
        raise;
      end;
    end;
    ADecoder.Free;
    ADecoder := NewDecoder;
  finally
    Stream.Free;
  end;
end;

procedure TUploadPage.OnPost(Request: TdjRequest;
  Response: TdjResponse);
begin
  if IsHeaderMediaType(Request.ContentType, 'multipart/form-data') then
  begin
    HandleMultipartUpload(Request, Response, ProcessMimePart);
  end;
end;

end.

