(*
   Copyright (c) Michael Justin

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

unit djFileUploadHelper;

// note: this is unsupported example code

interface

uses
  IdMessageCoder, djTypes,
  Classes;

type
  TMimeHandler = procedure(const Decoder: TIdMessageDecoder;
    const Dest: TMemoryStream; const Response: TdjResponse) of object;

procedure HandleMultipartUpload(Request: TdjRequest; Response:
  TdjResponse; MimeHandler: TMimeHandler);

implementation

uses
  IdGlobalProtocols, IdGlobal, IdMessageCoderMIME,
  SysUtils;

// based on https://en.delphipraxis.net/topic/10918-multipartform-data-vs-x-www-form-urlencoded-indy-http-server/?do=findComment&comment=87010
procedure HandleMultipartUpload(Request: TdjRequest; Response:
  TdjResponse; MimeHandler: TMimeHandler);
var
  MsgEnd: Boolean;
  Decoder, NewDecoder: TIdMessageDecoder;
  Line, Boundary, BoundaryStart, BoundaryEnd: string;
  Dest: TMemoryStream;
begin
  Boundary := ExtractHeaderSubItem(Request.ContentType, 'boundary', QuoteHTTP);
  BoundaryStart := '--' + Boundary;
  BoundaryEnd := BoundaryStart + '--';

  repeat
    Line := ReadLnFromStream(Request.PostStream, -1, True);
    if Line = BoundaryEnd then Exit;
  until Line = BoundaryStart;

  Decoder := TIdMessageDecoderMIME.Create(nil);
  try
    MsgEnd := False;
    repeat
      TIdMessageDecoderMIME(Decoder).MIMEBoundary := Boundary;
      Decoder.SourceStream := Request.PostStream;
      Decoder.FreeSourceStream := False;

      Decoder.ReadHeader;
      case Decoder.PartType of
        mcptText, mcptAttachment:
        begin
          Dest := TMemoryStream.Create;
          try
            NewDecoder := Decoder.ReadBody(Dest, MsgEnd);
            try
              // use Dest as needed...
              MimeHandler(Decoder, Dest, Response);
            finally
              Decoder.Free;
              Decoder := NewDecoder;
            end;
          finally
            Dest.Free;
          end;
        end;
        mcptIgnore:
        begin
          FreeAndNil(Decoder);
          Decoder := TIdMessageDecoderMIME.Create(nil);
        end;
        mcptEOF:
        begin
          MsgEnd := True;
        end;
      end;
    until (Decoder = nil) or MsgEnd;
  finally
    Decoder.Free;
  end;
end;

end.
