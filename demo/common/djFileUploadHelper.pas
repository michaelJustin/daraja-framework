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

{$i IdCompilerDefines.inc}

uses
  IdMessageCoder, djTypes;

type
  TMimeHandler = procedure(var VDecoder: TIdMessageDecoder;
    var VMsgEnd: Boolean; const Response: TdjResponse) of object;

procedure HandleMultipartUpload(Request: TdjRequest; Response:
  TdjResponse; MimeHandler: TMimeHandler);

implementation

uses
  IdGlobalProtocols, IdGlobal,
  IdMessageCoderMIME,
  Classes, SysUtils;

procedure HandleMultipartUpload(Request: TdjRequest; Response:
  TdjResponse; MimeHandler: TMimeHandler);
var
  LBoundary, LBoundaryStart, LBoundaryEnd: string;
  LDecoder: TIdMessageDecoder;
  LLine: string;
  LBoundaryFound, LIsStartBoundary, LMsgEnd: Boolean;
begin
  LBoundary := ExtractHeaderSubItem(Request.ContentType, 'boundary',
    QuoteHTTP);
  if LBoundary = '' then
  begin
    Response.ResponseNo := 400;
    Response.CloseConnection := True;
    Response.WriteHeader;
    Exit;
  end;

  LBoundaryStart := '--' + LBoundary;
  LBoundaryEnd := LBoundaryStart + '--';

  LDecoder := TIdMessageDecoderMIME.Create(nil);

  try
    TIdMessageDecoderMIME(LDecoder).MIMEBoundary := LBoundary;
    LDecoder.SourceStream := Request.PostStream;
    LDecoder.FreeSourceStream := False;

    // Copy from
    // http://stackoverflow.com/questions/27257577/indy-mime-decoding-of-multipart-form-data-requests-returns-trailing-cr-lf
    // note: this is a workaround, which enforces a specific encoding
    TIdMessageDecoderMIME(LDecoder).Headers.Values['Content-Transfer-Encoding'] := '8bit';
    TIdMessageDecoderMIME(LDecoder).BodyEncoded := False;

    LBoundaryFound := False;
    LIsStartBoundary := False;
    repeat
      LLine := ReadLnFromStream(Request.PostStream, -1, True);
      if LLine = LBoundaryStart then
      begin
        LBoundaryFound := True;
        LIsStartBoundary := True;
      end
      else if LLine = LBoundaryEnd then
      begin
        LBoundaryFound := True;
      end;
    until LBoundaryFound;

    if (not LBoundaryFound) or (not LIsStartBoundary) then
    begin
      Response.ResponseNo := 400;
      Response.CloseConnection := True;
      Response.WriteHeader;
      Exit;
    end;

    LMsgEnd := False;
    repeat
      TIdMessageDecoderMIME(LDecoder).MIMEBoundary := LBoundary;
      LDecoder.SourceStream := Request.PostStream;
      LDecoder.FreeSourceStream := False;

      LDecoder.ReadHeader;
      case LDecoder.PartType of
        mcptText, mcptAttachment:
          begin
            MimeHandler(LDecoder, LMsgEnd, Response);
          end;
        mcptIgnore:
          begin
            LDecoder.Free;
            LDecoder := TIdMessageDecoderMIME.Create(nil);
          end;
        mcptEOF:
          begin
            LDecoder.Free;
            LMsgEnd := True;
          end;
      end;
    until (LDecoder = nil) or LMsgEnd;
  finally
    LDecoder.Free;
  end;
end;

end.
