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
  IdGlobalProtocols, IdGlobal, IdMessageCoderMIME, IdMessage,
  SysUtils;

type
  // Special Decoder for multipart/form-data
  TIdMessageDecoderMultipartFormData = class(TIdMessageDecoderMIME)
  public
    function ReadBody(ADestStream: TStream; var VMsgEnd: Boolean): TIdMessageDecoder; override;
  end;

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

  Decoder := TIdMessageDecoderMultipartFormData.Create;
  try
    MsgEnd := False;
    repeat
      TIdMessageDecoderMultipartFormData(Decoder).MIMEBoundary := Boundary;
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
          Decoder := TIdMessageDecoderMultipartFormData.Create;
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

{ TIdMessageDecoderMultipartFormData }

function TIdMessageDecoderMultipartFormData.ReadBody(ADestStream: TStream;
  var VMsgEnd: Boolean): TIdMessageDecoder;
var
  LLine: string;
  LBinaryLineBreak: string;
  LIsThisTheFirstLine: Boolean;
  LBoundaryStart, LBoundaryEnd: string;
  LEncoding: IIdTextEncoding;
begin
  LIsThisTheFirstLine := True;
  VMsgEnd := False;
  Result := nil;

  if PartType <> mcptAttachment then
  begin
    // raise Exception. Can't handle this.
  end;
  if BodyEncoded then
  begin
    // raise Exception. Can't handle this.
  end;
  if '' <> (FHeaders.Values['Content-Transfer-Encoding']) {Do not Localize} then
  begin
    // raise Exception. Can't handle this.
  end;

  LBoundaryStart := '--' + MIMEBoundary; {Do not Localize}
  LBoundaryEnd := LBoundaryStart + '--'; {Do not Localize}

  repeat
    if not FProcessFirstLine then begin
      EnsureEncoding(LEncoding, enc8Bit);
      // For binary, need EOL because the default LF causes spurious CRs in the output...
      // TODO: don't use ReadLnRFC() for binary data at all.  Read into an intermediate
      // buffer instead, looking for the next MIME boundary and message terminator while
      // flushing the buffer to the destination stream along the way.  Otherwise, at the
      // very least, we need to detect the type of line break used (CRLF vs bare-LF) so
      // we can duplicate it correctly in the output.  Most systems use CRLF, per the RFCs,
      // but have seen systems use bare-LF instead...
      LLine := ReadLnRFC(VMsgEnd, EOL, '.', LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF}); {do not localize}
      LBinaryLineBreak := EOL; // TODO: detect the actual line break used
    end else begin
      LLine := FFirstLine;
      FFirstLine := '';    {Do not Localize}
      FProcessFirstLine := False;
    end;
    if VMsgEnd then begin
      Break;
    end;
    // New boundary - end self and create new coder
    if MIMEBoundary <> '' then begin
      if TextIsSame(LLine, LBoundaryStart) then begin
        Result := TIdMessageDecoderMultipartFormData.Create(Owner);
        Break;
        // End of all coders (not quite ALL coders)
      end;
      if TextIsSame(LLine, LBoundaryEnd) then begin
        // POP the boundary
        if Owner is TIdMessage then begin
          TIdMessage(Owner).MIMEBoundary.Pop;
        end;
        Break;
      end;
    end;
    // Data to save, but not decode
    if Assigned(ADestStream) then begin
      EnsureEncoding(LEncoding, enc8Bit);
    end;
    //In this case, we have to make sure we dont write out an EOL at the
    //end of the file.
    if LIsThisTheFirstLine then begin
      LIsThisTheFirstLine := False;
    end else begin
      if Assigned(ADestStream) then begin
        WriteStringToStream(ADestStream, LBinaryLineBreak, LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF});
      end;
    end;
    if Assigned(ADestStream) then begin
      WriteStringToStream(ADestStream, LLine, LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF});
    end;
  until False;
end;

end.
