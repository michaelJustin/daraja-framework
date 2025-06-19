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
//  LContentType, LContentTransferEncoding: string;
//  LDecoder: TIdDecoder;
  LLine: string;
  LBinaryLineBreak: string;
//  LBuffer: string;  //Needed for binhex4 because cannot decode line-by-line.
  LIsThisTheFirstLine: Boolean; //Needed for binary encoding
  LBoundaryStart, LBoundaryEnd: string;
//  LIsBinaryContentTransferEncoding: Boolean;
  LEncoding: IIdTextEncoding;
begin
  LIsThisTheFirstLine := True;
  VMsgEnd := False;
  Result := nil;
//  if FBodyEncoded then begin
//    LContentType := TIdMessage(Owner).ContentType;
//    LContentTransferEncoding := ExtractHeaderItem(TIdMessage(Owner).ContentTransferEncoding);
//  end else begin
//    LContentType := FHeaders.Values['Content-Type']; {Do not Localize}
//    LContentTransferEncoding := ExtractHeaderItem(FHeaders.Values['Content-Transfer-Encoding']); {Do not Localize}
//  end;
//  if LContentTransferEncoding = '' then begin
//    // RLebeau 04/08/2014: According to RFC 2045 Section 6.1:
//    // "Content-Transfer-Encoding: 7BIT" is assumed if the
//    // Content-Transfer-Encoding header field is not present."
//    if IsHeaderMediaType(LContentType, 'application/mac-binhex40') then begin  {Do not Localize}
//      LContentTransferEncoding := 'binhex40'; {do not localize}
//    end
//    else if not IsHeaderMediaType(LContentType, 'application/octet-stream') then begin  {Do not Localize}
//      LContentTransferEncoding := '7bit'; {do not localize}
//    end;
//  end
//  else if IsHeaderMediaType(LContentType, 'multipart') then {do not localize}
//  begin
    // RLebeau 08/17/09 - According to RFC 2045 Section 6.4:
    // "If an entity is of type "multipart" the Content-Transfer-Encoding is not
    // permitted to have any value other than "7bit", "8bit" or "binary"."
    //
    // However, came across one message where the "Content-Type" was set to
    // "multipart/related" and the "Content-Transfer-Encoding" was set to
    // "quoted-printable".  Outlook and Thunderbird were apparently able to parse
    // the message correctly, but Indy was not.  So let's check for that scenario
    // and ignore illegal "Content-Transfer-Encoding" values if present...
//    if PosInStrArray(LContentTransferEncoding, ['7bit', '8bit', 'binary'], False) = -1 then begin {do not localize}
//      LContentTransferEncoding := '';
//    end;
//  end;

//  if TextIsSame(LContentTransferEncoding, 'base64') then begin {Do not Localize}
//    LDecoder := TIdDecoderMIMELineByLine.Create(nil);
//  end else if TextIsSame(LContentTransferEncoding, 'quoted-printable') then begin {Do not Localize}
//    LDecoder := TIdDecoderQuotedPrintable.Create(nil);
//  end else if TextIsSame(LContentTransferEncoding, 'binhex40') then begin {Do not Localize}
//    LDecoder := TIdDecoderBinHex4.Create(nil);
//  end else begin
//    LDecoder := nil;
//  end;

//  try
//    if LDecoder <> nil then begin
//      LDecoder.DecodeBegin(ADestStream);
//    end;

    if MIMEBoundary <> '' then begin
      LBoundaryStart := '--' + MIMEBoundary; {Do not Localize}
      LBoundaryEnd := LBoundaryStart + '--'; {Do not Localize}
    end;

//    if LContentTransferEncoding <> '' then begin
//      case PosInStrArray(LContentTransferEncoding, ['7bit', 'quoted-printable', 'base64', '8bit', 'binary'], False) of {do not localize}
//        0..2: LIsBinaryContentTransferEncoding := False;
//        3..4: LIsBinaryContentTransferEncoding := True;
//      else
//        // According to RFC 2045 Section 6.4:
//        // "Any entity with an unrecognized Content-Transfer-Encoding must be
//        // treated as if it has a Content-Type of "application/octet-stream",
//        // regardless of what the Content-Type header field actually says."
//        LIsBinaryContentTransferEncoding := True;
//        LContentTransferEncoding := '';
//      end;
//    end else begin
//      LIsBinaryContentTransferEncoding := True;
//    end;

    repeat
      if not FProcessFirstLine then begin
        EnsureEncoding(LEncoding, enc8Bit);
//        if LIsBinaryContentTransferEncoding then begin
          // For binary, need EOL because the default LF causes spurious CRs in the output...
          // TODO: don't use ReadLnRFC() for binary data at all.  Read into an intermediate
          // buffer instead, looking for the next MIME boundary and message terminator while
          // flushing the buffer to the destination stream along the way.  Otherwise, at the
          // very least, we need to detect the type of line break used (CRLF vs bare-LF) so
          // we can duplicate it correctly in the output.  Most systems use CRLF, per the RFCs,
          // but have seen systems use bare-LF instead...
          LLine := ReadLnRFC(VMsgEnd, EOL, '.', LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF}); {do not localize}
          LBinaryLineBreak := EOL; // TODO: detect the actual line break used
//        end else begin
//          LLine := ReadLnRFC(VMsgEnd, LF, '.', LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF}); {do not localize}
//        end;
      end else begin
        LLine := FFirstLine;
        FFirstLine := '';    {Do not Localize}
        FProcessFirstLine := False;
//        // Do not use ADELIM since always ends with . (standard)
//        if LLine = '.' then begin {Do not Localize}
//          VMsgEnd := True;
//          Break;
//        end;
//        if TextStartsWith(LLine, '..') then begin
//          Delete(LLine, 1, 1);
//        end;
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
//      if LDecoder = nil then begin
        // Data to save, but not decode
        if Assigned(ADestStream) then begin
          EnsureEncoding(LEncoding, enc8Bit);
        end;
//        if LIsBinaryContentTransferEncoding then begin {do not localize}
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
//        end else begin
//          if Assigned(ADestStream) then begin
//            WriteStringToStream(ADestStream, LLine + EOL, LEncoding{$IFDEF STRING_IS_ANSI}, LEncoding{$ENDIF});
//          end;
//        end;
//      end
//      else begin
//        // Data to decode
//        if LDecoder is TIdDecoderQuotedPrintable then begin
//          // For TIdDecoderQuotedPrintable, we have to make sure all EOLs are intact
//          LDecoder.Decode(LLine + EOL);
//        end else if LDecoder is TIdDecoderBinHex4 then begin
//          // We cannot decode line-by-line because lines don't have a whole
//          // number of 4-byte blocks due to the : inserted at the start of
//          // the first line, so buffer the file...
//          // TODO: flush the buffer periodically when it has enough blocks
//          // in it, otherwise we are buffering the entire file in memory
//          // before decoding it...
//          LBuffer := LBuffer + LLine;
//        end else if LLine <> '' then begin
//          LDecoder.Decode(LLine);
//        end;
//      end;
    until False;
//    if LDecoder <> nil then begin
//      if LDecoder is TIdDecoderBinHex4 then begin
//        //Now decode the complete block...
//        LDecoder.Decode(LBuffer);
//      end;
//      LDecoder.DecodeEnd;
//    end;
//  finally
//    FreeAndNil(LDecoder);
//  end;
end;

end.
