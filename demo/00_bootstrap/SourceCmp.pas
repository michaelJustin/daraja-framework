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

unit SourceCmp;

// note: this is unsupported example code

interface

uses
  djWebComponent, djTypes;

type
  TSourcePage = class(TdjWebComponent)
  public
    procedure OnGet(Request: TdjRequest; Response: TdjResponse); override;
  end;

implementation

uses
  BindingFramework,
  SysUtils, Classes;

// from http://stackoverflow.com/a/2971923/80901

function HTMLEscape(const Data: string): string;
var
  iPos, i: Integer;

  procedure Encode(const AStr: string);
  begin
    Move(AStr[1], result[iPos], Length(AStr) * SizeOf(Char));
    Inc(iPos, Length(AStr));
  end;

begin
  SetLength(result, Length(Data) * 6);
  iPos := 1;
  for i := 1 to length(Data) do
    case Data[i] of
      '<': Encode('&lt;');
      '>': Encode('&gt;');
      '&': Encode('&amp;');
      '"': Encode('&quot;');
    else
      result[iPos] := Data[i];
      Inc(iPos);
    end;
  SetLength(result, iPos - 1);
end;

{ TSourcePage }

procedure TSourcePage.OnGet(Request: TdjRequest;
  Response: TdjResponse);
var
  FileName: string;
  AbsolutePath: string;
  Source: string;
  Tmp: string;
  SL: TStrings;
begin
  Tmp := Bind(Config.GetContext.GetContextPath, 'source.html');

  FileName := Request.Params.Values['file'];

  if FileName <> '' then
  begin

    AbsolutePath := ExpandFileName(ExtractFilePath(ParamStr(0)) + FileName);

    if ExtractFilePath(AbsolutePath) <> ExtractFilePath(ParamStr(0)) then
    begin
      // do not access files in other directories!
      Source := 'Invalid file name!';
    end
    else
    begin

      SL := TStringlist.Create;
      try
        SL.LoadFromFile(AbsolutePath);
        Source := HTMLEscape(SL.Text);
      finally
        SL.Free;
      end;

    end;
  end;

  Tmp := StringReplace(Tmp, '${sourcefile}', FileName, [rfReplaceAll]);
  Tmp := StringReplace(Tmp, '${source}', Source, [rfReplaceAll]);

  Response.ContentText := Tmp;
  Response.ContentType := 'text/html';
  Response.CharSet := 'utf-8';
end;

end.

