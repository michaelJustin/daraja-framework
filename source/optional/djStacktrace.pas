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

unit djStacktrace;

(*
 * @note This unit is unsupported demonstration code.
 *)
    
{$IFDEF DARAJA_PROJECT_STAGE_DEVELOPMENT}

  {$MESSAGE HINT 'Options for development stage enabled'}

  // madExcept support (experimental) ----------------------------------------
  {$IFDEF DARAJA_MADEXCEPT}

    {$IFOPT D+}
      {$MESSAGE HINT 'Support for madExcept stacktrace enabled'}
    {$ELSE}
      {$MESSAGE ERROR 'Support for stacktrace requires linking with debug information'}
    {$ENDIF}

  {$ENDIF DARAJA_MADEXCEPT}

  // JclDebug support (experimental) -----------------------------------------
  {$IFDEF DARAJA_JCLDEBUG}

    {$IFOPT D+}
      {$MESSAGE HINT 'Support for JclDebug stacktrace enabled'}
    {$ELSE}
      {$MESSAGE ERROR 'Support for stacktrace requires linking with debug information'}
    {$ENDIF}

  {$ENDIF DARAJA_JCLDEBUG}

{$ENDIF}

interface

// {$i IdCompilerDefines.inc}

{$IFDEF DARAJA_JCLDEBUG}
function GetStackList(
  IncludeModuleName: Boolean = False;
  IncludeAddressOffset: Boolean = False;
  IncludeStartProcLineOffset: Boolean = False;
  IncludeVAddress: Boolean = False): string;
{$ENDIF DARAJA_JCLDEBUG}

implementation

{$IFDEF DARAJA_JCLDEBUG}
uses
   JclDebug,
   Classes;
{$ENDIF DARAJA_JCLDEBUG}

{$IFDEF DARAJA_JCLDEBUG}
// found at http://www.delphipraxis.net/181608-aktuelle-quellcodezeile-im-programm-ermitteln.html#post1270190

function GetStackList(
  IncludeModuleName: Boolean = False;
  IncludeAddressOffset: Boolean = False;
  IncludeStartProcLineOffset: Boolean = False;
  IncludeVAddress: Boolean = False): string;
var
  sl: TStrings;
begin
  sl := TStringList.Create;
  try
    with TJclStackInfoList.Create(True, 0, nil) do
    try
      Delete(0); // TJclStackInfoList
      Delete(0); // TJclStackInfoList
      Delete(0); // GetStackList
      AddToStrings(sl, IncludeModuleName, IncludeAddressOffset, IncludeStartProcLineOffset, IncludeVAddress);
    finally
      Free;
    end;
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;
{$ENDIF DARAJA_JCLDEBUG}

end.
