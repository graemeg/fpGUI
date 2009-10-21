{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2009 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Abstracted OS specific function to work in a cross-platform manner.
}

unit fpg_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base;

// *** Platform specific functions ***

function fpgToOSEncoding(aString: TfpgString): string;
function fpgFromOSEncoding(aString: string): TfpgString;
procedure fpgOpenURL(const aURL: TfpgString);


// *** Common functions for all platforms ***

function fpgAddTrailingValue(const ALine, AValue: TfpgString; ADuplicates: Boolean = True): TfpgString;
function fpgAppendPathDelim(const Path: TfpgString): TfpgString;
function fpgHasSubDirs(const Dir: TfpgString; AShowHidden: Boolean): Boolean;
function fpgAllFilesMask: TfpgString;
function fpgConvertLineEndings(const s: TfpgString): TfpgString;


 // RTL wrapper filesystem functions with platform independant encoding
 // These functions are common for all platforms and rely on fpgXXXPlatformEncoding

function fpgFindFirst(const Path: TfpgString; Attr: longint; out Rslt: TSearchRec): longint;
function fpgFindNext(var Rslt: TSearchRec): longint;
function fpgGetCurrentDir: TfpgString;
function fpgSetCurrentDir(const NewDir: TfpgString): Boolean;
function fpgExpandFileName(const FileName: TfpgString): TfpgString;
function fpgFileExists(const FileName: TfpgString): Boolean;
function fpgDirectoryExists(const ADirectory: TfpgString): Boolean;
function fpgExtractFileDir(const FileName: TfpgString): TfpgString;
function fpgExtractFilePath(const FileName: TfpgString): TfpgString;


implementation

 { No USES clause is allowed here! Add it to the include file shown below. }


 // Platform specific encoding handling functions
{$I fpg_utils_impl.inc}


function fpgAddTrailingValue(const ALine, AValue: TfpgString; ADuplicates: Boolean = True): TfpgString;
begin
  if ALine = '' then
  begin
    Result := ALine;
    Exit; //==>
  end;

  if ADuplicates then
  begin
    Result := ALine + AValue;
    Exit; //==>
  end;

  if (not SameText(Copy(ALine, Length(ALine) - Length(AValue) + 1, Length(AValue)), AValue)) then
    Result := ALine + AValue
  else
    Result := ALine;
end;

function fpgFindFirst(const Path: TfpgString; Attr: longint; out Rslt: TSearchRec): longint;
begin
  Result    := FindFirst(fpgToOSEncoding(Path), Attr, Rslt);
  Rslt.Name := fpgFromOSEncoding(Rslt.Name);
end;

function fpgFindNext(var Rslt: TSearchRec): longint;
begin
  Result    := FindNext(Rslt);
  Rslt.Name := fpgFromOSEncoding(Rslt.Name);
end;

function fpgGetCurrentDir: TfpgString;
begin
  Result := fpgFromOSEncoding(GetCurrentDir);
end;

function fpgSetCurrentDir(const NewDir: TfpgString): Boolean;
begin
  Result := SetCurrentDir(fpgToOSEncoding(NewDir));
end;

function fpgExpandFileName(const FileName: TfpgString): TfpgString;
begin
  Result := fpgFromOSEncoding(ExpandFileName(fpgToOSEncoding(FileName)));
end;

function fpgFileExists(const FileName: TfpgString): Boolean;
begin
  Result := FileExists(fpgToOSEncoding(FileName));
end;

function fpgDirectoryExists(const ADirectory: TfpgString): Boolean;
begin
  Result := DirectoryExists(fpgToOSEncoding(ADirectory));
end;

function fpgExtractFileDir(const FileName: TfpgString): TfpgString;
begin
  Result := ExtractFileDir(fpgToOSEncoding(FileName));
end;

function fpgExtractFilePath(const FileName: TfpgString): TfpgString;
begin
  Result := ExtractFilePath(fpgToOSEncoding(Filename));
end;

function fpgAppendPathDelim(const Path: TfpgString): TfpgString;
begin
  if (Path <> '') and (Path[length(Path)] <> PathDelim) then
    Result := Path + PathDelim
  else
    Result := Path;
end;

{function fpgHasSubDirs returns True if the directory passed has subdirectories}
function fpgHasSubDirs(const Dir: TfpgString; AShowHidden: Boolean): Boolean;
var
  FileInfo: TSearchRec;
  FCurrentDir: TfpgString;
begin
  //Assume No
  Result := False;
  if Dir <> '' then
  begin
    FCurrentDir := fpgAppendPathDelim(Dir);
    FCurrentDir := FCurrentDir + fpgAllFilesMask;
    try
      if fpgFindFirst(FCurrentDir, faAnyFile or $00000080, FileInfo) = 0 then
        repeat
          if FileInfo.Name = '' then
            Continue;

            // check if special file
          if ((FileInfo.Name = '.') or (FileInfo.Name = '..')) or
            // unix dot directories (aka hidden directories)
            ((FileInfo.Name[1] in ['.']) and AShowHidden) or
            // check Hidden attribute
            (((faHidden and FileInfo.Attr) > 0) and AShowHidden) then
            Continue;

          Result := ((faDirectory and FileInfo.Attr) > 0);

          //We found at least one non special dir, that's all we need.
          if Result then
            break;
        until fpgFindNext(FileInfo) <> 0;
    finally
      FindClose(FileInfo);
    end;
  end;
end;

function fpgAllFilesMask: TfpgString;
begin
  {$Note In FPC 2.2.2 onwards we can use AllFilesMask which is part of RTL }
  {$IFDEF WINDOWS}
  Result := '*.*';
  {$ELSE}
  Result := '*';
  {$ENDIF}
end;

function fpgConvertLineEndings(const s: TfpgString): TfpgString;
var
  i: integer;
  EndingStart: longint;
begin
  Result := s;
  i      := 1;
  while (i <= length(Result)) do
    if Result[i] in [#10, #13] then
    begin
      EndingStart := i;
      Inc(i);
      if (i <= length(Result)) and (Result[i] in [#10, #13]) and (Result[i] <> Result[i - 1]) then
        Inc(i);
      if (length(LineEnding) <> i - EndingStart) or (LineEnding <> copy(Result, EndingStart, length(LineEnding))) then
      begin
        // line end differs => replace with current LineEnding
        Result := copy(Result, 1, EndingStart - 1) + LineEnding + copy(Result, i, length(Result));
        i      := EndingStart + length(LineEnding);
      end;
    end
    else
      Inc(i);
end;


end.

