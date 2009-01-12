{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Abstracted OS specific function to work in a cross-platfrom manner.
}

unit fpg_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base;

// *** Platform specific functions ***

function  fpgToOSEncoding(aString: TfpgString): string;
function  fpgFromOSEncoding(aString: string): TfpgString;
procedure fpgOpenURL(const aURL: TfpgString);


// *** Common functions for all platforms ***

function  fpgAddTrailingValue(const ALine, AValue: TfpgString; ADuplicates: boolean = true): TfpgString;


// RTL wrapper filesystem functions with platform independant encoding
// These functions are common for all platforms and rely on fpgXXXPlatformEncoding

function  fpgFindFirst(const Path: TfpgString; Attr: Longint; out Rslt: TSearchRec): Longint;
function  fpgFindNext(var Rslt: TSearchRec): Longint;
function  fpgGetCurrentDir: TfpgString;
function  fpgSetCurrentDir(const NewDir: TfpgString): Boolean;
function  fpgExpandFileName(const FileName: TfpgString): TfpgString;
function  fpgFileExists(const FileName: TfpgString): Boolean;
function  fpgAppendPathDelim(const Path: TfpgString): TfpgString;
function  fpgHasSubDirs(const Dir: TfpgString; AShowHidden: Boolean): Boolean;


implementation

{ No USES clause is allowed here! Add it to the include file shown below. }


// Platform specific encoding handling functions
{$I fpg_utils_impl.inc}



function fpgAddTrailingValue(const ALine, AValue: TfpgString; ADuplicates: boolean = true): TfpgString;
begin
  if ALine = '' then
  begin
    result := ALine;
    Exit; //==>
  end;

  if ADuplicates then
  begin
    result := ALine + AValue;
    Exit; //==>
  end;

  if (not SameText(Copy(ALine, Length(ALine) - Length(AValue) + 1, Length(AValue)), AValue)) then
    result := ALine + AValue
  else
    result := ALine;
end;

function fpgFindFirst(const Path: TfpgString; Attr: Longint; out
  Rslt: TSearchRec): Longint;
begin
  Result := FindFirst(fpgToOSEncoding(Path), Attr, Rslt);
  Rslt.Name := fpgFromOSEncoding(Rslt.Name);
end;

function fpgFindNext(var Rslt: TSearchRec): Longint;
begin
  Result := FindNext(Rslt);
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
    FCurrentDir := FCurrentDir + AllFilesMask;
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


end.

