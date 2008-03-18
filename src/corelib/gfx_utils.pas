unit gfx_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gfxbase;

// Platform specific encoding handling functions
function fpgToOSEncoding(aString: TfpgString): string;
function fpgFromOSEncoding(aString: string): TfpgString;

// Common functions for all platforms
function fpgAddTrailingValue(const ALine, AValue: TfpgString; ADuplicates: boolean = true): TfpgString;

// RTL wrapper filesystem functions with platform independant encoding
// These functions are common for all platforms and rely on fpgXXXPlatformEncoding

function fpgFindFirst(const Path: TfpgString; Attr: Longint; out Rslt: TSearchRec): Longint;
function fpgFindNext(var Rslt: TSearchRec): Longint;
function fpgGetCurrentDir: TfpgString;
function fpgSetCurrentDir(const NewDir: TfpgString): Boolean;
function fpgExpandFileName(const FileName: TfpgString): TfpgString;
function fpgFileExists(const FileName: TfpgString): Boolean;

{ ***  Examples of others we could do  *** }

// function fpgCreateDir(const NewDir: TfpgString): Boolean;
// function fpgRemoveDir(const Dir: TfpgString): Boolean;
// function fpgForceDirectories(const Dir: TfpgString): Boolean;
// function fpgDeleteFile(const FileName: TfpgString): Boolean;
// function fpgRenameFile(const OldName, NewName: TfpgString): Boolean;
// function fpgFileSearch(const Name, DirList: TfpgString): TfpgString;
// function fpgFileIsReadOnly(const FileName: TfpgString): Boolean;
// ....


implementation


// Platform specific encoding handling functions
{$I gfx_utils_impl.inc}


// the common code for all platforms
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

// RTL wrapper filesystem functions

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


end.

