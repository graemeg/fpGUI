unit gfx_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gfxbase;
  
  
// Common functions for all platforms
function fpgAddTrailingValue(const ALine, AValue: TfpgString; ADuplicates: boolean = true): TfpgString;



// RTL wrapper filesystem functions with platform specific encodings

// function fpgFindFirst(const Path: TfpgString; Attr: Longint; out Rslt: TSearchRec): Longint;


{ ***  Examples of others we could do  *** }

// function fpgGetCurrentDir: TfpgString;
// function fpgSetCurrentDir(const NewDir: TfpgString): Boolean;
// function fpgCreateDir(const NewDir: TfpgString): Boolean;
// function fpgRemoveDir(const Dir: TfpgString): Boolean;
// function fpgForceDirectories(const Dir: TfpgString): Boolean;
// function fpgDeleteFile(const FileName: TfpgString): Boolean;
// function fpgRenameFile(const OldName, NewName: TfpgString): Boolean;
// function fpgFileSearch(const Name, DirList: TfpgString): TfpgString;
// function fpgFileIsReadOnly(const FileName: TfpgString): Boolean;
// ....


implementation


// RTL wrapper filesystem functions with platform specific encodings
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





end.

