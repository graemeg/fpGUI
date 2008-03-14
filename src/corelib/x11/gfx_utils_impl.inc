{%mainunit gfx_utils.pas}

// X11 specific filesystem implementations of RTL wrapper functions

function fpgFindFirst(const Path: TfpgString; Attr: Longint; out
  Rslt: TSearchRec): Longint;
begin
  Result := FindFirst(Path, Attr, Rslt);
end;

function fpgFindNext(var Rslt: TSearchRec): Longint;
begin
  Result := FindNext(Rslt);
end;

function fpgGetCurrentDir: TfpgString;
begin
  Result := GetCurrentDir;
end;

function fpgSetCurrentDir(const NewDir: TfpgString): Boolean;
begin
  Result := SetCurrentDir(NewDir);
end;

function fpgExpandFileName(const FileName: TfpgString): TfpgString;
begin
  Result := ExpandFileName(FileName);
end;

function fpgFileExists(const FileName: TfpgString): Boolean;
begin
  Result := FileExists(FileName);
end;


