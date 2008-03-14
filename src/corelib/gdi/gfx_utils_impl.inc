{%mainunit gfx_utils.pas}

// GDI specific implementations of RTL wrapper functions

function fpgFindFirst(const Path: TfpgString; Attr: Longint; out
  Rslt: TSearchRec): Longint;
begin
  Result := FindFirst(Utf8ToAnsi(Path), Attr, Rslt);
  Rslt.Name := AnsiToUtf8(Rslt.Name);
end;

function fpgFindNext(var Rslt: TSearchRec): Longint;
begin
  Result := FindNext(Rslt);
  Rslt.Name := AnsiToUtf8(Rslt.Name);
end;

function fpgGetCurrentDir: TfpgString;
begin
  Result := AnsiToUtf8(GetCurrentDir);
end;

function fpgSetCurrentDir(const NewDir: TfpgString): Boolean;
begin
  Result := SetCurrentDir(Utf8ToAnsi(NewDir));
end;

function fpgExpandFileName(const FileName: TfpgString): TfpgString;
begin
  Result := AnsiToUtf8(ExpandFileName(Utf8ToAnsi(FileName)));
end;

function fpgFileExists(const FileName: TfpgString): Boolean;
begin
  Result := FileExists(Utf8ToAnsi(FileName));
end;



