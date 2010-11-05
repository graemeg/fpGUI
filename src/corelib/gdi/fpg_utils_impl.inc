{%mainunit fpg_utils.pas}

uses
  Shellapi, Windows, fpg_constants, fpg_stringutils;
  
// GDI specific implementations of encoding functions

function fpgToOSEncoding(aString: TfpgString): string;
begin
  Result := Utf8ToAnsi(aString);
end;

function fpgFromOSEncoding(aString: string): TfpgString;
begin
  Result := AnsiToUtf8(aString);
end;

procedure fpgOpenURL(const aURL: TfpgString);
begin
  try
    {$IFNDEF wince}
    ShellExecute(0, 'open', PChar(aURL), nil, nil, 1 {SW_SHOWNORMAL});
    {$ENDIF}
  except
    // do nothing
  end;
end;

function fpgFileSize(const AFilename: TfpgString): integer;
var
  FindData: TWIN32FindDataW;
  FindHandle: THandle;
  Str: widestring;
begin
  // Don't assign the widestring to TSearchRec.name because it is of type
  // string, which will generate a conversion to the system encoding
  Str := UTF8Decode(AFilename);
  FindHandle:=Windows.FindFirstFileW(PWideChar(Str), FindData);
  if FindHandle=Windows.Invalid_Handle_value then
  begin
    Result:=-1;
    exit;
  end;
  Result := (int64(FindData.nFileSizeHigh) shl 32)+FindData.nFileSizeLow;
  Windows.FindClose(FindHandle);
end;

