{%mainunit fpg_utils.pas}

uses
  Registry,
  Shellapi;
  
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

