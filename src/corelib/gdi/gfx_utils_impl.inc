{%mainunit gfx_utils.pas}

// GDI specific implementations of encoding functions

function fpgToOSEncoding(aString: TfpgString): string;
begin
  Result := Utf8ToAnsi(aString);
end;

function fpgFromOSEncoding(aString: string): TfpgString;
begin
  Result := AnsiToUtf8(aString);
end;

