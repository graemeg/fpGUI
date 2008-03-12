{%mainunit gfx_utils.pas}

// X11 specific filesystem implementations of RTL wrapper functions

//  Example

{
function fpgFindFirst(const Path: TfpgString; Attr: Longint; out Rslt: TSearchRec): Longint;
begin
  Result := FindFirst(Path, Attr, Rstl);
end;
}
