unit gui_mig_constraintparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_lc;
  
type
  TConstraintParser = class(TObject)
  public
    function ParseLayoutConstraint(const s: string): TLC;
  end;

implementation

uses
  gfx_tokenlibrary;

{ TConstraintParser }

function TConstraintParser.ParseLayoutConstraint(const s: string): TLC;
var
  tokenizer: TTokens;
  parts: TStringList;
  part: string;
  i: integer;
  len: integer;
begin
  result := TLC.Create;
  
  if s = '' then
    Exit;
    
  tokenizer := TTokens.Create(s, ',', '"', '"', '\', tsSingleSeparatorBetweenTokens);
  parts := TStringList.Create;
  for i := 1 to tokenizer.TokenCount do
    parts.Add(trim(tokenizer.Token(i)));
  tokenizer.Free;
  
  // First check for "ltr" or "rtl" since that will affect the interpretation
  // of the other constraints.
  for i := 0 to parts.Count-1 do
  begin
    part := parts[i];
    if part = '' then
      continue;
      
    len := Length(part);
    if (len = 3) or (len = 11) then // optimization
    begin
      if (part = 'ltr') or (part = 'rtl') or (part = 'lefttoright') or (part = 'righttoleft') then
      begin
        result.LeftToRight_prop := part[1] = 'l';
        parts[i] := ''; // so we don't process it again
      end;
      if (part = 'ttb') or (part = 'btt') or (part = 'toptobottom') or (part = 'bottomtotop') then
      begin
        result.TopToBottom_prop := part[1] = 't';
        parts[i] := ''; // so we don't process it again
      end;
    end;
  end;
  
  for i := 0 to parts.Count-1 do
  begin
    part := parts[i];
    if part = '' then
      continue;

  end;
  
  parts.Free;
end;

end.

