{
    fpGUI IDE - INI File Syntax Highlighter

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Token-based syntax highlighter for INI/CFG files.
      Recognises sections [name], key=value pairs, comments (;/#),
      and string values.

      Token category mapping:
        hcKeyword1   - section headers [Section]
        hcIdentifier - keys (left side of =)
        hcString1    - values (right side of =)
        hcComment1   - comments (; or # lines)
        hcSymbol     - the = separator
}
unit ide.highlighter.ini;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ide.highlighter;

type

  { TINIHighlighter }

  TINIHighlighter = class(TEditorHighlighter)
  protected
    procedure DoTokenise(const AText: string); override;
  end;


implementation

procedure TINIHighlighter.DoTokenise(const AText: string);
var
  Lines: TStringList;
  LineIdx, Col, Start, Len: Integer;
  S: string;
  Ch: Char;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    EnsureLineCount(Lines.Count);

    for LineIdx := 0 to Lines.Count - 1 do
    begin
      S := Lines[LineIdx];
      if Length(S) = 0 then
        Continue;

      { Skip leading whitespace }
      Col := 1;
      while (Col <= Length(S)) and (S[Col] in [' ', #9]) do
        Inc(Col);

      if Col > Length(S) then
        Continue;

      Ch := S[Col];

      { Comment line: ; or # }
      if Ch in [';', '#'] then
      begin
        AddToken(LineIdx, Col - 1, Length(S) - Col + 1, hcComment1);
        Continue;
      end;

      { Section header: [SectionName] }
      if Ch = '[' then
      begin
        { Find closing bracket }
        Start := Col;
        while (Col <= Length(S)) and (S[Col] <> ']') do
          Inc(Col);
        if (Col <= Length(S)) then
          Len := Col - Start + 1
        else
          Len := Length(S) - Start + 1;
        AddToken(LineIdx, Start - 1, Len, hcKeyword1);
        Continue;
      end;

      { Key = Value pair }
      Start := Col;
      while (Col <= Length(S)) and not (S[Col] in ['=', ' ', #9]) do
        Inc(Col);
      { Key token }
      if Col > Start then
        AddToken(LineIdx, Start - 1, Col - Start, hcIdentifier);

      { Skip whitespace before = }
      while (Col <= Length(S)) and (S[Col] in [' ', #9]) do
        Inc(Col);

      { = separator }
      if (Col <= Length(S)) and (S[Col] = '=') then
      begin
        AddToken(LineIdx, Col - 1, 1, hcSymbol);
        Inc(Col);

        { Skip whitespace after = }
        while (Col <= Length(S)) and (S[Col] in [' ', #9]) do
          Inc(Col);

        { Value token — rest of line }
        if Col <= Length(S) then
          AddToken(LineIdx, Col - 1, Length(S) - Col + 1, hcString1);
      end;
    end;
  finally
    Lines.Free;
  end;
end;

end.
