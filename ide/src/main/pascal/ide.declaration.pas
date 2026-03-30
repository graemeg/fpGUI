{
    fpGUI IDE - Go to Declaration

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Provides "Go to Declaration" functionality using FPC's fcl-passrc
      package (TPasParser + TPasResolver) for scope-aware identifier
      resolution. Ctrl+B on an identifier navigates to where it is
      declared.
}
unit ide.declaration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ide.highlighter;

type
  TDeclarationResult = record
    Found: Boolean;
    DeclFile: string;     { absolute path to declaration's source file }
    DeclLine: Integer;    { 1-based line number (as stored by TPasElement) }
    DeclName: string;     { name of the resolved declaration }
  end;

{ ALine and ACol are both 0-based (matching editor CaretPos_V / CaretPos_H) }
function GetIdentifierAtCursor(AHighlighter: TPascalHighlighter;
  ALines: TStrings; ALine, ACol: Integer): string;


implementation

{ Helper: extract text for a token from the source line }
function TokenText(ALines: TStrings; ALineIdx: Integer;
  const AToken: THighlightToken): string;
begin
  if (ALineIdx >= 0) and (ALineIdx < ALines.Count) then
    Result := Copy(ALines[ALineIdx], AToken.Column + 1, AToken.Length)
  else
    Result := '';
end;

function GetIdentifierAtCursor(AHighlighter: TPascalHighlighter;
  ALines: TStrings; ALine, ACol: Integer): string;
var
  tokens: THighlightTokenArray;
  i: Integer;
  tok: THighlightToken;
begin
  Result := '';
  if (ALine < 0) or (ALine >= AHighlighter.LineCount) then
    Exit;
  tokens := AHighlighter.GetLineTokens(ALine);
  for i := 0 to Length(tokens) - 1 do
  begin
    tok := tokens[i];
    if (ACol >= tok.Column) and (ACol < tok.Column + tok.Length) then
    begin
      if tok.Category = hcIdentifier then
        Result := TokenText(ALines, ALine, tok);
      Exit;
    end;
  end;
end;

end.
