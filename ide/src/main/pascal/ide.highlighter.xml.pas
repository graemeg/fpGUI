{
    fpGUI IDE - XML Syntax Highlighter

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Token-based syntax highlighter for XML/HTML files.
      Handles tags, attributes, attribute values, comments, CDATA,
      processing instructions, and entity references.

      Token category mapping:
        hcKeyword1   - tag names (including < > / delimiters)
        hcIdentifier - attribute names
        hcString1    - attribute values (quoted strings)
        hcComment1   - comments <!-- ... -->
        hcDirective  - processing instructions <?...?> and CDATA
        hcSymbol     - = sign and entity references (&amp; etc.)
}
unit ide.highlighter.xml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ide.highlighter;

type

  { TXMLHighlighter }

  TXMLHighlighter = class(TEditorHighlighter)
  protected
    procedure DoTokenise(const AText: string); override;
  end;


implementation

type
  TXMLState = (
    xsText,          // outside any tag
    xsTag,           // inside < ... > (tag name area)
    xsAttribName,    // reading attribute name
    xsAttribEq,      // expecting = after attribute name
    xsAttribValue,   // reading quoted attribute value
    xsComment,       // inside <!-- ... -->
    xsCDATA,         // inside <![CDATA[ ... ]]>
    xsPI             // inside <? ... ?>
  );

procedure TXMLHighlighter.DoTokenise(const AText: string);
var
  Lines: TStringList;
  State: TXMLState;
  LineIdx, Col, Start, Len: Integer;
  S: string;
  QuoteChar: Char;

  function Ahead(APos, ACount: Integer): string;
  begin
    Result := Copy(S, APos, ACount);
  end;

  function CharsLeft: Integer;
  begin
    Result := Length(S) - Col + 1;
  end;

begin
  Lines := TStringList.Create;
  try
    Lines.Text := AText;
    EnsureLineCount(Lines.Count);
    State := xsText;
    QuoteChar := #0;

    for LineIdx := 0 to Lines.Count - 1 do
    begin
      S := Lines[LineIdx];
      Col := 1;

      while Col <= Length(S) do
      begin
        case State of

          xsText:
          begin
            if S[Col] = '<' then
            begin
              { Check for comment <!-- }
              if (CharsLeft >= 4) and (Ahead(Col, 4) = '<!--') then
              begin
                Start := Col;
                State := xsComment;
                Inc(Col, 4);
              end
              { Check for CDATA <![CDATA[ }
              else if (CharsLeft >= 9) and (Ahead(Col, 9) = '<![CDATA[') then
              begin
                Start := Col;
                State := xsCDATA;
                Inc(Col, 9);
              end
              { Check for processing instruction <? }
              else if (CharsLeft >= 2) and (S[Col + 1] = '?') then
              begin
                Start := Col;
                State := xsPI;
                Inc(Col, 2);
              end
              else
              begin
                { Opening or closing tag: tokenise < and tag name together }
                Start := Col;
                Inc(Col); { skip < }
                if (Col <= Length(S)) and (S[Col] = '/') then
                  Inc(Col); { skip / in closing tags }
                { Read tag name }
                while (Col <= Length(S)) and not (S[Col] in [' ', #9, '>', '/', #13, #10]) do
                  Inc(Col);
                AddToken(LineIdx, Start - 1, Col - Start, hcKeyword1);
                State := xsTag;
              end;
            end
            { Entity reference &...; }
            else if S[Col] = '&' then
            begin
              Start := Col;
              Inc(Col);
              while (Col <= Length(S)) and (S[Col] <> ';') and not (S[Col] in [' ', #9, '<']) do
                Inc(Col);
              if (Col <= Length(S)) and (S[Col] = ';') then
                Inc(Col);
              AddToken(LineIdx, Start - 1, Col - Start, hcSymbol);
            end
            else
              Inc(Col); { plain text - no token }
          end;

          xsTag:
          begin
            { Inside a tag — looking for attributes, / or > }
            if S[Col] in [' ', #9] then
              Inc(Col)
            else if S[Col] = '>' then
            begin
              AddToken(LineIdx, Col - 1, 1, hcKeyword1);
              Inc(Col);
              State := xsText;
            end
            else if (S[Col] = '/') and (Col + 1 <= Length(S)) and (S[Col + 1] = '>') then
            begin
              AddToken(LineIdx, Col - 1, 2, hcKeyword1);
              Inc(Col, 2);
              State := xsText;
            end
            else if S[Col] = '=' then
            begin
              AddToken(LineIdx, Col - 1, 1, hcSymbol);
              Inc(Col);
              { Skip whitespace after = }
              while (Col <= Length(S)) and (S[Col] in [' ', #9]) do
                Inc(Col);
              { Start reading quoted value }
              if (Col <= Length(S)) and (S[Col] in ['"', '''']) then
              begin
                QuoteChar := S[Col];
                Start := Col;
                State := xsAttribValue;
                Inc(Col);
              end;
            end
            else
            begin
              { Attribute name }
              Start := Col;
              while (Col <= Length(S)) and not (S[Col] in [' ', #9, '=', '>', '/']) do
                Inc(Col);
              if Col > Start then
                AddToken(LineIdx, Start - 1, Col - Start, hcIdentifier);
            end;
          end;

          xsAttribValue:
          begin
            { Reading quoted attribute value — find closing quote }
            if S[Col] = QuoteChar then
            begin
              AddToken(LineIdx, Start - 1, Col - Start + 1, hcString1);
              Inc(Col);
              State := xsTag;
            end
            else
              Inc(Col);
          end;

          xsComment:
          begin
            { Inside <!-- ... --> — look for --> }
            if (CharsLeft >= 3) and (Ahead(Col, 3) = '-->') then
            begin
              Inc(Col, 3);
              AddToken(LineIdx, Start - 1, Col - Start, hcComment1);
              State := xsText;
            end
            else
              Inc(Col);
          end;

          xsCDATA:
          begin
            { Inside <![CDATA[ ... ]]> — look for ]]> }
            if (CharsLeft >= 3) and (Ahead(Col, 3) = ']]>') then
            begin
              Inc(Col, 3);
              AddToken(LineIdx, Start - 1, Col - Start, hcDirective);
              State := xsText;
            end
            else
              Inc(Col);
          end;

          xsPI:
          begin
            { Inside <? ... ?> — look for ?> }
            if (CharsLeft >= 2) and (Ahead(Col, 2) = '?>') then
            begin
              Inc(Col, 2);
              AddToken(LineIdx, Start - 1, Col - Start, hcDirective);
              State := xsText;
            end
            else
              Inc(Col);
          end;

        end; { case State }
      end; { while Col }

      { Handle multi-line tokens: emit partial token at end of line }
      case State of
        xsComment:
        begin
          AddToken(LineIdx, Start - 1, Length(S) - Start + 1, hcComment1);
          Start := 1; { continue on next line from column 1 }
        end;
        xsCDATA:
        begin
          AddToken(LineIdx, Start - 1, Length(S) - Start + 1, hcDirective);
          Start := 1;
        end;
        xsPI:
        begin
          AddToken(LineIdx, Start - 1, Length(S) - Start + 1, hcDirective);
          Start := 1;
        end;
        xsAttribValue:
        begin
          AddToken(LineIdx, Start - 1, Length(S) - Start + 1, hcString1);
          Start := 1;
        end;
      end;

    end; { for LineIdx }
  finally
    Lines.Free;
  end;
end;

end.
