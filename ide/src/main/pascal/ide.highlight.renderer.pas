{
    fpGUI IDE - Highlight Renderer

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Extracted from ide.form.main.pas — separates highlight rendering logic
      from the main form.

      BuildRenderSegments is a pure function: given a highlighter's token
      output, a theme, and bracket match state, it produces an array of
      TRenderSegment records describing what to draw. PaintSegments then
      renders those segments onto a canvas.

      THighlighterCache manages the per-language highlighter instances and
      tracks which editor was last tokenised to avoid redundant work.
}
unit ide.highlight.renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, regexpr,
  fpg_base, fpg_main,
  ide.highlighter, ide.highlighter.ini, ide.highlighter.xml,
  ide.editor.theme, ide.bracketmatch;

type

  { Describes one rendered segment: position, text, colours, style }
  TRenderSegment = record
    Column: Integer;       // 0-based column
    Length: Integer;
    Text: TfpgString;
    Foreground: TfpgColor;
    Background: TfpgColor;
    Style: TTokenStyleFlags;
  end;
  TRenderSegmentArray = array of TRenderSegment;


  { THighlighterCache - manages highlighter instances and editor tracking }

  THighlighterCache = class(TObject)
  private
    FPascalHighlighter: TPascalHighlighter;
    FPascalEditor: TObject;
    FINIHighlighter: TINIHighlighter;
    FINIEditor: TObject;
    FXMLHighlighter: TXMLHighlighter;
    FXMLEditor: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    { Tokenise AEditor's content if it differs from the cached editor }
    procedure EnsurePascalTokenised(AEditor: TObject; ALines: TStrings);
    procedure EnsureINITokenised(AEditor: TObject; ALines: TStrings);
    procedure EnsureXMLTokenised(AEditor: TObject; ALines: TStrings);
    { Clear cached editor reference (call when a tab closes) }
    procedure InvalidateEditor(AEditor: TObject);
    property PascalHighlighter: TPascalHighlighter read FPascalHighlighter;
    property PascalEditor: TObject read FPascalEditor;
    property INIHighlighter: TINIHighlighter read FINIHighlighter;
    property INIEditor: TObject read FINIEditor;
    property XMLHighlighter: TXMLHighlighter read FXMLHighlighter;
    property XMLEditor: TObject read FXMLEditor;
  end;


{ Pure function: given tokens + theme + bracket match, produce rendering plan.
  Returns an empty array for empty lines. Returns a single plain-text segment
  when no tokens exist for the given line index. }
function BuildRenderSegments(
  AHighlighter: TEditorHighlighter;
  const ALineText: TfpgString;
  ALineIndex: Integer;
  const ATheme: TEditorTheme;
  const ABracketMatch: TBracketMatchResult;
  AShowBracketMatch: Boolean
): TRenderSegmentArray;

{ Pure function: patch/diff highlighting via regex.
  Returns segments with appropriate colours for added/removed/hunk lines. }
function BuildPatchRenderSegments(
  const ALineText: TfpgString;
  const ATheme: TEditorTheme
): TRenderSegmentArray;

{ Draws pre-computed segments onto a canvas. }
procedure PaintSegments(
  const ASegments: TRenderSegmentArray;
  const ALineText: TfpgString;
  AFontWidth: Integer;
  ACanvas: TfpgCanvas;
  ATextRect: TfpgRect;
  const ATheme: TEditorTheme;
  const AEditorFontDesc: string
);

{ Draws the trailing area after the last segment. }
procedure PaintTrailingGap(
  ALastCol: Integer;
  const ALineText: TfpgString;
  AFontWidth: Integer;
  ACanvas: TfpgCanvas;
  ATextRect: TfpgRect;
  const ATheme: TEditorTheme
);


implementation

{ -------------------------------------------------------------------------- }
{ THighlighterCache                                                          }
{ -------------------------------------------------------------------------- }

constructor THighlighterCache.Create;
begin
  inherited Create;
  FPascalHighlighter := TPascalHighlighter.Create;
  FPascalEditor := nil;
  FINIHighlighter := TINIHighlighter.Create;
  FINIEditor := nil;
  FXMLHighlighter := TXMLHighlighter.Create;
  FXMLEditor := nil;
end;

destructor THighlighterCache.Destroy;
begin
  FreeAndNil(FPascalHighlighter);
  FreeAndNil(FINIHighlighter);
  FreeAndNil(FXMLHighlighter);
  inherited Destroy;
end;

procedure THighlighterCache.EnsurePascalTokenised(AEditor: TObject; ALines: TStrings);
begin
  if (AEditor <> FPascalEditor) or (AEditor = nil) then
  begin
    FPascalEditor := AEditor;
    if Assigned(ALines) then
      FPascalHighlighter.Tokenise(ALines.Text);
  end;
end;

procedure THighlighterCache.EnsureINITokenised(AEditor: TObject; ALines: TStrings);
begin
  if (AEditor <> FINIEditor) or (AEditor = nil) then
  begin
    FINIEditor := AEditor;
    if Assigned(ALines) then
      FINIHighlighter.Tokenise(ALines.Text);
  end;
end;

procedure THighlighterCache.EnsureXMLTokenised(AEditor: TObject; ALines: TStrings);
begin
  if (AEditor <> FXMLEditor) or (AEditor = nil) then
  begin
    FXMLEditor := AEditor;
    if Assigned(ALines) then
      FXMLHighlighter.Tokenise(ALines.Text);
  end;
end;

procedure THighlighterCache.InvalidateEditor(AEditor: TObject);
begin
  if (AEditor = nil) or (AEditor = FPascalEditor) then
    FPascalEditor := nil;
  if (AEditor = nil) or (AEditor = FINIEditor) then
    FINIEditor := nil;
  if (AEditor = nil) or (AEditor = FXMLEditor) then
    FXMLEditor := nil;
end;

{ -------------------------------------------------------------------------- }
{ BuildRenderSegments                                                        }
{ -------------------------------------------------------------------------- }

function BuildRenderSegments(
  AHighlighter: TEditorHighlighter;
  const ALineText: TfpgString;
  ALineIndex: Integer;
  const ATheme: TEditorTheme;
  const ABracketMatch: TBracketMatchResult;
  AShowBracketMatch: Boolean
): TRenderSegmentArray;
var
  tokens: THighlightTokenArray;
  tok: THighlightToken;
  ts: TTokenStyle;
  seg: TRenderSegment;
  i, lLastCol, cnt: Integer;
  fg, bg: TfpgColor;
  gapText: TfpgString;
begin
  Result := nil;

  if not Assigned(AHighlighter) then
    Exit;

  if ALineText = '' then
    Exit;

  tokens := AHighlighter.GetLineTokens(ALineIndex);

  if tokens = nil then
  begin
    { No tokens for this line — return a single plain-text segment }
    SetLength(Result, 1);
    Result[0].Column := 0;
    Result[0].Length := System.Length(ALineText);
    Result[0].Text := ALineText;
    Result[0].Foreground := ATheme.Chrome.Foreground;
    Result[0].Background := ATheme.Chrome.Background;
    Result[0].Style := [];
    Exit;
  end;

  { Pre-allocate: at most 2*tokens (gap + token per entry) + 1 trailing }
  SetLength(Result, System.Length(tokens) * 2 + 1);
  cnt := 0;
  lLastCol := 0;

  for i := 0 to System.Length(tokens) - 1 do
  begin
    tok := tokens[i];

    { Extract token text }
    seg.Text := Copy(ALineText, tok.Column + 1, tok.Length);
    if seg.Text = '' then
      Continue;

    { Fill any gap before this token }
    if tok.Column > lLastCol then
    begin
      gapText := Copy(ALineText, lLastCol + 1, tok.Column - lLastCol);
      Result[cnt].Column := lLastCol;
      Result[cnt].Length := tok.Column - lLastCol;
      Result[cnt].Text := gapText;
      Result[cnt].Foreground := ATheme.Chrome.Foreground;
      Result[cnt].Background := ATheme.Chrome.Background;
      Result[cnt].Style := [];
      Inc(cnt);
    end;

    { Determine style for this token }
    ts := ATheme.TokenStyles[tok.Category];

    if ts.Foreground <> clNone then
      fg := ts.Foreground
    else
      fg := ATheme.Chrome.Foreground;

    if ts.Background <> clNone then
      bg := ts.Background
    else
      bg := ATheme.Chrome.Background;

    { Bracket match highlight }
    if AShowBracketMatch and ABracketMatch.Found then
    begin
      if (ALineIndex = ABracketMatch.SourceLine) and (tok.Column = ABracketMatch.SourceCol) then
        bg := ATheme.Chrome.BracketMatch;
      if (ALineIndex = ABracketMatch.MatchLine) and (tok.Column = ABracketMatch.MatchCol) then
        bg := ATheme.Chrome.BracketMatch;
    end;

    Result[cnt].Column := tok.Column;
    Result[cnt].Length := tok.Length;
    Result[cnt].Text := seg.Text;
    Result[cnt].Foreground := fg;
    Result[cnt].Background := bg;
    Result[cnt].Style := ts.Style;
    Inc(cnt);

    lLastCol := tok.Column + tok.Length;
  end;

  { Trailing text after last token }
  if lLastCol < System.Length(ALineText) then
  begin
    Result[cnt].Column := lLastCol;
    Result[cnt].Length := System.Length(ALineText) - lLastCol;
    Result[cnt].Text := Copy(ALineText, lLastCol + 1, System.Length(ALineText) - lLastCol);
    Result[cnt].Foreground := ATheme.Chrome.Foreground;
    Result[cnt].Background := ATheme.Chrome.Background;
    Result[cnt].Style := [];
    Inc(cnt);
  end;

  SetLength(Result, cnt);
end;

{ -------------------------------------------------------------------------- }
{ BuildPatchRenderSegments                                                   }
{ -------------------------------------------------------------------------- }

function BuildPatchRenderSegments(
  const ALineText: TfpgString;
  const ATheme: TEditorTheme
): TRenderSegmentArray;
const
  cRemovedLines = '^(-[^-]|\<|!).*';
  cAddedLines = '^(\+[^\+]|\>).*';
  cLeftFile = '^--- .*';
  cRightFile = '^(\+\+\+|\*\*\*) .*';
  cHunk = '^\@\@.*';
  cStartOfFile = '^(diff|index) .*';
var
  re: TRegExpr;
  fg, bg: TfpgColor;
begin
  if ALineText = '' then
  begin
    Result := nil;
    Exit;
  end;

  fg := ATheme.Chrome.Foreground;
  bg := ATheme.Chrome.Background;

  re := TRegExpr.Create;
  try
    re.Expression := cRemovedLines;
    if re.Exec(ALineText) then
    begin
      fg := clRed;
    end
    else
    begin
      re.Expression := cAddedLines;
      if re.Exec(ALineText) then
        fg := clGreen
      else
      begin
        re.Expression := cLeftFile;
        if re.Exec(ALineText) then
          fg := clMagenta
        else
        begin
          re.Expression := cRightFile;
          if re.Exec(ALineText) then
            fg := clMagenta
          else
          begin
            re.Expression := cHunk;
            if re.Exec(ALineText) then
              fg := clBlue
            else
            begin
              re.Expression := cStartOfFile;
              if re.Exec(ALineText) then
              begin
                fg := ATheme.Chrome.Foreground;
                bg := clSilver;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    re.Free;
  end;

  SetLength(Result, 1);
  Result[0].Column := 0;
  Result[0].Length := System.Length(ALineText);
  Result[0].Text := ALineText;
  Result[0].Foreground := fg;
  Result[0].Background := bg;
  Result[0].Style := [];
end;

{ -------------------------------------------------------------------------- }
{ PaintSegments                                                              }
{ -------------------------------------------------------------------------- }

procedure PaintSegments(
  const ASegments: TRenderSegmentArray;
  const ALineText: TfpgString;
  AFontWidth: Integer;
  ACanvas: TfpgCanvas;
  ATextRect: TfpgRect;
  const ATheme: TEditorTheme;
  const AEditorFontDesc: string
);
var
  oldfont: TfpgFontResourceBase;
  seg: TRenderSegment;
  r: TfpgRect;
  lFontDesc: string;
  lNeedFont: Boolean;
  i: Integer;
begin
  oldfont := TfpgFontResourceBase(ACanvas.Font);

  for i := 0 to System.Length(ASegments) - 1 do
  begin
    seg := ASegments[i];

    { Apply font style if needed }
    lNeedFont := seg.Style <> [];
    if lNeedFont then
    begin
      lFontDesc := AEditorFontDesc;
      if tsfBold in seg.Style then
        lFontDesc := lFontDesc + ':bold';
      if tsfItalic in seg.Style then
        lFontDesc := lFontDesc + ':italic';
      ACanvas.SetFont(fpgApplication.FontManager.GetFont(lFontDesc));
    end;

    r.SetRect(ATextRect.Left + (AFontWidth * seg.Column), ATextRect.Top,
        (AFontWidth * seg.Length), ATextRect.Height);

    ACanvas.Color := seg.Background;
    ACanvas.TextColor := seg.Foreground;
    ACanvas.FillRectangle(r);
    ACanvas.DrawString(r.Left, r.Top, seg.Text);

    if lNeedFont then
      ACanvas.SetFont(oldfont);
  end;
end;

procedure PaintTrailingGap(
  ALastCol: Integer;
  const ALineText: TfpgString;
  AFontWidth: Integer;
  ACanvas: TfpgCanvas;
  ATextRect: TfpgRect;
  const ATheme: TEditorTheme
);
var
  r: TfpgRect;
begin
  if ALastCol * AFontWidth < ATextRect.Width then
  begin
    r.SetRect(ATextRect.Left + (AFontWidth * ALastCol), ATextRect.Top,
        ATextRect.Width - (AFontWidth * ALastCol), ATextRect.Height);
    ACanvas.Color := ATheme.Chrome.Background;
    ACanvas.FillRectangle(r);
    if ALastCol < System.Length(ALineText) then
    begin
      ACanvas.TextColor := ATheme.Chrome.Foreground;
      ACanvas.DrawString(r.Left, r.Top,
          Copy(ALineText, ALastCol + 1, System.Length(ALineText) - ALastCol));
    end;
  end;
end;

end.
