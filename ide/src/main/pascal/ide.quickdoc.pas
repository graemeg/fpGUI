{
    fpGUI IDE - Quick Documentation

    Copyright (C) 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Extracts quick documentation info (declaration signature, source
      location, doc comment) from a declaration site and displays it
      in a custom hint window.
}
unit ide.quickdoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form;

type

  TQuickDocInfo = record
    Found: Boolean;
    DeclName: string;       // identifier name
    Signature: string;      // full declaration line(s)
    SourceFile: string;     // filename only (for display)
    SourceLine: Integer;    // 1-based line number
    DocComment: string;     // comment text above declaration (empty if none)
  end;

  { TQuickDocHintWindow - lightweight popup form for quick documentation }

  TQuickDocHintWindow = class(TfpgForm)
  private
    FDocInfo: TQuickDocInfo;
    FSignatureFont: TfpgFontResourceBase;
    FLocationFont: TfpgFontResourceBase;
    FCommentFont: TfpgFontResourceBase;
    FTimer: TfpgTimer;
    FTime: Integer;
    procedure TimerFired(Sender: TObject);
  protected
    procedure HandleShow; override;
    procedure HandlePaint; override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
      var consumed: boolean); override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure DoAllocateWindowHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDocInfo(const AInfo: TQuickDocInfo);
    procedure CalcSize;
    property Time: Integer read FTime write FTime;
  end;


{ Extract declaration signature and doc comment from a source file
  at the given declaration line. }
function ExtractQuickDoc(const ADeclFile: string; ADeclLine: Integer;
  const ADeclName: string): TQuickDocInfo;


implementation


{ ---------------------------------------------------------------------------
  ExtractQuickDoc
  --------------------------------------------------------------------------- }

function ExtractQuickDoc(const ADeclFile: string; ADeclLine: Integer;
  const ADeclName: string): TQuickDocInfo;
var
  Lines: TStringList;
  i, LineIdx: Integer;
  Sig, Line, Trimmed: string;
  CommentLines: TStringList;
  InBraceComment: Boolean;
  InParenComment: Boolean;
begin
  Result.Found := False;
  Result.DeclName := ADeclName;
  Result.SourceFile := ExtractFileName(ADeclFile);
  Result.SourceLine := ADeclLine;
  Result.Signature := '';
  Result.DocComment := '';

  if not FileExists(ADeclFile) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(ADeclFile);
    LineIdx := ADeclLine - 1;  // 0-based
    if (LineIdx < 0) or (LineIdx >= Lines.Count) then
      Exit;

    Result.Found := True;

    { --- Extract signature --- }
    Sig := Trim(Lines[LineIdx]);
    i := LineIdx + 1;
    { For class/record/object/interface declarations, the body follows on
      subsequent lines — do not read past the opening declaration line.
      Use a simple heuristic: if the trimmed line contains '= class',
      '= record', '= object', or '= interface' it is a type declaration
      header and we stop immediately. }
    if not (
        (Pos('= class', LowerCase(Sig)) > 0) or
        (Pos('= record', LowerCase(Sig)) > 0) or
        (Pos('= object', LowerCase(Sig)) > 0) or
        (Pos('= interface', LowerCase(Sig)) > 0) or
        (Pos('= packed record', LowerCase(Sig)) > 0)
       ) then
    begin
      { Continue reading if line doesn't end with ; and we haven't hit
        begin/var/const/type or another declaration keyword }
      while (i < Lines.Count) and (Pos(';', Sig) = 0) do
      begin
        Trimmed := Trim(Lines[i]);
        if (Trimmed = '') or
           (CompareText(Copy(Trimmed, 1, 5), 'begin') = 0) or
           (CompareText(Copy(Trimmed, 1, 3), 'var') = 0) or
           (CompareText(Copy(Trimmed, 1, 4), 'type') = 0) or
           (CompareText(Copy(Trimmed, 1, 5), 'const') = 0) then
          Break;
        Sig := Sig + ' ' + Trimmed;
        Inc(i);
      end;
      { Truncate at first semicolon }
      i := Pos(';', Sig);
      if i > 0 then
        Sig := Copy(Sig, 1, i);
    end;
    Result.Signature := Sig;

    { --- Extract doc comment above declaration --- }
    CommentLines := TStringList.Create;
    try
      i := LineIdx - 1;

      { Skip blank lines between comment and declaration }
      while (i >= 0) and (Trim(Lines[i]) = '') do
        Dec(i);

      if i >= 0 then
      begin
        Trimmed := Trim(Lines[i]);

        { Check for // line comment block }
        if Copy(Trimmed, 1, 2) = '//' then
        begin
          while (i >= 0) and (Copy(Trim(Lines[i]), 1, 2) = '//') do
          begin
            Line := Trim(Lines[i]);
            { Strip leading // and optional space }
            Delete(Line, 1, 2);
            if (Length(Line) > 0) and (Line[1] = ' ') then
              Delete(Line, 1, 1);
            CommentLines.Insert(0, Line);
            Dec(i);
          end;
        end
        // Check for closing brace comment
        else if (Length(Trimmed) > 0) and (Trimmed[Length(Trimmed)] = '}') then
        begin
          // Scan backward to find opening brace
          InBraceComment := True;
          while (i >= 0) and InBraceComment do
          begin
            Line := Lines[i];
            if Pos('{', Line) > 0 then
              InBraceComment := False;
            CommentLines.Insert(0, Line);
            Dec(i);
          end;
          // Clean up: strip brace delimiters and trim
          if CommentLines.Count > 0 then
          begin
            // Strip opening brace from first line
            Line := CommentLines[0];
            i := Pos('{', Line);
            if i > 0 then
              CommentLines[0] := Trim(Copy(Line, i + 1, MaxInt));
            // Strip closing brace from last line
            Line := CommentLines[CommentLines.Count - 1];
            i := Pos('}', Line);
            if i > 0 then
              CommentLines[CommentLines.Count - 1] := Trim(Copy(Line, 1, i - 1));
          end;
        end
        { Check for *) closing paren-star comment }
        else if (Length(Trimmed) >= 2) and
                (Trimmed[Length(Trimmed) - 1] = '*') and
                (Trimmed[Length(Trimmed)] = ')') then
        begin
          InParenComment := True;
          while (i >= 0) and InParenComment do
          begin
            Line := Lines[i];
            if Pos('(*', Line) > 0 then
              InParenComment := False;
            CommentLines.Insert(0, Line);
            Dec(i);
          end;
          { Strip (* *) delimiters }
          if CommentLines.Count > 0 then
          begin
            Line := CommentLines[0];
            i := Pos('(*', Line);
            if i > 0 then
              CommentLines[0] := Trim(Copy(Line, i + 2, MaxInt));
            Line := CommentLines[CommentLines.Count - 1];
            i := Pos('*)', Line);
            if i > 0 then
              CommentLines[CommentLines.Count - 1] := Trim(Copy(Line, 1, i - 1));
          end;
        end;
      end;

      { Build comment string }
      if CommentLines.Count > 0 then
      begin
        { Trim empty leading/trailing lines }
        while (CommentLines.Count > 0) and (Trim(CommentLines[0]) = '') do
          CommentLines.Delete(0);
        while (CommentLines.Count > 0) and
              (Trim(CommentLines[CommentLines.Count - 1]) = '') do
          CommentLines.Delete(CommentLines.Count - 1);
        Result.DocComment := Trim(CommentLines.Text);
      end;
    finally
      CommentLines.Free;
    end;
  finally
    Lines.Free;
  end;
end;


{ ---------------------------------------------------------------------------
  TQuickDocHintWindow
  --------------------------------------------------------------------------- }

constructor TQuickDocHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowPosition := wpUser;
  Sizeable := False;
  BackgroundColor := clHintWindow;
  FSignatureFont := fpgApplication.FontManager.GetFont(FPG_DEFAULT_FIXED + '-10');
  FLocationFont := fpgApplication.FontManager.GetFont(FPG_DEFAULT_SANS + '-8');
  FCommentFont := fpgApplication.FontManager.GetFont(FPG_DEFAULT_SANS + '-9');
  FTime := 10000;
  FTimer := TfpgTimer.Create(FTime);
  FTimer.OnTimer := @TimerFired;
end;

destructor TQuickDocHintWindow.Destroy;
begin
  FTimer.Free;
  FTimer := nil;
  FSignatureFont := nil;
  FLocationFont := nil;
  FCommentFont := nil;
  inherited Destroy;
end;

procedure TQuickDocHintWindow.TimerFired(Sender: TObject);
begin
  FTimer.Enabled := False;
  Hide;
end;

procedure TQuickDocHintWindow.DoAllocateWindowHandle;
begin
  inherited DoAllocateWindowHandle;
  Window.WindowType := wtPopup;
end;

procedure TQuickDocHintWindow.HandleShow;
begin
  inherited HandleShow;
  FTimer.Interval := FTime;
  FTimer.Enabled := True;
end;

procedure TQuickDocHintWindow.HandlePaint;
var
  X, Y: Integer;
  CommentLines: TStringList;
  i: Integer;
  LocationText: string;
const
  cMargin = 6;
begin
  inherited HandlePaint;

  // Border
  Canvas.SetColor(clShadow1);
  Canvas.DrawRectangle(0, 0, ActualWidth, ActualHeight);

  X := cMargin;
  Y := cMargin;

  // Signature in monospace font
  Canvas.SetFont(FSignatureFont);
  Canvas.SetTextColor(clBlack);
  Canvas.DrawString(X, Y, FDocInfo.Signature);
  Y := Y + FSignatureFont.GetHeight + 4;

  // Separator line
  Canvas.SetColor(clShadow1);
  Canvas.DrawLine(X, Y + 2, ActualWidth - cMargin, Y + 2);
  Y := Y + 6;

  // Source file and line in grey
  Canvas.SetFont(FLocationFont);
  Canvas.SetTextColor(clShadow1);
  LocationText := FDocInfo.SourceFile + ':' + IntToStr(FDocInfo.SourceLine);
  Canvas.DrawString(X, Y, LocationText);
  Y := Y + FLocationFont.GetHeight + 2;

  // Doc comment (if present)
  if FDocInfo.DocComment <> '' then
  begin
    Y := Y + 4;
    Canvas.SetFont(FCommentFont);
    Canvas.SetTextColor(clText1);
    CommentLines := TStringList.Create;
    try
      CommentLines.Text := FDocInfo.DocComment;
      for i := 0 to CommentLines.Count - 1 do
      begin
        Canvas.DrawString(X, Y, CommentLines[i]);
        Y := Y + FCommentFont.GetHeight;
      end;
    finally
      CommentLines.Free;
    end;
  end;
end;

procedure TQuickDocHintWindow.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  Hide;
  consumed := True;
end;

procedure TQuickDocHintWindow.HandleLMouseDown(x, y: integer;
  shiftstate: TShiftState);
begin
  Hide;
end;

procedure TQuickDocHintWindow.SetDocInfo(const AInfo: TQuickDocInfo);
begin
  FDocInfo := AInfo;
end;

procedure TQuickDocHintWindow.CalcSize;
var
  W, H, TW: Integer;
  CommentLines: TStringList;
  i: Integer;
const
  cMargin = 6;
begin
  W := 0;
  H := cMargin * 2;

  // Signature line
  TW := FSignatureFont.GetTextWidth(FDocInfo.Signature);
  if TW > W then
    W := TW;
  H := H + FSignatureFont.GetHeight + 4;

  // Separator
  H := H + 6;

  // Source file:line
  TW := FLocationFont.GetTextWidth(FDocInfo.SourceFile + ':' + IntToStr(FDocInfo.SourceLine));
  if TW > W then
    W := TW;
  H := H + FLocationFont.GetHeight + 2;

  // Doc comment
  if FDocInfo.DocComment <> '' then
  begin
    H := H + 4;
    CommentLines := TStringList.Create;
    try
      CommentLines.Text := FDocInfo.DocComment;
      for i := 0 to CommentLines.Count - 1 do
      begin
        TW := FCommentFont.GetTextWidth(CommentLines[i]);
        if TW > W then
          W := TW;
        H := H + FCommentFont.GetHeight;
      end;
    finally
      CommentLines.Free;
    end;
  end;

  Width := W + (cMargin * 2) + 2;
  Height := H + 2;
end;

end.
