{
    fpGUI IDE - Find Usages Dialog

    Copyright (C) 2026 by Graeme Geldenhuys

    Modal dialog displaying all usages of an identifier across
    project source files. Results are grouped by file. Press
    Enter or double-click to navigate, Esc to dismiss.
}
unit ide.form.findusages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_grid, fpg_basegrid, fpg_panel,
  ide.findusages;

type

  TFindUsagesResult = record
    FullPath: string;
    Line: Integer;
    Column: Integer;      // 1-based column of identifier
  end;

  { Row map entry: maps a grid row to either a group header or a usage entry }
  TRowMapEntry = record
    IsGroupHeader: Boolean;
    GroupIndex: Integer;    // index into FGroups
    EntryIndex: Integer;    // index into group's Entries (ignored if header)
  end;
  TRowMapArray = array of TRowMapEntry;

  { TFindUsagesForm }

  TFindUsagesForm = class(TfpgForm)
  private
    grdUsages: TfpgStringGrid;
    StatusBar: TfpgPanel;
    FGroups: TUsageGroupArray;
    FRowMap: TRowMapArray;
    FResult: TFindUsagesResult;
    FTotalCount: Integer;
    FIdent: string;
    FIdentLen: Integer;
    procedure GridKeyPressed(Sender: TObject; var KeyCode: word;
      var ShiftState: TShiftState; var Consumed: boolean);
    procedure GridDoubleClicked(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; const AMousePos: TPoint);
    procedure GridDrawCell(Sender: TObject; const ARow, ACol: Integer;
      const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
      var ADefaultDrawing: boolean);
    procedure SelectCurrentAndClose;
    procedure FillGrid;
  public
    procedure AfterCreate; override;
    property Groups: TUsageGroupArray read FGroups write FGroups;
    property TotalCount: Integer read FTotalCount write FTotalCount;
    property SelectedResult: TFindUsagesResult read FResult;
  end;

{ Show the find usages dialog. Returns the selected result with file path
  and line, or empty path if cancelled. }
function DisplayFindUsages(const AIdent: string;
  const AGroups: TUsageGroupArray; ATotalCount: Integer): TFindUsagesResult;


implementation

uses
  fpg_widget;


function DisplayFindUsages(const AIdent: string;
  const AGroups: TUsageGroupArray; ATotalCount: Integer): TFindUsagesResult;
var
  frm: TFindUsagesForm;
begin
  Result.FullPath := '';
  Result.Line := 0;
  frm := TFindUsagesForm.Create(nil);
  try
    frm.WindowTitle := 'Usages of ''' + AIdent + '''';
    frm.FIdent := AIdent;
    frm.FIdentLen := Length(AIdent);
    frm.Groups := AGroups;
    frm.TotalCount := ATotalCount;
    frm.FillGrid;
    frm.ShowModal;
    Result := frm.SelectedResult;
  finally
    frm.Free;
  end;
end;


{ TFindUsagesForm }

procedure TFindUsagesForm.GridKeyPressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    keyEnter:
      begin
        SelectCurrentAndClose;
        Consumed := True;
      end;
    keyEscape:
      begin
        FResult.FullPath := '';
        FResult.Line := 0;
        Close;
        Consumed := True;
      end;
  end;
end;

procedure TFindUsagesForm.GridDoubleClicked(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  SelectCurrentAndClose;
end;

procedure TFindUsagesForm.GridDrawCell(Sender: TObject; const ARow,
  ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
  var ADefaultDrawing: boolean);
var
  r: TfpgRect;
  txt, seg: string;
  IsSelected: boolean;
  BgColor, FgColor, HighlightBg, HighlightFg, NormalFg: TfpgColor;
  x, MatchPos: Integer;
begin
  if (ARow < 0) or (ARow > High(FRowMap)) then
    Exit;

  IsSelected := (ARow = grdUsages.FocusRow);

  { --- Group header rows: grey background across all columns --- }
  if FRowMap[ARow].IsGroupHeader then
  begin
    ADefaultDrawing := False;
    if IsSelected then
    begin
      BgColor := clGridSelection;
      FgColor := clGridSelectionText;
    end
    else
    begin
      BgColor := TfpgColor($FFE8E8E8);
      FgColor := clText1;
    end;
    grdUsages.Canvas.SetColor(BgColor);
    grdUsages.Canvas.FillRectangle(ARect);
    if ACol = 0 then
    begin
      grdUsages.Canvas.SetTextColor(FgColor);
      grdUsages.Canvas.DrawString(ARect.Left + 4,
        ARect.Top + (ARect.Height - grdUsages.Canvas.Font.GetHeight) div 2,
        grdUsages.Cells[0, ARow]);
    end;
    Exit;
  end;

  { --- Context column (col 2): highlight the identifier --- }
  if ACol <> 2 then
    Exit;

  txt := grdUsages.Cells[2, ARow];
  if Length(txt) = 0 then
    Exit;

  { Find the identifier in the context text (case-insensitive) }
  MatchPos := Pos(UpperCase(FIdent), UpperCase(txt));
  if MatchPos = 0 then
    Exit;  // no match found, let default drawing handle it

  ADefaultDrawing := False;

  { Colour scheme matching symbol finder }
  if IsSelected then
  begin
    NormalFg := clGridSelectionText;
    HighlightBg := TfpgColor($FF1E5A9E);
    HighlightFg := clGridSelectionText;
    BgColor := clGridSelection;
  end
  else
  begin
    NormalFg := clText1;
    HighlightBg := TfpgColor($FFFFFFA0);
    HighlightFg := clText1;
    BgColor := grdUsages.BackgroundColor;
  end;

  { Fill row background }
  grdUsages.Canvas.SetColor(BgColor);
  grdUsages.Canvas.FillRectangle(ARect);

  r := ARect;
  r.Left := r.Left + 2;
  x := r.Left;

  { Draw text before the match }
  if MatchPos > 1 then
  begin
    seg := Copy(txt, 1, MatchPos - 1);
    grdUsages.Canvas.SetTextColor(NormalFg);
    grdUsages.Canvas.DrawString(x,
      r.Top + (r.Height - grdUsages.Canvas.Font.GetHeight) div 2, seg);
    Inc(x, grdUsages.Canvas.Font.GetTextWidth(seg));
  end;

  { Draw the highlighted identifier }
  seg := Copy(txt, MatchPos, FIdentLen);
  grdUsages.Canvas.SetColor(HighlightBg);
  grdUsages.Canvas.FillRectangle(x, r.Top,
    grdUsages.Canvas.Font.GetTextWidth(seg), r.Height);
  grdUsages.Canvas.SetTextColor(HighlightFg);
  grdUsages.Canvas.DrawString(x,
    r.Top + (r.Height - grdUsages.Canvas.Font.GetHeight) div 2, seg);
  Inc(x, grdUsages.Canvas.Font.GetTextWidth(seg));

  { Draw text after the match }
  if MatchPos + FIdentLen <= Length(txt) then
  begin
    seg := Copy(txt, MatchPos + FIdentLen, MaxInt);
    grdUsages.Canvas.SetTextColor(NormalFg);
    grdUsages.Canvas.DrawString(x,
      r.Top + (r.Height - grdUsages.Canvas.Font.GetHeight) div 2, seg);
  end;
end;

procedure TFindUsagesForm.SelectCurrentAndClose;
var
  Row: Integer;
  rme: TRowMapEntry;
  grp: TUsageGroup;
begin
  Row := grdUsages.FocusRow;
  if (Row >= 0) and (Row <= High(FRowMap)) then
  begin
    rme := FRowMap[Row];
    if rme.IsGroupHeader then
    begin
      { Clicking a group header opens the file at line 1 }
      grp := FGroups[rme.GroupIndex];
      FResult.FullPath := grp.FullPath;
      FResult.Line := 1;
      FResult.Column := 1;
    end
    else
    begin
      grp := FGroups[rme.GroupIndex];
      FResult.FullPath := grp.Entries[rme.EntryIndex].FullPath;
      FResult.Line := grp.Entries[rme.EntryIndex].Line;
      FResult.Column := grp.Entries[rme.EntryIndex].Column;
    end;
  end
  else
  begin
    FResult.FullPath := '';
    FResult.Line := 0;
    FResult.Column := 1;
  end;
  Close;
end;

procedure TFindUsagesForm.FillGrid;
var
  g, e, Row: Integer;
  TotalRows: Integer;
  grp: TUsageGroup;
begin
  { Count total rows: one header per group + entries }
  TotalRows := 0;
  for g := 0 to High(FGroups) do
    TotalRows := TotalRows + 1 + Length(FGroups[g].Entries);

  SetLength(FRowMap, TotalRows);

  grdUsages.BeginUpdate;
  try
    grdUsages.RowCount := TotalRows;
    Row := 0;
    for g := 0 to High(FGroups) do
    begin
      grp := FGroups[g];

      { Group header row }
      FRowMap[Row].IsGroupHeader := True;
      FRowMap[Row].GroupIndex := g;
      FRowMap[Row].EntryIndex := -1;
      grdUsages.Cells[0, Row] := grp.RelativePath + grp.FileName +
        ' (' + IntToStr(Length(grp.Entries)) + ')';
      grdUsages.Cells[1, Row] := '';
      grdUsages.Cells[2, Row] := '';
      Inc(Row);

      { Usage entry rows }
      for e := 0 to High(grp.Entries) do
      begin
        FRowMap[Row].IsGroupHeader := False;
        FRowMap[Row].GroupIndex := g;
        FRowMap[Row].EntryIndex := e;
        grdUsages.Cells[0, Row] := '';
        grdUsages.Cells[1, Row] := IntToStr(grp.Entries[e].Line);
        grdUsages.Cells[2, Row] := grp.Entries[e].LineText;
        Inc(Row);
      end;
    end;

    { Focus the first usage entry (skip first group header) }
    if (TotalRows > 1) then
      grdUsages.FocusRow := 1
    else if TotalRows > 0 then
      grdUsages.FocusRow := 0;
  finally
    grdUsages.EndUpdate;
  end;

  StatusBar.Text := Format('%d usages in %d files',
    [FTotalCount, Length(FGroups)]);
end;

procedure TFindUsagesForm.AfterCreate;
begin
  Name := 'FindUsagesForm';
  SetPosition(300, 200, 700, 450);
  WindowTitle := 'Find Usages';
  Hint := '';
  WindowPosition := wpMainFormCenter;

  grdUsages := TfpgStringGrid.Create(self);
  with grdUsages do
  begin
    Name := 'grdUsages';
    SetPosition(4, 4, 692, 418);
    Anchors := [anLeft, anRight, anTop, anBottom];
    BackgroundColor := TfpgColor($80000002);
    AddColumn('File', 200, taLeftJustify);
    AddColumn('Line', 60, taRightJustify);
    AddColumn('Context', 420, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := True;
    TabOrder := 1;
    HeaderStyle := ghsFlat;
    OnKeyPress := @GridKeyPressed;
    OnDoubleClick := @GridDoubleClicked;
    OnDrawCell := @GridDrawCell;
  end;

  StatusBar := TfpgPanel.Create(self);
  with StatusBar do
  begin
    Name := 'StatusBar';
    SetPosition(0, 426, 700, 24);
    Align := alBottom;
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := '';
  end;
end;

end.
