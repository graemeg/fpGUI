{
    fpGUI IDE - Navigate to Symbol Dialog

    Copyright (C) 2026 by Graeme Geldenhuys

    Modal dialog for fuzzy symbol search (Ctrl+N).
    Type a few characters to filter project symbols, then
    press Enter to open the file at the symbol's declaration.
}
unit ide.form.symbolfinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_edit, fpg_grid,
  fpg_basegrid, fpg_panel,
  ide.filefinder, ide.symbolfinder;

type

  TSymbolFinderResult = record
    FullPath: string;
    Line: Integer;
  end;

  { TSymbolFinderForm }

  TSymbolFinderForm = class(TfpgForm)
  private
    edtSearch: TfpgEdit;
    grdSymbols: TfpgStringGrid;
    StatusBar: TfpgPanel;
    FSymbols: TSymbolEntryArray;
    FFiltered: TFilteredSymbolArray;
    FResult: TSymbolFinderResult;
    procedure SearchTextChanged(Sender: TObject);
    procedure SearchEditKeyPressed(Sender: TObject; var KeyCode: word;
      var ShiftState: TShiftState; var Consumed: boolean);
    procedure GridDoubleClicked(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; const AMousePos: TPoint);
    procedure GridDrawCell(Sender: TObject; const ARow, ACol: Integer;
      const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
      var ADefaultDrawing: boolean);
    procedure FillGrid;
    procedure SelectCurrentAndClose;
  public
    procedure AfterCreate; override;
    property Symbols: TSymbolEntryArray read FSymbols write FSymbols;
    property SelectedResult: TSymbolFinderResult read FResult;
  end;

{ Show the symbol finder dialog. Returns the result with file path and line,
  or empty path if cancelled. }
function DisplaySymbolFinder(const ASymbols: TSymbolEntryArray): TSymbolFinderResult;


implementation

uses
  fpg_widget;

function DisplaySymbolFinder(const ASymbols: TSymbolEntryArray): TSymbolFinderResult;
var
  frm: TSymbolFinderForm;
begin
  Result.FullPath := '';
  Result.Line := 0;
  frm := TSymbolFinderForm.Create(nil);
  try
    frm.Symbols := ASymbols;
    frm.FillGrid;
    frm.ShowModal;
    Result := frm.SelectedResult;
  finally
    frm.Free;
  end;
end;


{ TSymbolFinderForm }

procedure TSymbolFinderForm.SearchTextChanged(Sender: TObject);
begin
  FillGrid;
end;

procedure TSymbolFinderForm.SearchEditKeyPressed(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    keyUp:
      begin
        if grdSymbols.FocusRow > 0 then
          grdSymbols.FocusRow := grdSymbols.FocusRow - 1;
        Consumed := True;
      end;
    keyDown:
      begin
        if grdSymbols.FocusRow < grdSymbols.RowCount - 1 then
          grdSymbols.FocusRow := grdSymbols.FocusRow + 1;
        Consumed := True;
      end;
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

procedure TSymbolFinderForm.GridDoubleClicked(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  SelectCurrentAndClose;
end;

procedure TSymbolFinderForm.GridDrawCell(Sender: TObject; const ARow,
  ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState;
  var ADefaultDrawing: boolean);
var
  SymName: string;
  Ranges: TMatchRangeArray;
  r: TfpgRect;
  x, i, j, charStart, charEnd: Integer;
  seg: string;
  IsSelected: boolean;
  HighlightBg, NormalFg, HighlightFg: TfpgColor;
  InHighlight: array of boolean;
begin
  { Only custom-draw the Symbol column (column 0) when we have match ranges }
  if (ACol <> 0) or (ARow < 0) or (ARow > High(FFiltered)) then
    Exit;

  Ranges := FFiltered[ARow].MatchRanges;
  if Length(Ranges) = 0 then
    Exit;

  ADefaultDrawing := False;
  SymName := FFiltered[ARow].Entry.Name;
  if Length(SymName) = 0 then
    Exit;

  IsSelected := (ARow = grdSymbols.FocusRow);

  if IsSelected then
  begin
    NormalFg := clGridSelectionText;
    HighlightBg := TfpgColor($FF1E5A9E);
    HighlightFg := clGridSelectionText;
  end
  else
  begin
    NormalFg := clText1;
    HighlightBg := TfpgColor($FFFFFFA0);
    HighlightFg := clText1;
  end;

  { Build per-character highlight map }
  SetLength(InHighlight, Length(SymName) + 1);
  for i := 0 to High(InHighlight) do
    InHighlight[i] := False;
  for i := 0 to High(Ranges) do
    for j := Ranges[i].Start to Ranges[i].Start + Ranges[i].Length - 1 do
      if (j >= 1) and (j <= Length(SymName)) then
        InHighlight[j] := True;

  r := ARect;
  r.Left := r.Left + 2;
  x := r.Left;

  charStart := 1;
  while charStart <= Length(SymName) do
  begin
    charEnd := charStart;
    while (charEnd < Length(SymName)) and
          (InHighlight[charEnd + 1] = InHighlight[charStart]) do
      Inc(charEnd);

    seg := Copy(SymName, charStart, charEnd - charStart + 1);

    if InHighlight[charStart] then
    begin
      grdSymbols.Canvas.SetColor(HighlightBg);
      grdSymbols.Canvas.FillRectangle(x, r.Top,
        grdSymbols.Canvas.Font.GetTextWidth(seg), r.Height);
      grdSymbols.Canvas.SetTextColor(HighlightFg);
    end
    else
      grdSymbols.Canvas.SetTextColor(NormalFg);

    grdSymbols.Canvas.DrawString(x,
      r.Top + (r.Height - grdSymbols.Canvas.Font.GetHeight) div 2, seg);
    Inc(x, grdSymbols.Canvas.Font.GetTextWidth(seg));

    charStart := charEnd + 1;
  end;
end;

procedure TSymbolFinderForm.FillGrid;
var
  i: Integer;
  Total: Integer;
begin
  Total := Length(FSymbols);

  if Length(edtSearch.Text) = 0 then
  begin
    SetLength(FFiltered, Total);
    for i := 0 to Total - 1 do
    begin
      FFiltered[i].Entry := FSymbols[i];
      FFiltered[i].Score := 0;
      SetLength(FFiltered[i].MatchRanges, 0);
    end;
  end
  else
    FFiltered := FilterSymbolsEx(edtSearch.Text, FSymbols);

  grdSymbols.BeginUpdate;
  try
    grdSymbols.RowCount := Length(FFiltered);
    for i := 0 to High(FFiltered) do
    begin
      grdSymbols.Cells[0, i] := FFiltered[i].Entry.Name;
      grdSymbols.Cells[1, i] := SymbolKindToStr(FFiltered[i].Entry.Kind);
      grdSymbols.Cells[2, i] := FFiltered[i].Entry.FileName;
    end;
    if grdSymbols.RowCount > 0 then
      grdSymbols.FocusRow := 0;
  finally
    grdSymbols.EndUpdate;
  end;

  if Length(edtSearch.Text) = 0 then
    StatusBar.Text := Format('%d symbols', [Total])
  else
    StatusBar.Text := Format('%d of %d symbols', [Length(FFiltered), Total]);
end;

procedure TSymbolFinderForm.SelectCurrentAndClose;
begin
  if (grdSymbols.RowCount > 0) and (grdSymbols.FocusRow >= 0) and
     (grdSymbols.FocusRow <= High(FFiltered)) then
  begin
    FResult.FullPath := FFiltered[grdSymbols.FocusRow].Entry.FullPath;
    FResult.Line := FFiltered[grdSymbols.FocusRow].Entry.Line;
  end
  else
  begin
    FResult.FullPath := '';
    FResult.Line := 0;
  end;
  Close;
end;

procedure TSymbolFinderForm.AfterCreate;
begin
  Name := 'SymbolFinderForm';
  SetPosition(300, 200, 600, 400);
  WindowTitle := 'Navigate to Symbol';
  Hint := '';
  WindowPosition := wpMainFormCenter;

  edtSearch := TfpgEdit.Create(self);
  with edtSearch do
  begin
    Name := 'edtSearch';
    SetPosition(4, 4, 592, 24);
    Anchors := [anLeft, anRight, anTop];
    ExtraHint := 'Type to filter symbols...';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
    Text := '';
    OnChange := @SearchTextChanged;
    OnKeyPress := @SearchEditKeyPressed;
  end;

  grdSymbols := TfpgStringGrid.Create(self);
  with grdSymbols do
  begin
    Name := 'grdSymbols';
    SetPosition(4, 32, 592, 340);
    Anchors := [anLeft, anRight, anTop, anBottom];
    BackgroundColor := TfpgColor($80000002);
    AddColumn('Symbol', 250, taLeftJustify);
    AddColumn('Kind', 80, taLeftJustify);
    AddColumn('File', 240, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := True;
    TabOrder := 2;
    HeaderStyle := ghsFlat;
    OnDoubleClick := @GridDoubleClicked;
    OnDrawCell := @GridDrawCell;
  end;

  StatusBar := TfpgPanel.Create(self);
  with StatusBar do
  begin
    Name := 'StatusBar';
    SetPosition(0, 376, 600, 24);
    Align := alBottom;
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := '';
  end;
end;

end.
