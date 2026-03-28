{
    fpGUI IDE - Navigate to File Dialog

    Modal dialog for fuzzy file search (Ctrl+Shift+N).
    Type a few characters to filter project files, then
    press Enter to open the selected file.
}
unit ide.form.filefinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_edit, fpg_grid,
  fpg_panel,
  ide.filefinder;

type

  { TFileFinderForm }

  TFileFinderForm = class(TfpgForm)
  private
    edtSearch: TfpgEdit;
    grdFiles: TfpgStringGrid;
    StatusBar: TfpgPanel;
    FFiles: TFileEntryArray;
    FFiltered: TFileEntryArray;
    FSelectedFile: string;
    procedure SearchTextChanged(Sender: TObject);
    procedure SearchEditKeyPressed(Sender: TObject; var KeyCode: word;
      var ShiftState: TShiftState; var Consumed: boolean);
    procedure GridDoubleClicked(Sender: TObject; AButton: TMouseButton;
      AShift: TShiftState; const AMousePos: TPoint);
    procedure FillGrid;
    procedure SelectCurrentAndClose;
  public
    procedure AfterCreate; override;
    property Files: TFileEntryArray read FFiles write FFiles;
    property SelectedFile: string read FSelectedFile;
  end;

{ Show the file finder dialog. Returns the full path of the selected file,
  or empty string if cancelled. Caller must populate AFiles before calling. }
function DisplayFileFinder(const AFiles: TFileEntryArray): string;


implementation

uses
  fpg_widget,
  fpg_basegrid;

function DisplayFileFinder(const AFiles: TFileEntryArray): string;
var
  frm: TFileFinderForm;
begin
  Result := '';
  frm := TFileFinderForm.Create(nil);
  try
    frm.Files := AFiles;
    frm.FillGrid;
    frm.ShowModal;
    Result := frm.SelectedFile;
  finally
    frm.Free;
  end;
end;


{ TFileFinderForm }

procedure TFileFinderForm.SearchTextChanged(Sender: TObject);
begin
  FillGrid;
end;

procedure TFileFinderForm.SearchEditKeyPressed(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    keyUp:
      begin
        if grdFiles.FocusRow > 0 then
          grdFiles.FocusRow := grdFiles.FocusRow - 1;
        Consumed := True;
      end;
    keyDown:
      begin
        if grdFiles.FocusRow < grdFiles.RowCount - 1 then
          grdFiles.FocusRow := grdFiles.FocusRow + 1;
        Consumed := True;
      end;
    keyEnter:
      begin
        SelectCurrentAndClose;
        Consumed := True;
      end;
    keyEscape:
      begin
        FSelectedFile := '';
        Close;
        Consumed := True;
      end;
  end;
end;

procedure TFileFinderForm.GridDoubleClicked(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  SelectCurrentAndClose;
end;

procedure TFileFinderForm.FillGrid;
var
  i: Integer;
  Total: Integer;
begin
  Total := Length(FFiles);

  if Length(edtSearch.Text) = 0 then
    FFiltered := FFiles
  else
    FFiltered := FilterFiles(edtSearch.Text, FFiles);

  grdFiles.BeginUpdate;
  try
    grdFiles.RowCount := Length(FFiltered);
    for i := 0 to High(FFiltered) do
    begin
      grdFiles.Cells[0, i] := FFiltered[i].FileName;
      grdFiles.Cells[1, i] := FFiltered[i].RelativePath;
    end;
    if grdFiles.RowCount > 0 then
      grdFiles.FocusRow := 0;
  finally
    grdFiles.EndUpdate;
  end;

  if Length(edtSearch.Text) = 0 then
    StatusBar.Text := Format('%d files', [Total])
  else
    StatusBar.Text := Format('%d of %d files', [Length(FFiltered), Total]);
end;

procedure TFileFinderForm.SelectCurrentAndClose;
begin
  if (grdFiles.RowCount > 0) and (grdFiles.FocusRow >= 0) and
     (grdFiles.FocusRow <= High(FFiltered)) then
    FSelectedFile := FFiltered[grdFiles.FocusRow].FullPath
  else
    FSelectedFile := '';
  Close;
end;

procedure TFileFinderForm.AfterCreate;
begin
  Name := 'FileFinderForm';
  SetPosition(300, 200, 560, 400);
  WindowTitle := 'Navigate to File';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  edtSearch := TfpgEdit.Create(self);
  with edtSearch do
  begin
    Name := 'edtSearch';
    SetPosition(4, 4, 552, 24);
    Anchors := [anLeft, anRight, anTop];
    ExtraHint := 'Type to filter files...';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
    Text := '';
    OnChange := @SearchTextChanged;
    OnKeyPress := @SearchEditKeyPressed;
  end;

  grdFiles := TfpgStringGrid.Create(self);
  with grdFiles do
  begin
    Name := 'grdFiles';
    SetPosition(4, 32, 552, 340);
    Anchors := [anLeft, anRight, anTop, anBottom];
    BackgroundColor := TfpgColor($80000002);
    AddColumn('Name', 250, taLeftJustify);
    AddColumn('Path', 280, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := True;
    TabOrder := 2;
    HeaderStyle := ghsFlat;
    OnDoubleClick := @GridDoubleClicked;
  end;

  StatusBar := TfpgPanel.Create(self);
  with StatusBar do
  begin
    Name := 'StatusBar';
    SetPosition(0, 376, 560, 24);
    Align := alBottom;
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := '';
  end;
end;

end.
