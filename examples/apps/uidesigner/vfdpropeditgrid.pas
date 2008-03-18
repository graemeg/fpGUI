{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Property editor for grid columns.
}

unit vfdpropeditgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  gfx_widget,
  gui_form,
  gui_label,
  gui_edit,
  gui_button,
  gui_listbox,
  gui_memo,
  gui_combobox,
  gui_customgrid,
  gui_grid,
  gui_checkbox,
  vfdforms,
  vfdwidgetclass,
  vfdprops,
  vfdformparser;

type

  TPropertyDBColumns = class(TVFDWidgetProperty)
  public
    function    ParseSourceLine(wg: TfpgWidget; const line: string): boolean; override;
    function    GetPropertySource(wg: TfpgWidget; const ident: string): string; override;
    function    GetValueText(wg: TfpgWidget): string; override;
    function    CreateEditor(AOwner: TComponent): TVFDPropertyEditor; override;
    procedure   OnExternalEdit(wg: TfpgWidget); override;
  end;


  TColumnsGrid = class(TfpgCustomGrid)
  protected
    function    GetRowCount: integer; override;
    procedure   DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer);  override;
  public
    dbgrid: TfpgStringGrid;
    constructor Create(AOwner: TComponent); override;
  end;


  TColumnEditForm = class(TfpgForm)
  private
    procedure GridRowChange(Sender: TObject; row: integer);
    procedure EditChange(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure UpDownButtonClick(Sender: TObject);
    procedure SaveColumn(row: integer);
  public
    {@VFD_HEAD_BEGIN: ColumnEditForm}
    lbLabel1: TfpgLabel;
    grid: TColumnsGrid;
    lbLabel2: TfpgLabel;
    lbLabel3: TfpgLabel;
    lbLabel6: TfpgLabel;
    lbLabel5: TfpgLabel;
    lbCOLNO: TfpgLabel;
    edTITLE: TfpgEdit;
    edCOLWIDTH: TfpgEdit;
    chlALIGN: TfpgComboBox;
    btnNew: TfpgButton;
    btnDelete: TfpgButton;
    btnUP: TfpgButton;
    btnDOWN: TfpgButton;
    btnClose: TfpgButton;
    {@VFD_HEAD_END: ColumnEditForm}

    dbgrid: TfpgStringGrid;
    procedure AfterCreate; override;
  end;


implementation


procedure EditStringGridColumns(agrid: TfpgStringGrid);
var
  frm: TColumnEditForm;
begin
  frm        := TColumnEditForm.Create(nil);
  frm.dbgrid := agrid;
  frm.grid.dbgrid := agrid;
  frm.ShowModal;
  frm.Free;
end;

{ TColumnEditForm }

procedure TColumnEditForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: ColumnEditForm}
  Name := 'ColumnEditForm';
  SetPosition(270, 267, 511, 269);
  WindowTitle := 'Column editor';
  Sizeable := False;

  lbLabel1 := TfpgLabel.Create(self);
  with lbLabel1 do
  begin
    Name := 'lbLabel1';
    SetPosition(8, 4, 110, 16);
    Text := 'String Grid columns:';
    FontDesc := '#Label1';
    AutoSize := True;
  end;

  grid := TColumnsGrid.Create(self);
  with grid do
  begin
    Name := 'grid';
    SetPosition(8, 24, 328, 204);
    OnRowChange := @GridRowChange;
  end;

  lbLabel2 := TfpgLabel.Create(self);
  with lbLabel2 do
  begin
    Name := 'lbLabel2';
    SetPosition(344, 24, 56, 16);
    Text := 'Column:';
    FontDesc := '#Label1';
    AutoSize := True;
  end;

  lbLabel3 := TfpgLabel.Create(self);
  with lbLabel3 do
  begin
    Name := 'lbLabel3';
    SetPosition(344, 56, 34, 16);
    Text := 'Title:';
    FontDesc := '#Label1';
    AutoSize := True;
  end;

  lbLabel6 := TfpgLabel.Create(self);
  with lbLabel6 do
  begin
    Name := 'lbLabel6';
    SetPosition(344, 100, 88, 16);
    Text := 'Column width:';
    FontDesc := '#Label1';
    AutoSize := True;
  end;

  lbLabel5 := TfpgLabel.Create(self);
  with lbLabel5 do
  begin
    Name := 'lbLabel5';
    SetPosition(344, 144, 83, 16);
    Text := 'Alignment:';
    FontDesc := '#Label1';
    AutoSize := True;
  end;

  lbCOLNO := TfpgLabel.Create(self);
  with lbCOLNO do
  begin
    Name := 'lbCOLNO';
    SetPosition(404, 24, 54, 16);
    Text := '--';
    FontDesc := '#Label1';
    AutoSize := True;
  end;

  edTITLE := TfpgEdit.Create(self);
  with edTITLE do
  begin
    Name := 'edTITLE';
    SetPosition(344, 72, 160, 22);
    Text := '';
    FontDesc := '#Edit1';
    OnChange := @EditChange;
  end;

  edCOLWIDTH := TfpgEdit.Create(self);
  with edCOLWIDTH do
  begin
    Name := 'edCOLWIDTH';
    SetPosition(344, 116, 160, 22);
    Text := '';
    FontDesc := '#Edit1';
    OnChange := @EditChange;
  end;

  chlALIGN := TfpgComboBox.Create(self);
  with chlALIGN do
  begin
    Name := 'chlALIGN';
    SetPosition(344, 162, 160, 22);
    Items.Add('Left');
    Items.Add('Right');
    Items.Add('Center');
    FontDesc := '#List';
    OnChange := @EditChange;
  end;

  btnNew := TfpgButton.Create(self);
  with btnNew do
  begin
    Name := 'btnNew';
    SetPosition(8, 236, 75, 24);
    Text := 'New';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
    OnClick := @NewButtonClick;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(86, 236, 75, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
    OnClick := @DeleteButtonClick;
  end;

  btnUP := TfpgButton.Create(self);
  with btnUP do
  begin
    Name := 'btnUP';
    SetPosition(182, 236, 75, 24);
    Text := 'UP';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
    OnClick := @UpDownButtonClick;
  end;

  btnDOWN := TfpgButton.Create(self);
  with btnDOWN do
  begin
    Name := 'btnDOWN';
    SetPosition(260, 236, 75, 24);
    Text := 'DOWN';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
    OnClick := @UpDownButtonClick;
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(428, 236, 75, 24);
    Text := 'Close';
    FontDesc := '#Label1';
    ImageName := 'stdimg.close';
    ModalResult := 0;
    OnClick := @CloseButtonClick;
  end;

  {@VFD_BODY_END: ColumnEditForm}
end;

procedure TColumnEditForm.GridRowChange(Sender: TObject; row: integer);
var
  i: integer;
  c: TfpgStringColumn;
begin
  c := dbgrid.Columns[row{ - 1}];
  if c = nil then
    Exit;

  lbCOLNO.Text     := IntToStr(row);
  edTITLE.Text     := c.Title;
  edCOLWIDTH.Text  := IntToStr(c.Width);
  case c.Alignment of
    taRightJustify:
        i := 2;
    taCenter:
        i := 3
    else
        i := 1;
  end;
  chlALIGN.FocusItem := i;
end;

procedure TColumnEditForm.SaveColumn(row: integer);
var
  c: TfpgStringColumn;
begin
  c := dbgrid.Columns[row{ - 1}];
  if c = nil then
    Exit;

  c.Title      := edTITLE.Text;
  c.Width      := StrToIntDef(edCOLWIDTH.Text, 30);
  case chlALIGN.FocusItem of
    2: c.Alignment := taRightJustify;
    3: c.Alignment := taCenter;
    else
      c.Alignment  := taLeftJustify;
  end;

  grid.RePaint;
  dbgrid.Update;
end;

procedure TColumnEditForm.EditChange(Sender: TObject);
begin
  if grid.FocusRow < 1 then
    Exit;

  SaveColumn(grid.FocusRow);
end;

procedure TColumnEditForm.NewButtonClick(Sender: TObject);
begin
  dbgrid.AddColumn('New', 50, taLeftJustify);
  grid.FocusRow := grid.RowCount;
  grid.Update;
  GridRowChange(Sender, grid.FocusRow);
end;

procedure TColumnEditForm.DeleteButtonClick(Sender: TObject);
begin
  dbgrid.DeleteColumn(grid.FocusRow);
  grid.Update;
end;

procedure TColumnEditForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TColumnEditForm.UpDownButtonClick(Sender: TObject);
begin
  if Sender = btnUP then
  begin
    if grid.FocusRow > 1 then
    begin
      dbgrid.MoveColumn(grid.FocusRow - 1, grid.FocusRow - 2);
      grid.FocusRow := grid.FocusRow - 1;
      grid.Update;
    end;
  end
  else if grid.FocusRow < grid.RowCount then
  begin
    dbgrid.MoveColumn(grid.FocusRow - 1, grid.FocusRow);
    grid.FocusRow := grid.FocusRow + 1;
    grid.Update;
  end;
end;


{ TColumnsGrid }

function TColumnsGrid.GetRowCount: integer;
begin
  try
    Result := dbgrid.ColumnCount;
  except
    Result := inherited GetRowCount;
  end;
end;

procedure TColumnsGrid.DrawCell(ARow, ACol: integer; ARect: TfpgRect; AFlags: integer);
var
  s: string;
  x: integer;
  c: TfpgStringColumn;
begin
//  writeln('ARow=', ARow, '  ACol=', ACol);
  c := dbgrid.Columns[ARow{ - 1}];
  if c = nil then
  begin
//    writeln(' TColumnsGrid.DrawCell -> exit early because c = nil');
    Exit;
  end;
//  writeln(' ... we passed the nil test');
  x := ARect.Left + 1;

  case ACol of
    1:  s := IntToStr(ARow);
    2:  s := c.Title;
    3:  s := IntToStr(c.Width);
    4:  case c.Alignment of
          taRightJustify:
              s := 'Right';
          taCenter:
              s := 'Center';
          else
              s := 'Left';
        end;
    else
      s := '?';
  end;

  Canvas.DrawString(x, ARect.Top + 1, s);
end;

constructor TColumnsGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RowSelect := True;
  ColumnCount := 0;
  RowCount := 0;
  AddColumn('Col.', 30);
  AddColumn('Title', 80);
  AddColumn('Width', 40);
  AddColumn('Align', 50);
end;

{ TPropertyDBColumns }

function TPropertyDBColumns.CreateEditor(AOwner: TComponent): TVFDPropertyEditor;
begin
  Result := TExternalPropertyEditor.Create(AOwner, self);
end;

function TPropertyDBColumns.GetValueText(wg: TfpgWidget): string;
begin
  with TfpgStringGrid(wg) do
    Result := '[' + IntToStr(ColumnCount) + ' columns]';
end;

procedure TPropertyDBColumns.OnExternalEdit(wg: TfpgWidget);
begin
  if not Assigned(wg) then
    raise Exception.Create('TPropertyDBColumns.OnExternalEdit(wg) - wg widget may not be nil.');
  if not (wg is TfpgStringGrid) then
    raise Exception.Create('TPropertyDBColumns.OnExternalEdit(wg) - wg widget is not a TfpgStringGrid.');
  EditStringGridColumns(TfpgStringGrid(wg));
end;

function TPropertyDBColumns.ParseSourceLine(wg: TfpgWidget; const line: string): boolean;
var
  c: TfpgStringColumn;
  s: string;
  sval: string;
begin
  s      := line;
  Result := False;
  if UpperCase(GetIdentifier(s)) <> UpperCase('ADDCOLUMN') then
    Exit;

  c := TfpgStringColumn.Create;

  Result := CheckSymbol(s, '(');

  if Result then
    c.Title := GetStringValue(s);
  Result := Result and CheckSymbol(s, ',');
  if Result then
    c.Width := GetIntValue(s);
  Result := Result and CheckSymbol(s, ',');
  if Result then
  begin
    sval := UpperCase(GetIdentifier(s));
    if sval = 'TARIGHTJUSTIFY' then
      c.Alignment := taRightJustify
    else if sval = 'TACENTER' then
      c.Alignment := taCenter
    else
      c.Alignment := taLeftJustify;
  end;

  Result := Result and CheckSymbol(s, ')');
  Result := Result and CheckSymbol(s, ';');

  if Result then
    TfpgStringGrid(wg).AddColumn(c.Title, c.Width, c.Alignment);

  c.Free;
end;

function TPropertyDBColumns.GetPropertySource(wg: TfpgWidget; const ident: string): string;
var
  f: integer;
  c: TfpgStringColumn;
  alstr: string;
begin
  Result := '';
  with TfpgStringGrid(wg) do
  begin
    for f := 1 to ColumnCount do
    begin
      c := Columns[f];
      case c.Alignment of
        taRightJustify:
            alstr := 'taRightJustify';
        taCenter:
            alstr := 'taCenter';
        else
            alstr := 'taLeftJustify';
      end;
      Result := Result + ident
          + 'AddColumn(' + QuotedStr(c.Title) + ', '
          + IntToStr(c.Width) + ', '
          + alstr + ');' + LineEnding;
    end;  { for }
  end;  { with }
end;

end.

