program listviewtest;

{$mode objfpc}{$H+}

uses
  Classes, fpgfx, sysutils ,
  gui_listview, gui_form, gui_button, gui_edit, gfxbase, gui_checkbox,
  fpgui_toolkit;
  
type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    FEdit: TfpgEdit;
    FAddButton: TfpgButton;
    FListView: TfpgListView;
    FTmpListView: TfpgListView;
    FQuitButton: TfpgButton;
    FCheck: TfpgCheckBox;
    
    procedure CloseBttn(Sender: TObject);
    procedure AddBttn(Sender: TObject);
    procedure ShowHeadersChange(Sender: TObject);
    procedure PaintItem(ListView: TfpgListView; ACanvas: TfpgCanvas; Item: TfpgLVItem;
                                   ItemIndex: Integer; Area:TfpgRect; var PaintPart: TfpgLVItemPaintPart);
    procedure ItemSelectionChanged(ListView: TfpgListView; Item: TfpgLVItem;
                                    ItemIndex: Integer; Selected: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.CloseBttn(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AddBttn(Sender: TObject);
var
  Item: TfpgLVItem;
  I: Integer;
begin
  FListView.BeginUpdate;
  FTmpListView.BeginUpdate;
  //FListView.Items.Capacity := FListView.Items.Capacity + 2000000;
  //for I := 0 to 1999999 do begin
    Item := FListView.ItemAdd;
    Item.Caption :=FEdit.Text+IntToStr(FListView.Items.Count);
    Item.SubItems.Add('0');
    Item.SubItems.Add('1');
    Item.SubItems.Add('2');
    Item.SubItems.Add('3');
    Item.SubItems.Add('4');
  //end;
  FListView.EndUpdate;
  FTmpListView.EndUpdate;

end;

procedure TMainForm.ShowHeadersChange(Sender: TObject);
begin
  FListView.ShowHeaders := TfpgCheckBox(Sender).Checked;
end;

procedure TMainForm.PaintItem(ListView: TfpgListView; ACanvas: TfpgCanvas;
  Item: TfpgLVItem; ItemIndex: Integer; Area: TfpgRect; var PaintPart: TfpgLVItemPaintPart);
begin
  if ItemIndex mod 2 = 0 then  ACanvas.TextColor := clRed;
  if ItemIndex mod 3 = 0 then  ACanvas.TextColor := clBlue;
  if ItemIndex mod 4 = 0 then  ACanvas.TextColor := clGray;
  if ItemIndex mod 5 = 0 then  ACanvas.TextColor := clPink;
end;

procedure TMainForm.ItemSelectionChanged(ListView: TfpgListView;
  Item: TfpgLVItem; ItemIndex: Integer; Selected: Boolean);
begin
  WriteLn('Item changed: ', ItemIndex, ' ', Item.Caption, ' ',Selected);
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  LVColumn: TfpgLVColumn;
begin
  inherited Create(AOwner);

  WindowTitle := 'ListView Test';
  SetPosition(200, 200, 610, 455);

  FListView := TfpgListView.Create(Self);
  with FListView do begin
    Parent := Self;
    Top := 10;
    Left := 10;
    Width := 320;
    Height := 400;
    OnPaintItem := @PaintItem;
    OnSelectionChanged := @ItemSelectionChanged;
    MultiSelect := True;
  end;

  FTmpListView := TfpgListView.Create(Self);
  with FTmpListView do begin
    Parent := Self;
    Top := 10;
    Left := 335;
    Width := 270;
    Height := 400;
    //OnPaintItem := @PaintItem;
    Items := FListView.Items;
  end;

  
  LVColumn := TfpgLVColumn.Create(FListView.Columns);
  LVColumn.Caption := 'Column 1';
  LVColumn.Width := 150;
  LVColumn.Height := 50;
  LVColumn.Resizable := True;
  LVColumn.Alignment := taRightJustify;
  FListView.Columns.Add(LVColumn);
  FTmpListView.Columns.Add(LVColumn);
  
  LVColumn := TfpgLVColumn.Create(FListView.Columns);
  LVColumn.Caption := 'Column 2';
  LVColumn.Width := 100;
  LVColumn.Height := 50;
  LVColumn.Alignment := taCenter;
  //LVColumn.Visible := False;
  FListView.Columns.Add(LVColumn);
  //FTmpListView.Columns.Add(LVColumn);

  LVColumn := TfpgLVColumn.Create(FListView.Columns);
  LVColumn.Caption := 'Column 3';
  LVColumn.Width := 200;
  LVColumn.Height := 50;
  //LVColumn.Visible := False;
  LVColumn.Resizable := True;
  LVColumn.Alignment := taRightJustify;
  FListView.Columns.Add(LVColumn);
  FTmpListView.Columns.Add(LVColumn);
  LVColumn.ColumnIndex := 2;


  FEdit := TfpgEdit.Create(Self);
  with FEdit do begin
    Parent := Self;
    Top := 420;
    Left := 10;
    Width := 100;
  end;

  FAddButton := TfpgButton.Create(Self);
  with FAddButton do begin
    Parent := Self;
    Top := 420;
    Left := 120;
    Width := 80;
    Text := 'Add';
    OnClick := @AddBttn;
  end;

  FQuitButton := TfpgButton.Create(Self);
  with FQuitButton do begin
    Parent := Self;
    ImageName := 'stdimg.Quit';
    ShowImage := True;
    Top := 420;
    Left := 210;
    Width := 80;
    Text := 'Quit';
    OnClick := @CloseBttn;
  end;
  
  FCheck := TfpgCheckBox.Create(Self);
  with FCheck do begin
    Parent := Self;
    Top := 420;
    Left := 290;
    Width := 110;
    Checked := True;
    Text := 'ShowHeaders';
    OnChange := @ShowHeadersChange;
  end;

end;

begin
  fpgApplication.Initialize;
  with TMainForm.Create(nil) do
  begin
    Show;
    fpgApplication.Run;
    Free;
  end;
end.

