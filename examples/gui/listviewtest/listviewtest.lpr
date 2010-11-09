program listviewtest;

{$mode objfpc}{$H+}

uses
  Classes, sysutils,
  fpg_base, fpg_main, fpg_listview, fpg_form, fpg_button, fpg_edit, fpg_checkbox, fpg_splitter, fpg_panel;
  
type

  TMainForm = class(TfpgForm)
  private
    FEdit: TfpgEdit;
    FAddButton: TfpgButton;
    FListView: TfpgListView;
    FSplitter: TfpgSplitter;
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
    Item.Caption := FEdit.Text + IntToStr(FListView.Items.Count);
    Item.SubItems.Add('c0');
    Item.SubItems.Add('c1');
    Item.SubItems.Add('c2');
    Item.SubItems.Add('c3');
    Item.SubItems.Add('c4');
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
  //WriteLn('Item changed: ', ItemIndex, ' ', Item.Caption, ' ',Selected);
end;

constructor TMainForm.Create(AOwner: TComponent);
var
  LVColumn: TfpgLVColumn;
  TopPanel,
  BottomPanel: TfpgPanel;
begin
  inherited Create(AOwner);

  WindowTitle := 'ListView Test';
  SetPosition(200, 200, 640, 480);


  BottomPanel := TfpgPanel.Create(Self);
  BottomPanel.Align  := alBottom;
  BottomPanel.Height := 40;
  BottomPanel.Parent := Self;
  BottomPanel.Text   := '';

  TopPanel         := TfpgPanel.Create(Self);
  TopPanel.Align   := alClient;
  TopPanel.Parent  := Self;
  TopPanel.Text    := '';


  FListView := TfpgListView.Create(TopPanel);
  with FListView do begin
    Parent := TopPanel;
    Align := alLeft;
    Width := 320;
    OnPaintItem := @PaintItem;
    OnSelectionChanged := @ItemSelectionChanged;
    MultiSelect := True;
  end;

  FSplitter := TfpgSplitter.Create(TopPanel);
  with FSplitter do begin
    Parent := TopPanel;
    Align:=alLeft;
  end;
  FTmpListView := TfpgListView.Create(TopPanel);
  with FTmpListView do begin
    Parent := TopPanel;
    Align := alClient;
    //OnPaintItem := @PaintItem;
    Items := FListView.Items;
  end;

  
  LVColumn := TfpgLVColumn.Create(FListView.Columns);
  LVColumn.Caption := 'Column 1';
  LVColumn.Width := 150;
  LVColumn.Height := 50;
  LVColumn.Resizable := True;
  LVColumn.Alignment := taLeftJustify;
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


  FEdit := TfpgEdit.Create(BottomPanel);
  with FEdit do begin
    Parent := BottomPanel;
    Top := 10;
    Left := 10;
    Width := 100;
  end;

  FAddButton := TfpgButton.Create(BottomPanel);
  with FAddButton do begin
    Parent := BottomPanel;
    Top := 10;
    Left := 120;
    Width := 80;
    Text := 'Add';
    OnClick := @AddBttn;
  end;

  FQuitButton := TfpgButton.Create(BottomPanel);
  with FQuitButton do begin
    Parent := BottomPanel;
    ImageName := 'stdimg.Quit';
    ShowImage := True;
    Top := 10;
    Left := -10;
    Width := 80;
    Text := 'Quit';
    Anchors := [anRight, anBottom];
    OnClick := @CloseBttn;
  end;
  
  FCheck := TfpgCheckBox.Create(BottomPanel);
  with FCheck do begin
    Parent := BottomPanel;
    Top := 10;
    Left := 205;
    Width := 110;
    Checked := True;
    Text := 'Show Headers';
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

