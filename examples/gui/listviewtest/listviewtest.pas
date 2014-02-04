program listviewtest;

{$mode objfpc}{$H+}

uses
  Classes, sysutils,
  fpg_base, fpg_main, fpg_listview, fpg_form, fpg_button, fpg_edit,
  fpg_checkbox, fpg_splitter, fpg_panel, fpg_imagelist, fpg_stringutils;
  
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
    procedure LVColumnClicked(Listview: TfpgListView; Column: TfpgLVColumn; Button: Integer);
    procedure CloseBttn(Sender: TObject);
    procedure AddBttn(Sender: TObject);
    procedure SortButton(Sender: TObject);
    procedure ShowHeadersChange(Sender: TObject);
    procedure PaintItem(ListView: TfpgListView; ACanvas: TfpgCanvas; Item: TfpgLVItem;
                                   ItemIndex: Integer; Area:TfpgRect; var PaintPart: TfpgLVItemPaintPart);
    procedure ItemSelectionChanged(ListView: TfpgListView; Item: TfpgLVItem;
                                    ItemIndex: Integer; Selected: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;



{ TMainForm }

procedure TMainForm.LVColumnClicked(Listview: TfpgListView; Column: TfpgLVColumn;
  Button: Integer);
begin
  if Column.ColumnIndex = 0 then
  begin
    SortButton(nil);
  end;
end;

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
    Item.Caption := FEdit.Text + IntToStr(Random(1000));
    Item.SubItems.Add('c0');
    Item.SubItems.Add('c1');
    Item.SubItems.Add('c2');
    Item.SubItems.Add('c3');
    Item.SubItems.Add('c4');
  //end;
  FListView.EndUpdate;
  FTmpListView.EndUpdate;
end;

function CompareProc(Item1, Item2: Pointer): Integer;
var
  a, b: TfpgLVItem;
begin
  a := TfpgLVItem(Item1);
  b := TfpgLVItem(Item2);
  if UTF8Length(a.Caption) < UTF8Length(b.Caption) then
    Result := -1
  else if UTF8Length(a.Caption) > UTF8Length(b.Caption) then
    Result := 1
  else
    Result := CompareText(a.Caption, b.Caption);
end;

procedure TMainForm.SortButton(Sender: TObject);
begin
  FListView.Items.Sort(@CompareProc);
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
  IL: TStringList;
  i: Integer;
  FImageList: TfpgImageList;
  FSelectedImageList: TfpgImageList;
  TmpImage: TfpgImage;
begin
  inherited Create(AOwner);
  Randomize;

  WindowTitle := 'ListView Test';
  SetPosition(200, 200, 640, 480);

  IL := TStringList.Create;

  fpgImages.ListImages(IL);

  FImageList := TfpgImageList.Create;
  FSelectedImageList := TfpgImageList.Create;

  for i := 0 to IL.Count-1 do
    FImageList.AddImage(fpgImages.GetImage(IL.Strings[i]));

  IL.Free;

  // invert the items for the 'selected' images
  for i := 0 to FImageList.Count-1 do
  begin
    TmpImage := FImageList.Items[i].Image.ImageFromSource;
    TmpImage.Invert;
    FSelectedImageList.AddImage(TmpImage);
  end;

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
    Images := FImageList;
    SubItemImages := FImageList;
    ImagesSelected := FSelectedImageList;
    OnColumnClick  := @LVColumnClicked;
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
  LVColumn.ColumnIndex := 0;
  FListView.Columns.Add(LVColumn);
  FTmpListView.Columns.Add(LVColumn);
  
  LVColumn := TfpgLVColumn.Create(FListView.Columns);
  LVColumn.Caption := 'Column 2';
  LVColumn.Width := 100;
  LVColumn.Height := 50;
  LVColumn.Alignment := taCenter;
  LVColumn.ColumnIndex := 1;
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
  LVColumn.ColumnIndex := 2;
  FListView.Columns.Add(LVColumn);
  FTmpListView.Columns.Add(LVColumn);


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

