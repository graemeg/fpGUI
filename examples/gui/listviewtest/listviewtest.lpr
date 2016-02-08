program listviewtest;

{$mode objfpc}{$H+}

uses
  Classes, sysutils,
  fpg_base, fpg_main, fpg_listview, fpg_form, fpg_button, fpg_edit,
  fpg_checkbox, fpg_splitter, fpg_panel, fpg_imagelist, fpg_stringutils;
  
type

  { TMainForm }

  TMainForm = class(TfpgForm)
    procedure ShowFocusItemChange(Sender: TObject);
  private
    {@VFD_HEAD_BEGIN: MainForm}
    BottomPanel: TfpgPanel;
    TopPanel: TfpgPanel;
    FListView: TfpgListView;
    FSplitter: TfpgSplitter;
    FTmpListView: TfpgListView;
    FEdit: TfpgEdit;
    FAddButton: TfpgButton;
    FQuitButton: TfpgButton;
    FCheck: TfpgCheckBox;
    FShowFocus: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    FImageList,
    FSelectedImageList: TfpgImageList;
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
    procedure AfterCreate; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TMainForm.AfterCreate;
var
  LVColumn: TfpgLVColumn;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(622, 213, 640, 480);
  WindowTitle := 'ListView Test';
  Hint := '';
  IconName := '';

  BottomPanel := TfpgPanel.Create(self);
  with BottomPanel do
  begin
    Name := 'BottomPanel';
    SetPosition(0, 440, 640, 40);
    Align := alBottom;
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  TopPanel := TfpgPanel.Create(self);
  with TopPanel do
  begin
    Name := 'TopPanel';
    SetPosition(0, 0, 640, 400);
    Align := alClient;
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  FListView := TfpgListView.Create(TopPanel);
  with FListView do
  begin
    Name := 'FListView';
    SetPosition(2, 2, 320, 396);
    Align := alLeft;
    Hint := '';
    MultiSelect := True;
    ShowHeaders := True;
    ShowHeaders := True;
    TabOrder := 1;
    ViewStyleClass := TfpgLVReportPainter;
    OnPaintItem := @PaintItem;
    OnSelectionChanged := @ItemSelectionChanged;
    Images := FImageList;
    SubItemImages := FImageList;
    ImagesSelected := FSelectedImageList;
    OnColumnClick  := @LVColumnClicked;
  end;

  FSplitter := TfpgSplitter.Create(TopPanel);
  with FSplitter do
  begin
    Name := 'FSplitter';
    SetPosition(252, 2, 8, 396);
    Align := alLeft;
  end;

  FTmpListView := TfpgListView.Create(TopPanel);
  with FTmpListView do
  begin
    Name := 'FTmpListView';
    SetPosition(260, 2, 378, 396);
    Align := alClient;
    Hint := '';
    MultiSelect := False;
    ShowHeaders := True;
    ShowHeaders := True;
    TabOrder := 3;
    ViewStyleClass := TfpgLVIconPainter;
    Items := FListView.Items;
    Images := FImageList;
  end;

  FEdit := TfpgEdit.Create(BottomPanel);
  with FEdit do
  begin
    Name := 'FEdit';
    SetPosition(5, 5, 120, 24);
    ExtraHint := 'List item prefix';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
    Text := '';
    Options := [eo_ExtraHintIfFocus];
  end;

  FAddButton := TfpgButton.Create(BottomPanel);
  with FAddButton do
  begin
    Name := 'FAddButton';
    SetPosition(130, 5, 85, 23);
    Text := 'Add';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @AddBttn;
  end;

  FQuitButton := TfpgButton.Create(BottomPanel);
  with FQuitButton do
  begin
    Name := 'FQuitButton';
    SetPosition(550, 5, 80, 23);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.Quit';
    TabOrder := 3;
    OnClick := @CloseBttn;
  end;

  FCheck := TfpgCheckBox.Create(BottomPanel);
  with FCheck do
  begin
    Name := 'FCheck';
    SetPosition(220, 5, 120, 19);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Show Headers';
    OnChange := @ShowHeadersChange;
  end;

  FShowFocus := TfpgCheckBox.Create(BottomPanel);
  with FShowFocus do
  begin
    Name := 'FShowFocus';
    SetPosition(330, 5, 125, 19);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 5;
    Text := 'Show Item Focus';
    OnChange:=@ShowFocusItemChange;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
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
end;



{ TMainForm }

procedure TMainForm.ShowFocusItemChange(Sender: TObject);
begin
  FListView.ShowFocusRect:=TfpgCheckBox(Sender).Checked;
end;

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
//  I: Integer;
begin
  FListView.BeginUpdate;
  FTmpListView.BeginUpdate;
  //FListView.Items.Capacity := FListView.Items.Capacity + 2000000;
  //for I := 0 to 1999999 do begin
    Item := FListView.AddItem;
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
  IL: TStringList;
  i: Integer;
  TmpImage: TfpgImage;
begin
  inherited Create(AOwner);
  Randomize;



  IL := TStringList.Create;
  fpgImages.ListImages(IL); // just assigns image references
  FImageList := TfpgImageList.Create;
  FSelectedImageList := TfpgImageList.Create;
  for i := 0 to IL.Count-1 do
    FImageList.AddImage(TfpgImage(IL.Objects[i]));  // just adds image references
  IL.Free;

  { invert the items for the 'selected' images - this creates
    new images which we must free later. }
  for i := 0 to FImageList.Count-1 do
  begin
    TmpImage := FImageList.Items[i].Image.ImageFromSource;
    TmpImage.Invert;
    FSelectedImageList.AddImage(TmpImage);
  end;
end;

destructor TMainForm.Destroy;
var
  i: integer;
begin
  // Must assign Image = nil otherwise TfpgImageList is going to try and free them
  for i := 0 to FImageList.Count-1 do
    FImageList[i].Image := nil;
  FImageList.Free;

  FSelectedImageList.Free;
  inherited Destroy;
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

