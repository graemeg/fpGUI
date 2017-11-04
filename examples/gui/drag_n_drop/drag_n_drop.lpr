program dndexample;

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS} {$apptype gui} {$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_grid, fpg_panel,
  fpg_label, fpg_edit, fpg_stdimages, fpg_checkbox;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
    procedure PaintDragPreview(ASender: TfpgDrag; ACanvas: TfpgCanvas);
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Bevel1: TfpgPanel;
    Grid1: TfpgStringGrid;
    Button1: TfpgButton;
    btnClear: TfpgButton;
    MyDragSourceLabel: TfpgLabel;
    Edit1: TfpgEdit;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    chkAcceptDrops: TfpgCheckBox;
    chkAccept: TfpgCheckBox;
    {@VFD_HEAD_END: MainForm}
    procedure CheckAcceptDropsChanged(Sender: TObject);
    procedure Edit1DragDrop(Sender: TfpgDrop; AData: variant);
    procedure Edit1DragEnter(Sender: TfpgDrop);
    procedure Bevel1DragEnter(Sender: TfpgDrop);
    procedure Bevel1DragLeave(Sender: TfpgDrop);
    procedure PanelDragDrop(Sender: TfpgDrop; AData: variant);
    procedure Button1Clicked(Sender: TObject);
    procedure btnClearClicked(Sender: TObject);
    procedure LabelDragStartDetected(Sender: TObject);
    procedure ShowMimeList(AMimeList: TfpgMimeDataItemList);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.PaintDragPreview(ASender: TfpgDrag; ACanvas: TfpgCanvas);
begin
  ACanvas.Clear(clWindowBackground);
  ACanvas.DrawString(0,0, ASender.MimeData.Text);
end;

procedure TMainForm.CheckAcceptDropsChanged(Sender: TObject);
begin
  if chkAcceptDrops.Checked then
    Edit1.DropHandler := TfpgDropEventHandler.Create(@Edit1DragEnter, nil, @Edit1DragDrop, nil)
  else
    Edit1.DropHandler := nil;
end;

procedure TMainForm.Edit1DragDrop(Sender: TfpgDrop; AData: variant);
begin
  Edit1.Text := AData;
end;

procedure TMainForm.Edit1DragEnter(Sender: TfpgDrop);
var
  s: string;
begin
  ShowMimeList(Sender.Mimetypes);
  s := 'text/plain';
  if chkAccept.Checked then
    Sender.CanDrop := False
  else
    Sender.CanDrop := Sender.AcceptMimeType([s]);
end;

procedure TMainForm.Bevel1DragEnter(Sender: TfpgDrop);
var
  s: string;
begin
  ShowMimeList(Sender.Mimetypes);
  { the mime type we want to accept }
  s := 'text/html';
  { if we will accept the drop, set CanDrop to True }
  Sender.CanDrop := Sender.AcceptMimeType([s]);
  if Sender.CanDrop then
  begin
    Bevel1.BackgroundColor := clRed;
  end;
end;

procedure TMainForm.Bevel1DragLeave(Sender: TfpgDrop);
begin
  Bevel1.BackgroundColor := clWindowBackground;
end;

procedure TMainForm.PanelDragDrop(Sender: TfpgDrop; AData: variant);
var
  s: string;
begin
  s := AData;
  Bevel1.Text := Format('Drop event at (%d,%d) with value(s):'+LineEnding+'%s', [Sender.MousePos.X, Sender.MousePos.Y, s]);
  Bevel1DragLeave(nil);
end;

procedure TMainForm.Button1Clicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnClearClicked(Sender: TObject);
begin
  Grid1.RowCount := 0;
  Edit1.Text := '';
  Bevel1.Text := '';
end;

procedure TMainForm.LabelDragStartDetected(Sender: TObject);
var
  m: TfpgMimeData;
  d: TfpgDrag;
begin
  m := TfpgMimeData.Create;
  { via convenience properties }
  m.Text := 'My name is Earl';
  m.HTML := 'My name is <b>Earl</b>';
  { Could also have used the generic SetData function }
//  m.SetData('text/plain', 'My name is Earl');
//  m.SetData('text/html', 'My name is <b>Earl</b>');

  { tell TfpgDrag who is the Source of the drag }
  d := TfpgDrag.Create(Sender as TfpgWidgetBase);

  { TfpgDrag now takes ownership of TfpgMimeData }
  d.MimeData := m;

  D.OnPaintPreview:=@PaintDragPreview;
  D.PreviewSize := fpgSize(Canvas.Font.TextWidth(d.MimeData.Text), Canvas.Font.Height);

  { TfpgDrag instance will be freed later when DND action is completed }
  d.Execute([daCopy]);
end;

procedure TMainForm.ShowMimeList(AMimeList: TfpgMimeDataItemList);
var
  i: integer;
begin
  { for debug purposes, output the various mime types supported by the source }
  Grid1.RowCount := AMimeList.Count;
  for i := 0 to AMimeList.Count-1 do
  begin
    Grid1.Cells[0, i] := IntToStr(i+1);
    Grid1.Cells[1, i] := AMimeList.Items[i].format;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 186, 512, 429);
  WindowTitle := 'Drop Site Demo';
  Hint := '';

  Bevel1 := TfpgPanel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(260, 40, 244, 140);
    Anchors := [anLeft,anRight,anTop];
    Alignment := taLeftJustify;
    BorderStyle := bsDouble;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlTop;
    Style := bsLowered;
    Text := '';
    WrapText := True;
    DropHandler := TfpgDropEventHandler.Create(@Bevel1DragEnter, @Bevel1DragLeave, @PanelDragDrop, nil);
  end;

  Grid1 := TfpgStringGrid.Create(self);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(8, 224, 496, 167);
    Anchors := [anLeft,anRight,anTop,anBottom];
    BackgroundColor := TfpgColor($80000002);
    AddColumn('#', 20, taLeftJustify);
    AddColumn('MIME Type', 190, taLeftJustify);
    AddColumn('Data', 250, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 2;
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(424, 400, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    Down := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick :=@Button1Clicked;
  end;

  btnClear := TfpgButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(340, 400, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Clear';
    Down := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnClearClicked;
  end;

  MyDragSourceLabel := TfpgLabel.Create(self);
  with MyDragSourceLabel do
  begin
    Name := 'MyDragSourceLabel';
    SetPosition(28, 20, 84, 40);
    Alignment := taCenter;
    BackgroundColor := TfpgColor($67D47A);
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlCenter;
    Text := 'Drag Me!';
    OnDragStartDetected := @LabelDragStartDetected;
  end;

  Edit1 := TfpgEdit.Create(self);
  with Edit1 do
  begin
    Name := 'Edit1';
    SetPosition(8, 156, 240, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 7;
    Text := '';
    DropHandler := TfpgDropEventHandler.Create(@Edit1DragEnter, nil, @Edit1DragDrop, nil);
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(260, 20, 244, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Accepts ''text/html''';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 136, 240, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Accepts ''text/plain''';
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 204, 284, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Available drop formats';
  end;

  chkAcceptDrops := TfpgCheckBox.Create(self);
  with chkAcceptDrops do
  begin
    Name := 'chkAcceptDrops';
    SetPosition(4, 96, 168, 20);
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 10;
    Text := 'Enable DropHandler';
    OnChange :=@CheckAcceptDropsChanged;
  end;

  chkAccept := TfpgCheckBox.Create(self);
  with chkAccept do
  begin
    Name := 'chkAccept';
    SetPosition(4, 116, 244, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 11;
    Text := 'Set Accept to False in OnDragEnter';
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

