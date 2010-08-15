program edittest;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_imgfmt_bmp,
  fpg_stringutils,
  fpg_form,
  fpg_label,
  fpg_button,
  fpg_edit,
  fpg_combobox,
  fpg_scrollbar,
  fpg_memo,
  fpg_dialogs,
  fpg_listbox,
  fpg_checkbox,
  fpg_radiobutton,
  fpg_trackbar,
  fpg_progressbar;

type

  TXPButton = class(TfpgButton)
  private
    State: integer;
      // 0 - normal
      // 1 - hover
      // 2 - mouse down
      // 3 - disabled
      // 4 - got focus or default
    image: TfpgImage;
    procedure   SetThemeImage(const AValue: TfpgImage);
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    { this property is only for demo purposes! }
    property    ThemeImage: TfpgImage read image write SetThemeImage;
  end;


  TMyWidget = class(TfpgWidget)
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  TMainForm = class(TfpgForm)
  private
    FbtnRuntime: TfpgButton;
    procedure Trackbar1Changed(Sender: TObject; APosition: integer);
    procedure btnCloseClick(Sender: TObject);
    procedure btnDisplayBMP(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure checkbox1Changed(Sender: TObject);
    procedure TrackBarChanged(Sender: TObject; APosition: integer);
    procedure xpsilverClick(Sender: TObject);
    procedure Combo1Changed(Sender: TObject);
  public
    label1: TfpgLabel;
    label2: TfpgLabel;
    lblTrackBarPos: TfpgLabel;
    edit1: TfpgEdit;
    edit2: TfpgEdit;
    btn: TfpgButton;
    btn2: TfpgButton;
    btn3: TfpgButton;
    memo: TfpgMemo;
    listbox: TfpgListBox;
    combo1: TfpgComboBox;
    combo2: TfpgComboBox;
    sbar: TfpgScrollBar;
    xpluna: TXPButton;
    xpsilver: TXPButton;
    checkbox1: TfpgCheckBox;
    checkbox2: TfpgCheckBox;
    radiobtn1: TfpgRadioButton;
    radiobtn2: TfpgRadioButton;
    radiobtn3: TfpgRadioButton;
    trackbar1: TfpgTrackBar;
    trackbar2: TfpgTrackBarExtra;
    w: TMyWidget;
    progress: TfpgProgressBar;
    procedure AfterCreate; override;
  end;

{ TMyWidget }

procedure TMyWidget.HandlePaint;
var
  r: TfpgRect;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(clBlue);

  r.SetRect(0, 0, Width, Height);
  Canvas.SetColor(clBlack);
  Canvas.DrawRectangle(r);

  InflateRect(r, -1, -1);
  Canvas.SetColor(clRed);
  Canvas.DrawRectangle(r);
  
  InflateRect(r, -1, -1);
//  Canvas.DrawControlFrame(2, 2, Width-4, Height-4);
  Canvas.DrawControlFrame(r);

{
  Canvas.SetColor(clGreen);
  Canvas.FillRectangle(r);
}
  Canvas.EndDraw;
end;

constructor TMyWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 15;
  FHeight := 22;
end;

{ TXPButton }

procedure TXPButton.SetThemeImage(const AValue: TfpgImage);
begin
  if Assigned(image) then
    image.Free;
  image := AValue;
  Repaint;
end;

procedure TXPButton.HandlePaint;
var
  x, i: integer;
  r: TfpgRect;
  iy, y: integer;
  w: integer;
  pofs: integer;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  Canvas.Clear(clButtonFace);

  r.SetRect(0, 0, Width, Height);
  
  if State <> 1 then
  begin
    if Down then
      State := 2
    else if Focused then
      State := 4
    else if not Enabled then
      State := 3
    else
      State := 0;
  end;

  x := 0;
  { left }
  Canvas.DrawImagePart(x, 0, image, state*32, 0, 3, 21);
  { body }
  for i := (x+3) to (x+3+69) do
    Canvas.DrawImagePart(i, 0, image, (state*32)+3, 0, 1, 21);
  { right }
  Canvas.DrawImagePart(i, 0, image, (state*32)+29, 0, 3, 21);

  if Focused and (not Embedded) then
  begin
    Canvas.SetColor(clText1);
    Canvas.SetLineStyle(1, lsDot);
    InflateRect(r, -3, -3);
    Canvas.DrawRectangle(r);
  end
  else
  begin
    Canvas.SetTextColor(clText1);
    Canvas.SetColor(clText1);
  end;

  if not Enabled then
    Canvas.SetTextColor(clShadow1);

  Canvas.SetClipRect(r);
  Canvas.SetFont(Font);
  y := (Height div 2) - (FFont.Height div 2);
  if y < 3 then
    y := 3;

  // offset text and image
  if Down then
    pofs := 1
  else
    pofs := 0;

  if (ShowImage) and (FImage <> nil) then
  begin
    iy := (Height div 2) - (FImage.Height div 2);
    if ImageMargin = -1 then // centered
    begin
      w := FFont.TextWidth(FText) + FImage.Width;
      if FImageSpacing > 0 then
        Inc(w, FImageSpacing);
      x := (Width div 2) - (w div 2);
      if x < 3 then
        x := 3;
    end
    else
    begin
      x := FImageMargin + 3;
    end;

    Canvas.DrawImage(x + pofs, iy + pofs, FImage);
    Inc(x, FImage.Width);
    if FImageSpacing > 0 then
      Inc(x, FImageSpacing);

    if (FImageSpacing = -1) and (FImageMargin >= 0) then
    begin
      w := (Width - 3 - x) div 2 - FFont.TextWidth(FText) div 2;
      if w < 1 then
        w := 1; // minimal spacing
      x := x + w;
    end;
  end
  else
    x := (Width div 2) - (FFont.TextWidth(FText) div 2);

  if x < 3 then
    x := 3;

  Canvas.DrawString(x + pofs, y + pofs, FText);
  Canvas.EndDraw;
end;

procedure TXPButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  State := 2;
  Repaint;
  inherited HandleLMouseDown(X, Y, ShiftState);
end;

procedure TXPButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  State := 1;
  Repaint;
  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TXPButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if Enabled then
  begin
    State := 0;
    Repaint;
  end;
end;

procedure TXPButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if Enabled then
  begin
    State := 1;
    Repaint;
  end;
end;

constructor TXPButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 75;
  Height := 21;
  State := 0;
  
  image := LoadImage_BMP(SetDirSeparators('../../../images/themes/luna/button.bmp'));
  image.CreateMaskFromSample(0, 0);
  image.UpdateImage;
  if not Assigned(image) then
     writeln('Image is nil');
end;

destructor TXPButton.Destroy;
begin
  image.Free;
  inherited Destroy;
end;


{ TMainForm }

procedure TMainForm.xpsilverClick(Sender: TObject);
begin
  if BackgroundColor = clWindowBackground then
    BackgroundColor := clGreen
  else
    BackgroundColor := clWindowBackground;
end;

procedure TMainForm.Combo1Changed(Sender: TObject);
begin
  // ilImageLeft, ilImageTop, ilImageRight, ilImageBottom
  case combo1.FocusItem of
    0:  btn.ImageLayout := ilImageLeft;
    1:  btn.ImageLayout := ilImageTop;
    2:  btn.ImageLayout := ilImageRight;
    3:  btn.ImageLayout := ilImageBottom;
  end;
end;

procedure TMainForm.Trackbar1Changed(Sender: TObject; APosition: integer);
begin
  progress.Position := APosition;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnDisplayBMP(Sender: TObject);
begin
  if MouseCursor = mcDefault then
    MouseCursor := mcHourGlass
  else
    MouseCursor := mcDefault;

  if not Assigned(FbtnRuntime) then
    FbtnRuntime := CreateButton(self, 100, 130, 75, 'At Runtime', nil);
end;

procedure TMainForm.btn3Click(Sender: TObject);
begin
  ShowMessage('Do you really want to quit this application?' + #10 +
      'We can always keep playing and quit at a later date.' +
      #10#10 +
      'This is a new paragraph and very long line that must be split automatically, ' +
      'and it should have done so. If not, there is a bug in the code. It ' +
      'has also been optimized to wordwrap and not split words over two lines.'
      , 'My cool message title');
  if btn2.TextColor = clBlue then
  begin
    edit1.TextColor     := clText1;
    btn2.TextColor      := clText1;
    checkbox2.TextColor := clText1;
    radiobtn1.TextColor := clText1;
    memo.TextColor      := clText1;
  end
  else
  begin
    edit1.TextColor     := clBlue;
    btn2.TextColor      := clBlue;
    checkbox2.TextColor := clBlue;
    radiobtn1.TextColor := clBlue;
    memo.TextColor      := clBlue;
  end;
end;

procedure TMainForm.checkbox1Changed(Sender: TObject);
begin
  label1.Enabled := not checkbox1.Checked;
  label2.Enabled := not checkbox1.Checked;
  btn.Enabled := not checkbox1.Checked;
  btn2.Enabled := not checkbox1.Checked;
  btn3.Enabled := not checkbox1.Checked;
  checkbox2.Enabled := not checkbox1.Checked;
  radiobtn1.Enabled := not checkbox1.Checked;
  radiobtn2.Enabled := not checkbox1.Checked;
  radiobtn3.Enabled := not checkbox1.Checked;
  combo1.Enabled := not checkbox1.Checked;
  combo2.Enabled := not checkbox1.Checked;
  edit1.Enabled := not checkbox1.Checked;
  listbox.Enabled := not checkbox1.Checked;
  xpluna.Enabled := not checkbox1.Checked;
  xpsilver.Enabled := not checkbox1.Checked;
end;

procedure TMainForm.TrackBarChanged(Sender: TObject; APosition: integer);
begin
  lblTrackBarPos.Text := IntToStr(APosition);
end;

procedure TMainForm.AfterCreate;
var
  i: integer;
  bmp: TfpgImage;
begin
  SetPosition(200, 200, 500, 350);
  WindowTitle := 'UTF-8 Title -> Òåñò';

  label1 := CreateLabel(self, 5, 5, 'Hello world!');
  label2 := CreateLabel(self, 5, 20, 'Hello world in Bold!');
  label2.FontDesc := 'Sans-12:bold:underline';
  label2.Width := 200;
  
  w := TMyWidget.Create(self);
  w.Top := 40;
  w.Left := 140;

  edit1      := CreateEdit(self, 10, 40, 120, 22);
  edit1.AutoSelect := False;
  edit1.HideSelection := False;
  edit1.Text := 'Hello world. Hello world. Hello world.';
  edit2      := CreateEdit(self, 10, 70, 200, 22);
  edit2.Text := 'UTF-8 text -> Òåñò';

//  writeln(UTF8Length(edit2.text));
//  writeln(Length(edit2.text));
//  UTF8Insert('ö', edit2.Text, 15);

  // left to right and right to left text in one
  // fpGUI doesn't handle this correctly yet.
  // See http://www.catch22.net/tuts/editor18.asp  for how it needs to display and work
//    edit2.Text := 'HelloيُساوِيWorld';

  btn2          := CreateButton(self, 10, 100, 75, 'Normal', nil);
  btn2.OnClick  := @btnDisplayBMP;
//    btn2.Enabled  := False;
  
  btn3          := CreateButton(self, 100, 100, 75, 'Embedded', nil);
  btn3.Embedded := True;
  btn3.OnClick  := @btn3Click;

  btn           := CreateButton(self, 10, 130, 75, 'Close', @btnCloseClick);
  btn.ImageName := 'stdimg.close';
  btn.ShowImage := True;
  btn.Height := 55;

  combo1 := CreateComboBox(self, 10, 200, 120, nil);
  combo1.BackgroundColor := clYellow;
  combo1.TextColor := clBlue;
  combo1.Items.Add('ilImageLeft');
  combo1.Items.Add('ilImageTop');
  combo1.Items.Add('ilImageRight');
  combo1.Items.Add('ilImageBottom');
  combo1.FocusItem := 0;
  combo1.OnChange := @Combo1Changed;

  combo2 := CreateComboBox(self, 10, 230, 120, nil);
  for i := 1 to 20 do
    combo2.Items.Add(Format('Items %.2d', [i]));

  memo        := TfpgMemo.Create(self);
  memo.Top    := 10;
  memo.Left   := 250;
  memo.Width  := 200;
  memo.Height := 80;
//  memo.Anchors := [anLeft, anTop, anRight, anBottom];

  listbox         := TfpgListBox.Create(self);
  listbox.Top     := 100;
  listbox.Left    := 250;
  listbox.Width   := 200;
  listbox.Height  := 80;
  listbox.TextColor := clRed;
  listbox.BackgroundColor := clYellow;
  for i := 1 to 20 do
    listbox.Items.Add(Format('Items %.2d', [i]));
  listbox.FocusItem := 3;


  sbar        := TfpgScrollBar.Create(self);
  sbar.Top    := 160;
  sbar.Left   := 150;
  sbar.Height := 100;
  sbar.Max    := 15;

  xpluna := TXPButton.Create(self);
  xpluna.Left    := 250;
  xpluna.Top     := 200;
  xpluna.Width   := 75;
  xpluna.Text    := 'XP Luna';

  xpsilver := TXPButton.Create(self);
  xpsilver.Left    := 250;
  xpsilver.Top     := 230;
  xpsilver.Width   := 75;
  xpsilver.Text    := 'XP Silver';
  bmp := LoadImage_BMP(SetDirSeparators('../../../images/themes/silver/button.bmp'));
  bmp.CreateMaskFromSample(0, 0);
  bmp.UpdateImage;
  xpsilver.ThemeImage := bmp;
  xpsilver.OnClick := @xpsilverClick;
  
  checkbox1  := CreateCheckBox(self, 10, 265, 'Disable components');
  checkbox1.OnChange := @checkbox1Changed;
  checkbox2  := CreateCheckBox(self, 10, 285, 'Checkbox Two');
  
  radiobtn1 := CreateRadioButton(self, 180, 265, 'Radio One');
  radiobtn1.GroupIndex := 2;
  radiobtn2 := CreateRadioButton(self, 180, 285, 'Radio Two');
  radiobtn2.GroupIndex := 2;
  radiobtn3 := CreateRadioButton(self, 180, 305, 'Radio Three');
  radiobtn3.GroupIndex := 2;
  radiobtn1.Checked := True;
  
  trackbar1 := TfpgTrackBar.Create(self);
  trackbar1.Top    := 230;
  trackbar1.Left   := 335;
  trackbar1.Width  := 100;
  trackbar1.Height := 25;
  trackbar1.ShowPosition := True;
  trackbar1.OnChange :=@Trackbar1Changed;

  lblTrackBarPos := CreateLabel(self, 420, 200, '0');

  trackbar2 := TfpgTrackBarExtra.Create(self);
  trackbar2.Top    := 230;
  trackbar2.Left   := 440;
  trackbar2.Orientation := orVertical;
  trackbar2.Width  := 25;
  trackbar2.Height := 100;
  trackbar2.OnChange := @TrackBarChanged;
  
  progress := TfpgProgressBar.Create(self);
  progress.Left := 300;
  progress.Top := 275;
  progress.ShowCaption := True;
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;


begin
  MainProc;
end.

