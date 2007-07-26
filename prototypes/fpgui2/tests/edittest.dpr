program edittest;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_imgfmt_bmp,
  gui_form,
  gui_label,
  gui_button,
  gui_edit,
  gui_combobox,
  gui_scrollbar,
  gui_memo,
  gui_dialogs,
  gui_listbox,
  gui_checkbox,
  gui_radiobutton;

type

  { TXPButton }

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

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    procedure btnCloseClick(Sender: TObject);
    procedure btnDisplayBMP(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure checkbox1Changed(Sender: TObject);
  public
    label1: TfpgLabel;
    label2: TfpgLabel;
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
    xp2: TXPButton;
    xpsilver: TXPButton;
    checkbox1: TfpgCheckBox;
    checkbox2: TfpgCheckBox;
    radiobtn1: TfpgRadioButton;
    radiobtn2: TfpgRadioButton;
    radiobtn3: TfpgRadioButton;
    procedure AfterCreate; override;
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
    Canvas.DrawRectangle(3, 3, Width - 6, Height - 6);
  end
  else
  begin
    Canvas.SetTextColor(clText1);
    Canvas.SetColor(clText1);
  end;

  if not Enabled then
    Canvas.SetTextColor(clShadow1);

  r.left   := 2;
  r.top    := 2;
  r.Width  := Width - 4;
  r.Height := Height - 4;
  Canvas.SetClipRect(r);

  Canvas.SetFont(Font);
  y := Height div 2 - FFont.Height div 2;
  if y < 3 then
    y := 3;

  // offset text and image
  if Down then
    pofs := 1
  else
    pofs := 0;

  if (ShowImage) and (FImage <> nil) then
  begin
    iy := Height div 2 - FImage.Height div 2;
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

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnDisplayBMP(Sender: TObject);
var
  bmp: TfpgImage;
begin
  exit;
  bmp := LoadImage_BMP(SetDirSeparators('../../../images/themes/luna/button.bmp'));
  bmp.CreateMaskFromSample(0, 0);
  bmp.UpdateImage;

  Canvas.BeginDraw;
  Canvas.ClearClipRect;
  // For some reason, under Windows if I don't call this
  // the forms background goes black.
  Canvas.Clear(clWindowBackground);

  Canvas.DrawImage(10, 200, bmp);
  Canvas.DrawImagePart(10, 240, bmp, 0, 0, 32, 21);
  Canvas.DrawImagePart(50, 240, bmp, 32, 0, 32, 21);

  Canvas.EndDraw;
  bmp.Free;
end;

procedure TMainForm.btn3Click(Sender: TObject);
begin
  ShowMessage('Do you really want to quit this application?' + #10 +
      'We can always keep playing and quite at a later date.' +
      #10#10 +
      'This is a very long line that has to must be split automatically ' +
      'and it should have done so. If not there is a bug in the code. It ' +
      'has also been optimized to wordwrap and not split words in half.'
      , 'My cool message title');
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
end;

procedure TMainForm.AfterCreate;
var
  i: integer;
  bmp: TfpgImage;
begin
  SetPosition(200, 200, 500, 350);
  WindowTitle := 'Test Russian text -> Òåñò';

  label1 := CreateLabel(self, 5, 5, 'Hello world!');
  label2 := CreateLabel(self, 5, 20, 'Hello world in Bold!');
  label2.FontDesc := 'Sans-12:bold:underline';

  edit1      := CreateEdit(self, 10, 40, 120, 22);
  edit1.Text := 'Hello world. Hello world. Hello world.';
  edit2      := CreateEdit(self, 10, 70, 200, 22);
  edit2.Text := 'Test Russian text -> Òåñò';
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

  combo1 := CreateComboBox(self, 10, 160, 120, nil);
  for i := 1 to 5 do
    combo1.Items.Add(Format('Items %.2d', [i]));
  combo2 := CreateComboBox(self, 10, 190, 120, nil);
  for i := 1 to 20 do
    combo2.Items.Add(Format('Items %.2d', [i]));

  memo        := TfpgMemo.Create(self);
  memo.Top    := 10;
  memo.Left   := 250;
  memo.Width  := 200;
  memo.Height := 80;

  listbox         := TfpgListBox.Create(self);
  listbox.Top     := 100;
  listbox.Left    := 250;
  listbox.Width   := 200;
  listbox.Height  := 80;
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

  xp2 := TXPButton.Create(self);
  xp2.Left    := 335;
  xp2.Top     := 200;
  xp2.Width   := 75;
  xp2.Text    := 'XP Button2';
  xp2.Enabled := False;

  xpsilver := TXPButton.Create(self);
  xpsilver.Left    := 250;
  xpsilver.Top     := 230;
  xpsilver.Width   := 75;
  xpsilver.Text    := 'XP Silver';
  bmp := LoadImage_BMP(SetDirSeparators('../../../images/themes/silver/button.bmp'));
  bmp.CreateMaskFromSample(0, 0);
  bmp.UpdateImage;
  xpsilver.ThemeImage := bmp;
  
  checkbox1  := CreateCheckBox(self, 10, 265, 'Disable components');
  checkbox1.OnChange := @checkbox1Changed;
  checkbox2  := CreateCheckBox(self, 10, 285, 'Checkbox Two');
  
  radiobtn1 := CreateRadioButton(self, 180, 265, 'Radio One');
  radiobtn2 := CreateRadioButton(self, 180, 285, 'Radio Two');
  radiobtn3 := CreateRadioButton(self, 180, 305, 'Radio Three');
  radiobtn1.Checked := True;
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

