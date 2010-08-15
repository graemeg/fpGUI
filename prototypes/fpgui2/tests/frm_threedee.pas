{
  This project is a prototype theme for use in Master Maths (Pty) Ltd.
  Designed by Graeme Geldenhuys.
}

unit frm_threedee;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button, fpg_dialogs,
  fpg_checkbox, fpg_trackbar, fpg_progressbar, fpg_menu;

type

  TthreedeeEdit = class(TfpgEdit)
  private
    FimgLeft: TfpgImage;
    FimgTop: TfpgImage;
    FimgLeftTop: TfpgImage;
    FInError: boolean;
    FErrorColor: TfpgColor;
    procedure   Draw3DControlShadow(ARect: TfpgRect);
    procedure   SetErrorColor(const AValue: TfpgColor);
    procedure   SetInError(const AValue: boolean);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    InError: boolean read FInError write SetInError;
    property    ErrorColor: TfpgColor read FErrorColor write SetErrorColor;
  end;
  

  TfrmMain = class(TfpgForm)
  private
    procedure TrackBarChanged(Sender: TObject; APosition: integer);
    procedure btnQuitClicked(Sender: TObject);
    procedure cbChangeColor(Sender: TObject);
    procedure BGColorChange(Sender: TObject);
    procedure edtThreeDeeChanged(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: frmMain}
    btnQuit: TfpgButton;
    edtName1: TfpgEdit;
    edtThreeDee: TthreedeeEdit;
    cbName1: TfpgCheckBox;
    cbBGColor: TfpgCheckBox;
    tb: TfpgTrackBar;
    pb: TfpgProgressBar;
    Custom1: TthreedeeEdit;
    Custom2: TthreedeeEdit;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    {@VFD_HEAD_END: frmMain}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_stringutils,
  fpg_imgfmt_bmp;
  
const
  clM2DarkBlue = $187EC0;
  clM2Grey = $b0b0b0;
  

Const
  stdimg_gradient_lefttop_7x7 : Array[0..221] of byte = (
      66, 77,222,  0,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0,  7,  0,  0,  0,  7,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
     168,  0,  0,  0,196, 14,  0,  0,196, 14,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,175,175,175,189,189,189,204,204,204,218,218,218,229,229,
     229,236,236,236,242,242,242,  0,  0,  0,174,174,174,187,187,187,201,
     201,201,214,214,214,225,225,225,231,231,231,236,236,236,  0,  0,  0,
     171,171,171,183,183,183,196,196,196,208,208,208,217,217,217,225,225,
     225,229,229,229,  0,  0,  0,168,168,168,178,178,178,190,190,190,200,
     200,200,208,208,208,214,214,214,218,218,218,  0,  0,  0,164,164,164,
     172,172,172,182,182,182,191,191,191,196,196,196,201,201,201,205,205,
     205,  0,  0,  0,160,160,160,166,166,166,172,172,172,178,178,178,183,
     183,183,187,187,187,187,187,187,  0,  0,  0,157,157,157,160,160,160,
     164,164,164,168,168,168,171,171,171,174,174,174,174,174,174,  0,  0,
       0);

Const
  stdimg_gradient_left_7x4 : Array[0..149] of byte = (
      66, 77,150,  0,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0,  7,  0,  0,  0,  4,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
      96,  0,  0,  0,196, 14,  0,  0,196, 14,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,176,176,176,190,190,190,206,206,206,221,221,221,231,231,
     231,239,239,239,245,245,245,  0,  0,  0,176,176,176,190,190,190,206,
     206,206,221,221,221,231,231,231,239,239,239,245,245,245,  0,  0,  0,
     176,176,176,190,190,190,206,206,206,221,221,221,231,231,231,239,239,
     239,245,245,245,  0,  0,  0,176,176,176,190,190,190,206,206,206,221,
     221,221,231,231,231,239,239,239,245,245,245,  0,  0,  0);

Const
  stdimg_gradient_top_4x7 : Array[0..137] of byte = (
      66, 77,138,  0,  0,  0,  0,  0,  0,  0, 54,  0,  0,  0, 40,  0,  0,
       0,  4,  0,  0,  0,  7,  0,  0,  0,  1,  0, 24,  0,  0,  0,  0,  0,
      84,  0,  0,  0,196, 14,  0,  0,196, 14,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0,245,245,245,245,245,245,245,245,245,245,245,245,239,239,
     239,239,239,239,239,239,239,239,239,239,231,231,231,231,231,231,231,
     231,231,231,231,231,221,221,221,221,221,221,221,221,221,221,221,221,
     206,206,206,206,206,206,206,206,206,206,206,206,190,190,190,190,190,
     190,190,190,190,190,190,190,176,176,176,176,176,176,176,176,176,176,
     176,176);
     
{ This procedure creates a sunken 3d effect in a rectangle with a color gradient }
procedure FillRectGradient(Canvas: TfpgCanvas; X, Y, W, H: TfpgCoord;
    Strip: Integer; Astart, Astop: TfpgColor);
var
  RGBStart: TFPColor;
  RGBStop: TFPColor;
  RDiff, GDiff, BDiff: Integer;
  count: Integer;
  i: Integer;
  newcolor: TFPColor;
  Hx, Hy: TfpgCoord; // Coordinates for Horizontal Lines
  Vx, Vy: TfpgCoord; // Coordinates for Vertical Lines
  avgcolor: TfpgColor;
begin
  RGBStart := fpgColorToFPColor(fpgColorToRGB(AStart));
  RGBStop  := fpgColorToFPColor(fpgColorToRGB(AStop));

  count := Strip;
  Hx := X;
  Hy := Y;
  Vx := X;
  Vy := Y;

  RDiff := RGBStop.Red - RGBStart.Red;
  GDiff := RGBStop.Green - RGBStart.Green;
  BDiff := RGBStop.Blue - RGBStart.Blue;

//  Changing;
  Canvas.BeginDraw;
  for i := 0 to count do
  begin
    newcolor.Red    := RGBStart.Red + (i * RDiff) div count;
    newcolor.Green  := RGBStart.Green + (i * GDiff) div count;
    newcolor.Blue   := RGBStart.Blue + (i * BDiff) div count;
   
    Canvas.SetLineStyle(1, lsSolid);
    Canvas.SetColor(FPColorTofpgColor(newcolor));
    Canvas.DrawLine(Hx, Hy, W+2, Hy); // Horizontal Line
    Canvas.DrawLine(Vx, Vy, Vx, H+2); // Vertical Line
    // next Horizontal Line: one pixel lower, one pixel shorter on the left
    Hx := Hx + 1;
    Hy := Hy + 1;
    // Next Vertical Line: One pixel to the right, one pixel shorter on top
    Vx := Vx + 1;
    Vy := Vy + 1
  end;
 
  //  Changed;
  Canvas.EndDraw;
end;
     

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.TrackBarChanged(Sender: TObject; APosition: integer);
begin
  pb.Position := APosition;
end;

procedure TfrmMain.btnQuitClicked (Sender: TObject );
begin
  Close;
end;

procedure TfrmMain.cbChangeColor(Sender: TObject);
begin
  if cbName1.Checked then
  begin
    edtName1.BackgroundColor := clYellow;
//    edtThreeDee.BackgroundColor := clYellow;
  end
  else
  begin
    edtName1.BackgroundColor := clBoxColor;
//    edtThreeDee.BackgroundColor := clBoxColor;
  end;
  try
    if edtThreeDee.Text <> '' then
      edtThreeDee.ErrorColor := TfpgColor(StrToInt64(edtThreeDee.Text));
  except
    on E: Exception do
      edtThreeDee.ErrorColor := clRed;
  end;
  edtThreeDee.InError := cbName1.Checked;
end;

procedure TfrmMain.BGColorChange(Sender: TObject);
begin
  if cbBGColor.Checked then
  begin
    BackgroundColor           := clWindowBackground;
    cbName1.BackgroundColor   := clWindowBackground;
    cbBGColor.BackgroundColor := clWindowBackground;
    tb.BackgroundColor        := clWindowBackground;
    lblName1.BackgroundColor  := clWindowBackground;
    lblName2.BackgroundColor  := clWindowBackground;
    btnQuit.BackgroundColor   := clButtonFace;
  end
  else
  begin
    BackgroundColor           := clM2DarkBlue;
    cbName1.BackgroundColor   := clM2DarkBlue;
    cbBGColor.BackgroundColor := clM2DarkBlue;
    tb.BackgroundColor        := clM2DarkBlue;
    lblName1.BackgroundColor  := clM2DarkBlue;
    lblName2.BackgroundColor  := clM2DarkBlue;
    btnQuit.BackgroundColor   := clM2DarkBlue;
  end;
end;

procedure TfrmMain.edtThreeDeeChanged(Sender: TObject);
begin
  Custom1.Text := edtThreeDee.Text;
  Custom2.Text := edtThreeDee.Text;
end;

procedure TfrmMain.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmMain}
  Name := 'frmMain';
  SetPosition(304, 229, 373, 279);
  WindowTitle := 'MasterMaths custom theme test';
  WindowPosition := wpScreenCenter;
  BackgroundColor := clM2DarkBlue;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(289, 249, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := 'stdimg.quit';
    BackgroundColor := clM2DarkBlue;
    OnClick := @btnQuitClicked;
  end;

  edtName1 := TfpgEdit.Create(self);
  with edtName1 do
  begin
    Name := 'edtName1';
    SetPosition(48, 24, 120, 22);
    Text := 'hello';
    FontDesc := '#Edit1';
  end;

  edtThreeDee := TthreedeeEdit.Create(self);
  with edtThreeDee do
  begin
    Name := 'edtThreeDee';
    SetPosition(48, 112, 120, 22);
    FontDesc := '#Edit1';
    Text := '$800000';
    OnChange := @edtThreeDeeChanged;
    BackgroundColor := $F5F5F5;
  end;

  cbName1 := TfpgCheckBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(12, 196, 120, 20);
    Text := 'In Error';
    FontDesc := '#Label1';
    BackgroundColor := clM2DarkBlue;
    OnChange := @cbChangeColor;
  end;

  cbBGColor := TfpgCheckBox.Create(self);
  with cbBGColor do
  begin
    Name := 'cbBGColor';
    SetPosition(12, 220, 180, 20);
    Text := 'Normal background color';
    FontDesc := '#Label1';
    BackgroundColor := clM2DarkBlue;
    OnChange := @BGColorChange;
  end;

  tb := TfpgTrackBar.Create(self);
  with tb do
  begin
    Name := 'tb';
    SetPosition(224, 76, 100, 30);
    BackgroundColor := clM2DarkBlue;
    OnChange := @TrackBarChanged;
  end;

  pb := TfpgProgressBar.Create(self);
  with pb do
  begin
    Name := 'pb';
    SetPosition(208, 112, 150, 22);
    ShowCaption := True;
  end;

  Custom1 := TthreedeeEdit.Create(self);
  with Custom1 do
  begin
    Name := 'Custom1';
    SetPosition(48, 144, 120, 22);
    FontDesc := '#Edit1';
    Text := '$800000';
    BackgroundColor := $B0C4DE
  end;

  Custom2 := TthreedeeEdit.Create(self);
  with Custom2 do
  begin
    Name := 'Custom2';
    SetPosition(48, 80, 120, 22);
    FontDesc := '#Edit1';
    Text := '$800000';
    BackgroundColor := $B0C4DE;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(4, 4, 144, 16);
    Text := 'Standard Style:';
    FontDesc := '#Label2';
    BackgroundColor := clM2DarkBlue;
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(4, 60, 120, 16);
    Text := 'M2 Custom Style:';
    FontDesc := '#Label2';
    BackgroundColor := clM2DarkBlue;
  end;

  {@VFD_BODY_END: frmMain}
end;


{ TthreedeeEdit }

procedure TthreedeeEdit.Draw3DControlShadow(ARect: TfpgRect);
var
  r: TfpgRect;
begin
(*
  Canvas.DrawImage(ARect.Left, ARect.Top, FimgLeftTop);
//  Canvas.StretchDraw(1, 8, 7, ARect.Height, FimgLeft);
  r.SetRect(ARect.Left, 8, 8, ARect.Height);
  Canvas.GradientFill(r, clM2Grey, BackgroundColor, gdHorizontal);
//  Canvas.StretchDraw(8, 1, Width+5, 7, FimgTop);
  r.SetRect(8, ARect.Top, Width+5, 8);
  Canvas.GradientFill(r, clM2Grey, BackgroundColor, gdVertical);
*)
  FillRectGradient(Canvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height, 7, TfpgColor($777777), BackgroundColor);
end;

procedure TthreedeeEdit.SetErrorColor(const AValue: TfpgColor);
begin
  if FErrorColor = AValue then
    Exit; //==>
  FErrorColor := AValue;
  Repaint;
end;

procedure TthreedeeEdit.SetInError(const AValue: boolean);
begin
  if FInError = AValue then
    Exit; //==>
  FInError := AValue;
  RePaint;
end;

procedure TthreedeeEdit.HandlePaint;
var
  r: TfpgRect;
  tw, tw2, st, len: integer;
  dtext: string;
  c: TfpgColor;
begin
  Canvas.BeginDraw;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  
  if InError then
    Canvas.SetColor(ErrorColor)
  else
    Canvas.SetColor(clWhite);
  Canvas.FillRectangle(r);
  
  if InError then
    InflateRect(r, -2, -2)
  else
    InflateRect(r, -1, -1);
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(BackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);
  Canvas.FillRectangle(r);
  Draw3DControlShadow(r);

  dtext := GetDrawText;
  Canvas.SetTextColor(clText1);
  Canvas.SetFont(Font);
  fpgStyle.DrawString(Canvas, -FDrawOffset + FSideMargin, 3, dtext, Enabled);

  if Focused then
  begin
    // drawing selection
    if FSelOffset <> 0 then
    begin
      len := FSelOffset;
      st  := FSelStart;
      if len < 0 then
      begin
        st  := st + len;
        len := -len;
      end;

      tw  := Font.TextWidth(UTF8copy(dtext, 1, st));
      tw2 := Font.TextWidth(UTF8copy(dtext, 1, st + len));
      Canvas.XORFillRectangle(fpgColorToRGB(clSelection) xor $FFFFFF, -FDrawOffset +
        FSideMargin + tw, 3, tw2 - tw, Font.Height);
    end;

    // drawing cursor
    tw := Font.TextWidth(UTF8copy(dtext, 1, FCursorPos));
    fpgCaret.SetCaret(Canvas, -FDrawOffset + FSideMargin + tw, 3, fpgCaret.Width, Font.Height);
  end
  else
    fpgCaret.UnSetCaret(Canvas);

  Canvas.EndDraw;
end;

constructor TthreedeeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FimgLeft    := CreateImage_BMP(@stdimg_gradient_left_7x4, SizeOf(stdimg_gradient_left_7x4)); // LoadImage_BMP('../../../images/gradient_left_7x4.bmp');
  FimgTop     := CreateImage_BMP(@stdimg_gradient_top_4x7, SizeOf(stdimg_gradient_top_4x7)); // LoadImage_BMP('../../../images/gradient_top_4x7.bmp');
  FimgLeftTop := CreateImage_BMP(@stdimg_gradient_lefttop_7x7, SizeOf(stdimg_gradient_lefttop_7x7)); // LoadImage_BMP('../../../images/gradient_lefttop_7x7.bmp');

  FInError := False;
  FErrorColor := clMaroon;
end;

destructor TthreedeeEdit.Destroy;
begin
  FimgLeft.Free;
  FimgTop.Free;
  FimgLeftTop.Free;
  inherited Destroy;
end;

end.
