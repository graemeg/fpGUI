{ This was a very quick and dirty demo to show how custom windows with your
  own style of borders (eg: like Chrome or elementryOS) can be implement.
  There is obviously lots of scope for improving this code and creating
  custom widgets to make better use of code reuse and abstraction. }
program custom_window;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_button,
  fpg_stylemanager, fpg_cmdlineparams, fpg_grid,
  fpg_StringGridBuilder, fpg_editbtn, fpg_checkbox,
  fpg_panel, fpg_dialogs;

type

  TMainForm = class(TfpgForm)
    procedure ResizeClicked(Sender: TObject);
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnQuit: TfpgButton;
    Grid1: TfpgStringGrid;
    FilenameEdit1: TfpgFileNameEdit;
    btnGo: TfpgButton;
    CheckBox1: TfpgCheckBox;
    bvlTitle: TfpgBevel;
    btnClose: TfpgImagePanel;
    btnResize: TfpgImagePanel;
    bvlTasks: TfpgBevel;
    {@VFD_HEAD_END: MainForm}
    FLastPos: TPoint;
    FMouseTracked: Boolean;
    procedure   TitleMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   TitleMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   TitleMouseMoved(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure   btnCloseClicked(Sender: TObject);
    procedure   PaintTasksPanel(Sender: TObject);
    procedure   PaintTitle(Sender: TObject);
    procedure   FormPaint(Sender: TObject);
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnGoClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

const
  cBackground = TfpgColor($FFf5f5f5);
  cGradientTop = TfpgColor($FFe5e5e5);
  cGradientBottom = TfpgColor($FFbcbcbc);
  cBorder = TfpgColor($FF7c7c7c);
  cGrayPanel = TfpgColor($FFdedede);

  { tip: probably best to use specific fonts for specific OSes }
  cHeader1 = 'Arial-11:bold:antialias=true';
  cHeader2 = 'Arial-10:antialias=true';

{$I images.inc}

{@VFD_NEWFORM_IMPL}

procedure TMainForm.ResizeClicked(Sender: TObject);
begin
  ShowMessage('I''ll leave this one up to you to implement and experiment with. ;-)', 'Hint');
end;

procedure TMainForm.TitleMouseDown(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  FMouseTracked := True;
  FLastPos := bvlTitle.WindowToScreen(self, AMousePos);
  bvlTitle.CaptureMouse;
end;

procedure TMainForm.TitleMouseUp(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
begin
  FMouseTracked := False;
  bvlTitle.ReleaseMouse;
end;

procedure TMainForm.TitleMouseMoved(Sender: TObject; AShift: TShiftState;
  const AMousePos: TPoint);
var
  dx, dy: integer;
  pt: TPoint;
begin
  pt := WindowToScreen(self, AMousePos);
  if not FMouseTracked then
  begin
    FLastPos := pt;
    Exit;
  end;

  dx := pt.X - FLastPos.X;
  dy := pt.Y - FLastPos.Y;
  Left := Left + dx;
  Top := Top + dy;
  FLastPos := pt;
  UpdateWindowPosition;
end;

procedure TMainForm.btnCloseClicked(Sender: TObject);
begin
  btnQuit.Click;
end;

procedure TMainForm.PaintTasksPanel(Sender: TObject);
begin
  with bvlTasks do
  begin
    Canvas.Clear(cGrayPanel);

    Canvas.Color := cBorder;
    Canvas.DrawRectangle(0, 0, Width, Height);

    Canvas.TextColor := cBorder;
    // Output some sample text
    Canvas.Font := fpgGetFont(cHeader1);
    Canvas.DrawText(8, 10, 'Personal');
    Canvas.Font := fpgGetFont(cHeader2);
    Canvas.DrawText(20, 30, 'Home');
    Canvas.DrawText(20, 50, 'Documents');
    Canvas.DrawText(20, 70, 'Music');
    Canvas.DrawText(20, 90, 'Pictures');
    Canvas.Font := fpgGetFont(cHeader1);
    Canvas.DrawText(8, 110, 'Network');
    Canvas.Font := fpgGetFont(cHeader2);
    Canvas.DrawText(20, 130, 'Entire network');
  end;
end;

procedure TMainForm.PaintTitle(Sender: TObject);
var
  r: TfpgRect;
begin
  r.SetRect(0, 1, Width, 46);
  with bvlTitle do
  begin
    Canvas.GradientFill(r, cGradientTop, cGradientBottom, gdVertical);

    Canvas.Color := TfpgColor($FFc9c9c9);
    Canvas.DrawLine(0, Height-2, Width, Height-2);

    Canvas.Color := cBorder;
    Canvas.DrawRectangle(0, 0, Width, Height);

    Canvas.TextColor := cBorder;
    Canvas.Font := fpgGetFont(cHeader1);
    Canvas.DrawText(30, 8, Width-60, 20, WindowTitle, [txtHCenter, txtTop]);
  end;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  Canvas.Color := cBorder;
  Canvas.DrawRectangle(0, 0, Width, Height);
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnGoClicked(Sender: TObject);
var
  sgb: TStringGridBuilder;
begin
  try
    sgb := TStringGridBuilder.CreateCustom(Grid1, FilenameEdit1.FileName, CheckBox1.Checked);
    sgb.Run;
  finally
    sgb.Free;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Include(FWindowAttributes, waBorderLess);  // borderless and steals focus like a normal form
  FMouseTracked := False;

  fpgSetNamedColor(clWindowBackground, cBackground);

  fpgImages.AddBMP(  // 8x9 pixels.
            'my.close',
            @img_close,
      sizeof(img_close));

  fpgImages.AddBMP(  // 10x11 pixels.
            'my.resize',
            @img_resize,
      sizeof(img_resize));
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(464, 271, 866, 473);
  WindowTitle := 'fpGUI Custom Window Demo';
  Hint := '';
  OnPaint := @FormPaint;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(772, 436, 80, 23);
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnQuitClicked;
  end;

  Grid1 := TfpgStringGrid.Create(self);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(182, 120, 666, 276);
    BackgroundColor := TfpgColor($80000002);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 2;
  end;

  FilenameEdit1 := TfpgFileNameEdit.Create(self);
  with FilenameEdit1 do
  begin
    Name := 'FilenameEdit1';
    SetPosition(182, 92, 510, 24);
    ExtraHint := '';
    FileName := '';
    Filter := 'CSV Files (*.csv)|*.csv';
    InitialDir := '';
    TabOrder := 3;
  end;

  btnGo := TfpgButton.Create(self);
  with btnGo do
  begin
    Name := 'btnGo';
    SetPosition(768, 92, 80, 23);
    Text := 'GO';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnGoClicked;
  end;

  CheckBox1 := TfpgCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(184, 68, 120, 19);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 5;
    Text := 'Has Header';
  end;

  bvlTitle := TfpgBevel.Create(self);
  with bvlTitle do
  begin
    Name := 'bvlTitle';
    SetPosition(0, 0, 866, 48);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsSpacer;
    OnPaint := @PaintTitle;
    OnMouseMove := @TitleMouseMoved;
    OnMouseDown := @TitleMouseDown;
    OnMouseUp := @TitleMouseUp;
  end;

  btnClose := TfpgImagePanel.Create(bvlTitle);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(9, 8, 8, 9);
    OwnsImage := False;
    OnClick := @btnCloseClicked;
  end;

  btnResize := TfpgImagePanel.Create(bvlTitle);
  with btnResize do
  begin
    Name := 'btnResize';
    SetPosition(849, 8, 10, 11);
    OwnsImage := False;
    OnClick := @ResizeClicked;
  end;

  bvlTasks := TfpgBevel.Create(self);
  with bvlTasks do
  begin
    Name := 'bvlTasks';
    SetPosition(0, 47, 170, 426);
    Anchors := [anLeft,anTop,anBottom];
    Hint := '';
    Shape := bsSpacer;
    OnPaint := @PaintTasksPanel;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  btnClose.Image := fpgImages.GetImage('my.close');
  btnResize.Image := fpgImages.GetImage('my.resize');
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;

  { Set our new style as the default (before we create any forms), unless
    a the end-user specified a different style via the command line. }
  if not gCommandLineParams.IsParam('style') then
  begin
    if fpgStyleManager.SetStyle('Plastic Light Gray') then
      fpgStyle := fpgStyleManager.Style;
  end;

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

