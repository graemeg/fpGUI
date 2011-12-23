unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget, fpg_form, fpg_menu,
  fpg_panel, fpg_button;


{$I globe_data.inc}
{$I images.inc}

type
  TGlobe = class(TfpgWidget)
  private
    FShowGrid: boolean;
    FGridStep: integer;
    R, Fi, Th, Wfi, Wth, Y, X, Xold, Yold: integer;
    CosFi, SinFi: Double;  { Fi is longitude }
    CosTh, SinTh: Double;  { Th is latitude }
    CosWth, SinWth: Double;
    procedure   DrawSpace;
    procedure   DrawGlobe;
    procedure   DrawGrid;
    procedure   DrawSegment;
    procedure   Convert;
    function    GetRadius: integer;
    procedure   SetRadius(const AValue: integer);
    procedure   SetShowGrid(const AValue: boolean);
    procedure   SetGridStep(const AValue: integer);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   SetViewPoint(const ALon, ALat: integer);
    property    GridStep: integer read FGridStep write SetGridStep default 5;
    property    ShowGrid: boolean read FShowGrid write SetShowGrid default True;
    property    Radius: integer read GetRadius write SetRadius default 150;
  end;


  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    MainMenu: TfpgMenuBar;
    Globe1: TGlobe;
    pmFile: TfpgPopupMenu;
    pmView: TfpgPopupMenu;
    pmHelp: TfpgPopupMenu;
    StatusPanel: TfpgPanel;
    Bevel1: TfpgBevel;
    btnQuit: TfpgButton;
    btnGrid: TfpgButton;
    btnZoomIn: TfpgButton;
    btnZoomOut: TfpgButton;
    btnHelp: TfpgButton;
    Bevel2: TfpgBevel;
    Bevel3: TfpgBevel;
    {@VFD_HEAD_END: MainForm}
    miShowGrid: TfpgMenuItem;
    FShowGrid: boolean;
    FWfi: integer;
    FWth: integer;
    FStep: integer;
    procedure FormKeyPressed(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure miFileQuitClicked(Sender: TObject);
    procedure miShowGridClicked(Sender: TObject);
    procedure miZoomInClicked(Sender: TObject);
    procedure miZoomOutClicked(Sender: TObject);
    procedure btnHelpClicked(Sender: TObject);
    procedure miHelpAbout(Sender: TObject);
    procedure miHelpAboutFPGui(Sender: TObject);
    procedure UpdateStatus;
    procedure MoveWest;
    procedure MoveEast;
    procedure MoveNorth;
    procedure MoveSouth;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_dialogs, math;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.FormKeyPressed(Sender: TObject; var KeyCode: word;
  var ShiftState: TShiftState; var Consumed: boolean);
begin
  case KeyCode of
    81: // Q key
        begin
          Consumed := True;
//          if ShiftState = [ssCtrl] then
            Close;
        end;
    keyUp:
        begin
          MoveNorth;
          Consumed := True;
        end;
    keyDown:
        begin
          MoveSouth;
          Consumed := True;
        end;
    keyLeft:
        begin
          MoveWest;
          Consumed := True;
        end;
    keyRight:
        begin
          MoveEast;
          Consumed := True;
        end;
    71: // g key
        begin
          Consumed := True;
          miShowGridClicked(nil);
        end;
    73: // i key
        begin
          Consumed := True;
          miZoomInClicked(nil);
        end;
    79: // o key
        begin
          Consumed := True;
          miZoomOutClicked(nil);
        end;
    keyF1:
        begin
          Consumed := True;
          btnHelpClicked(nil);
        end;
    keyF11:
        begin
          Consumed := True;
          miHelpAboutFPGui(nil);
        end;
    keyF12:
        begin
          Consumed := True;
          miHelpAbout(nil);
        end;
  end;
end;

procedure TMainForm.miFileQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miShowGridClicked(Sender: TObject);
begin
  FShowGrid := not FShowGrid;
  miShowGrid.Checked := FShowGrid;
  btnGrid.Down := FShowGrid;
  Globe1.ShowGrid := FShowGrid;
end;

procedure TMainForm.miZoomInClicked(Sender: TObject);
var
  r: integer;
begin
  r := Globe1.Radius;
  if r < 1000 then
    r := r + 50;
  Globe1.Radius := r;
end;

procedure TMainForm.miZoomOutClicked(Sender: TObject);
var
  r: integer;
begin
  r := Globe1.Radius;
  if r > 50 then
    r := r - 50;
  Globe1.Radius := r;
end;

procedure TMainForm.btnHelpClicked(Sender: TObject);
var
  frm: TfpgMessageBox;
begin
  frm := TfpgMessageBox.Create(nil);
  try
    frm.WindowTitle := 'Keyboard Help';
    frm.CentreText := False;
    frm.FontDesc := '#Edit2'; // mono font
    frm.SetMessage(
        'G:   Toggles the grid' + LineEnding
      + 'I:   Zoom in' + LineEnding
      + 'O:   Zoom out' + LineEnding
      + 'Q:   Quit the application' + LineEnding
      + LineEnding
      + 'The arrow keys rotate the globe up,down,left and right' + LineEnding
      + LineEnding
      + 'F1:  Shows this help' + LineEnding
      + 'F11: About fpGUI Toolkit' + LineEnding
      + 'F12: About this program');
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.miHelpAbout(Sender: TObject);
begin
  ShowMessage('fpGUI Globe written by Graeme Geldenhuys - 2010', 'About...', True);
end;

procedure TMainForm.miHelpAboutFPGui(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui('');
end;

procedure TMainForm.UpdateStatus;
var
  fi, th: integer;
  ew, ns: TfpgString;
begin
  if FWth >= 0 then
  begin
    th := FWth;
    ns := 'N';
  end
  else
  begin
    th := - FWth;
    ns := 'S';
  end;
  fi := FWfi mod 360;
  if (fi > 180) then
    fi := fi - 360
  else if (fi < -180) then
    fi := fi + 360;
  if fi >= 0 then
    ew := 'E'
  else
  begin
    fi := -fi;
    ew := 'W';
  end;
  StatusPanel.Text := Format('View Point: %d %s, %d %s', [th, ns, fi, ew]);
end;

procedure TMainForm.MoveWest;
begin
  FWfi := FWfi - FStep;
  FWfi := FWfi mod 360;
  Globe1.SetViewPoint(FWfi, FWth);
  UpdateStatus;
end;

procedure TMainForm.MoveEast;
begin
  FWfi := FWfi + FStep;
  FWfi := FWfi mod 360;
  Globe1.SetViewPoint(FWfi, FWth);
  UpdateStatus;
end;

procedure TMainForm.MoveNorth;
begin
  if FWth <= (90-FStep) then
    FWth := FWth + FStep
  else
    FWth := 90;
  Globe1.SetViewPoint(FWfi, FWth);
  UpdateStatus;
end;

procedure TMainForm.MoveSouth;
begin
  if FWth >= (FStep-90) then
    FWth := FWth - FStep
  else
    FWth := -90;
  Globe1.SetViewPoint(FWfi, FWth);
  UpdateStatus;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStep := 5;
  FShowGrid := True;
  OnKeyPress := @FormKeyPressed;
  fpgImages.AddMaskedBMP(  // 16x16 image
            'usr.toggle_grid',
            @usr_toggle_grid,
      sizeof(usr_toggle_grid), 0, 0);
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(357, 219, 363, 430);
  WindowTitle := 'fpGUI Globe';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpOneThirdDown;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 363, 21);
    Anchors := [anLeft,anRight,anTop];
  end;

  Globe1 := TGlobe.Create(self);
  with Globe1 do
  begin
    Name := 'Globe1';
    SetPosition(7, 64, 350, 330);
    Anchors := [anLeft,anRight,anTop,anBottom];
    //    OnKeyPress := @FormKeyPressed;
  end;

  pmFile := TfpgPopupMenu.Create(self);
  with pmFile do
  begin
    Name := 'pmFile';
    SetPosition(248, 36, 120, 20);
    AddMenuItem('&Quit', 'Q', @miFileQuitClicked);
  end;

  pmView := TfpgPopupMenu.Create(self);
  with pmView do
  begin
    Name := 'pmView';
    SetPosition(248, 56, 120, 20);
    miShowGrid := AddMenuItem('Show Grid', '', @miShowGridClicked);
    miShowGrid.Checked := True;
    AddMenuItem('Zoom In', '', @miZoomInClicked);
    AddMenuItem('Zoom Out', '', @miZoomOutClicked);
  end;

  pmHelp := TfpgPopupMenu.Create(self);
  with pmHelp do
  begin
    Name := 'pmHelp';
    SetPosition(248, 76, 120, 20);
    AddMenuItem('Keyboard Shortcuts...', 'F1', @btnHelpClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem('About fpGUI Toolkit...', 'F11', @miHelpAboutFPGui);
    AddMenuItem('About...', 'F12', @miHelpAbout);
  end;

  StatusPanel := TfpgPanel.Create(self);
  with StatusPanel do
  begin
    Name := 'StatusPanel';
    SetPosition(0, 405, 363, 24);
    Anchors := [anLeft,anRight,anBottom];
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'Ready.';
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 22, 363, 30);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Style := bsLowered;
    Shape := bsBottomLine;
  end;

  btnQuit := TfpgButton.Create(Bevel1);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 2, 24, 24);
    Text := '';
    Down := False;
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Quit the application';
    ImageMargin := -1;
    ImageName := 'stdimg.quit';
    ImageSpacing := 0;
    TabOrder := 1;
    Focusable := False;
    OnClick := @miFileQuitClicked;
  end;

  btnGrid := TfpgButton.Create(Bevel1);
  with btnGrid do
  begin
    Name := 'btnGrid';
    SetPosition(35, 2, 24, 24);
    Text := '';
    AllowAllUp := True;
    Down := True;
    Flat := True;
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := 'Toggle the grid on or off';
    ImageMargin := -1;
    ImageName := 'usr.toggle_grid';
    ImageSpacing := 0;
    TabOrder := 2;
    Focusable := False;
    OnClick := @miShowGridClicked;
  end;

  btnZoomIn := TfpgButton.Create(Bevel1);
  with btnZoomIn do
  begin
    Name := 'btnZoomIn';
    SetPosition(60, 2, 24, 24);
    Text := '';
    Down := False;
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Zoom in';
    ImageMargin := -1;
    ImageName := 'stdimg.add';
    ImageSpacing := 0;
    TabOrder := 3;
    Focusable := False;
    OnClick := @miZoomInClicked;
  end;

  btnZoomOut := TfpgButton.Create(Bevel1);
  with btnZoomOut do
  begin
    Name := 'btnZoomOut';
    SetPosition(84, 2, 24, 24);
    Text := '';
    Down := False;
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Zoom out';
    ImageMargin := -1;
    ImageName := 'stdimg.remove';
    ImageSpacing := 0;
    TabOrder := 4;
    Focusable := False;
    OnClick := @miZoomOutClicked;
  end;

  btnHelp := TfpgButton.Create(Bevel1);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(116, 2, 24, 24);
    Text := '';
    Down := False;
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Keyboard shortcut help';
    ImageMargin := -1;
    ImageName := 'stdimg.help';
    ImageSpacing := 0;
    TabOrder := 5;
    Focusable := False;
    OnClick := @btnHelpClicked;
  end;

  Bevel2 := TfpgBevel.Create(Bevel1);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(30, 2, 3, 24);
    Hint := '';
    Style := bsLowered;
    Shape := bsLeftLine;
  end;

  Bevel3 := TfpgBevel.Create(Bevel1);
  with Bevel3 do
  begin
    Name := 'Bevel3';
    SetPosition(112, 2, 3, 24);
    Hint := '';
    Style := bsLowered;
    Shape := bsLeftLine;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  { hook up the menus to menu bar }
  MainMenu.AddMenuItem('&File', nil).SubMenu := pmFile;
  MainMenu.AddMenuItem('&View', nil).SubMenu := pmView;
  MainMenu.AddMenuItem('&Help', nil).SubMenu := pmHelp;
end;


{ TGlobe }

procedure TGlobe.DrawSpace;
var
  i: integer;
begin
  Canvas.Color := clGray; //clNavy;
  i := 0;
  while i < R do
  begin
    X := Trunc(R - (sqrt(R*R - (R-i)*(R-i) )) - 0.5);
    Canvas.DrawLine(0,     i,     X,   i);
    Canvas.DrawLine(2*R-X, i,     2*R, i);
    Canvas.DrawLine(0,     2*R-i, X,   2*R-i);
    Canvas.DrawLine(2*R-X, 2*R-i, 2*R, 2*R-i);
    inc(i, 2);
  end;
end;

procedure TGlobe.DrawGlobe;
var
  i: integer;
begin
  DrawSpace;
  Canvas.Color := clGreen;

  CosWth := cos(degtorad(Wth));
  SinWth := sin(degtorad(Wth));
  i := 0;

  Fi := globe_data[i];
  inc(i);
  Th := globe_data[i];
  inc(i);
  Convert;
  Xold := X;
  Yold := Y;

  { plot }
  while i < Length(globe_data) do
  begin
    Fi := globe_data[i];
    inc(i);
    Th := globe_data[i];
    inc(i);
    if (Fi <> 0) or (Th <> 0) then
    begin
      Convert;
      DrawSegment;
      Xold := X;
      Yold := Y;
      continue;
    end;

    if i = Length(globe_data) then
      break;

    Fi := globe_data[i];
    inc(i);
    Th := globe_data[i];
    inc(i);
    Convert;
    Xold := X;
    Yold := Y;
  end;

  if FShowGrid then
    DrawGrid;
end;

procedure TGlobe.DrawGrid;
var
  temp: integer;
begin
  Canvas.Color := TfpgColor($8080FF); // clGray;
  Canvas.SetLineStyle(0, lsDash);
  { Parallels }
  temp := Wfi;
  Wfi := 0;
  Th := -90+GridStep;
  while Th < 90 do
  begin
    Fi := -180;
    Convert;
    Xold := X;
    Yold := Y;
    while Fi < 180 do
    begin
      Fi := Fi + 6;
      Convert;
      DrawSegment;
      Xold := X;
      Yold := Y;
    end;
    inc(Th, GridStep);
  end;

  Wfi := temp;
  Fi := -180;
  { Meridians }
  while Fi < 180 do
  begin
    Th := -84;
    Convert;
    Xold := X;
    Yold := Y;
    while Th < 84 do
    begin
      Th := Th + 6;
      Convert;
      DrawSegment;
      Xold := X;
      Yold := Y;
    end;
    inc(Fi, GridStep);
  end;
end;

procedure TGlobe.DrawSegment;
begin
  if (CosFi * CosTh * CosWth + SinTh * SinWth) > 0.0 then
    Canvas.DrawLine(Xold+R, Yold+R, X+R, Y+R);
end;

procedure TGlobe.Convert;
var
  dFi: integer;
begin
  dFi := Fi - Wfi;

  CosFi := cos(degtorad(dFi));
  SinFi := sin(degtorad(dFi));
  CosTh := cos(degtorad(Th));
  SinTh := sin(degtorad(Th));

  X := Trunc(CosTh * SinFi * R);
  Y := Trunc((CosFi * CosTh * SinWth - SinTh * CosWth) * R);
end;

function TGlobe.GetRadius: integer;
begin
  Result := R;
end;

procedure TGlobe.SetRadius(const AValue: integer);
begin
  R := AValue;
  Repaint;
end;

procedure TGlobe.SetShowGrid(const AValue: boolean);
begin
  if FShowGrid=AValue then exit;
  FShowGrid:=AValue;
  Repaint;
end;

procedure TGlobe.SetGridStep(const AValue: integer);
begin
  if FGridStep = AValue then exit;
  FGridStep := AValue;
  if FShowGrid then
    Repaint;
end;

procedure TGlobe.HandlePaint;
begin
  Canvas.BeginDraw;
  Canvas.Clear(clWindowBackground);
//  R := Width div 2;
  // inherited HandlePaint;
  DrawGlobe;
  Canvas.EndDraw;
end;

constructor TGlobe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFocusable := True;
  FGridStep := 15;
  FShowGrid := True;
  R := 150;
end;

procedure TGlobe.SetViewPoint(const ALon, ALat: integer);
begin
  if ALat < -90 then
    Wth := -90
  else if ALat > 90 then
    Wth := 90
  else
    Wth := ALat;
  Wfi := ALon mod 360;
  Repaint;
end;

end.
