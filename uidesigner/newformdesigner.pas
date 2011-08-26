{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Essential classes used by the fpGUI Designer
}

unit newformdesigner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_widget,
  fpg_form,
  fpg_label,
  fpg_button,
  fpg_edit,
  fpg_listbox,
  fpg_memo,
  fpg_combobox,
  fpg_menu,
  fpg_mru,
  fpg_hyperlink,
  vfdwidgetclass,
  vfdwidgets;

type

  TwgPaletteButton = class(TfpgButton)
  public
    VFDWidget: TVFDWidgetClass;
  end;
  

  TwgPalette = class(TfpgWidget)
  protected
    procedure HandlePaint; override;
  end;
  

  TfrmMain = class(TfpgForm)
  private
    FFileOpenRecent: TfpgMenuItem;
    procedure   FormShow(Sender: TObject);
    procedure   PaletteBarResized(Sender: TObject);
    procedure   miHelpAboutClick(Sender: TObject);
    procedure   miHelpAboutGUI(Sender: TObject);
    procedure   miMRUClick(Sender: TObject; const FileName: string);
  public
    {@VFD_HEAD_BEGIN: frmMain}
    MainMenu: TfpgMenuBar;
    btnNewForm: TfpgButton;
    btnOpen: TfpgButton;
    btnSave: TfpgButton;
    wgpalette: TwgPalette;
    chlPalette: TfpgComboBox;
    filemenu: TfpgPopupMenu;
    formmenu: TfpgPopupMenu;
    setmenu: TfpgPopupMenu;
    miOpenRecentMenu: TfpgPopupMenu;
    helpmenu: TfpgPopupMenu;
    previewmenu: TfpgPopupMenu;
    {@VFD_HEAD_END: frmMain}
    mru: TfpgMRU;
    constructor Create(AOwner: TComponent); override;
    function    GetSelectedWidget: TVFDWidgetClass;
    procedure   SetSelectedWidget(wgc: TVFDWidgetClass);
    procedure   AfterCreate; override;
    procedure   BeforeDestruction; override;
    procedure   OnPaletteClick(Sender: TObject);
    property    SelectedWidget: TVFDWidgetClass read GetSelectedWidget write SetSelectedWidget;
  end;


  TPropertyList = class(TObject)
  private
    FList: TList;
    function    GetCount: integer;
  public
    Widget: TfpgWidget;
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    property    Count: integer read GetCount;
    procedure   AddItem(aProp: TVFDWidgetProperty);
    function    GetItem(index: integer): TVFDWidgetProperty;
  end;


  TwgPropertyList = class(TfpgListBox)
  protected
    procedure   DrawItem(num: integer; rect: TfpgRect; flags: integer); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleSetFocus; override;
    procedure   HandleKillFocus; override;
    procedure   OnRowChange(Sender: TObject);
    procedure   OnScrolling(Sender: TObject);
    procedure   OnUpdateProperty(Sender: TObject);
  public
    Props: TPropertyList;
    NameWidth: integer;
    editor: TVFDPropertyEditor;
    NameDrag: boolean;
    NameDragPos: integer;
    constructor Create(AOwner: TComponent); override;
    procedure   ReleaseEditor;
    procedure   AllocateEditor;
    function    ItemCount: integer; override;
    function    RowHeight: integer; override;
    procedure   RealignEditor;
  end;


  TfrmProperties = class(TfpgForm)
  protected
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    l1, l2, l3, l4, l5, l6, l7, l8: TfpgLabel;
    lbClass: TfpgLabel;
    edName: TfpgEdit;
    edOther: TfpgMemo;
    btnTop, btnLeft, btnWidth, btnHeight: TfpgButton;
    btnAnLeft, btnAnTop, btnAnRight, btnAnBottom: TfpgButton;
    lstProps: TwgPropertyList;
    procedure   AfterCreate; override;
    procedure   BeforeDestruction; override;
  end;


  TfrmAbout = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: frmAbout}
    lblName1: TfpgLabel;
    lblVersion: TfpgLabel;
    btnName1: TfpgButton;
    lblName3: TfpgLabel;
    lblName4: TfpgHyperlink;
    lblCompiled: TfpgLabel;
    {@VFD_HEAD_END: frmAbout}
  public
    procedure AfterCreate; override;
    class procedure Execute;
  end;

{@VFD_NEWFORM_DECL}

var
  frmProperties: TfrmProperties;
  frmMain: TfrmMain;

  PropList: TPropertyList;

implementation

uses
  fpg_main,
  vfdmain,
  fpg_iniutils,
  fpg_dialogs;


// Anchor images
{$I anchors.inc}


{@VFD_NEWFORM_IMPL}

procedure TfrmAbout.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: frmAbout}
  Name := 'frmAbout';
  SetPosition(378, 267, 276, 180);
  WindowTitle := 'Product Information...';
  Hint := '';
  Sizeable := False;
  WindowPosition := wpScreenCenter;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(12, 16, 255, 31);
    FontDesc := 'Arial-20';
    Hint := '';
    Text := 'fpGUI UI Designer';
  end;

  lblVersion := TfpgLabel.Create(self);
  with lblVersion do
  begin
    Name := 'lblVersion';
    SetPosition(62, 48, 195, 20);
    Alignment := taRightJustify;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Version:  %s';
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(194, 148, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Close';
    Down := False;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.close';
    ModalResult := mrOK;
    TabOrder := 2;
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(12, 100, 241, 14);
    FontDesc := 'Arial-9';
    Hint := '';
    Text := 'Written by Graeme Geldenhuys';
  end;

  lblName4 := TfpgHyperlink.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(12, 116, 246, 14);
    Text := 'http://fpgui.sourceforge.net';
    URL := 'http://fpgui.sourceforge.net';
    FontDesc := 'Arial-9:underline';
    TextColor := clRoyalBlue;
    HotTrackColor := clBlue;
    HotTrackFont := 'Arial-9:underline';
  end;

  lblCompiled := TfpgLabel.Create(self);
  with lblCompiled do
  begin
    Name := 'lblCompiled';
    SetPosition(12, 132, 191, 13);
    FontDesc := 'Arial-8';
    Hint := '';
    Text := 'Compiled on:  %s';
  end;

  {@VFD_BODY_END: frmAbout}
  {%endregion}
end;

class procedure TfrmAbout.Execute;
var
  frm: TfrmAbout;
begin
  frm := TfrmAbout.Create(nil);
  try
    frm.lblVersion.Text := Format(frm.lblVersion.Text, [program_version]);
    frm.lblCompiled.Text := Format(frm.lblCompiled.Text, [ {$I %date%} + ' ' + {$I %time%}]);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.AfterCreate;
var
  n: integer;
  x, y: integer;
  wgc: TVFDWidgetClass;
  btn: TwgPaletteButton;
  mi: TfpgMenuItem;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: frmMain}
  Name := 'frmMain';
  SetPosition(338, 140, 754, 92);
  WindowTitle := 'frmMain';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpUser;
  MinHeight := 82;
  MinWidth := 315;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 753, 24);
    Align := alTop;
  end;

  btnNewForm := TfpgButton.Create(self);
  with btnNewForm do
  begin
    Name := 'btnNewForm';
    SetPosition(4, 28, 25, 24);
    Text := '';
    Down := False;
    FontDesc := '#Label1';
    Hint := 'Add New Form to Unit';
    ImageMargin := -1;
    ImageName := 'vfd.newform';
    ImageSpacing := 0;
    TabOrder := 1;
    Focusable := False;
    OnClick   := @(maindsgn.OnNewForm);
  end;

  btnOpen := TfpgButton.Create(self);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(30, 28, 25, 24);
    Text := '';
    Down := False;
    FontDesc := '#Label1';
    Hint := 'Open a file';
    ImageMargin := -1;
    ImageName := 'stdimg.open';
    ImageSpacing := 0;
    TabOrder := 2;
    Focusable := False;
    OnClick   := @(maindsgn.OnLoadFile);
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(56, 28, 25, 24);
    Text := '';
    Down := False;
    FontDesc := '#Label1';
    Hint := 'Save the current form design';
    ImageMargin := -1;
    ImageName := 'stdimg.save';
    ImageSpacing := 0;
    TabOrder := 3;
    Focusable := False;
    Tag := 10;
    OnClick   := @(maindsgn.OnSaveFile);
  end;

  wgpalette := TwgPalette.Create(self);
  with wgpalette do
  begin
    Name := 'wgpalette';
    SetPosition(152, 28, 600, 62);
    Anchors := [anLeft,anRight,anTop,anBottom];
    //    Width := self.Width - Left - 3;
    Focusable := False;
    OnResize := @PaletteBarResized;
  end;

  chlPalette := TfpgComboBox.Create(self);
  with chlPalette do
  begin
    Name := 'chlPalette';
    SetPosition(4, 67, 144, 22);
    Anchors := [anLeft,anBottom];
    FontDesc := '#List';
    Hint := '';
    Items.Add('-');
    TabOrder := 5;
    FocusItem := 0;
  end;

  filemenu := TfpgPopupMenu.Create(self);
  with filemenu do
  begin
    Name := 'filemenu';
    SetPosition(464, 64, 120, 20);
    AddMenuItem('Create New File...', 'Ctrl+N', @(maindsgn.OnNewFile));
    AddMenuItem('Open...', 'Ctrl+O', @(maindsgn.OnLoadFile));
    FFileOpenRecent := AddMenuItem('Open Recent...', '', nil);
    AddMenuItem('-', '', nil);
    mi := AddMenuItem('Save', 'Ctrl+S', @(maindsgn.OnSaveFile));
    mi.Tag := 10;
    AddMenuItem('Save As New Template Unit...', 'Ctrl+Shift+S', @(maindsgn.OnSaveFile));
    AddMenuItem('-', '', nil);
    AddMenuItem('Add New Form to Unit...', '', @(maindsgn.OnNewForm));
    AddMenuItem('-', '', nil);
    AddMenuItem('Exit', 'Ctrl+Q', @(maindsgn.OnExit));
  end;

  formmenu := TfpgPopupMenu.Create(self);
  with formmenu do
  begin
    Name := 'formmenu';
    SetPosition(464, 48, 120, 20);
    AddMenuItem('Widget Order...', '', @(maindsgn.OnEditWidgetOrder));
    AddMenuItem('Tab Order...', '', @(maindsgn.OnEditTabOrder));
    AddMenuItem('-', '', nil);
    AddMenuItem('Edit special...', '', nil).Enabled := False; // TODO
  end;

  setmenu := TfpgPopupMenu.Create(self);
  with setmenu do
  begin
    Name := 'setmenu';
    SetPosition(464, 29, 120, 20);
    AddMenuItem('General options...', '', @(maindsgn.OnOptionsClick));
  end;

  miOpenRecentMenu := TfpgPopupMenu.Create(self);
  with miOpenRecentMenu do
  begin
    Name := 'miOpenRecentMenu';
    SetPosition(336, 68, 128, 20);
  end;

  helpmenu := TfpgPopupMenu.Create(self);
  with helpmenu do
  begin
    Name := 'helpmenu';
    SetPosition(328, 52, 120, 20);
    AddMenuItem('About fpGUI Toolkit...', '', @miHelpAboutGUI);
    AddMenuItem('Product Information...', '', @miHelpAboutClick);
  end;

  previewmenu := TfpgPopupMenu.Create(self);
  with previewmenu do
  begin
    Name := 'previewmenu';
    SetPosition(324, 36, 120, 20);
    AddMenuItem('with Windows 9x', '', nil).Enabled := False;
    AddMenuItem('with Windows XP', '', nil).Enabled := False;
    AddMenuItem('with OpenSoft', '', nil).Enabled := False;
    AddMenuItem('with Motif', '', nil).Enabled := False;
    AddMenuItem('with OpenLook', '', nil).Enabled := False;
  end;

  {@VFD_BODY_END: frmMain}
  {%endregion}

  { Build component palette }
  x := 0;
  y := 0;
  for n := 0 to VFDWidgetCount-1 do
  begin
    wgc           := VFDWidget(n);
    btn           := TwgPaletteButton.Create(wgpalette);
    btn.VFDWidget := wgc;
    btn.SetPosition(x, y, 30, 28);
    btn.ImageName := wgc.WidgetIconName;
    btn.ImageMargin := -1;
    btn.Text      := '';
    btn.Hint      := wgc.WidgetClass.ClassName;
    btn.Focusable := False;
    btn.OnClick   := @OnPaletteClick;
    btn.AllowDown := True;
    btn.AllowAllUp := True;
    chlPalette.Items.AddObject(wgc.WidgetClass.ClassName, wgc);

    Inc(x, 32);
    if (x+30) >= wgpalette.Width then
    begin
      x := 0;
      Inc(y, 30);
    end;
  end;

  chlPalette.Items.Sort;
  MainMenu.AddMenuItem('&File', nil).SubMenu     := filemenu;
  MainMenu.AddMenuItem('&Settings', nil).SubMenu := setmenu;
  MainMenu.AddMenuItem('Fo&rm', nil).SubMenu     := formmenu;
  MainMenu.AddMenuItem('&Preview', nil).SubMenu  := previewmenu;
  MainMenu.AddMenuItem('&Help', nil).SubMenu     := helpmenu;
  
  FFileOpenRecent.SubMenu := miOpenRecentMenu;

  mru := TfpgMRU.Create(self);
  mru.ParentMenuItem  := miOpenRecentMenu;
  mru.OnClick         := @miMRUClick;
  mru.MaxItems        := gINI.ReadInteger('Options', 'MRUFileCount', 4);
  mru.ShowFullPath    := gINI.ReadBool('Options', 'ShowFullPath', True);
  mru.LoadMRU;
end;

procedure TfrmMain.BeforeDestruction;
begin
  gINI.WriteFormState(self);
  inherited BeforeDestruction;
end;

procedure TfrmMain.OnPaletteClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  i := -1;
  if TwgPaletteButton(Sender).Down then
  begin
    s := TwgPaletteButton(Sender).VFDWidget.WidgetClass.ClassName;
    i := chlPalette.Items.IndexOf(s);
  end;
  if i = -1 then
    i := 0; // select the '-' item
  chlPalette.FocusItem := i;
end;

{ TfrmProperties }

procedure TfrmProperties.AfterCreate;
var
  x, x2, w, y, gap: integer;
begin
  {%region 'Auto-generated GUI code' -fold}
  inherited;
  Name := 'frmProperties';
  WindowPosition := wpUser;
  WindowTitle := 'Properties';
  SetPosition(43, 150, 250, 450);
  gINI.ReadFormState(self);

  fpgImages.AddMaskedBMP(
    'vfd.anchorleft', @vfd_anchorleft,
    sizeof(vfd_anchorleft), 0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.anchorright', @vfd_anchorright,
    sizeof(vfd_anchorright), 0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.anchortop', @vfd_anchortop,
    sizeof(vfd_anchortop), 0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.anchorbottom', @vfd_anchorbottom,
    sizeof(vfd_anchorbottom), 0, 0);

  x     := 3;
  x2    := x + 50;
  gap   := 20;
  w     := Width - x2;
  y     := 3;

  l1      := CreateLabel(self, 0, y, 'Class:');
  lbClass := CreateLabel(self, x2, y, 'CLASS');
  lbClass.Width := w;
  lbClass.FontDesc := '#Label2';
  lbClass.Anchors := [anLeft, anRight, anTop];
  Inc(y, gap);

  l2           := CreateLabel(self, 0, y + 1, 'Name:');
  edName       := CreateEdit(self, x2, y, w, 0);
  edName.Text  := 'NAME';
  edName.Anchors := [anLeft, anRight, anTop];
  edName.OnChange := @(maindsgn.OnPropNameChange);

  Inc(y, gap + 5);

  lstProps         := TwgPropertyList.Create(self);
  lstProps.SetPosition(0, y, Width, self.Height - y - 220);
  lstProps.Anchors := AllAnchors;
  lstProps.Props   := PropList;
  lstProps.Props.Widget := edName;

  y := lstProps.Bottom + 5;

  //inc(y, gap+5);

  l3         := CreateLabel(self, 3, y + 1, 'Left:');
  l3.Anchors := [anLeft, anBottom];
  btnLeft    := CreateButton(self, 50, y - 2, 48, '1234', @(maindsgn.OnPropPosEdit));
  with btnLeft do
  begin
    Height        := 22;
    Anchors       := [anLeft, anBottom];
    Focusable     := False;
  end;
  l4 := CreateLabel(self, 110, y, 'Top:');
  l4.Anchors := [anLeft, anBottom];
  btnTop     := CreateButton(self, 160, y - 2, 48, '45', @(maindsgn.OnPropPosEdit));
  with btnTop do
  begin
    Height        := 22;
    Anchors       := [anLeft, anBottom];
    Focusable     := False;
  end;
  Inc(y, gap + 5);
  l5         := CreateLabel(self, 3, y + 1, 'Width:');
  l5.Anchors := [anLeft, anBottom];
  btnWidth   := CreateButton(self, 50, y - 2, 48, '1234', @(maindsgn.OnPropPosEdit));
  with btnWidth do
  begin
    Height        := 22;
    Anchors       := [anLeft, anBottom];
    Focusable     := False;
  end;
  l6 := CreateLabel(self, 110, y, 'Height:');
  l6.Anchors := [anLeft, anBottom];
  btnHeight  := CreateButton(self, 160, y - 2, 48, '45', @(maindsgn.OnPropPosEdit));
  with btnHeight do
  begin
    Height        := 22;
    Anchors       := [anLeft, anBottom];
    Focusable     := False;
  end;
  Inc(y, gap + 5);

  l8         := CreateLabel(self, 3, y + 1, 'Anchors:');
  l8.Anchors := [anLeft, anBottom];

  x := 64;

  btnAnLeft := CreateButton(self, x, y - 2, 28, '', nil);
  with btnAnLeft do
  begin
    ImageName  := 'vfd.anchorleft';
    ShowImage  := True;
    AllowAllUp := True;
    GroupIndex := 1;
    Focusable  := False;
    Anchors    := [anLeft, anBottom];
    OnClick    := @(maindsgn.OnAnchorChange);
  end;

  Inc(x, 30);
  btnAnTop := CreateButton(self, x, y - 2, 26, '', nil);
  with btnAnTop do
  begin
    ImageName  := 'vfd.anchortop';
    ShowImage  := True;
    AllowAllUp := True;
    GroupIndex := 2;
    Focusable  := False;
    Anchors    := [anLeft, anBottom];
    OnClick    := @(maindsgn.OnAnchorChange);
  end;

  Inc(x, 30);
  btnAnBottom := CreateButton(self, x, y - 2, 26, '', nil);
  with btnAnBottom do
  begin
    ImageName  := 'vfd.anchorbottom';
    ShowImage  := True;
    AllowAllUp := True;
    GroupIndex := 3;
    Focusable  := False;
    Anchors    := [anLeft, anBottom];
    OnClick    := @(maindsgn.OnAnchorChange);
  end;

  Inc(x, 30);
  btnAnRight := CreateButton(self, x, y - 2, 26, '', nil);
  with btnAnRight do
  begin
    ImageName  := 'vfd.anchorright';
    ShowImage  := True;
    AllowAllUp := True;
    GroupIndex := 4;
    Focusable  := False;
    Anchors    := [anLeft, anBottom];
    OnClick    := @(maindsgn.OnAnchorChange);
  end;

  y := btnAnRight.Bottom + 5;

  l7         := CreateLabel(self, 0, y, 'Unknown lines:');
  l7.Anchors := [anLeft, anBottom];
  Inc(y, 16);

  edOther          := TfpgMemo.Create(self);
  edOther.SetPosition(0, y, self.Width, self.Height - y);
  edOther.Anchors  := [anLeft, anRight, anBottom];
  edOther.FontDesc := '#Edit2';
  edOther.OnChange := @(maindsgn.OnOtherChange);
  {%endregion}
end;

procedure TfrmProperties.BeforeDestruction;
begin
  gINI.WriteFormState(self);
  inherited BeforeDestruction;
end;

procedure TfrmProperties.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyF11 then
  begin
    if maindsgn.selectedform <> nil then
    begin
      maindsgn.selectedform.Form.SetFocus;
      maindsgn.selectedform.Form.ActivateWindow;
    end;
    consumed := True;
  end;
  inherited;
end;

{ TPropertyList }

procedure TPropertyList.AddItem(aProp: TVFDWidgetProperty);
begin
  FList.Add(aProp);
end;

procedure TPropertyList.Clear;
begin
  FList.Clear;
  Widget := nil;
end;

constructor TPropertyList.Create;
begin
  FList  := TList.Create;
  Widget := nil;
end;

destructor TPropertyList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TPropertyList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TPropertyList.GetItem(index: integer): TVFDWidgetProperty;
begin
  if (index < 0) or (index > Count-1) then
    Result := nil
  else
    Result := TVFDWidgetProperty(FList[index]);
end;

{ TwgPropertyList }

constructor TwgPropertyList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NameWidth       := 80;
  editor          := nil;
  OnChange        := @OnRowChange;
  OnScroll        := @OnScrolling;
  BackgroundColor := clWindowBackground;
  NameDrag        := False;
  //FontName := 'arial-10:antialias=false';
end;

procedure TwgPropertyList.OnRowChange(Sender: TObject);
begin
  AllocateEditor;
end;

procedure TwgPropertyList.OnScrolling(Sender: TObject);
begin
  AllocateEditor;
end;

procedure TwgPropertyList.DrawItem(num: integer; rect: TfpgRect; flags: integer);
var
  x,
  y,
  fy: integer;
  s: string;
  prop: TVFDWidgetProperty;
  r: TfpgRect;
begin
  prop := Props.GetItem(num);
  if prop = nil then
    Exit; //==>

  x  := rect.left;
  y  := rect.top;
  fy := y + rect.Height div 2 - FFont.Height div 2;

  s := prop.Name;
  Canvas.DrawString(x + 1, fy, s);

  Inc(x, NameWidth);
  Canvas.SetColor(clShadow1);
  Canvas.DrawLine(x, rect.top, x, rect.bottom);
  Inc(x);
  // Drawing the contents
  r.SetRect(x, y, rect.right - x, rect.Height);
  Canvas.SetColor(BackgroundColor);
  Canvas.FillRectangle(r);
  Canvas.SetTextColor(clText1);
  Inc(r.left, 2);
  Dec(r.Width, 2);
  prop.DrawValue(props.Widget, Canvas, r, flags);

  Canvas.SetColor(clShadow1);
  Canvas.DrawLine(0, rect.bottom, rect.right, rect.bottom);
end;

function TwgPropertyList.ItemCount: integer;
begin
  Result := Props.Count;
end;

function TwgPropertyList.RowHeight: integer;
begin
  Result := 23;
end;

procedure TwgPropertyList.OnUpdateProperty(Sender: TObject);
begin
  editor.StoreValue(props.Widget);
end;

procedure TwgPropertyList.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
begin
  if not NameDrag then
  begin
    if (x >= FMargin + NameWidth - 2) and (x <= FMargin + NameWidth + 2) then
      MouseCursor := mcSizeEW
    else
      MouseCursor := mcDefault;
  end
  else
  begin
    NameWidth := x - FMargin;
    ReAlignEditor;
    RePaint;
  end;
  inherited;
end;

procedure TwgPropertyList.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  if MouseCursor = mcSizeEW then
    NameDrag := True
    //NameDragPos := x;
  else
    inherited;
end;

procedure TwgPropertyList.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  if NameDrag then
    NameDrag := False
  else
    inherited;
  if (Editor <> nil) and (Editor.Visible) then
    Editor.SetFocus;
end;

procedure TwgPropertyList.HandleMouseScroll(x, y: integer;
  shiftstate: TShiftState; delta: smallint);
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  AllocateEditor;
end;

procedure TwgPropertyList.HandleSetFocus;
begin
  inherited HandleSetFocus;
  if Editor <> nil then
    Editor.Visible := True
  else
    AllocateEditor;
end;

procedure TwgPropertyList.HandleKillFocus;
begin
  inherited HandleKillFocus;
  if Editor <> nil then
    Editor.Visible := True;
end;

procedure TwgPropertyList.RealignEditor;
var
  x: integer;
begin
  if editor = nil then
    Exit;
  x := 3 + NameWidth;
  editor.SetPosition(x, editor.Top, Width - ScrollBarWidth - x, editor.Height);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  gINI.ReadFormState(self);
  UpdateWindowPosition;
end;

procedure TfrmMain.PaletteBarResized(Sender: TObject);
var
  btn: TwgPaletteButton;
  x, y, n: integer;
begin
  x := 0;
  y := 0;
  for n := 0 to wgPalette.ComponentCount-1 do
  begin
    btn := wgPalette.Components[n] as TwgPaletteButton;
    btn.SetPosition(x, y, 30, 28);
    btn.ImageMargin   := -1;
    btn.ImageSpacing  := 0;
    Inc(x, 32);
    if (x+30) >= wgpalette.Width then
    begin
      x := 0;
      Inc(y, 30);
    end;
  end;
end;

procedure TfrmMain.miHelpAboutClick(Sender: TObject);
begin
  TfrmAbout.Execute;
end;

procedure TfrmMain.miHelpAboutGUI(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui;
end;

procedure TfrmMain.miMRUClick(Sender: TObject; const FileName: string);
begin
  maindsgn.EditedFileName := FileName;
  maindsgn.OnLoadFile(maindsgn);
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow := @FormShow;
end;

function TfrmMain.GetSelectedWidget: TVFDWidgetClass;
begin
  if chlPalette.FocusItem > 0 then
    Result := TVFDWidgetClass(chlPalette.Items.Objects[chlPalette.FocusItem])
  else
    Result := nil;
end;

procedure TfrmMain.SetSelectedWidget(wgc: TVFDWidgetClass);
var
  n: integer;
begin
  if wgc = nil then
  begin
    chlPalette.FocusItem := 0;
    for n := 0 to wgpalette.ComponentCount - 1 do
      if wgpalette.Components[n] is TwgPaletteButton then
        TwgPaletteButton(wgpalette.Components[n]).Down := False;
  end;
end;

procedure TwgPropertyList.ReleaseEditor;
begin
  self.ActiveWidget := nil;
  if editor <> nil then
    editor.Free;
  editor := nil;
end;

procedure TwgPropertyList.AllocateEditor;
var
  x, y: integer;
  prop: TVFDWidgetProperty;
begin
  prop := Props.GetItem(FFocusItem);
  if prop = nil then
    Exit;

  self.ActiveWidget := nil;
  if editor <> nil then
    editor.Free;

  editor := prop.CreateEditor(Self);
  x      := 3 + NameWidth;
  y      := FMargin + ((FFocusItem - FFirstItem) * RowHeight);
  editor.SetPosition(x, y, Width - FMargin - ScrollBarWidth - x, RowHeight-1); // last -1 is so cell border lines are still visible
  editor.CreateLayout;
  editor.OnUpdate := @OnUpdateProperty;
  editor.LoadValue(Props.Widget);
  editor.Visible := True;

  self.ActiveWidget := editor;
end;

{ TwgPalette }

procedure TwgPalette.HandlePaint;
begin
  Canvas.Clear(clWindowBackground);
end;


end.

