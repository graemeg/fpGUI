{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Essential classes used by the uiDesigner
}

unit newformdesigner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  gfxbase,
  gfx_widget,
  gui_form,
  gui_label,
  gui_button,
  gui_edit,
  gui_listbox,
  gui_memo,
  gui_combobox,
  gui_menu,
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
  public
    {@VFD_HEAD_BEGIN: frmMain}
    btnOpen: TfpgButton;
    MainMenu: TfpgMenuBar;
    btnSave: TfpgButton;
    wgpalette: TwgPalette;
    chlPalette: TfpgComboBox;
    {@VFD_HEAD_END: frmMain}

    FileMenu: TfpgPopupMenu;
    FormMenu: TfpgPopupMenu;
    SetMenu: TfpgPopupMenu;
    function GetSelectedWidget: TVFDWidgetClass;
    procedure SetSelectedWidget(wgc: TVFDWidgetClass);
    procedure AfterCreate; override;
    procedure OnPaletteClick(Sender: TObject);
    property SelectedWidget: TVFDWidgetClass read GetSelectedWidget write SetSelectedWidget;
  end;


  TPropertyList = class(TObject)
  private
    FList: TList;
  public
    Widget: TfpgWidget;
    constructor Create;
    destructor Destroy; override;
    function GetCount: integer;
    procedure Clear;
    property Count: integer read GetCount;
    procedure AddItem(aProp: TVFDWidgetProperty);
    function GetItem(index: integer): TVFDWidgetProperty;
  end;


  TwgPropertyList = class(TfpgListBox)
  protected
    procedure DrawItem(num: integer; rect: TfpgRect; flags: integer); override;
    procedure HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleSetFocus; override;
    procedure HandleKillFocus; override;
    procedure OnRowChange(Sender: TObject);
    procedure OnUpdateProperty(Sender: TObject);
  public
    Props: TPropertyList;
    NameWidth: integer;
    editor: TVFDPropertyEditor;
    NameDrag: boolean;
    NameDragPos: integer;
    BackgroundColor: TfpgColor;
    procedure ReleaseEditor;
    procedure AllocateEditor;
    constructor Create(AOwner: TComponent); override;
    function ItemCount: integer; override;
    function RowHeight: integer; override;
    procedure RealignEditor;
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
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

var
  frmProperties: TfrmProperties;
  frmMain: TfrmMain;

  PropList: TPropertyList;

implementation

uses
  fpgfx,
  vfdmain;

const
  vfd_anchorbottom: array[0..121] of byte = (
    66, 77, 122, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 0, 40, 0, 0,
    0, 15, 0, 0, 0, 15, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
    60, 0, 0, 0, 235, 10, 0, 0, 235, 10, 0, 0, 2, 0, 0, 0, 2,
    0, 0, 0, 255, 255, 255, 0, 0, 0, 0, 0, 255, 254, 0, 0, 128, 2,
    0, 0, 254, 254, 0, 0, 252, 126, 0, 0, 248, 62, 0, 0, 240, 30, 0,
    0, 255, 254, 0, 0, 255, 254, 0, 0, 255, 254, 0, 0, 255, 254, 0, 0,
    255, 254, 0, 0, 255, 254, 0, 0, 255, 254, 0, 0, 255, 254, 0, 0, 255,
    254, 0, 0);

  vfd_anchorleft: array[0..121] of byte = (
    66, 77, 122, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 0, 40, 0, 0,
    0, 15, 0, 0, 0, 15, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
    60, 0, 0, 0, 235, 10, 0, 0, 235, 10, 0, 0, 2, 0, 0, 0, 2,
    0, 0, 0, 255, 255, 255, 0, 0, 0, 0, 0, 255, 254, 0, 0, 191, 254,
    0, 0, 191, 254, 0, 0, 191, 254, 0, 0, 187, 254, 0, 0, 179, 254, 0,
    0, 163, 254, 0, 0, 131, 254, 0, 0, 163, 254, 0, 0, 179, 254, 0, 0,
    187, 254, 0, 0, 191, 254, 0, 0, 191, 254, 0, 0, 191, 254, 0, 0, 255,
    254, 0, 0);

  vfd_anchorright: array[0..121] of byte = (
    66, 77, 122, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 0, 40, 0, 0,
    0, 15, 0, 0, 0, 15, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
    60, 0, 0, 0, 235, 10, 0, 0, 235, 10, 0, 0, 2, 0, 0, 0, 2,
    0, 0, 0, 255, 255, 255, 0, 0, 0, 0, 0, 255, 254, 0, 0, 255, 250,
    0, 0, 255, 250, 0, 0, 255, 250, 0, 0, 255, 186, 0, 0, 255, 154, 0,
    0, 255, 138, 0, 0, 255, 130, 0, 0, 255, 138, 0, 0, 255, 154, 0, 0,
    255, 186, 0, 0, 255, 250, 0, 0, 255, 250, 0, 0, 255, 250, 0, 0, 255,
    254, 0, 0);

  vfd_anchortop: array[0..121] of byte = (
    66, 77, 122, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 0, 40, 0, 0,
    0, 15, 0, 0, 0, 15, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
    60, 0, 0, 0, 235, 10, 0, 0, 235, 10, 0, 0, 2, 0, 0, 0, 2,
    0, 0, 0, 255, 255, 255, 0, 0, 0, 0, 0, 255, 254, 0, 0, 255, 254,
    0, 0, 255, 254, 0, 0, 255, 254, 0, 0, 255, 254, 0, 0, 255, 254, 0,
    0, 255, 254, 0, 0, 255, 254, 0, 0, 255, 254, 0, 0, 240, 30, 0, 0,
    248, 62, 0, 0, 252, 126, 0, 0, 254, 254, 0, 0, 128, 2, 0, 0, 255,
    254, 0, 0);

{@VFD_NEWFORM_IMPL}

procedure TfrmMain.AfterCreate;
var
  n: integer;
  x: integer;
  wgc: TVFDWidgetClass;
  btn: TwgPaletteButton;
  mi: TfpgMenuItem;
begin
  {@VFD_BODY_BEGIN: frmMain}
  WindowPosition := wpUser;
  WindowTitle   := 'frmMain';
  SetPosition(0, 0, 506, 87);

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
    SetPosition(0, 0, 500, 24);

  btnOpen := TfpgButton.Create(self);
  with btnOpen do
  begin
    SetPosition(4, 40, 25, 24);
    Text      := '';
    ImageName := 'stdimg.open';
    ShowImage := True;
    Focusable := False;
    OnClick   := @(maindsgn.OnLoadFile);
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    SetPosition(32, 40, 25, 24);
    Text      := '';
    ImageName := 'stdimg.save';
    ShowImage := True;
    Focusable := False;
    OnClick   := @(maindsgn.OnSaveFile);
  end;

  wgpalette := TwgPalette.Create(self);
  with wgpalette do
    SetPosition(116, 28, 384, 28);

  chlPalette := TfpgComboBox.Create(self);
  with chlPalette do
  begin
    SetPosition(116, 60, 386, 22);
    Items.Add('-');
  end;

  {@VFD_BODY_END: frmMain}


  wgpalette.Focusable := False;

  x := 0;
  for n := 1 to VFDWidgetCount do
  begin
    wgc           := VFDWidget(n);
    btn           := TwgPaletteButton.Create(wgpalette);
    btn.VFDWidget := wgc;
    btn.SetPosition(x, 0, 30, 28);
    btn.ImageName := wgc.WidgetIconName;
    btn.ImageMargin := -1;
    btn.Text      := '';
    btn.Focusable := False;
    btn.OnClick   := @OnPaletteClick;
    btn.GroupIndex := 1;
    btn.AllowAllUp := True;
    chlPalette.Items.AddObject(wgc.WidgetClass.ClassName, wgc);

    Inc(x, 32);
  end;

  filemenu := TfpgPopupMenu.Create(self);
  with filemenu do
  begin
    mi         := AddMenuItem('New', '', nil);
    mi.OnClick := @(maindsgn.OnNewFile);
    mi         := AddMenuItem('Open', '', nil);
    mi.OnClick := @(maindsgn.OnLoadFile);
    mi         := AddMenuItem('Save', '', nil);
    mi.OnClick := @(maindsgn.OnSaveFile);
    AddMenuItem('-', '', nil);
    mi         := AddMenuItem('New Form...', '', nil);
    mi.OnClick := @(maindsgn.OnNewForm);
    AddMenuItem('-', '', nil);
    mi         := AddMenuItem('Exit', '', nil);
    mi.OnClick := @(maindsgn.OnExit);
  end;

  formmenu := TfpgPopupMenu.Create(self);
  with formmenu do
  begin
    mi         := AddMenuItem('Widget Order...', '', nil);
    mi.OnClick := @(maindsgn.OnEditWidgetOrder);
    AddMenuItem('-', '', nil);
    AddMenuItem('Edit special...', '', nil);
  end;

  setmenu := TfpgPopupMenu.Create(self);
  with setmenu do
  begin
    mi         := AddMenuItem('General options ...', '', nil);
    mi.OnClick := @(maindsgn.OnOptionsClick);
  end;

  MainMenu.AddMenuItem('&File', nil).SubMenu     := filemenu;
  MainMenu.AddMenuItem('&Settings', nil).SubMenu := setmenu;
  MainMenu.AddMenuItem('Fo&rm', nil).SubMenu     := formmenu;
end;

procedure TfrmMain.OnPaletteClick(Sender: TObject);
var
  s: string;
  i: integer;
begin
  if TwgPaletteButton(Sender).Down then
  begin
    s := TwgPaletteButton(Sender).VFDWidget.WidgetClass.ClassName;
    i := chlPalette.Items.IndexOf(s);
    if i >= 0 then
      chlPalette.FocusItem := i + 1;
  end
  else
    chlPalette.FocusItem := 1;
end;

{ TfrmProperties }

procedure TfrmProperties.AfterCreate;
var
  x, x2, w, y, gap: integer;
begin
  inherited;

  fpgImages.AddBMP(
    'vfd.anchorleft', @vfd_anchorleft,
    sizeof(vfd_anchorleft)
    );

  fpgImages.AddBMP(
    'vfd.anchorright', @vfd_anchorright,
    sizeof(vfd_anchorright)
    );

  fpgImages.AddBMP(
    'vfd.anchortop', @vfd_anchortop,
    sizeof(vfd_anchortop)
    );

  fpgImages.AddBMP(
    'vfd.anchorbottom', @vfd_anchorbottom,
    sizeof(vfd_anchorbottom)
    );


  WindowPosition := wpUser;
  WindowTitle := 'Properties';
  SetPosition(0, 120, 250, 450);

  x   := 3;
  x2  := x + 50;
  gap := 20;
  w := Width - x2;
  y := 3;

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
    Height          := 22;
    btnLeft.Anchors := [anLeft, anBottom];
    Focusable       := False;
  end;
  l4 := CreateLabel(self, 110, y, 'Top:');
  l4.Anchors := [anLeft, anBottom];
  btnTop     := CreateButton(self, 160, y - 2, 48, '45', @(maindsgn.OnPropPosEdit));
  with btnTop do
  begin
    Height          := 22;
    btnLeft.Anchors := [anLeft, anBottom];
    Focusable       := False;
  end;
  Inc(y, gap + 5);
  l5         := CreateLabel(self, 3, y + 1, 'Width:');
  l5.Anchors := [anLeft, anBottom];
  btnWidth   := CreateButton(self, 50, y - 2, 48, '1234', @(maindsgn.OnPropPosEdit));
  with btnWidth do
  begin
    Height          := 22;
    btnLeft.Anchors := [anLeft, anBottom];
    Focusable       := False;
  end;
  l6 := CreateLabel(self, 110, y, 'Height:');
  l6.Anchors := [anLeft, anBottom];
  btnHeight  := CreateButton(self, 160, y - 2, 48, '45', @(maindsgn.OnPropPosEdit));
  with btnHeight do
  begin
    Height          := 22;
    btnLeft.Anchors := [anLeft, anBottom];
    Focusable       := False;
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
end;

procedure TfrmProperties.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyEnter) or (keycode = keyF11) then
  begin
    if maindsgn.selectedform <> nil then
      maindsgn.selectedform.Form.SetFocus;
//      GfxActivateWindow(maindsgn.selectedform.Form.WinHandle);
    consumed := True;
  end
  else
    inherited;
end;

{ TPropertyList }

procedure TPropertyList.AddItem(aProp: TVFDWidgetProperty);
begin
  {
  result := TPropertyLine.Create;
  result.name := aPropName;
  result.propclass := apropclass;
  result.value := aPropName;
}
  FList.Add(aProp);
end;

procedure TPropertyList.Clear;
 //var
 //  n : integer;
begin
  //for n:=0 to FList.Count-1 do TObject(FList[n]).Free;
  FList.Clear;
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
  if (index < 1) or (index > Count) then
    Result := nil
  else
    Result := TVFDWidgetProperty(FList[index - 1]);
end;

{ TwgPropertyList }

constructor TwgPropertyList.Create(AOwner: TComponent);
begin
  inherited;
  NameWidth       := 80;
  editor          := nil;
  OnChange        := @OnRowChange;
  BackgroundColor := clWindowBackground;
  NameDrag        := False;
  //FontName := 'arial-10:antialias=false';
end;

procedure TwgPropertyList.OnRowChange(Sender: TObject);
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
  //inherited;
  prop := Props.GetItem(num);
  if prop = nil then
    Exit;

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
  Canvas.SetColor(FBackgroundColor);
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
  Result := 22;
end;

procedure TwgPropertyList.OnUpdateProperty(Sender: TObject);
begin
//  writeln('updating property...');
  editor.StoreValue(props.Widget);
  props.Widget.UpdateWindowPosition;
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

function TfrmMain.GetSelectedWidget: TVFDWidgetClass;
begin
  if chlPalette.FocusItem > 1 then
    Result := TVFDWidgetClass(chlPalette.Items.Objects[chlPalette.FocusItem - 1])
  else
    Result := nil;
end;

procedure TfrmMain.SetSelectedWidget(wgc: TVFDWidgetClass);
var
  n: integer;
begin
  if wgc = nil then
  begin
    chlPalette.FocusItem := 1;
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
  y      := FMargin + (FFocusItem - FFirstItem) * RowHeight;
  editor.SetPosition(x, y - 1, Width - ScrollBarWidth - x, RowHeight);
  editor.CreateLayout;
  editor.OnUpdate := @OnUpdateProperty;
  editor.LoadValue(Props.Widget);
  editor.Visible := True;

  self.ActiveWidget := editor;
end;

{ TwgPalette }

procedure TwgPalette.HandlePaint;
begin
//  inherited HandlePaint;
  Canvas.BeginDraw;
  Canvas.Clear(clWindowBackground);
  Canvas.EndDraw;
end;

end.

