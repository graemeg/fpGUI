{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines the UI Designer's main form and Object Inspector and
      essential widgets (eg: Palette, Palette Button, Property List).
}

unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
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
  fpg_panel,
  fpg_tree,
  fpg_splitter,
  vfd_widgetclass,
  vfd_widgets;

const
  MIME_VFD_WIDGET_CLASS = 'x-object/fpgui-vfd-widgetclass';

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
    procedure   OnPaletteDragStart(Sender: TObject);
    procedure   PaintPaletteButtonForDrag(ASender: TfpgDrag; ACanvas: TfpgCanvas);
    procedure   PaletteBarResized(Sender: TObject);
    procedure   miHelpAboutClick(Sender: TObject);
    procedure   miHelpAboutGUI(Sender: TObject);
    procedure   miMRUClick(Sender: TObject; const FileName: string);
    procedure   SetupCaptions;
    procedure   BuildThemePreviewMenu;
    procedure   ToggleDesignerGrid(Sender: TObject);
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
    btnGrid: TfpgButton;
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
  private
    procedure   FormShow(Sender: TObject);
    procedure   FormResized(Sender: TObject);
    procedure   TreeSelectionChanged(Sender: TObject);
    procedure   SetHierarchyMaxHeight;
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    {@VFD_HEAD_BEGIN: frmProperties}
    Bevel1: TfpgBevel;
    TreeView1: TfpgTreeView;
    Splitter1: TfpgSplitter;
    bvlOI: TfpgBevel;
    l1: TfpgLabel;
    lbClass: TfpgLabel;
    l2: TfpgLabel;
    edName: TfpgEdit;
    lstProps: TwgPropertyList;
    l3: TfpgLabel;
    btnLeft: TfpgButton;
    l4: TfpgLabel;
    btnTop: TfpgButton;
    l5: TfpgLabel;
    btnWidth: TfpgButton;
    l6: TfpgLabel;
    btnHeight: TfpgButton;
    l8: TfpgLabel;
    btnAnLeft: TfpgButton;
    btnAnTop: TfpgButton;
    btnAnBottom: TfpgButton;
    btnAnRight: TfpgButton;
    l7: TfpgLabel;
    edOther: TfpgMemo;
    {@VFD_HEAD_END: frmProperties}
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
    procedure   BeforeDestruction; override;
    procedure   UpdateWidgetHierachyTreeview(AMainComp, ASelected: TComponent);
  end;


  TfrmAbout = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: frmAbout}
    lblAppName: TfpgLabel;
    lblVersion: TfpgLabel;
    btnClose: TfpgButton;
    lblWrittenBy: TfpgLabel;
    lblURL: TfpgHyperlink;
    lblCompiled: TfpgLabel;
    {@VFD_HEAD_END: frmAbout}
    procedure   SetupCaptions;
    procedure   FormShow(Sender: TObject);
  public
    procedure   AfterCreate; override;
    class procedure Execute;
  end;

{@VFD_NEWFORM_DECL}

var
  frmProperties: TfrmProperties;
  frmMain: TfrmMain;

  PropList: TPropertyList;

implementation

uses
  fpg_iniutils,
  fpg_dialogs,
  fpg_constants,
  fpg_stylemanager,
  fpg_window,
  vfd_main,
  vfd_constants;


// Anchor images
{$I anchors.inc}


{@VFD_NEWFORM_IMPL}

procedure TfrmAbout.SetupCaptions;
begin
  WindowTitle := rsDlgProductInfo;
  lblAppName.Text := cAppName;
  lblVersion.Text := Format(rsVersion, [cAppVersion]);
  lblWrittenBy.Text := Format(rsWrittenBy, ['Graeme Geldenhuys']);
  lblURL.URL := fpGUIWebsite;
  lblURL.Text := fpGUIWebsite;
  lblCompiled.Text := Format(rsCompiledOn, [{$I %date%} + ' ' + {$I %time%}]);
  btnClose.Text := rsClose;
end;

procedure TfrmAbout.FormShow(Sender: TObject);
begin
  SetupCaptions;
  lblURL.HotTrackColor := clBlue;
  lblURL.TextColor := clRoyalBlue;
end;

procedure TfrmAbout.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: frmAbout}
  Name := 'frmAbout';
  SetPosition(378, 267, 276, 180);
  WindowTitle := rsDlgProductInfo + '...';
  Hint := '';
  WindowPosition := wpScreenCenter;
  OnShow := @FormShow;

  lblAppName := TfpgLabel.Create(self);
  with lblAppName do
  begin
    Name := 'lblAppName';
    SetPosition(12, 16, 255, 31);
    FontDesc := 'Arial-20';
    Hint := '';
    Text := cAppName;
  end;

  lblVersion := TfpgLabel.Create(self);
  with lblVersion do
  begin
    Name := 'lblVersion';
    SetPosition(62, 48, 195, 20);
    Alignment := taRightJustify;
    FontDesc := '#Label2';
    Hint := '';
    Text := Format(rsVersion, [cAppVersion]);
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(194, 148, 75, 24);
    Anchors := [anRight,anBottom];
    Text := rsClose;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.close';
    ModalResult := mrOK;
    TabOrder := 2;
  end;

  lblWrittenBy := TfpgLabel.Create(self);
  with lblWrittenBy do
  begin
    Name := 'lblWrittenBy';
    SetPosition(12, 100, 241, 14);
    FontDesc := 'Arial-9';
    Hint := '';
    Text := Format(rsWrittenBy, ['Graeme Geldenhuys']);
  end;

  lblURL := TfpgHyperlink.Create(self);
  with lblURL do
  begin
    Name := 'lblURL';
    SetPosition(12, 116, 246, 14);
    FontDesc := 'Arial-9:underline';
    Hint := '';
    HotTrackColor := clBlue;
    HotTrackFont := 'Arial-9:underline';
    Text := 'http://fpgui.sourceforge.net';
    TextColor := clRoyalBlue;
    URL := 'http://fpgui.sourceforge.net';
  end;

  lblCompiled := TfpgLabel.Create(self);
  with lblCompiled do
  begin
    Name := 'lblCompiled';
    SetPosition(12, 132, 191, 13);
    FontDesc := 'Arial-8';
    Hint := '';
    Text := Format(rsCompiledOn, [{$I %date%} + ' ' + {$I %time%}]);
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
    FontDesc := '#Label1';
    Hint := rsAddNewFormToUnit;
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
    FontDesc := '#Label1';
    Hint := '';
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
    FontDesc := '#Label1';
    Hint := rsSaveCurrentFormDesign;
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
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    Items.Add('-');
    FocusItem := 0;
    TabOrder := 5;
  end;

  filemenu := TfpgPopupMenu.Create(self);
  with filemenu do
  begin
    Name := 'filemenu';
    SetPosition(464, 64, 120, 20);
    AddMenuItem(rsCreateNewFile + '...', 'Ctrl+N', @(maindsgn.OnNewFile));
    AddMenuItem(rsOpen + '...', 'Ctrl+O', @(maindsgn.OnLoadFile));
    FFileOpenRecent := AddMenuItem(rsOpenRecent + '...', '', nil);
    AddMenuItem('-', '', nil);
    mi := AddMenuItem(rsSave, 'Ctrl+S', @(maindsgn.OnSaveFile));
    mi.Tag := 10;
    AddMenuItem(rsSaveAsNewTemplateUnit + '...', 'Ctrl+Shift+S', @(maindsgn.OnSaveFile));
    AddMenuItem('-', '', nil);
    AddMenuItem(rsAddNewFormToUnit + '...', '', @(maindsgn.OnNewForm));
    AddMenuItem('-', '', nil);
    AddMenuItem(rsExit, 'Ctrl+Q', @(maindsgn.OnExit));
  end;

  formmenu := TfpgPopupMenu.Create(self);
  with formmenu do
  begin
    Name := 'formmenu';
    SetPosition(464, 48, 120, 20);
    AddMenuItem(rsDlgWidgetOrder + '...', '', @(maindsgn.OnEditWidgetOrder));
    AddMenuItem(rsDlgTabOrder + '...', '', @(maindsgn.OnEditTabOrder));
    AddMenuItem('-', '', nil);
    AddMenuItem(rsEditSpecial + '...', '', nil).Enabled := False; // TODO
  end;

  setmenu := TfpgPopupMenu.Create(self);
  with setmenu do
  begin
    Name := 'setmenu';
    SetPosition(464, 29, 120, 20);
    AddMenuItem(rsGeneralOptions + '...', '', @(maindsgn.OnOptionsClick));
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
    SetPosition(336, 49, 120, 20);
    AddMenuItem(rsAboutFpGuiToolkit + '...', '', @miHelpAboutGUI);
    AddMenuItem(rsDlgProductInfo + '...', '', @miHelpAboutClick);
  end;

  previewmenu := TfpgPopupMenu.Create(self);
  with previewmenu do
  begin
    Name := 'previewmenu';
    SetPosition(336, 30, 120, 20);
  end;

  btnGrid := TfpgButton.Create(self);
  with btnGrid do
  begin
    Name := 'btnGrid';
    SetPosition(103, 28, 25, 24);
    Text := '';
    AllowAllUp := True;
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := rsToggleDesignerGrid;
    ImageMargin := -1;
    ImageName := 'vfd.grid';
    ImageSpacing := 0;
    TabOrder := 13;
    Focusable := False;
    AllowDown := True;
    OnClick := @ToggleDesignerGrid;
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
    btn.OnDragStartDetected:=@OnPaletteDragStart;
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

  BuildThemePreviewMenu;

  chlPalette.Items.Sort;
  MainMenu.AddMenuItem(rsFile, nil).SubMenu     := filemenu;
  MainMenu.AddMenuItem(rsSettings, nil).SubMenu := setmenu;
  MainMenu.AddMenuItem(rsForm, nil).SubMenu     := formmenu;
  MainMenu.AddMenuItem(rsPreview, nil).SubMenu  := previewmenu;
  MainMenu.AddMenuItem(rsHelp, nil).SubMenu     := helpmenu;

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

procedure TfrmProperties.FormShow(Sender: TObject);
begin
  gINI.ReadFormState(self);
end;

procedure TfrmProperties.FormResized(Sender: TObject);
begin
  SetHierarchyMaxHeight;
end;

procedure TfrmProperties.TreeSelectionChanged(Sender: TObject);
var
  aObj: TfpgWidget;
  aName: string;
begin
  if (maindsgn.selectedform <> nil) then
  aName := Copy(TreeView1.Selection.Text, 1, pos(':', TreeView1.Selection.Text)-1);
  maindsgn.SelectedForm.DeselectAll;
  begin
    if (maindsgn.SelectedForm.Form.Name <> aName) then
    begin
      aObj := maindsgn.SelectedForm.FindWidgetByName(aName);
      if aObj <> nil then
        maindsgn.SelectedForm.WidgetDesigner(aObj).Selected := true;
    end
    else
      edName.Text := aName;
    maindsgn.SelectedForm.UpdatePropWin;
  end;
end;

procedure TfrmProperties.SetHierarchyMaxHeight;
begin
  Bevel1.MaxHeight := Round(Height * 0.4); // no more than 40% of window height
end;

procedure TfrmProperties.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: frmProperties}
  Name := 'frmProperties';
  SetPosition(43, 150, 250, 533);
  WindowTitle := rsProperties;
  Hint := '';
  IconName := '';
  WindowPosition := wpUser;
  MinHeight := 435;
  OnShow := @FormShow;
  OnResize := @FormResized;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 0, 250, 100);
    Hint := '';
    Shape := bsSpacer;
  end;

  TreeView1 := TfpgTreeView.Create(Bevel1);
  with TreeView1 do
  begin
    Name := 'TreeView1';
    SetPosition(4, 4, 242, 92);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 1;
    OnChange := @TreeSelectionChanged;
  end;

  Splitter1 := TfpgSplitter.Create(self);
  with Splitter1 do
  begin
    Name := 'Splitter1';
    SetPosition(0, 100, 250, 8);
    Align := alNone;
  end;

  bvlOI := TfpgBevel.Create(self);
  with bvlOI do
  begin
    Name := 'bvlOI';
    SetPosition(0, 108, 250, 461);
    Hint := '';
    Shape := bsSpacer;
  end;

  l1 := TfpgLabel.Create(bvlOI);
  with l1 do
  begin
    Name := 'l1';
    SetPosition(3, 3, 50, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := rsClass + ':';
  end;

  lbClass := TfpgLabel.Create(bvlOI);
  with lbClass do
  begin
    Name := 'lbClass';
    SetPosition(53, 3, 197, 15);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label2';
    Hint := '';
    Text := rsClassUpperCase;
  end;

  l2 := TfpgLabel.Create(bvlOI);
  with l2 do
  begin
    Name := 'l2';
    SetPosition(3, 27, 50, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := rsName + ':';
  end;

  edName := TfpgEdit.Create(bvlOI);
  with edName do
  begin
    Name := 'edName';
    SetPosition(53, 23, 195, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Label2';
    Hint := '';
    TabOrder := 4;
    Text := rsNameUpperCase;
    OnChange := @(maindsgn.OnPropNameChange);
  end;

  lstProps := TwgPropertyList.Create(bvlOI);
  with lstProps do
  begin
    Name := 'lstProps';
    SetPosition(0, 50, 250, 192);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Props := PropList;
    Props.Widget := edName;
  end;

  l3 := TfpgLabel.Create(bvlOI);
  with l3 do
  begin
    Name := 'l3';
    SetPosition(3, 248, 50, 15);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := rsLeft + ':';
  end;

  btnLeft := TfpgButton.Create(bvlOI);
  with btnLeft do
  begin
    Name := 'btnLeft';
    SetPosition(50, 245, 50, 22);
    Anchors := [anLeft,anBottom];
    Text := '1234';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    Focusable := False;
    OnClick := @(maindsgn.OnPropPosEdit);
  end;

  l4 := TfpgLabel.Create(bvlOI);
  with l4 do
  begin
    Name := 'l4';
    SetPosition(110, 248, 50, 15);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := rsTop + ':';
  end;

  btnTop := TfpgButton.Create(bvlOI);
  with btnTop do
  begin
    Name := 'btnTop';
    SetPosition(160, 245, 50, 22);
    Anchors := [anLeft,anBottom];
    Text := '1234';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 9;
    Focusable := False;
    OnClick := @(maindsgn.OnPropPosEdit);
  end;

  l5 := TfpgLabel.Create(bvlOI);
  with l5 do
  begin
    Name := 'l5';
    SetPosition(3, 272, 50, 15);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := rsWidth + ':';
  end;

  btnWidth := TfpgButton.Create(bvlOI);
  with btnWidth do
  begin
    Name := 'btnWidth';
    SetPosition(50, 269, 50, 22);
    Anchors := [anLeft,anBottom];
    Text := '1234';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 11;
    Focusable := False;
    OnClick := @(maindsgn.OnPropPosEdit);
  end;

  l6 := TfpgLabel.Create(bvlOI);
  with l6 do
  begin
    Name := 'l6';
    SetPosition(110, 272, 50, 15);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := rsHeight + ':';
  end;

  btnHeight := TfpgButton.Create(bvlOI);
  with btnHeight do
  begin
    Name := 'btnHeight';
    SetPosition(160, 269, 50, 22);
    Anchors := [anLeft,anBottom];
    Text := '1234';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 13;
    Focusable := False;
    OnClick := @(maindsgn.OnPropPosEdit);
  end;

  l8 := TfpgLabel.Create(bvlOI);
  with l8 do
  begin
    Name := 'l8';
    SetPosition(3, 299, 50, 15);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := rsAnchors + ':';
  end;

  btnAnLeft := TfpgButton.Create(bvlOI);
  with btnAnLeft do
  begin
    Name := 'btnAnLeft';
    SetPosition(64, 295, 26, 25);
    Anchors := [anLeft,anBottom];
    Text := '';
    AllowAllUp := True;
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := '';
    ImageName := 'vfd.anchorleft';
    TabOrder := 15;
    Focusable := False;
    OnClick := @(maindsgn.OnAnchorChange);
  end;

  btnAnTop := TfpgButton.Create(bvlOI);
  with btnAnTop do
  begin
    Name := 'btnAnTop';
    SetPosition(94, 295, 26, 25);
    Anchors := [anLeft,anBottom];
    Text := '';
    AllowAllUp := True;
    FontDesc := '#Label1';
    GroupIndex := 2;
    Hint := '';
    ImageName := 'vfd.anchortop';
    TabOrder := 16;
    Focusable := False;
    OnClick := @(maindsgn.OnAnchorChange);
  end;

  btnAnBottom := TfpgButton.Create(bvlOI);
  with btnAnBottom do
  begin
    Name := 'btnAnBottom';
    SetPosition(124, 295, 26, 25);
    Anchors := [anLeft,anBottom];
    Text := '';
    AllowAllUp := True;
    FontDesc := '#Label1';
    GroupIndex := 3;
    Hint := '';
    ImageName := 'vfd.anchorbottom';
    TabOrder := 17;
    Focusable := False;
    OnClick := @(maindsgn.OnAnchorChange);
  end;

  btnAnRight := TfpgButton.Create(bvlOI);
  with btnAnRight do
  begin
    Name := 'btnAnRight';
    SetPosition(154, 295, 26, 25);
    Anchors := [anLeft,anBottom];
    Text := '';
    AllowAllUp := True;
    FontDesc := '#Label1';
    GroupIndex := 4;
    Hint := '';
    ImageName := 'vfd.anchorright';
    TabOrder := 18;
    Focusable := False;
    OnClick := @(maindsgn.OnAnchorChange);
  end;

  l7 := TfpgLabel.Create(bvlOI);
  with l7 do
  begin
    Name := 'l7';
    SetPosition(3, 325, 200, 15);
    Anchors := [anLeft,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    Text := rsUnknownLines + ':';
  end;

  edOther := TfpgMemo.Create(bvlOI);
  with edOther do
  begin
    Name := 'edOther';
    SetPosition(0, 340, 250, 122);
    Anchors := [anLeft,anRight,anBottom];
    FontDesc := '#Edit2';
    Hint := '';
    TabOrder := 20;
    OnChange := @(maindsgn.OnOtherChange);
  end;

  {@VFD_BODY_END: frmProperties}
  {%endregion}

  Bevel1.Align := alTop;
  Splitter1.Align := alTop;
  bvlOI.Align := alClient;
end;

procedure TfrmProperties.BeforeDestruction;
begin
  gINI.WriteFormState(self);
  inherited BeforeDestruction;
end;

procedure TfrmProperties.UpdateWidgetHierachyTreeview(AMainComp: TComponent; ASelected: TComponent);

  procedure AddAllChildren(APar: TComponent; MainNode: TfpgTreeNode);
  var
    f: integer;
    fcd: TComponent;
    subNode: TfpgTreeNode;
  begin
    fcd := APar;
    for f := 0 to fcd.ComponentCount-1 do
    begin
      if (fcd.Components[f].Name = '') or (fcd.Components[f].ClassName = 'TwgResizer') then
        Continue;
      subNode := mainNode.AppendText(fcd.Components[f].Name + ': ' + fcd.Components[f].ClassName);
      if fcd.Components[f] = ASelected then
        TreeView1.Selection := subNode;
      if fcd.Components[f].ComponentCount > 0 then
        AddAllChildren(fcd.Components[f],subNode);
    end;
  end;

var
  s:string;
begin
  TreeView1.RootNode.Clear;
  if Assigned(AMainComp) then
  begin
    if AMainComp.ClassName = 'TDesignedForm' then
      s := AMainComp.Name + ': ' + 'TfpgForm'
    else
      s := AMainComp.Name + ': ' + AMainComp.ClassName;

    AddAllChildren(AMainComp, TreeView1.RootNode.AppendText(s));
    TreeView1.FullExpand;
  end
  else
    TreeView1.Invalidate;
end;

procedure TfrmProperties.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyF11 then
  begin
    if maindsgn.selectedform <> nil then
    begin
      maindsgn.selectedform.Form.SetFocus;
      maindsgn.selectedform.Form.Window.ActivateWindow;
      maindsgn.selectedform.Form.Window.BringToFront;
    end;
    consumed := True;
  end;
  inherited;
end;

constructor TfrmProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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
  UpdatePosition;
  SetupCaptions;
end;

type
  TDragHack = class(TfpgDrag);
  TWidgetHack = class(TfpgWidget);

procedure TfrmMain.OnPaletteDragStart(Sender: TObject);
var
  Drag: TfpgDrag;
  Preview: TfpgWindow;
  Button: TwgPaletteButton absolute Sender;
  Widget: TfpgWidget;
begin
  Drag := TfpgDrag.Create(Sender as TfpgWidget);
  Preview := TDragHack(Drag).FPreviewWin as TfpgWindow;
  Widget := Button.VFDWidget.CreateWidget(Preview);
  // set the position of the preview window
  TWidgetHack(Button).FDragStartPos := fpgPoint(0,0);
  Drag.MimeData := TfpgMimeData.Create;
  Drag.MimeData.Obj[MIME_VFD_WIDGET_CLASS] := Button.VFDWidget;
  Drag.PreviewSize := fpgSize(Widget.Width,Widget.Height);
  Drag.OnPaintPreview:=@PaintPaletteButtonForDrag;

  Drag.Execute();
  SelectedWidget := nil;
end;

procedure TfrmMain.PaintPaletteButtonForDrag(ASender: TfpgDrag; ACanvas: TfpgCanvas);
begin
  // Do Nothing, the widget paints itself
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

procedure TfrmMain.SetupCaptions;
begin
  btnOpen.Hint := rsOpenFormFile;
end;

procedure TfrmMain.BuildThemePreviewMenu;
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  fpgStyleManager.AssignStyleTypes(sl);
  sl.Sort;
  for i := 0 to sl.Count-1 do
  begin
    if sl[i] = 'auto' then
      continue;
    previewmenu.AddMenuItem(sl[i], '', nil).Enabled := False;
  end;
  sl.Free;
end;

procedure TfrmMain.ToggleDesignerGrid(Sender: TObject);
begin
  maindsgn.ShowGrid := btnGrid.Down;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpgImages.AddMaskedBMP(
    'vfd.grid', @vfd_grid,
    sizeof(vfd_grid), 0, 0);

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

