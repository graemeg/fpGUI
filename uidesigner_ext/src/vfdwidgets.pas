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
      Setting up of widgets, properties and images.
}

unit vfdwidgets;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  contnrs,
  vfdwidgetclass,
  vfdprops,
  typinfo;

procedure RegisterWidgets;
procedure RegisterVFDWidget(awc: TVFDWidgetClass);
function  VFDWidgetCount: integer;
function  VFDWidget(ind: integer): TVFDWidgetClass;
function  VFDFormWidget: TVFDWidgetClass;

var
  VFDOtherWidget: TVFDWidgetClass;

implementation

uses
  fpg_main,
  vfddesigner,
  fpg_widget,
  fpg_form,
  fpg_label,
  fpg_edit,
  fpg_button,
  fpg_listbox,
  fpg_memo,
  fpg_combobox,
  fpg_grid,
  fpg_checkbox,
  fpg_panel,
  fpg_tree,
  fpg_radiobutton,
  fpg_listview,
  fpg_trackbar,
  fpg_menu,
  fpg_progressbar,
  fpg_tab,
  fpg_popupcalendar,
  fpg_gauge,
  fpg_editbtn,
  fpg_ColorWheel,
  fpg_splitter,
  fpg_hyperlink,
  vfdpropeditgrid,
  vfdmain;

type
  TVFDPageControlWidgetClass = class(TVFDWidgetClass)
  private
    FWidget: TfpgPageControl;
    procedure AddTabSClicked(Sender: TObject);
    procedure DeleteTabClicked(Sender: TObject);
  public
    function CreatePopupMenu(AWidget: TfpgWidget): TfpgPopupMenu; override;
  end;

{ TVFDPageControlWidgetClass }

procedure TVFDPageControlWidgetClass.AddTabSClicked(Sender: TObject);
begin
  Exit;
  FWidget.AppendTabSheet('TabSheet' + IntToStr(FWidget.PageCount));
end;

procedure TVFDPageControlWidgetClass.DeleteTabClicked(Sender: TObject);
begin
  Exit;
  FWidget.RemoveTabSheet(FWidget.ActivePage);
end;

function TVFDPageControlWidgetClass.CreatePopupMenu(AWidget: TfpgWidget): TfpgPopupMenu;
begin
  FWidget := TfpgPageControl(AWidget);
  Result := TfpgPopupMenu.Create(nil);
  { TODO : These are disabled for now, because a TabSheet component are used
           instead of a menu item - for adding tabs. }
  Result.AddMenuItem('Add Tab', '', @AddTabSClicked).Enabled := False;
  Result.AddMenuItem('Delete Tab', '', @DeleteTabClicked).Enabled := False;
end;

var
  FVFDFormWidget: TVFDWidgetClass;
  FVFDWidgets: TObjectList;

function VFDFormWidget: TVFDWidgetClass;
begin
  Result := FVFDFormWidget;
end;

function VFDWidgetCount: integer;
begin
  Result := FVFDWidgets.Count;
end;

function VFDWidget(ind: integer): TVFDWidgetClass;
begin
  Result := TVFDWidgetClass(FVFDWidgets[ind]);
end;

procedure RegisterVFDWidget(awc: TVFDWidgetClass);
begin
  FVFDWidgets.Add(awc);
end;

{$I icons.inc}

procedure LoadIcons;
begin
  fpgImages.AddMaskedBMP(
    'vfd.arrow', @stdimg_vfd_arrow,
    sizeof(stdimg_vfd_arrow),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.label', @stdimg_vfd_label,
    sizeof(stdimg_vfd_label),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.edit', @stdimg_vfd_edit,
    sizeof(stdimg_vfd_edit),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.memo', @stdimg_vfd_memo,
    sizeof(stdimg_vfd_memo),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.button', @stdimg_vfd_button,
    sizeof(stdimg_vfd_button),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.checkbox', @stdimg_vfd_checkbox,
    sizeof(stdimg_vfd_checkbox),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.listbox', @stdimg_vfd_listbox,
    sizeof(stdimg_vfd_listbox),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.colorlistbox', @stdimg_vfd_colorlistbox,
    sizeof(stdimg_vfd_colorlistbox),
    0, 0);


  fpgImages.AddMaskedBMP(
    'vfd.combobox', @stdimg_vfd_combobox,
    sizeof(stdimg_vfd_combobox),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.panel', @stdimg_vfd_panel,
    sizeof(stdimg_vfd_panel),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.other', @stdimg_vfd_other,
    sizeof(stdimg_vfd_other),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.dbgrid', @stdimg_vfd_dbgrid,
    sizeof(stdimg_vfd_dbgrid),
    15,0 );

  fpgImages.AddMaskedBMP(
    'vfd.progressbar', @stdimg_vfd_progressbar,
    sizeof(stdimg_vfd_progressbar),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.trackbar', @stdimg_vfd_trackbar,
    sizeof(stdimg_vfd_trackbar),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.gauge', @stdimg_vfd_gauge,
    sizeof(stdimg_vfd_gauge),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.menubar', @stdimg_vfd_menubar,
    sizeof(stdimg_vfd_menubar),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.listview', @stdimg_vfd_listview,
    sizeof(stdimg_vfd_listview),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.stringgrid', @stdimg_vfd_stringgrid,
    sizeof(stdimg_vfd_stringgrid),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.radiobutton', @stdimg_vfd_radiobutton,
    sizeof(stdimg_vfd_radiobutton),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.pagecontrol', @stdimg_vfd_pagecontrol,
    sizeof(stdimg_vfd_pagecontrol),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.treeview', @stdimg_vfd_treeview,
    sizeof(stdimg_vfd_treeview),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.newform', @stdimg_vfd_newform,
    sizeof(stdimg_vfd_newform),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.combodateedit', @stdimg_vfd_dateedit,
    sizeof(stdimg_vfd_dateedit),
    0, 0);
    
  fpgImages.AddMaskedBMP(
    'vfd.bevel', @stdimg_vfd_bevel,
    sizeof(stdimg_vfd_bevel),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.tabsheet', @stdimg_vfd_tabsheet,
    sizeof(stdimg_vfd_tabsheet),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.editinteger', @stdimg_vfd_editinteger,
    sizeof(stdimg_vfd_editinteger),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.editfloat', @stdimg_vfd_editfloat,
    sizeof(stdimg_vfd_editfloat),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.editcurrency', @stdimg_vfd_editcurrency,
    sizeof(stdimg_vfd_editcurrency),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.groupbox', @stdimg_vfd_groupbox,
    sizeof(stdimg_vfd_groupbox),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.combodatecheckedit', @stdimg_vfd_combodatecheckedit,
    sizeof(stdimg_vfd_combodatecheckedit),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.editfilename', @stdimg_vfd_editfilename,
    sizeof(stdimg_vfd_editfilename),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.editdirectory', @stdimg_vfd_editdirectory,
    sizeof(stdimg_vfd_editdirectory),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.editfont', @stdimg_vfd_editfont,
    sizeof(stdimg_vfd_editfont),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.editbutton', @stdimg_vfd_editbutton,
    sizeof(stdimg_vfd_editbutton),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.colorwheel', @stdimg_vfd_colorwheel,
    sizeof(stdimg_vfd_colorwheel),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.valuebar', @stdimg_vfd_valuebar,
    sizeof(stdimg_vfd_valuebar),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.splitter', @stdimg_vfd_splitter,
    sizeof(stdimg_vfd_splitter),
    0, 0);

  fpgImages.AddMaskedBMP(
    'vfd.hyperlink', @stdimg_vfd_hyperlink,
    sizeof(stdimg_vfd_hyperlink),
    0, 0);
end;

procedure AddWidgetPosProps(wgc: TVFDWidgetClass);
begin
  wgc.AddProperty('Left', TPropertyInteger, '');
  wgc.AddProperty('Top', TPropertyInteger, '');
  wgc.AddProperty('Width', TPropertyInteger, '');
  wgc.AddProperty('Height', TPropertyInteger, '');
end;

procedure RegisterWidgets;
var
  wc: TVFDWidgetClass;
begin
  LoadIcons;

  wc          := TVFDWidgetClass.Create(TfpgForm);
  wc.NameBase := 'frm';
  wc.AddProperty('WindowTitle', TPropertyString, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('Sizeable', TPropertyBoolean, 'Can the form be resized at runtime');
  //wc.AddProperty('BackgroundColor', TPropertyColor, '');
  //wc.AddProperty('TextColor', TPropertyColor, '');
  //wc.AddProperty('MaxHeight', TPropertyInteger, '');
  //wc.AddProperty('MaxWidth', TPropertyInteger, '');
  //wc.AddProperty('MinHeight', TPropertyInteger, '');
  //wc.AddProperty('MinWidth', TPropertyInteger, '');
  //wc.AddProperty('FullScreen', TPropertyBoolean, '');
  //wc.AddProperty('WindowPosition', TPropertyEnum, '');
  FVFDFormWidget := wc;

  // Label
  wc          := TVFDWidgetClass.Create(TfpgLabel);
  wc.NameBase := 'Label';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Alignment', TPropertyEnum, 'Horizontal text alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the label text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Layout', TPropertyEnum, 'Vertical text layout');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('Text', TPropertyString, 'Label text');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.AddProperty('WrapText', TPropertyBoolean, 'If True text will wrap when it doesn''t fit the width');
  wc.WidgetIconName := 'vfd.label';
  RegisterVFDWidget(wc);

  // Edit
  wc          := TVFDWidgetClass.Create(TfpgEdit);
  wc.NameBase := 'Edit';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('AutoSelect', TPropertyBoolean, 'On receiving focus, auto select text');
  wc.AddProperty('AutoSize', TPropertyBoolean, 'Change Height based on FontDesc being set');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BorderStyle', TPropertyEnum, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('ExtraHint', TPropertyString, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('IgnoreMouseCursor', TPropertyBoolean, 'If True, then mouse pointer doesn''t change');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('PasswordMode', TPropertyBoolean, 'When enabled, it masks the text input');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('Text', TPropertyString, 'Initial text');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.edit';
  RegisterVFDWidget(wc);

  // Memo
  wc          := TVFDWidgetClass.Create(TfpgMemo);
  wc.NameBase := 'Memo';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BorderStyle', TPropertyEnum, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Lines', TPropertyStringList, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.memo';
  RegisterVFDWidget(wc);

  // Button
  wc          := TVFDWidgetClass.Create(TfpgButton);
  wc.NameBase := 'Button';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Text', TPropertyString, 'Initial text');
  wc.AddProperty('AllowAllUp', TPropertyBoolean, '');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Down', TPropertyBoolean, 'Only valid when in group mode');
  wc.AddProperty('Embedded', TPropertyBoolean, 'No focus rectangle will be drawn. eg: Toolbar buttons');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('Flat', TPropertyBoolean, 'Only draw button borders when mouse hovers over button');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('GroupIndex', TPropertyInteger, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ImageLayout', TPropertyEnum, 'Which side of the button contains the image');
  wc.AddProperty('ImageMargin', TPropertyInteger, 'Space between image and border, -1 centers image/text');
  wc.AddProperty('ImageName', TPropertyString, '');
  wc.AddProperty('ImageSpacing', TPropertyInteger, 'Space between image and text, -1 centers text');
  wc.AddProperty('ModalResult', TPropertyEnum, 'Modal Result returned and overrides the OnClick event handler');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowImage', TPropertyBoolean, 'Boolean value');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.button';
  RegisterVFDWidget(wc);

  // CheckBox
  wc          := TVFDWidgetClass.Create(TfpgCheckBox);
  wc.NameBase := 'CheckBox';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BoxLayout', TPropertyEnum, 'Is checkbox image on left or right');
  wc.AddProperty('Checked', TPropertyBoolean, 'Boolean value');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('Text', TPropertyString, 'Initial text');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.checkbox';
  RegisterVFDWidget(wc);

  // RadioButton
  wc          := TVFDWidgetClass.Create(TfpgRadioButton);
  wc.NameBase := 'RadioButton';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BoxLayout', TPropertyEnum, 'Is radiobutton image on left or right');
  wc.AddProperty('Checked', TPropertyBoolean, 'Boolean value');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('GroupIndex', TPropertyInteger, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('Text', TPropertyString, 'Initial text');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.radiobutton';
  RegisterVFDWidget(wc);

  // ComboBox
  wc          := TVFDWidgetClass.Create(TfpgComboBox);
  wc.NameBase := 'ComboBox';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('AutoSize', TPropertyBoolean, 'Change Height based on FontDesc being set');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('ExtraHint', TPropertyString, 'Extra hint that appears inside component while no item is selected');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Items', TPropertyStringList, '');
  wc.AddProperty('FocusItem', TPropertyInteger, 'Currently selected item');  // must be after 'Items' property for UI Designer!
  wc.AddProperty('Margin', TPropertyInteger, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.combobox';
  RegisterVFDWidget(wc);

  // Calendar ComboBox
  wc          := TVFDWidgetClass.Create(TfpgCalendarCombo);
  wc.NameBase := 'CalendarCombo';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('DateFormat', TPropertyString, 'Standard RTL date formatting applies');
  wc.AddProperty('DayColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('HolidayColor', TPropertyColor, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('SelectedColor', TPropertyColor, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.Addproperty('SingleClickSelect', TPropertyBoolean, 'Close calendar on date select');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('WeekStartDay', TPropertyInteger, '0 = Sun, 1 = Mon, etc.');
  wc.WidgetIconName := 'vfd.combodateedit';
  RegisterVFDWidget(wc);

  // Calendar ComboBox Checkbox
  wc          := TVFDWidgetClass.Create(TfpgCalendarCheckCombo);
  wc.NameBase := 'CalendarCombo';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Checked', TPropertyBoolean, 'Boolean value');
  wc.AddProperty('DateFormat', TPropertyString, 'Standard RTL date formatting applies');
  wc.AddProperty('DayColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('HolidayColor', TPropertyColor, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('SelectedColor', TPropertyColor, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.Addproperty('SingleClickSelect', TPropertyBoolean, 'Close calendar on date select');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('WeekStartDay', TPropertyInteger, '0 = Sun, 1 = Mon, etc.');
  wc.WidgetIconName := 'vfd.combodatecheckedit';
  RegisterVFDWidget(wc);

  // ListBox
  wc          := TVFDWidgetClass.Create(TfpgListBox);
  wc.NameBase := 'ListBox';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('AutoHeight', TPropertyBoolean, '');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.Addproperty('DragToReorder', TPropertyBoolean, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('HotTrack', TPropertyBoolean, '');
  wc.AddProperty('Items', TPropertyStringList, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('PopupFrame', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.WidgetIconName := 'vfd.listbox';
  RegisterVFDWidget(wc);

  // Color ListBox
  wc          := TVFDWidgetClass.Create(TfpgColorListBox);
  wc.NameBase := 'ColorListBox';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('AutoHeight', TPropertyBoolean, '');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Color', TPropertyColor, 'The currently selected color');
  wc.AddProperty('ColorPalette', TPropertyEnum, '');
  wc.Addproperty('DragToReorder', TPropertyBoolean, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('HotTrack', TPropertyBoolean, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('PopupFrame', TPropertyBoolean, '');
  wc.AddProperty('ShowColorNames', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.WidgetIconName := 'vfd.colorlistbox';
  RegisterVFDWidget(wc);

  // StringGrid
  wc := TVFDWidgetClass.Create(TfpgStringGrid);
  wc.NameBase := 'Grid';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('AlternateBGColor', TPropertyColor, 'The color of every alternative row. Dependent on grid Options property.');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BorderStyle', TPropertyEnum, '');
  wc.AddProperty('Columns', TPropertyDBColumns, 'Defines the various columns for a grid. At least one column must exist.');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('HeaderFontDesc', TPropertyFontDesc, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('RowCount', TPropertyInteger, 'Default number of rows in the grid');
  wc.AddProperty('RowSelect', TPropertyBoolean, 'If enabled, a whole row is selected, not just a cell');
  wc.AddProperty('ScrollbarStyle', TPropertyEnum, '');
  wc.AddProperty('ShowGrid', TPropertyBoolean, 'Must the grid lines be shown');
  wc.AddProperty('ShowHeader', TPropertyBoolean, 'Must the grid header be visible');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.WidgetIconName := 'vfd.stringgrid';
  RegisterVFDWidget(wc);

  // Bevel
  wc           := TVFDWidgetClass.Create(TfpgBevel);
  wc.NameBase  := 'Bevel';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BorderStyle', TPropertyEnum, 'Single or Double');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ParentBackgroundColor', TPropertyBoolean, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('Shape', TPropertyEnum, 'Box, Frame, TopLine, Spacer etc..');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('Style', TPropertyEnum, 'Raised or Lower look');
  wc.WidgetIconName := 'vfd.bevel';
  wc.Container := True;
  RegisterVFDWidget(wc);

  // Panel
  wc           := TVFDWidgetClass.Create(TfpgPanel);
  wc.NameBase  := 'Panel';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Alignment', TPropertyEnum, 'Text alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BorderStyle', TPropertyEnum, 'Single or Double');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Layout', TPropertyEnum, 'Layout of the caption');
  wc.AddProperty('LineSpace', TPropertyInteger, 'Line spacing between wrapped caption');
  wc.AddProperty('Margin', TPropertyInteger, 'Margin width around the borders that the text should not paint in.');
  wc.AddProperty('ParentBackgroundColor', TPropertyBoolean, 'Should the color of the parent be used instead.');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('Style', TPropertyEnum, 'Raised or Lower look');
  wc.AddProperty('Text', TPropertyString, 'The panel caption');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.AddProperty('WrapText', TPropertyBoolean, 'Should the panel text be wrapped');
  wc.WidgetIconName := 'vfd.panel';
  wc.Container := True;
  RegisterVFDWidget(wc);

  // GroupBox
  wc           := TVFDWidgetClass.Create(TfpgGroupBox);
  wc.NameBase  := 'GroupBox';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Alignment', TPropertyEnum, 'Text alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('BorderStyle', TPropertyEnum, 'Single or Double');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Margin', TPropertyInteger, 'Margin width around the borders that the text should not paint in.');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('Style', TPropertyEnum, 'Raised or Lower look');
  wc.AddProperty('Text', TPropertyString, 'The panel caption');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.groupbox';
  wc.Container := True;
  RegisterVFDWidget(wc);

  // ProgressBar
  wc          := TVFDWidgetClass.Create(TfpgProgressBar);
  wc.NameBase := 'ProgressBar';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Max', TPropertyInteger, '');
  wc.AddProperty('Min', TPropertyInteger, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('Position', TPropertyInteger, '');
  wc.AddProperty('ShowCaption', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.progressbar';
  RegisterVFDWidget(wc);

  // TrackBar
  wc          := TVFDWidgetClass.Create(TfpgTrackBar);
  wc.NameBase := 'TrackBar';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Max', TPropertyInteger, '');
  wc.AddProperty('Min', TPropertyInteger, '');
  wc.AddProperty('Orientation', TPropertyEnum, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('Position', TPropertyInteger, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowPosition', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.trackbar';
  RegisterVFDWidget(wc);

  // ListView
  wc := TVFDWidgetClass.Create(TfpgListView);
  wc.NameBase := 'ListView';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('MultiSelect', TPropertyBoolean, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHeaders', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.WidgetIconName := 'vfd.listview';
  RegisterVFDWidget(wc);

  // Treeview
  wc := TVFDWidgetClass.Create(TfpgTreeView);
  wc.NameBase := 'TreeView';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('DefaultColumnWidth',TPropertyInteger, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc',TPropertyFontDesc, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ScrollWheelDelta', TPropertyInteger, 'Scroll amount with mouse wheel');
  wc.AddProperty('ShowColumns',TPropertyBoolean, 'Boolean value');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowImages',TPropertyBoolean, 'Boolean value');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TreeLineColor', TPropertyColor, '');
  wc.AddProperty('TreeLineStyle', TPropertyEnum, '');
  wc.WidgetIconName := 'vfd.treeview';
  RegisterVFDWidget(wc);
  
  // PageControl
  wc          := TVFDPageControlWidgetClass.Create(TfpgPageControl);
  wc.NameBase := 'PageControl';
  wc.AddProperty('ActivePageIndex', TPropertyInteger, '');
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FixedTabWidth', TPropertyInteger, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('SortPages', TPropertyBoolean, 'Boolean value');
  wc.AddProperty('Style', TPropertyEnum, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TabPosition', TPropertyEnum, '');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.WidgetIconName := 'vfd.pagecontrol';
  wc.Container := True;
  wc.BlockMouseMsg := False;
  RegisterVFDWidget(wc);

  // TabSheet
  wc          := TVFDWidgetClass.Create(TfpgTabSheet);
  wc.NameBase := 'TabSheet';
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('Text', TPropertyString, 'The tab title');
  wc.WidgetIconName := 'vfd.tabsheet';
  wc.Container := True;
  RegisterVFDWidget(wc);

  // Gauge
  wc          := TVFDWidgetClass.Create(TfpgGauge);
  wc.NameBase := 'Gauge';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Color', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FirstColor', TPropertyColor, '');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('Kind', TPropertyEnum, '');
  wc.AddProperty('MaxValue', TPropertyInteger, '');
  wc.AddProperty('MinValue', TPropertyInteger, '');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('Progress', TPropertyInteger, '');
  wc.AddProperty('SecondColor', TPropertyColor, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowText', TPropertyBoolean, 'Boolean value');
  wc.WidgetIconName := 'vfd.gauge';
  RegisterVFDWidget(wc);


  // Integer Edit
  wc          := TVFDWidgetClass.Create(TfpgEditInteger);
  wc.NameBase := 'EditInteger';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('NegativeColor', TPropertyColor, 'Color used for negative values');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ReadOnly', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowThousand', TPropertyBoolean, 'Show thousand separator');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.AddProperty('Value', TPropertyInteger, 'Initial value');
//  wc.AddProperty('CustomThousandSeparator', TPropertyString, 'Thousand separator character');
  wc.WidgetIconName := 'vfd.editinteger';
  RegisterVFDWidget(wc);

  // Float Edit
  wc          := TVFDWidgetClass.Create(TfpgEditFloat);
  wc.NameBase := 'EditFloat';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.Addproperty('Decimals', TPropertyInteger, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FixedDecimals', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('NegativeColor', TPropertyColor, 'Color used for negative values');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ReadOnly', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowThousand', TPropertyBoolean, 'Show thousand separator');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.AddProperty('Value', TPropertyFloat, 'Initial value');
//  wc.AddProperty('CustomDecimalSeparator', TPropertyString, 'Decimal separator character');
//  wc.AddProperty('CustomThousandSeparator', TPropertyString, 'Thousand separator character');
  wc.WidgetIconName := 'vfd.editfloat';
  RegisterVFDWidget(wc);

  // Currency Edit
  wc          := TVFDWidgetClass.Create(TfpgEditCurrency);
  wc.NameBase := 'EditCurrency';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.Addproperty('Decimals', TPropertyInteger, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('NegativeColor', TPropertyColor, 'Color used for negative values');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ReadOnly', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowThousand', TPropertyBoolean, 'Show thousand separator');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.AddProperty('Value', TPropertyFloat, 'Initial value');
//  wc.AddProperty('CustomDecimalSeparator', TPropertyString, 'Decimal separator character');
//  wc.AddProperty('CustomThousandSeparator', TPropertyString, 'Thousand separator character');
  wc.WidgetIconName := 'vfd.editcurrency';
  RegisterVFDWidget(wc);

  { TODO : UI Designer still has problems with components that have child components. }
  // Spin Edit
  //wc          := TVFDWidgetClass.Create(TfpgSpinEdit);
  //wc.NameBase := 'SpinEdit';
  //wc.AddProperty('ButtonWidth', TPropertyInteger, 'Spin button width');
  //wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  //wc.Addproperty('Hint', TPropertyString, '');
  //wc.AddProperty('Increment', TPropertyInteger, 'Increment value on short press');
  //wc.AddProperty('LargeIncrement', TPropertyInteger, 'Large increment value on long press');
  //wc.AddProperty('MaxValue', TPropertyInteger, 'Maximum value');
  //wc.AddProperty('MinValue', TPropertyInteger, 'Minimum value');
  //wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  //wc.AddProperty('ShowHint', TPropertyBoolean, '');
  //wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  //wc.AddProperty('Value', TPropertyInteger, 'Initial value');
  //wc.WidgetIconName := 'vfd.editinteger';
  //RegisterVFDWidget(wc);

  // Spin Edit Float
  //wc          := TVFDWidgetClass.Create(TfpgSpinEditFloat);
  //wc.NameBase := 'SpinEditFloat';
  //wc.AddProperty('ButtonWidth', TPropertyInteger, 'Spin button width');
  //wc.Addproperty('Decimals', TPropertyInteger, '');
  //wc.Addproperty('FixedDecimals', TPropertyBoolean, '');
  //wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the text');
  //wc.Addproperty('Hint', TPropertyString, '');
  //wc.AddProperty('Increment', TPropertyFloat, 'Increment value on short press');
  //wc.AddProperty('LargeIncrement', TPropertyFloat, 'Large increment value on long press');
  //wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  //wc.AddProperty('MaxValue', TPropertyFloat, 'Maximum value');
  //wc.AddProperty('MinValue', TPropertyFloat, 'Minimum value');
  //wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  //wc.AddProperty('ShowHint', TPropertyBoolean, '');
  //wc.AddProperty('Value', TPropertyFloat, 'Initial value');
  //wc.WidgetIconName := 'vfd.editfloat';
  //RegisterVFDWidget(wc);

  // Filename Edit
  wc          := TVFDWidgetClass.Create(TfpgFileNameEdit);
  wc.NameBase := 'FilenameEdit';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('ExtraHint', TPropertyString, '');
  wc.AddProperty('FileName', TPropertyString, 'Preset filename in edit component');
  wc.AddProperty('Filter', TPropertyString, 'Filename filters used in the dialog');
  wc.AddProperty('InitialDir', TPropertyString, 'Initial starting directory of the dialog');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.WidgetIconName := 'vfd.editfilename';
  RegisterVFDWidget(wc);

  // Directory Edit
  wc          := TVFDWidgetClass.Create(TfpgDirectoryEdit);
  wc.NameBase := 'DirectoryEdit';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Directory', TPropertyString, 'Preset directory name in edit component');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('ExtraHint', TPropertyString, '');
  wc.AddProperty('RootDirectory', TPropertyString, 'Initial starting directory of the dialog');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.WidgetIconName := 'vfd.editdirectory';
  RegisterVFDWidget(wc);

  // Font Edit
  wc          := TVFDWidgetClass.Create(TfpgFontEdit);
  wc.NameBase := 'DirectoryEdit';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('ExtraHint', TPropertyString, '');
  wc.AddProperty('FontDesc', TPropertyString, 'Specify a preset font description');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.WidgetIconName := 'vfd.editfont';
  RegisterVFDWidget(wc);

  // Edit Button
  wc          := TVFDWidgetClass.Create(TfpgEditButton);
  wc.NameBase := 'EditButton';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('ExtraHint', TPropertyString, '');
  wc.AddProperty('TabOrder', TPropertyInteger, 'The tab order');
  wc.AddProperty('Text', TPropertyString, 'Specify the initial text value');
  wc.WidgetIconName := 'vfd.editbutton';
  RegisterVFDWidget(wc);

  // Color wheel
  wc          := TVFDWidgetClass.Create(TfpgColorWheel);
  wc.NameBase := 'ColorWheel';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('CursorSize', TPropertyInteger, 'Size of cross-hair in color wheel');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('MarginWidth', TPropertyInteger, 'The margin that will not be painted on four sides of widget');
  wc.AddProperty('WhiteAreaPercent', TPropertyInteger, 'The percentage of the centre of the wheel which is white');
  wc.WidgetIconName := 'vfd.colorwheel';
  RegisterVFDWidget(wc);

  // Value Bar - works in accordance with color wheel
  wc          := TVFDWidgetClass.Create(TfpgValueBar);
  wc.NameBase := 'ValueBar';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('CursorHeight', TPropertyInteger, 'Size of selection cursor');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('MarginWidth', TPropertyInteger, 'The margin that will not be painted on four sides of widget');
  wc.AddProperty('Value', TPropertyFloat, '');
  wc.WidgetIconName := 'vfd.valuebar';
  RegisterVFDWidget(wc);

  // Splitter - works in accordance with color wheel
  wc          := TVFDWidgetClass.Create(TfpgSplitter);
  wc.NameBase := 'Splitter';
  wc.AddProperty('Align', TPropertyEnum, '');
  wc.AddProperty('ColorGrabBar', TPropertyColor, '');
  wc.AddProperty('AutoSnap', TPropertyBoolean, '');
  wc.WidgetIconName := 'vfd.splitter';
  RegisterVFDWidget(wc);

  // Hyperlink
  wc          := TVFDWidgetClass.Create(TfpgHyperlink);
  wc.NameBase := 'Hyperlink';
  wc.AddProperty('Align', TPropertyEnum, 'Component alignment');
  wc.AddProperty('Alignment', TPropertyEnum, 'Horizontal text alignment');
  wc.AddProperty('BackgroundColor', TPropertyColor, '');
  wc.AddProperty('Enabled', TPropertyBoolean, '');
  wc.AddProperty('FontDesc', TPropertyFontDesc, 'The font used for displaying the label text');
  wc.AddProperty('Hint', TPropertyString, 'Tooltip hint');
  wc.AddProperty('HotTrackColor', TPropertyColor, 'The color that the text must appear in when the mouse is over the widget.');
  wc.AddProperty('HotTrackFont', TPropertyFontDesc, 'The font that the text must use, when the mouse is over the widget.');
  wc.AddProperty('Layout', TPropertyEnum, 'Vertical text layout');
  wc.AddProperty('ParentShowHint', TPropertyBoolean, '');
  wc.AddProperty('ShowHint', TPropertyBoolean, '');
  wc.AddProperty('Text', TPropertyString, 'Label text');
  wc.AddProperty('TextColor', TPropertyColor, '');
  wc.AddProperty('URL', TPropertyString, 'The URL that must be opened in the default web browser. If OnClick is specified, then the URL has no affect.');
  wc.AddProperty('WrapText', TPropertyBoolean, 'If True text will wrap when it doesn''t fit the width');
  wc.WidgetIconName := 'vfd.hyperlink';
  RegisterVFDWidget(wc);

  // Other - do not delete!!! this should be the last...
  wc          := TVFDWidgetClass.Create(TOtherWidget);
  wc.NameBase := 'Custom';
  wc.WidgetIconName := 'vfd.other';
  wc.Container := True;
  RegisterVFDWidget(wc);
  VFDOtherWidget := wc;
end;


initialization
    FVFDWidgets := TObjectList.Create;

finalization
    FVFDWidgets.Free;
    FVFDFormWidget.Free;
    
end.

