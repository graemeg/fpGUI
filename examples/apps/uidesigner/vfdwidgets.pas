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
      Setting up of widgets, properties and images.
}

unit vfdwidgets;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
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
  vfddesigner,
  gui_form,
  gui_label,
  gui_edit,
  gui_button,
  gui_listbox,
  gui_memo,
  gui_combobox,
  gui_grid,
  gui_checkbox,
  gui_bevel,
  fpgfx
  ;

var
  FVFDFormWidget: TVFDWidgetClass;
  FVFDWidgets: TList;

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
  Result := TVFDWidgetClass(FVFDWidgets[ind - 1]);
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
    'vfd.choicelist', @stdimg_vfd_choicelist,
    sizeof(stdimg_vfd_choicelist),
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
                   'vfd.dbgrid',
            @stdimg_vfd_dbgrid,
      sizeof(stdimg_vfd_dbgrid),
            15,0 );

  {
  fpgImages.AddMaskedBMP(
                   'vfd.',
            @stdimg_vfd_,
      sizeof(stdimg_vfd_),
            0,0 );
}
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
  //wp : TVFDWidgetProperty;
begin
  LoadIcons;

  wc          := TVFDWidgetClass.Create(TfpgForm);
  wc.NameBase := 'frm';
  wc.AddProperty('WindowTitle', TPropertyString, '');
  FVFDFormWidget := wc;

  // Label
  wc          := TVFDWidgetClass.Create(TfpgLabel);
  wc.NameBase := 'lblName';
  wc.AddProperty('Text', TPropertyString, 'Label text');
  wc.AddProperty('FontDesc', TPropertyString, 'The font used for displaying the label text');
  wc.WidgetIconName := 'vfd.label';
  RegisterVFDWidget(wc);

  // Edit
  wc          := TVFDWidgetClass.Create(TfpgEdit);
  wc.NameBase := 'edtName';
  wc.AddProperty('Text', TPropertyString, 'Initial text');
  wc.AddProperty('FontDesc', TPropertyString, 'The font used for displaying the text');
  wc.WidgetIconName := 'vfd.edit';
  RegisterVFDWidget(wc);

  // Memo
  wc          := TVFDWidgetClass.Create(TfpgMemo);
  wc.NameBase := 'memName';
  wc.AddProperty('Lines', TPropertyStringList, '');
  wc.AddProperty('FontDesc', TPropertyString, 'The font used for displaying the text');
  wc.WidgetIconName := 'vfd.memo';
  RegisterVFDWidget(wc);

  // Button
  wc          := TVFDWidgetClass.Create(TfpgButton);
  wc.NameBase := 'btnName';
  wc.AddProperty('Text', TPropertyString, 'Initial text');
  wc.AddProperty('FontDesc', TPropertyString, 'The font used for displaying the text');
  wc.AddProperty('ImageName', TPropertyString, '');
//  wc.AddProperty('ShowImage', TPropertyEnum, '');
  wc.AddProperty('ModalResult', TPropertyInteger, '');
  wc.WidgetIconName := 'vfd.button';
  RegisterVFDWidget(wc);

  // CheckBox
  wc          := TVFDWidgetClass.Create(TfpgCheckBox);
  wc.NameBase := 'cbName';
  wc.AddProperty('Text', TPropertyString, 'Initial text');
  wc.AddProperty('FontDesc', TPropertyString, 'The font used for displaying the text');
  wc.WidgetIconName := 'vfd.checkbox';
  RegisterVFDWidget(wc);

  // ComboBox
  wc          := TVFDWidgetClass.Create(TfpgComboBox);
  wc.NameBase := 'cbName';
  //wc.AddProperty('Text',TPropertyString16,'');
  wc.AddProperty('Items', TPropertyStringList, '');
  wc.AddProperty('FontDesc', TPropertyString, 'The font used for displaying the text');
  wc.WidgetIconName := 'vfd.choicelist';
  RegisterVFDWidget(wc);

  // ListBox
  wc          := TVFDWidgetClass.Create(TfpgListBox);
  wc.NameBase := 'lstName';
  //wc.AddProperty('Text',TPropertyString16,'');
  wc.AddProperty('Items', TPropertyStringList, '');
  wc.AddProperty('FontDesc', TPropertyString, 'The font used for displaying the text');
  wc.WidgetIconName := 'vfd.listbox';
  RegisterVFDWidget(wc);

  // StringGrid
  wc := TVFDWidgetClass.Create(TfpgStringGrid);
  wc.NameBase := 'grdName';
//  wc.AddProperty('Columns',TPropertyDBColumns,'');
  wc.AddProperty('FontDesc',TPropertyString,'');
  wc.AddProperty('HeaderFontDesc',TPropertyString,'');
  wc.WidgetIconName := 'vfd.dbgrid';
  RegisterVFDWidget(wc);

  // Panel
  wc           := TVFDWidgetClass.Create(TfpgBevel);
  wc.NameBase  := 'pnlName';
  wc.AddProperty('shape', TPropertyEnum, '');
  wc.AddProperty('style', TPropertyEnum, '');
  wc.WidgetIconName := 'vfd.panel';
  wc.Container := True;
  RegisterVFDWidget(wc);

  // Other - do not delete!!! this should be the last...
  wc          := TVFDWidgetClass.Create(TOtherWidget);
  wc.NameBase := 'Custom';
  wc.WidgetIconName := 'vfd.other';
  RegisterVFDWidget(wc);
  VFDOtherWidget := wc;
end;

initialization
  begin
    FVFDWidgets := TList.Create;
  end;

end.

