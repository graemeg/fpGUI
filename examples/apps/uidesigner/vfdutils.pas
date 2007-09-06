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
      Some utility functions.
}

unit vfdutils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfx_widget,
  gui_form,
  gui_label,
  gui_edit,
  gui_button,
  gui_memo,
  gui_checkbox;


procedure SetWidgetText(wg: TfpgWidget; txt: string);
function  GetWidgetText(wg: TfpgWidget; out txt: string): boolean;


implementation

procedure SetWidgetText(wg: TfpgWidget; txt: string);
begin
  if wg is TfpgForm then
    TfpgForm(wg).WindowTitle  := txt
  else if wg is TfpgLabel then
    TfpgLabel(wg).Text        := txt
  else if wg is TfpgEdit then
    TfpgEdit(wg).Text         := txt
  else if wg is TfpgMemo then
    TfpgMemo(wg).Text         := txt
  else if wg is TfpgButton then
    TfpgButton(wg).Text       := txt
  else if wg is TfpgCheckBox then
    TfpgCheckBox(wg).Text     := txt;
end;

function GetWidgetText(wg: TfpgWidget; out txt: string): boolean;
begin
  Result := True;
  if wg is TfpgForm then
    txt := TfpgForm(wg).WindowTitle
  else if wg is TfpgLabel then
    txt    := TfpgLabel(wg).Text
  else if wg is TfpgEdit then
    txt    := TfpgEdit(wg).Text
  else if wg is TfpgMemo then
    txt    := TfpgMemo(wg).Text
  else if wg is TfpgButton then
    txt    := TfpgButton(wg).Text
  else if wg is TfpgCheckBox then
    txt    := TfpgCheckBox(wg).Text
  else
  begin
    Result := False;
    txt    := '';
  end;
end;


end.

