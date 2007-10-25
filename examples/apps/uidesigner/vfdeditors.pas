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
      Some property editors.
}

unit vfdeditors;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfx_widget,
  gui_label,
  gui_button,
  gui_memo,
  vfdforms;

type
  TItemEditorForm = class(TVFDDialog)
  public
    l1: TfpgLabel;
    edItems: TfpgMemo;
    btnOK,
    btnCancel: TfpgButton;
    procedure AfterCreate; override;
    procedure OnButtonClick(Sender: TObject);
  end;


implementation

uses
  fpgfx;

{ TItemEditorForm }

procedure TItemEditorForm.AfterCreate;
begin
  inherited;
  WindowTitle := 'Items';
  SetPosition(0, 0, 360, 230);

  l1 := CreateLabel(self, 8, 4, 'Items:');

  edItems := TfpgMemo.Create(self);
  with edItems do
  begin
    SetPosition(8, 24, 344, 168);
    Anchors := [anLeft, anTop, anRight, anBottom];
  end;

  btnOK         := CreateButton(self, 8, 200, 105, 'OK', @OnButtonClick);
  btnOK.Anchors := [anLeft, anBottom];
  btnCancel     := CreateButton(self, 244, 200, 105, 'Cancel', @OnButtonClick);
  btnCancel.Anchors := [anRight, anBottom];
end;

procedure TItemEditorForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := 1
  else
    ModalResult := 2;
end;


end.

