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
      Some property editors.
}

unit vfdeditors;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_widget,
  fpg_label,
  fpg_button,
  fpg_memo,
  vfdforms;

type

  TItemEditorForm = class(TVFDDialog)
  private
    procedure btnClearClicked(Sender: TObject);
    procedure OnButtonClick(Sender: TObject);
  public
    l1: TfpgLabel;
    edItems: TfpgMemo;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    btnClear: TfpgButton;
    procedure AfterCreate; override;
  end;


implementation

uses
  fpg_base,
  fpg_main;

{ TItemEditorForm }

procedure TItemEditorForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  inherited;
  WindowTitle := 'Items';
  SetPosition(0, 0, 360, 230);

  l1 := CreateLabel(self, 8, 4, 'Items:');

  edItems := TfpgMemo.Create(self);
  with edItems do
  begin
    SetPosition(8, 24, 344, 168);
    Anchors := AllAnchors;
  end;

  btnClear := CreateButton(self, 8, 200, 80, 'Clear', @btnClearClicked);
  btnClear.Anchors := [anLeft, anBottom];

  btnOK         := CreateButton(self, Width-168, 200, 80, 'OK', @OnButtonClick);
  btnOK.Anchors := [anRight, anBottom];

  btnCancel     := CreateButton(self, Width-84, 200, 80, 'Cancel', @OnButtonClick);
  btnCancel.Anchors := [anRight, anBottom];
  {%endregion}
end;

procedure TItemEditorForm.btnClearClicked(Sender: TObject);
begin
  edItems.Lines.Clear;
end;

procedure TItemEditorForm.OnButtonClick(Sender: TObject);
begin
  if Sender = btnOK then
    ModalResult := mrOK
  else
    ModalResult := mrCancel;
end;


end.

