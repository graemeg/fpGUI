unit frm_nested;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget, fpg_form, fpg_label,
  fpg_button, fpg_panel;

type
  TNestedAlignment = class(TfpgForm)
  private
    pnlName1: TfpgPanel;
    btnNameNE: TfpgButton;
    btnNameNW: TfpgButton;
    btnNameSW: TfpgButton;
    btnNameSE: TfpgButton;
  public
    procedure AfterCreate; override;
  end;

implementation


procedure TNestedAlignment.AfterCreate();
begin
  Width := 300;
  Height := 300;
  WindowTitle := 'Nested Alignment';
  WindowPosition := wpOneThirdDown;
  MinWidth := 50;
  MinHeight := 50;

  pnlName1 := TfpgPanel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    Left := 10;
    Top := 10;
    Width := 280;
    Height := 280;
    Text := '';
    BackgroundColor := clDarkKhaki;
    Anchors := [anLeft, anTop, anRight, anBottom];
  end;

  btnNameNW := TfpgButton.Create(pnlName1);
  with btnNameNW do
  begin
    Name := 'btnNameNW';
    Left := 8;
    Top := 8;
    Width := 30;
    Height := 24;
    Text := 'NW';
    Anchors := [anLeft,anTop];
  end;

  btnNameNE := TfpgButton.Create(pnlName1);
  with btnNameNE do
  begin
    Name := 'btnNameNE';
    Left := 280-8-30;
    Top := 8;
    Width := 30;
    Height := 24;
    Text := 'NE';
    Anchors := [anRight,anTop];
  end;

  btnNameSE := TfpgButton.Create(pnlName1);
  with btnNameSE do
  begin
    Name := 'btnNameSE';
    Left := 280-8-30;
    Top := 280-8-24;
    Width := 30;
    Height := 24;
    Text := 'SE';
    Anchors := [anRight,anBottom];
  end;

  btnNameSW := TfpgButton.Create(pnlName1);
  with btnNameSW do
  begin
    Name := 'btnNameSW';
    Left := 8;
    Top := 280-8-24;
    Width := 30;
    Height := 24;
    Text := 'SW';
    Anchors := [anLeft,anBottom];
  end;
end;

end.
