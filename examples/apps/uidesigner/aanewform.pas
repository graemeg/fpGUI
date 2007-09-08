unit aanewform;

{$ifdef FPC}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, gfxbase, wgedit, unitkeys, schar16, gfxstyle,
  gfxwidget, gfxform, wglabel, wgbutton,
  wglistbox, wgmemo, wgchoicelist, wggrid, sqldb, sqluis,
  wgdbgrid, gfxdialogs, wgcheckbox;

type

  TfrmValami = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: frmValami}
    ed1 : TfpgEdit;
    btn1 : TfpgButton;
    chl1 : TfpgComboBox;
    wg1 : Tfpg;
    lst1 : TfpgListBox;
    memo1 : TfpgMemo;
    grid1 : TfpgStringGrid;
    panel1 : TfpgBevel;
    lb1 : TfpgLabel;
    ed2 : TfpgEdit;
    panel2 : TfpgBevel;
    lb2 : TfpgLabel;
    {@VFD_HEAD_END: frmValami}

    procedure AfterCreate; override;
  end;

  TfrmVFDSetup = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmVFDSetup}
    panel1 : TfpgBevel;
    lb1 : TfpgLabel;
    chl1 : TfpgComboBox;
    btnOK : TfpgButton;
    btnCancel : TfpgButton;
    {@VFD_HEAD_END: frmVFDSetup}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmVFDSetup.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmVFDSetup}
  SetPosition(331,481,237,103);
  WindowTitle := 'General settings';

  panel1 := TfpgBevel.Create(self);
  with panel1 do
  begin
    SetPosition(8,12,220,52);
    shape := bsBox;
    style := bsRaised;
  end;

  lb1 := TfpgLabel.Create(panel1);
  with lb1 do
  begin
    SetPosition(8,16,92,16);
    Text := 'Grid resolution:';
    FontName := '#Label1';
  end;

  chl1 := TfpgComboBox.Create(panel1);
  with chl1 do
  begin
    SetPosition(116,14,56,22);
    Items.Add('1');
    Items.Add('4');
    Items.Add('5');
    FontName := '#List';
    FocusItem := 2;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    SetPosition(8,72,96,24);
    Text := 'OK';
    FontName := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 1;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    SetPosition(132,72,96,24);
    Text := 'Cancel';
    FontName := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := -1;
  end;

  {@VFD_BODY_END: frmVFDSetup}
end;


procedure TfrmValami.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmValami}
  SetPosition(310,122,491,340);
  WindowTitle := 'frmValami';

  ed1 := TfpgEdit.Create(self);
  with ed1 do
  begin
    SetPosition(12,48,120,22);
    Text := '';
    FontName := '#Edit1';
  end;

  btn1 := TfpgButton.Create(self);
  with btn1 do
  begin
    SetPosition(12,76,96,24);
    Text := 'Button';
    FontName := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 0;
  end;

  chl1 := TfpgComboBox.Create(self);
  with chl1 do
  begin
    SetPosition(148,40,120,22);
    Items.Add('egy');
    Items.Add('ketto');
    Items.Add('harom');
    FontName := '#List';
  end;

  wg1 := Tfpg.Create(self);
  with wg1 do
  begin
    SetPosition(148,84,120,32);
  end;

  lst1 := TfpgListBox.Create(self);
  with lst1 do
  begin
    SetPosition(12,116,104,92);
    Items.Add('as'));
    Items.Add('asdf');
    Items.Add('asd');
    Items.Add('f as');
    FontName := '#List';
  end;

  memo1 := TfpgMemo.Create(self);
  with memo1 do
  begin
    SetPosition(136,124,120,72);
    Lines.Add('valami szoveg');
    Lines.Add('masodik sor');
    FontName := '#Edit1';
  end;

  grid1 := TfpgStringGrid.Create(self);
  with grid1 do
  begin
    SetPosition(16,216,180,104);
    AddColumn('ID','',50,alLeft);
    AddColumn('NAME','',70,alLeft);
    FontName := '#Grid';
    HeaderFontName := '#GridHeader';
  end;

  panel1 := TfpgBevel.Create(self);
  with panel1 do
  begin
    SetPosition(276,156,192,148);
    shape := bsBox;
    style := bsRaised;
  end;

  lb1 := TfpgLabel.Create(panel1);
  with lb1 do
  begin
    SetPosition(16,8,80,16);
    Text := 'Label';
    FontName := '#Label1';
  end;

  ed2 := TfpgEdit.Create(panel1);
  with ed2 do
  begin
    SetPosition(8,32,120,22);
    Text := '';
    FontName := '#Edit1';
  end;

  panel2 := TfpgBevel.Create(panel1);
  with panel2 do
  begin
    SetPosition(40,64,124,56);
    shape := bsFrame;
    style := bsLowered;
  end;

  lb2 := TfpgLabel.Create(panel2);
  with lb2 do
  begin
    SetPosition(32,24,80,16);
    Text := 'Label';
    FontName := '#Label1';
  end;

  {@VFD_BODY_END: frmValami}
end;


end.
