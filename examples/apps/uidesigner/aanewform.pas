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

  TfrmValami = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmValami}
    ed1 : TwgEdit;
    btn1 : TwgButton;
    chl1 : TwgChoiceList;
    wg1 : Twg;
    lst1 : TwgTextListBox;
    memo1 : TwgMemo;
    grid1 : TwgDBGrid;
    panel1 : TwgBevel;
    lb1 : TwgLabel;
    ed2 : TwgEdit;
    panel2 : TwgBevel;
    lb2 : TwgLabel;
    {@VFD_HEAD_END: frmValami}

    procedure AfterCreate; override;
  end;

  TfrmVFDSetup = class(TGfxForm)
  public
    {@VFD_HEAD_BEGIN: frmVFDSetup}
    panel1 : TwgBevel;
    lb1 : TwgLabel;
    chl1 : TwgChoiceList;
    btnOK : TwgButton;
    btnCancel : TwgButton;
    {@VFD_HEAD_END: frmVFDSetup}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmVFDSetup.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmVFDSetup}
  SetDimensions(331,481,237,103);
  WindowTitle8 := 'General settings';

  panel1 := TwgBevel.Create(self);
  with panel1 do
  begin
    SetDimensions(8,12,220,52);
    shape := bsBox;
    style := bsRaised;
  end;

  lb1 := TwgLabel.Create(panel1);
  with lb1 do
  begin
    SetDimensions(8,16,92,16);
    Text := u8('Grid resolution:');
    FontName := '#Label1';
  end;

  chl1 := TwgChoiceList.Create(panel1);
  with chl1 do
  begin
    SetDimensions(116,14,56,22);
    Items.Add(u8('1'));
    Items.Add(u8('4'));
    Items.Add(u8('5'));
    FontName := '#List';
    FocusItem := 2;
  end;

  btnOK := TwgButton.Create(self);
  with btnOK do
  begin
    SetDimensions(8,72,96,24);
    Text := u8('OK');
    FontName := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 1;
  end;

  btnCancel := TwgButton.Create(self);
  with btnCancel do
  begin
    SetDimensions(132,72,96,24);
    Text := u8('Cancel');
    FontName := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := -1;
  end;

  {@VFD_BODY_END: frmVFDSetup}
end;


procedure TfrmValami.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmValami}
  SetDimensions(310,122,491,340);
  WindowTitle8 := 'frmValami';

  ed1 := TwgEdit.Create(self);
  with ed1 do
  begin
    SetDimensions(12,48,120,22);
    Text := u8('');
    FontName := '#Edit1';
  end;

  btn1 := TwgButton.Create(self);
  with btn1 do
  begin
    SetDimensions(12,76,96,24);
    Text := u8('Button');
    FontName := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := 0;
  end;

  chl1 := TwgChoiceList.Create(self);
  with chl1 do
  begin
    SetDimensions(148,40,120,22);
    Items.Add(u8('egy'));
    Items.Add(u8('ketto'));
    Items.Add(u8('harom'));
    FontName := '#List';
  end;

  wg1 := Twg.Create(self);
  with wg1 do
  begin
    SetDimensions(148,84,120,32);
  end;

  lst1 := TwgTextListBox.Create(self);
  with lst1 do
  begin
    SetDimensions(12,116,104,92);
    Items.Add(u8('as'));
    Items.Add(u8('asdf'));
    Items.Add(u8('asd'));
    Items.Add(u8('f as'));
    FontName := '#List';
  end;

  memo1 := TwgMemo.Create(self);
  with memo1 do
  begin
    SetDimensions(136,124,120,72);
    Lines.Add(u8('valami szoveg'));
    Lines.Add(u8('masodik sor'));
    FontName := '#Edit1';
  end;

  grid1 := TwgDBGrid.Create(self);
  with grid1 do
  begin
    SetDimensions(16,216,180,104);
    AddColumn8('ID','',50,alLeft);
    AddColumn8('NAME','',70,alLeft);
    FontName := '#Grid';
    HeaderFontName := '#GridHeader';
  end;

  panel1 := TwgBevel.Create(self);
  with panel1 do
  begin
    SetDimensions(276,156,192,148);
    shape := bsBox;
    style := bsRaised;
  end;

  lb1 := TwgLabel.Create(panel1);
  with lb1 do
  begin
    SetDimensions(16,8,80,16);
    Text := u8('Label');
    FontName := '#Label1';
  end;

  ed2 := TwgEdit.Create(panel1);
  with ed2 do
  begin
    SetDimensions(8,32,120,22);
    Text := u8('');
    FontName := '#Edit1';
  end;

  panel2 := TwgBevel.Create(panel1);
  with panel2 do
  begin
    SetDimensions(40,64,124,56);
    shape := bsFrame;
    style := bsLowered;
  end;

  lb2 := TwgLabel.Create(panel2);
  with lb2 do
  begin
    SetDimensions(32,24,80,16);
    Text := u8('Label');
    FontName := '#Label1';
  end;

  {@VFD_BODY_END: frmValami}
end;


end.
