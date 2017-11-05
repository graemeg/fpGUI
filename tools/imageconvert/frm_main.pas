{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_memo, fpg_menu,
  fpg_button, fpg_editbtn, fpg_label;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    MainMenu: TfpgMenuBar;
    FilenameEdit1: TfpgFileNameEdit;
    memImages: TfpgMemo;
    Button1: TfpgButton;
    pmFile: TfpgPopupMenu;
    btnClear: TfpgButton;
    Label1: TfpgLabel;
    btnCopy: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    procedure miFileQuit(Sender: TObject);
    procedure MemoDragEnter(Drop: TfpgDrop);
    procedure MemoDragDrop(Drop: TfpgDrop; AData: Variant);
    procedure btnClearClicked(Sender: TObject);
    procedure btnConvertClicked(Sender: TObject);
    procedure btnCopyClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  bin2pas;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.miFileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MemoDragEnter(Drop: TfpgDrop);
var
  s: string;
begin
  {TODO: Once Windows DND backend is 100% complete, this IFDEF can be removed.}
  {$IFDEF MSWINDOWS}
  s := 'FileName';
  {$ELSE}
  s := 'text/uri-list';
  {$ENDIF}

  Drop.CanDrop := Drop.AcceptMimeType([s]);
end;

procedure TMainForm.MemoDragDrop(Drop: TfpgDrop; AData: Variant);
var
  fileName: string;
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := AData;
    try
      memImages.BeginUpdate;
      for i := 0 to sl.Count-1 do
      begin
        fileName := sl[i];
        fileName := StringReplace(fileName, 'file://', '', []);
        memImages.Text := memImages.Text + ConvertImage(fileName);
      end;
    finally
      memImages.EndUpdate;
    end;
  finally
    sl.Free;
  end;
end;

procedure TMainForm.btnClearClicked(Sender: TObject);
begin
  memImages.Text := '';
end;

procedure TMainForm.btnConvertClicked(Sender: TObject);
begin
  memImages.BeginUpdate;
  try
    memImages.Text := memImages.Text + ConvertImage(FilenameEdit1.FileName);
  finally
    memImages.EndUpdate;
  end;
end;

procedure TMainForm.btnCopyClicked(Sender: TObject);
begin
  fpgClipboard.Text := memImages.Text;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(357, 227, 630, 378);
  WindowTitle := 'Image Conversion Tool';
  Hint := '';
  ShowHint := True;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 630, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  FilenameEdit1 := TfpgFileNameEdit.Create(self);
  with FilenameEdit1 do
  begin
    Name := 'FilenameEdit1';
    SetPosition(4, 44, 384, 24);
    ExtraHint := '';
    FileName := '';
    Filter := '';
    InitialDir := '';
    TabOrder := 3;
  end;

  memImages := TfpgMemo.Create(self);
  with memImages do
  begin
    Name := 'memImages';
    SetPosition(4, 88, 622, 286);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Edit2';
    Hint := '';
    TabOrder := 5;
    DropHandler := TfpgDropEventHandler.Create(@MemoDragEnter, nil, @MemoDragDrop, nil);
  end;

  Button1 := TfpgButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(396, 44, 80, 24);
    Text := 'Convert';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnConvertClicked;
  end;

  pmFile := TfpgPopupMenu.Create(self);
  with pmFile do
  begin
    Name := 'pmFile';
    SetPosition(236, 128, 120, 20);
    AddMenuItem('Add File...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Quit', 'Ctrl+Q', @miFileQuit);
  end;

  btnClear := TfpgButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(538, 64, 56, 23);
    Anchors := [anRight,anTop];
    Text := 'Clear';
    FontDesc := '#Label1';
    Hint := 'Clear text box';
    ImageName := '';
    TabOrder := 6;
    OnClick  := @btnClearClicked;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(4, 72, 315, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Drop one or more images on the text area below:';
  end;

  btnCopy := TfpgButton.Create(self);
  with btnCopy do
  begin
    Name := 'btnCopy';
    SetPosition(596, 64, 29, 23);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Copy to clipboard';
    ImageName := 'stdimg.copy';
    TabOrder := 8;
    OnClick := @btnCopyClicked;
  end;

  {@VFD_BODY_END: MainForm}

  MainMenu.AddMenuItem('File', nil).SubMenu := pmFile;
  {%endregion}
end;


end.
