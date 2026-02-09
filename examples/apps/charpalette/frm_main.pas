unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_panel,
  fpg_button,
  fpg_iniutils;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnPreferences: TfpgButton;
    Bevel1: TfpgBevel;
    {@VFD_HEAD_END: MainForm}
    FCount: integer;
    procedure FormShow(Sender: TObject);
    procedure btnToolbarClicked(Sender: TObject);
    procedure btnToolbarMouseEnter(Sender: TObject);
    procedure btnToolbarMouseExit(Sender: TObject);
    procedure btnEditClicked(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function NewButtonName: TfpgString;
    function CreateToolbarButton(const AChar: TfpgChar): TfpgButton;
    procedure EmptyPalette;
    procedure BuildPalette;
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_stringutils,
  fpg_utils,
  fpg_dialogs;

const
  cSpacing = 2;
  cSizeW = 17;
  cSizeH = 18;

var
  // the default palette values
  uPalette: TfpgString = '¤£¥¢$ áéíóúýêîôûèäëïöüÁÉÍÓÚÝÊÎÔÛÈÄËÏÖÜ';

{@VFD_NEWFORM_IMPL}

procedure TMainForm.FormShow(Sender: TObject);
begin
  BuildPalette;
end;

procedure TMainForm.btnToolbarClicked(Sender: TObject);
begin
  fpgClipBoard.Text := TfpgButton(Sender).Text;
end;

procedure TMainForm.btnToolbarMouseEnter(Sender: TObject);
begin
  TfpgButton(Sender).BackgroundColor := TfpgColor($FF6CB6EB); // light blue
end;

procedure TMainForm.btnToolbarMouseExit(Sender: TObject);
begin
  TfpgButton(Sender).BackgroundColor := clButtonFace;
end;

procedure TMainForm.btnEditClicked(Sender: TObject);
var
  v: TfpgString;
begin
  v := uPalette;
  if fpgInputQuery('Update the character palette', 'Make your changes', v) then
  begin
    uPalette := v;
    BuildPalette;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  gINI.ReadFormState(self);
  uPalette := gINI.ReadString('Palette', 'Value', uPalette);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState(self);
  gINI.WriteString('Palette', 'Value', uPalette);
end;

function TMainForm.NewButtonName: TfpgString;
begin
  Result := 'Button' + IntToStr(FCount);
  inc(FCount);
end;

function TMainForm.CreateToolbarButton(const AChar: TfpgChar): TfpgButton;
begin
  Result := TfpgButton.Create(Bevel1);
  with Result do
  begin
    SetPosition((FCount * cSpacing) + ((FCount-1) * cSizeW), 4, cSizeW, cSizeH);
    Name := NewButtonName;
    Text := AChar;
    Embedded := True;
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := '';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @btnToolbarClicked;
    OnMouseEnter := @btnToolbarMouseEnter;
    OnMouseExit := @btnToolbarMouseExit;
  end;
end;

procedure TMainForm.EmptyPalette;
var
  i: integer;
begin
  for i := Bevel1.ComponentCount-1 downto 0 do
  begin
    Bevel1.Components[i].Free
  end;
end;

procedure TMainForm.BuildPalette;
var
  n: integer;
begin
  FCount := 1;
  EmptyPalette;
  for n := 1 to UTF8Length(uPalette) do
    CreateToolbarButton(UTF8Copy(uPalette, n, 1));
  Width := (FCount * cSpacing) + ((FCount-1) * cSizeW) + 35;
  UpdatePosition;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(1200, 551, 40, 33);
  WindowTitle := 'Character Palette';
  Hint := '';
  IconName := '';
  OnShow := @FormShow;
  OnCreate := @FormCreate;
  OnDestroy := @FormDestroy;

  btnPreferences := TfpgButton.Create(self);
  with btnPreferences do
  begin
    Name := 'btnPreferences';
    SetPosition(16, 4, 20, 23);
    Anchors := [anRight,anTop];
    Text := 'P';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnEditClicked;
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(2, 2, 11, 29);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
