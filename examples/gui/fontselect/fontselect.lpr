program fontselect;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_main,
  fpg_form,
  fpg_dialogs,
  fpg_button,
  fpg_listbox,
  fpg_edit,
  fpg_label,
  fpg_constants;
  
  
resourcestring
  rsMyTitle = 'Font selection test';
  rsMyFontList = 'Font List';


type
  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    btnSelectFont: TfpgButton;
    lbFontList: TfpgListBox;
    edFontDesc: TfpgEdit;
    lblFontList: TfpgLabel;
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnSelectFontClick(Sender: TObject);
    procedure   CreateFontList;
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnSelectFontClick(Sender: TObject);
var
  fontdesc: string;
begin
  fontdesc := edFontDesc.Text;
  if SelectFontDialog(fontdesc) then
    edFontDesc.Text := fontdesc;
end;

procedure TMainForm.CreateFontList;
var
  fl: TStringList;
  i: integer;
begin
  lbFontList.Items.Clear;
  fl := fpgApplication.GetFontFaceList;
  for i := 0 to fl.Count-1 do
    lbFontList.Items.Add(fl.Strings[i]);
  fl.Free;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := rsMyTitle;
  SetPosition(100, 100, 500, 400);
  WindowPosition:= wpOneThirdDown;
  
  btnSelectFont := CreateButton(self, 10, 10, 110, rsSelectAFont, @btnSelectFontClick);

  edFontDesc := CreateEdit(self, 10, 45, Width - 20, 24);
  edFontDesc.Text := 'Bitstream Vera Sans-9';

  lblFontList := CreateLabel(self, 10, 80, rsMyFontList + ':');
  lbFontList := TfpgListBox.Create(self);
  with lbFontList do
  begin
    Name := 'lbFontList';
    SetPosition(10, 100, 232, 236);
    Items.Clear;
  end;

  CreateFontList;

  btnQuit := CreateButton(self, 415, 370, 80, rsExit, @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];

  btnSelectFont.TabOrder := 1;
  edFontDesc.TabOrder := 2;
  lbFontList.TabOrder := 3;
  btnQuit.TabOrder := 4;
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.

