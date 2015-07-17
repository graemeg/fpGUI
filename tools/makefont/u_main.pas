unit u_main;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Dos,
  fpg_main, fpg_base,
  fpg_form, fpg_button, fpg_label, fpg_dialogs, fpg_combobox;

type
  TF_MainForm= class(TfpgForm)
    private
      L_SelectMap: Tfpglabel;
      Cb_SelectMap: TfpgComboBox;
      Bt_SelectDest: TfpgButton;
      L_DestDir: TfpgLabel;
      Bt_SelectFile: TfpgButton;
      Bt_Exit: TfpgButton;
      procedure Bt_SelectDestClick(Sender: TObject);
      procedure Bt_SelectFileClick(Sender: TObject);
      procedure Bt_ExitClick(Sender: TObject);
    public
      constructor Create(AOwner: TComponent); override;
    end;

var
  F_MainForm: TF_MainForm;

implementation

uses
  u_Parsettf;

var
  MapList: TStringList;

procedure TF_MainForm.Bt_SelectDestClick(Sender: TObject);
begin
DestDir:= SelectDirDialog(RepCourant);
L_DestDir.Text:= DestDir;
end;

procedure TF_MainForm.Bt_SelectFileClick(Sender: TObject);
var
  FileDlg: TfpgFileDialog;
  Fichier,Extension,FontType: string;
begin
FileDlg:= TfpgFileDialog.Create(nil);
//FileDlg.Filter:= 'True type fonts (*.ttf;*.otf)|*.ttf;*.otf|Type1 fonts (*.pfa;*.pfb)|*.pfa;*.pfb';
FileDlg.Filter:= 'True type fonts (*.ttf;*.otf)|*.ttf;*.otf';
FileDlg.FontDesc:= 'bitstream vera sans-9';
{$ifdef linux}
FileDlg.InitialDir:= GetEnv('GS_LIB');
{$endif}
{$ifdef win32}
//FileDlg.InitialDir:= '/WINDOWS/Fonts';
{$endif}
try
  if FileDlg.RunOpenFile
  then
    begin
    Fichier:= ExtractFileName(FileDlg.FileName);
    //Extension:= Lowercase(Copy(Fichier,Length(Fichier)-3,3));
    //if (Extension= 'ttf') or (Extension= 'otf')
    //then
    //  FontType:= 'TrueType'
    //else
    //  if Extension= 'pfb'
    //  then
    //    FontType:= 'Type1';
    Parser:= T_Parser.Create(nil);
    Parser.MakeFont(Fichier,Cb_SelectMap.Text,True);
    end;
finally
  FileDlg.Free;
  end;
end;

procedure TF_MainForm.Bt_ExitClick(Sender: TObject);
begin
Parser.Free;
MapList.Free;
Close;
end;

constructor TF_MainForm.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
Name := 'F_MainForm';
WindowTitle:= 'TTF parser';
SetPosition(0, 0, 500, 250);
WindowPosition:= wpScreenCenter;
Sizeable:= False;
MapList:= TStringList.Create;
with MapList do
  begin
  Add('cp874');
  Add('cp1250');
  Add('cp1251');
  Add('cp1252');
  Add('cp1253');
  Add('cp1254');
  Add('cp1255');
  Add('cp1257');
  Add('cp1258');
  Add('iso-8859-1');
  Add('iso-8859-2');
  Add('iso-8859-4');
  Add('iso-8859-5');
  Add('iso-8859-7');
  Add('iso-8859-9');
  Add('iso-8859-11');
  Add('iso-8859-15');
  Add('iso-8859-16');
  Add('koi8-r');
  Add('koi8-u');
  end;
L_SelectMap:= CreateLabel(Self,200,30,'Select mapping',100,20,taCenter);
Cb_SelectMap:= CreateComboBox(Self,200,50,100,MapList,20);
Cb_SelectMap.FocusItem:= 3;
Bt_SelectDest:= CreateButton(Self,170,100,160,'Destination directory',@Bt_SelectDestClick);
L_DestDir:= CreateLabel(Self,0,130,'',500,18,taCenter);
Bt_SelectFile:= CreateButton(Self,200,180,100,'Select file',@Bt_SelectFileClick,'');
Bt_Exit:= CreateButton(Self,210,220,80,'Exit',@Bt_ExitClick,'');
RepCourant:= ExtractFilePath(Paramstr(0));
L_DestDir.Text:= Repcourant;
end;

end.

