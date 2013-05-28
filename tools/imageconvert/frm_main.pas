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
    procedure MemoDragEnter(Sender, Source: TObject; AMimeList: TStringList;
      var AMimeChoice: TfpgString; var ADropAction: TfpgDropAction;
      var Accept: Boolean);
    procedure MemoDragDrop(Sender, Source: TObject; X, Y: integer; AData: variant);
    function ConvertImage(const AFileName: string): string;
    procedure btnClearClicked(Sender: TObject);
    procedure btnConvertClicked(Sender: TObject);
    procedure btnCopyClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  fpg_utils;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.miFileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MemoDragEnter(Sender, Source: TObject;
  AMimeList: TStringList; var AMimeChoice: TfpgString;
  var ADropAction: TfpgDropAction; var Accept: Boolean);
var
  s: string;
begin
  {TODO: Once Windows DND backend is 100% complete, this IFDEF can be removed.}
  {$IFDEF MSWINDOWS}
  s := 'FileName';
  {$ELSE}
  s := 'text/uri-list';
  {$ENDIF}
  Accept := AMimeList.IndexOf(s) > -1;
  if Accept then
  begin
    if AMimeChoice <> s then
      AMimeChoice := s;
  end;
end;

procedure TMainForm.MemoDragDrop(Sender, Source: TObject; X, Y: integer;
  AData: variant);
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

function TMainForm.ConvertImage(const AFileName: string): string;
const
  Prefix = '     ';
  MaxLineLength = 72;
var
  InStream: TFileStream;
  I, Count: longint;
  b: byte;
  Line, ToAdd: String;
  ConstName: string;

  procedure WriteStr(const St: string);
  begin
    Result := Result + St;
  end;

  procedure WriteStrLn(const St: string);
  begin
    Result := Result + St + LineEnding;
  end;

begin
  InStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    ConstName := 'newimg_' + ChangeFileExt(fpgExtractFileName(AFileName), '');
    WriteStrLn('');
    WriteStrLn('const');

    InStream.Seek(0, soFromBeginning);
    Count := InStream.Size;
    WriteStrLn(Format('  %s: array[0..%d] of byte = (',[ConstName, Count-1]));
    Line := Prefix;
    for I := 1 to Count do
    begin
      InStream.Read(B, 1);
      ToAdd := Format('%3d',[b]);
      if I < Count then
        ToAdd := ToAdd + ',';
      Line := Line + ToAdd;
      if Length(Line) >= MaxLineLength then
      begin
        WriteStrLn(Line);
        Line := PreFix;
      end;
    end; { for }
    WriteStrln(Line+');');
    WriteStrLn('');
  finally
    InStream.Free;
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
  DNDEnabled := True;

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
    AcceptDrops := True;
    OnDragEnter  := @MemoDragEnter;
    OnDragDrop  := @MemoDragDrop;
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
