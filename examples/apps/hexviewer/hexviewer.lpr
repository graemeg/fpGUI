program hexview;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form, fpg_hexview, fpg_menu, fpg_dialogs;

type

  { TMainForm }

  TMainForm = class(TfpgForm)
  private
    FMenu: TfpgMenuBar;
    FFileOpen: TfpgMenuItem;
    FFileClose: TfpgMenuItem;
    FDialog: TfpgFileDialog;
    procedure CreateMenu;

    procedure FileCloseClick(Sender: TObject);
    procedure FileExitClick(Sender: TObject);
    procedure FileOpenClick(Sender: TObject);
    procedure OptionsSwapEndianClick(Sender: TObject);
    procedure OpenFile(AFileName: String);
  private
    {@VFD_HEAD_BEGIN: MainForm}
    HexView: TfpgHexView;
    HexPanel: TfpgHexPanel;
    {@VFD_HEAD_END: MainForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.CreateMenu;
var
  MTop: TfpgMenuItem;
begin
  FMenu := TfpgMenuBar.Create(Self);
  FMenu.Align:=alTop;
  // File
  MTop := FMenu.AddMenuItem('&File', nil);
  MTop.SubMenu := TfpgPopupMenu.Create(MTop);
  FFileOpen := MTop.SubMenu.AddMenuItem('&Open','', @FileOpenClick);
  FFileClose := MTop.SubMenu.AddMenuItem('&Close','', @FileCloseClick);
  MTop.SubMenu.AddMenuItem('-','', nil);
  MTop.SubMenu.AddMenuItem('E&xit','', @FileExitClick);

  // Options
  MTop := FMenu.AddMenuItem('&Options', nil);
  MTop.SubMenu := TfpgPopupMenu.Create(MTop);
  MTop.SubMenu.AddMenuItem('Swap Endian', '', @OptionsSwapEndianClick);


  FFileClose.Enabled:=False;

  HexView.Top := FMenu.Height;
  HexView.Height:=HexPanel.Top - HexView.Top;
end;

procedure TMainForm.FileCloseClick(Sender: TObject);
begin
  FFileClose.Enabled:=False;
  HexView.Stream := nil;
end;

procedure TMainForm.FileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FileOpenClick(Sender: TObject);
begin
  if FDialog.RunOpenFile then
  begin
    try
      OpenFile(FDialog.FileName);
    finally
      FDialog.Close;
    end;
  end;
end;

procedure TMainForm.OptionsSwapEndianClick(Sender: TObject);
var
  Item: TfpgMenuItem absolute Sender;
begin
  Item.Checked := not Item.Checked;
  HexPanel.ReverseEndian:=Item.Checked;
end;

procedure TMainForm.OpenFile(AFileName: String);
var
  F: TFileStream;
begin
  if not FileExists(AFileName) then Exit;
  F := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyNone);
  HexView.Stream := F;
  FFileClose.Enabled:=True;
  WindowTitle := 'Hex Viewer - ' + ExtractFileName(AFileName);
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}

  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(489, 201, 600, 346);
  WindowTitle := 'Hex Viewer';
  Hint := '';
  IconName := '';
  MinWidth := 600;
  MinHeight := 300;

  HexView := TfpgHexView.Create(self);
  with HexView do
  begin
    Name := 'HexView';
    SetPosition(0, 0, 600, 237);
    Anchors := [anLeft,anRight,anTop,anBottom];
    TabOrder := 2;
    OwnsStream := True;
  end;

  HexPanel := TfpgHexPanel.Create(self);
  with HexPanel do
  begin
    Name := 'HexPanel';
    SetPosition(0, 244, 602, 100);
    Anchors := [anLeft,anBottom];
    TabOrder := 3;
    HexView := Self.HexView;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
  CreateMenu;
  FDialog := TfpgFileDialog.Create(Self);

  if (ParamCount > 0) and (FileExists(ParamStr(1))) then
      OpenFile(ParamStr(1));
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  fpgApplication.CreateForm(TMainForm, frm);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

