{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This is the server part to a standard FPC unit - dbugintf. This unit
      offers a simple API to send messages to a debug server (using
      SimpleIPC), modeled after the GExperts GDebug tool for Delphi, with
      some minor enhancements. This is a great way to debug CGI apps on a
      server as well.

      NOTE: you would normally wrap the SendXXX methods with {$ifdef debug} so
      the code can be excluded from a final released product (without debug
      information). But this is obviously for you do decide.

      Typical usage is as follows:

      uses
        dbugintf, sysutils;

      procedure BackupFile(FN : String);
      var
         BFN: String;
      begin
         SendMethodEnter('BackupFile');
         BFN := FN + '.bak';
         SendDebug(Format('backup file "%s" exists, deleting',[BFN]));
         SendDebug(Format('Backing up "%s" to "%s"',[FN,BFN]));
         SendMethodExit('BackupFile');
      end;

      procedure SaveToFile(FN : String);
      begin
         SendMethodEnter('SaveToFile');
         BackupFile(FN);
         SendDebug('Saving to file '+FN);
         SendMethodExit('SaveToFile');
      end;


      Available SendXXX methods from the dbugintf unit are:

        procedure SendBoolean(const Identifier: string; const Value: Boolean);
        procedure SendDateTime(const Identifier: string; const Value: TDateTime);
        procedure SendInteger(const Identifier: string; const Value: Integer; HexNotation: Boolean = False);
        procedure SendPointer(const Identifier: string; const Value: Pointer);
        procedure SendDebugEx(const Msg: string; MType: TDebugLevel);
        procedure SendDebug(const Msg: string);
        procedure SendMethodEnter(const MethodName: string);
        procedure SendMethodExit(const MethodName: string);
        procedure SendSeparator;
        procedure SendDebugFmt(const Msg: string; const Args: array of const);
        procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TDebugLevel);

}
unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils
  ,Classes
  ,fpg_base
  ,fpg_main
  ,fpg_form
  ,fpg_button
  ,fpg_panel
  ,fpg_menu
  ,fpg_basegrid
  ,fpg_grid
  ,simpleipc
  ,dbugmsg
  ;

type
  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    MainMenu: TfpgMenuBar;
    Bevel1: TfpgBevel;
    grdMessages: TfpgStringGrid;
    mnuFile: TfpgPopupMenu;
    mnuEdit: TfpgPopupMenu;
    mnuHelp: TfpgPopupMenu;
    btnQuit: TfpgButton;
    Bevel2: TfpgBevel;
    btnPause: TfpgButton;
    btnStart: TfpgButton;
    btnClear: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    miPause: TfpgMenuItem;
    FIPCSrv: TSimpleIPCServer;
    FPaused: Boolean;
    FAddAtBottom: Boolean;
    FDiscarded: Integer;
    FShowOnMessage: Boolean;
    procedure   StartServer;
    procedure   StopServer;
    procedure   CheckMessages(Sender: TObject);
    procedure   CheckDebugMessages;
    procedure   ReadDebugMessage;
    procedure   ShowDebugMessage(const AMsg: TDebugmessage);
    procedure   ShowMessageWindow;
    procedure   miPauseClicked(Sender: TObject);
    procedure   miFileQuit(Sender: TObject);
    procedure   miHelpAboutFPGui(Sender: TObject);
    procedure   miHelpProductInformation(Sender: TObject);
    procedure   btnClearClicked(Sender: TObject);
    procedure   btnPauseClicked(Sender: TObject);
    procedure   btnStartClicked(Sender: TObject);
    procedure   GridDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  dateutils
  ,fpg_dialogs
  ;


{$I images.inc}
{$I state_images.inc}

{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnClearClicked(Sender: TObject);
begin
  grdMessages.RowCount := 0;
end;

procedure TMainForm.btnPauseClicked(Sender: TObject);
begin
  FPaused := not FPaused;
  miPause.Checked := FPaused;
end;

procedure TMainForm.btnStartClicked(Sender: TObject);
begin
  if Assigned(FIPCSrv) then
    Exit;
  StartServer;
end;

procedure TMainForm.GridDrawCell(Sender: TObject; const ARow, ACol: Integer;
  const ARect: TfpgRect; const AFlags: TfpgGridDrawState; var ADefaultDrawing: boolean);
var
  img: TfpgImage;
  i: integer;
  dx: integer;
begin
  if ACol = 0 then
  begin
    ADefaultDrawing := False;
    try
      i := StrToInt(grdMessages.Cells[ACol, ARow]);
      case i of
        -1:  img := fpgImages.GetImage('dbs.state.stop');
         0:  img := fpgImages.GetImage('dbs.state.info');
         1:  img := fpgImages.GetImage('dbs.state.warning');
         2:  img := fpgImages.GetImage('dbs.state.error');
         3:  img := fpgImages.GetImage('dbs.state.identify');
      end;
      dx := (grdMessages.ColumnWidth[ACol] - 16) div 2;
      grdMessages.Canvas.DrawImage(ARect.Left + dx, ARect.Top {+ y}, img);
    except
      on E: Exception do
        begin
//          writeln('Cell text = ' + grdMessages.Cells[ACol, ARow]);
        end;
    end;
  end;
end;

procedure TMainForm.StartServer;
begin
  FIPCSrv := TSimpleIPCServer.Create(nil);
  FIPCSrv.ServerID := DebugServerID;
  FIPCSrv.Global := True;
  FIPCSrv.Active := True;
  FIPCSrv.StartServer;
  fpgApplication.OnIdle := @CheckMessages;
//  ITMessages.Enabled:=True;
end;

procedure TMainForm.StopServer;
begin
  fpgApplication.OnIdle := nil;
//  ITMessages.Enabled := False;
  FreeAndNil(FIPCSrv);
end;

procedure TMainForm.CheckMessages(Sender: TObject);
begin
  CheckDebugMessages;
end;

procedure TMainForm.CheckDebugMessages;
begin
  while FIPCSrv.PeekMessage(1, True) do
    ReadDebugMessage;
end;

procedure TMainForm.ReadDebugMessage;
var
  Msg: TDebugMessage;
begin
  FIPCSrv.MsgData.Seek(0, soFromBeginning);
  ReadDebugMessageFromStream(FIPCSrv.MsgData, MSg);
  if not FPaused then
    ShowDebugMessage(Msg)
  else
    Inc(FDiscarded);
end;

procedure TMainForm.ShowDebugMessage(const AMsg: TDebugmessage);
var
  r: integer;
begin
  grdMessages.BeginUpdate;
  try
    grdMessages.RowCount := grdMessages.RowCount + 1;
    r := grdMessages.RowCount-1;
    //if FAddAtBottom then
    //  grdMessages.Items.Add(LI)
    //else
    //  grdMessages.Items.InsertItem(LI, 0);
    grdMessages.Cells[0, r] := IntToStr(AMsg.MsgType);
    grdMessages.Cells[1, r] := FormatDateTime('HH:mm:ss', AMsg.MsgTimeStamp);
    grdMessages.Cells[2, r] := AMsg.Msg;
    grdMessages.FocusCol := 0;
    grdMessages.FocusRow := grdMessages.RowCount-1;
  finally
    grdMessages.EndUpdate;
  end;
  if FShowOnMessage then
    ShowMessageWindow;
end;

procedure TMainForm.ShowMessageWindow;
begin
  if not Visible then
    Show;
end;

procedure TMainForm.miPauseClicked(Sender: TObject);
begin
  FPaused := not FPaused;
  btnPause.Down := FPaused;
end;

procedure TMainForm.miFileQuit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miHelpAboutFPGui(Sender: TObject);
begin
  TfpgMessageDialog.AboutFPGui;
end;

procedure TMainForm.miHelpProductInformation(Sender: TObject);
begin
  TfpgMessageDialog.Information('Product Information', WindowTitle + LineEnding + 'Written by Graeme Geldenhuys - 2010');
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPaused := False;
  FAddAtBottom := False;
  FShowOnMessage := False;
  StartServer;

  fpgImages.AddMaskedBMP('dbs.clean', @DBS_clean, sizeof(DBS_clean), 15, 0);
  fpgImages.AddMaskedBMP('dbs.stop', @DBS_stop, sizeof(DBS_stop), 0, 0);
  fpgImages.AddMaskedBMP('dbs.pause', @DBS_pause, sizeof(DBS_pause), 0, 0);
  fpgImages.AddMaskedBMP('dbs.run', @DBS_run, sizeof(DBS_run), 0, 0);

  fpgImages.AddMaskedBMP('dbs.state.info', @DBS_state_info, sizeof(DBS_state_info), 0, 0);
  fpgImages.AddMaskedBMP('dbs.state.warning', @DBS_state_warning, sizeof(DBS_state_warning), 0, 0);
  fpgImages.AddMaskedBMP('dbs.state.error', @DBS_state_error, sizeof(DBS_state_error), 0, 0);
  fpgImages.AddMaskedBMP('dbs.state.identify', @DBS_state_lightbulb, sizeof(DBS_state_lightbulb), 0, 0);
  fpgImages.AddMaskedBMP('dbs.state.stop', @DBS_state_lightbulb_off, sizeof(DBS_state_lightbulb_off), 0, 0);
end;

destructor TMainForm.Destroy;
begin
  StopServer;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(353, 245, 486, 313);
  WindowTitle := 'fpGUI''s Debug Server';
  Hint := '';
  ShowHint := True;

  MainMenu := TfpgMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 486, 21);
    Anchors := [anLeft,anRight,anTop];
  end;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 21, 486, 30);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Style := bsLowered;
    Shape := bsBottomLine;
  end;

  grdMessages := TfpgStringGrid.Create(self);
  with grdMessages do
  begin
    Name := 'grdMessages';
    SetPosition(4, 55, 478, 254);
    Anchors := [anLeft,anRight,anTop,anBottom];
    AddColumn('Type', 50, taLeftJustify);
    AddColumn('Time', 75, taCenter);
    AddColumn('Message', 330, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := True;
    ShowGrid := False;
    TabOrder := 2;
    Options := [go_SmoothScroll];
    OnDrawCell  := @GridDrawCell;
  end;

  mnuFile := TfpgPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(260, 100, 120, 24);
    miPause := AddMenuItem('Pause', '', @miPauseClicked);
    AddMenuItem('-', '', nil);
    AddMenuItem('Quit', '', @miFileQuit);
  end;

  mnuEdit := TfpgPopupMenu.Create(self);
  with mnuEdit do
  begin
    Name := 'mnuEdit';
    SetPosition(260, 126, 120, 24);
    AddMenuItem('Cut', '', nil).Enabled := False;
    AddMenuItem('Copy', '', nil).Enabled := False;
    AddMenuItem('Paste', '', nil).Enabled := False;
    AddMenuItem('-', '', nil);
    AddMenuItem('Preferences...', '', nil).Enabled := False;
  end;

  mnuHelp := TfpgPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(260, 152, 120, 24);
    AddMenuItem('About fpGUI...', '', @miHelpAboutFPGui);
    AddMenuItem('Product Information...', 'F1', @miHelpProductInformation);
  end;

  btnQuit := TfpgButton.Create(Bevel1);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.quit';
    ImageSpacing := 0;
    TabOrder := 0;
    Focusable := False;
    OnClick := @miFileQuit;
  end;

  Bevel2 := TfpgBevel.Create(Bevel1);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(34, 2, 8, 25);
    Hint := '';
    Style := bsLowered;
    Shape := bsLeftLine;
  end;

  btnPause := TfpgButton.Create(Bevel1);
  with btnPause do
  begin
    Name := 'btnPause';
    SetPosition(43, 2, 24, 24);
    Text := '';
    AllowAllUp := True;
    Flat := True;
    FontDesc := '#Label1';
    GroupIndex := 1;
    Hint := 'pause server';
    ImageMargin := -1;
    ImageName := 'dbs.pause';
    ImageSpacing := 0;
    TabOrder := 2;
    Focusable := False;
    OnClick :=@btnPauseClicked;
  end;

  btnStart := TfpgButton.Create(Bevel1);
  with btnStart do
  begin
    Name := 'btnStart';
    SetPosition(67, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'start server';
    ImageMargin := -1;
    ImageName := 'dbs.run';
    ImageSpacing := 0;
    TabOrder := 2;
    Focusable := False;
    OnClick := @btnStartClicked;
    Enabled := False;
  end;

  btnClear := TfpgButton.Create(Bevel1);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(91, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'clear listview';
    ImageMargin := -1;
    ImageName := 'dbs.clean';
    ImageSpacing := 0;
    TabOrder := 2;
    Focusable := False;
    OnClick :=@btnClearClicked;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  // Hook up the menus to the MenuBar
  MainMenu.AddMenuItem('Server', nil).SubMenu := mnuFile;
  MainMenu.AddMenuItem('Edit', nil).SubMenu := mnuEdit;
  MainMenu.AddMenuItem('Help', nil).SubMenu := mnuHelp;
end;


end.
