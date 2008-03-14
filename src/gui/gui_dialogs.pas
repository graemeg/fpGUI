{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Standard dialogs used by fpGUI based applications.
}

unit gui_dialogs;

{$mode objfpc}{$H+}

{
  TODO:
    * Try and refactor the code to remove all IFDEF's
    * Implement MessageDlg with icons and buttons [Work-In-Progress]
    * Select Directory dialog (treeview style)
}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_imgfmt_bmp,
  gfx_constants,
  gui_form,
  gui_button,
  gui_label,
  gui_listbox,
  gui_checkbox,
  gui_edit,
  gui_grid,
  gui_combobox,
  gui_bevel,
  gui_memo;

type
  TfpgMsgDlgType = (mtAbout, mtWarning, mtError, mtInformation, mtConfirmation,
      mtCustom);
      
  TfpgMsgDlgBtn = (mbNoButton, mbOK, mbCancel, mbYes, mbNo, mbAbort,
      mbRetry, mbIgnore, mbYesToAll, mbNoToAll, mbHelp, mbClose);
      
  TfpgMsgDlgButtons = set of TfpgMsgDlgBtn;

const
  mbYesNoCancel       = [mbYes, mbNo, mbCancel];
  mbYesNo             = [mbYes, mbNo];
  mbOKCancel          = [mbOK, mbCancel];
  mbAbortRetryIgnore  = [mbAbort, mbRetry, mbIgnore];


  cMsgDlgBtnText: array[TfpgMsgDlgBtn] of string =
      ( '', rsOK, rsCancel, rsYes, rsNo, rsAbort, rsRetry, rsIgnore,
        rsYesToAll, rsNoToAll, rsHelp, rsClose );

type

  TfpgMessageBox = class(TfpgForm)
  private
    FLines: TStringList;
    FFont: TfpgFont;
    FTextY: integer;
    FLineHeight: integer;
    FMaxLineWidth: integer;
    FButton: TfpgButton;
    FCentreText: Boolean;
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetMessage(AMessage: string);
    property    CentreText: Boolean read FCentreText write FCentreText default False;
  end;
  

  TfpgBaseDialog = class(TfpgForm)
  protected
    FSpacing: integer;
    FDefaultButtonWidth: integer;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure   btnOKClick(Sender: TObject); virtual;
    procedure   btnCancelClick(Sender: TObject); virtual;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   SetupCaptions; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TfpgFontSelectDialog = class(TfpgBaseDialog)
  private
    FSampleText: string;
    FMode: Byte;    // 1 - Normal Fonts;  2 - Alias Fonts
    lblLabel1: TfpgLabel;
    lblTypeface: TfpgLabel;
    lblSize: TfpgLabel;
    lblLabel4: TfpgLabel;
    lblLabel5: TfpgLabel;
    lbCollection: TfpgListBox;
    lbFaces: TfpgListBox;
    lbSize: TfpgListBox;
    cbBold: TfpgCheckBox;
    cbItalic: TfpgCheckBox;
    cbUnderline: TfpgCheckBox;
    cbAntiAlias: TfpgCheckBox;
    memSample: TfpgMemo;
    procedure   OnCollectionChanged(Sender: TObject);
    procedure   OnParamChange(Sender: TObject);
    procedure   CreateFontList;
    procedure   CreateFontAliasList;
    procedure   SetupUI(AMode: Byte);
  protected
    function    GetFontDesc: string;
    procedure   SetFontDesc(Desc: string);
    procedure   SetupCaptions; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   SetSampleText(AText: string);
  end;
  
  
  { TfpgFileDialog }

  TfpgFileDialog = class(TfpgBaseDialog)
  private
    chlDir: TfpgComboBox;
    grid: TfpgFileGrid;
    btnUpDir: TfpgButton;
    btnDirNew: TfpgButton;
    btnShowHidden: TfpgButton;
    panel1: TfpgBevel;
    lbFileInfo: TfpgLabel;
    edFilename: TfpgEdit;
    chlFilter: TfpgComboBox;
    lb1: TfpgLabel;
    lb2: TfpgLabel;
    FOpenMode: boolean;
    FFilterList: TStringList;
    FFilter: string;
    procedure   SetFilter(const Value: string);
    function    GetShowHidden: boolean;
    procedure   SetShowHidden(const Value: boolean);
    procedure   ListChanged(Sender: TObject; ARow: integer);
    procedure   GridDblClicked(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   InitializeComponents;
    procedure   ProcessFilterString;
    function    GetFileFilter: string;
    procedure   FilterChange(Sender: TObject);
    procedure   DirChange(Sender: TObject);
    procedure   UpDirClick(Sender: TObject);
    procedure   btnDirNewClicked(Sender: TObject);
    procedure   edFilenameChanged(Sender: TObject);
    procedure   UpdateButtonState;
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   btnOKClick(Sender: TObject); override;
    procedure   SetCurrentDirectory(const ADir: string);
  public
    FileName: string;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    SelectFile(const AFilename: string): boolean;
    function    RunOpenFile: boolean;
    function    RunSaveFile: boolean;
    property    Filter: string read FFilter write SetFilter;
    property    ShowHidden: boolean read GetShowHidden write SetShowHidden;
  end;
  

{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$define read_interface}
{$undef read_implementation}


{$I messagedialog.inc}
{$I newdirdialog.inc}
{$I promptuserdialog.inc}



procedure ShowMessage(AMessage, ATitle: string; ACentreText: Boolean = False); overload;
procedure ShowMessage(AMessage: string; ACentreText: Boolean = False); overload;

function SelectFontDialog(var FontDesc: string): boolean;


implementation

uses
  gfx_widget,
  gfx_utils,
  gfx_utf8utils
  {$IFDEF MSWINDOWS}
  ,Windows   // used by File Dialog
  {$ENDIF}
  ,DateUtils
  ;
  
  
procedure WrapText(const AText: String; ALines: TStrings; AFont: TfpgFont;
    const ALineWidth: Integer; out AWidth: Integer);
var
  maxw: integer;
  n: integer;
  s, s2: string;
  c: char;

  // -----------------
  procedure AddLine(all: boolean);
  var
    w: integer;
    m: integer;
  begin
    s2  := s;
    w   := AFont.TextWidth(s2);
    if w > ALineWidth then
    begin
      while w > ALineWidth do
      begin
        m := UTF8Length(s);
        repeat
          Dec(m);
          s2  := UTF8Copy(s,1,m);
          w   := AFont.TextWidth(s2);
        until w <= ALineWidth;
        if w > maxw then
          maxw := w;

        // are we in the middle of a word. If so find the beginning of word.
        while UTF8Copy(s2, m, m+1) <> ' ' do
        begin
          Dec(m);
          s2  := UTF8Copy(s,1,m);
        end;

        ALines.Add(s2);
        s   := UTF8Copy(s, m+1, UTF8length(s));
        s2  := s;
        w   := AFont.TextWidth(s2);
      end; { while }
      if all then
      begin
        ALines.Add(s2);
        s := '';
      end;
    end
    else
    begin
      ALines.Add(s2);
      s := '';
    end; { if/else }

    if w > maxw then
      maxw := w;
  end;

begin
  s := '';
  ALines.Clear;
  n := 1;
  maxw := 0;
  while n <= Length(AText) do
  begin
    c := AText[n];
    if (c = #13) or (c = #10) then
    begin
      // True indicates that if the text is split over multiple lines the last
      // line must also be pocessed before continuing. If False then double CR
      // can get ignored.
      AddLine(true);
      if (c = #13) and (n < Length(AText)) and (AText[n+1] = #10) then
        Inc(n);
    end
    else
      s := s + c;
    Inc(n);
  end; { while }

  AddLine(true);

  // set out variable
  AWidth := maxw;
end;

procedure ShowMessage(AMessage, ATitle: string; ACentreText: Boolean);
var
  frm: TfpgMessageBox;
begin
  frm := TfpgMessageBox.Create(nil);
  try
    frm.WindowTitle := ATitle;
    frm.CentreText := ACentreText;
    frm.SetMessage(AMessage);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure ShowMessage(AMessage: string; ACentreText: Boolean);
begin
  ShowMessage(AMessage, rsMessage, ACentreText);
end;

function SelectFontDialog(var FontDesc: string): boolean;
var
  frm: TfpgFontSelectDialog;
begin
  Result := False;
  frm := TfpgFontSelectDialog.Create(nil);
  frm.SetFontDesc(FontDesc);
  if frm.ShowModal = 1 then
  begin
    FontDesc := frm.GetFontDesc;
    Result := True;
  end;
  frm.Free;
end;


{ TfpgMessageBox }

procedure TfpgMessageBox.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if keycode = keyEscape then
    Close;
end;

procedure TfpgMessageBox.HandlePaint;
var
  n, y: integer;
  tw: integer;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;

  Canvas.SetFont(FFont);
  y := FTextY;
  for n := 0 to FLines.Count-1 do
  begin
    tw := FFont.TextWidth(FLines[n]);
    if CentreText then
      Canvas.DrawString(Width div 2 - tw div 2, y, FLines[n])
    else
      Canvas.DrawString(10, y, FLines[n]);
    Inc(y, FLineHeight);
  end;
  Canvas.EndDraw;
end;

procedure TfpgMessageBox.HandleShow;
begin
  inherited HandleShow;
  FButton.SetFocus;
end;

constructor TfpgMessageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowPosition := wpScreenCenter;
  Sizeable := False;
  
  FLines        := TStringList.Create;
  FFont         := fpgGetFont('#Label1');
  FTextY        := 10;
  FLineHeight   := FFont.Height + 4;
  MinWidth      := 200;
  FMaxLineWidth := 500;
  FCentreText   := False;
  
  FButton := TfpgButton.Create(self);
  FButton.Text    := cMsgDlgBtnText[mbOK];
  FButton.Width   := 75;
  FButton.ModalResult := Ord(mbOK);
end;

destructor TfpgMessageBox.Destroy;
begin
  FFont.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TfpgMessageBox.SetMessage(AMessage: string);
var
  outw: integer;
begin
  WrapText(AMessage, FLines, FFont, FMaxLineWidth, outw);
  
  // dialog width with 10 pixel border on both sides
  Width := outw + 2*10;

  if Width < FMinWidth then
    Width := FMinWidth;

  // center button
  FButton.Top   := FTextY + FLineHeight*FLines.Count + FTextY;
  FButton.Left  := (Width div 2) - (FButton.Width div 2);

  // adjust dialog's height
  Height := FButton.Top + FButton.Height + FTextY;
end;

{ TfpgBaseDialog }

procedure TfpgBaseDialog.btnOKClick(Sender: TObject);
begin
  ModalResult := 1;
end;

procedure TfpgBaseDialog.btnCancelClick(Sender: TObject);
begin
  ModalResult := 2;
  Close;
end;

procedure TfpgBaseDialog.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEscape then   // Esc cancels the dialog
    btnCancelClick(nil)
  else
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgBaseDialog.SetupCaptions;
begin
  btnCancel.Text  := rsCancel;
  btnOK.Text      := rsOK;
end;

constructor TfpgBaseDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width     := 500;
  Height    := 400;
  MinWidth  := 300;
  MinHeight := 300;
  WindowPosition := wpScreenCenter;
  FSpacing  := 6;
  FDefaultButtonWidth := 80;

  btnCancel := CreateButton(self, Width-FDefaultButtonWidth-FSpacing, 370, FDefaultButtonWidth, rsCancel, @btnCancelClick);
  btnCancel.Name      := 'btnCancel';
  btnCancel.ImageName := 'stdimg.Cancel';   // Do NOT localize
  btnCancel.ShowImage := True;
  btnCancel.Anchors   := [anRight, anBottom];
  btnCancel.TabOrder  := 2;

  btnOK := CreateButton(self, btnCancel.Left-FDefaultButtonWidth-FSpacing, 370, FDefaultButtonWidth, rsOK, @btnOKClick);
  btnOK.Name      := 'btnOK';
  btnOK.ImageName := 'stdimg.OK';   // Do NOT localize
  btnOK.ShowImage := True;
  btnOK.Anchors   := [anRight, anBottom];
  btnOK.TabOrder  := 1;
end;


{ TfpgFontSelectDialog }

procedure TfpgFontSelectDialog.OnCollectionChanged(Sender: TObject);
begin
  if lbCollection.Text = rsCollectionFontAliases then
  begin
    CreateFontAliasList;
    SetupUI(2);
  end
  else
  begin
    CreateFontList;
    SetupUI(1);
  end;
  OnParamChange(nil);
end;

procedure TfpgFontSelectDialog.OnParamChange(Sender: TObject);
var
  fdesc: string;
begin
  fdesc := GetFontDesc;
  {$IFDEF DEBUG} Writeln(fdesc); {$ENDIF}
  memSample.FontDesc := fdesc;
  memSample.Text := FSampleText;
  if FMode = 2 then
    memSample.Lines.Add(fpgGetNamedFontDesc(UTF8Copy(fdesc, 2, UTF8Length(fdesc)-1)));
end;

procedure TfpgFontSelectDialog.CreateFontList;
var
  fl: TStringList;
  i: integer;
begin
  lbFaces.Items.Clear;
  fl := fpgApplication.GetFontFaceList;
  for i := 0 to fl.Count-1 do
    lbFaces.Items.Add(fl[i]);
  fl.Free;
  lbFaces.FocusItem := 1;
end;

procedure TfpgFontSelectDialog.CreateFontAliasList;
var
  fl: TStringList;
  i: integer;
begin
  lbFaces.Items.Clear;
  fl := fpgGetNamedFontList;
  for i := 0 to fl.Count-1 do
    lbFaces.Items.Add(fl.Names[i]);
  fl.Free;
  lbFaces.FocusItem := 1
end;

procedure TfpgFontSelectDialog.SetupUI(AMode: Byte);
begin
  FMode := AMode;
  case FMode of
    1:  // Normal Fonts
      begin
        lblSize.Enabled := True;
        lblTypeFace.Enabled := True;
        lbSize.Enabled := True;
        cbBold.Enabled := True;
        cbItalic.Enabled := True;
        cbUnderline.Enabled := True;
        cbAntiAlias.Enabled := True;
      end;
    2:  // Font Aliases
      begin
        lblSize.Enabled := False;
        lblTypeFace.Enabled := False;
        lbSize.Enabled := False;
        cbBold.Enabled := False;
        cbItalic.Enabled := False;
        cbUnderline.Enabled := False;
        cbAntiAlias.Enabled := False;
      end;
  end;
end;

function TfpgFontSelectDialog.GetFontDesc: string;
var
  s: string;
begin
  if FMode = 2 then
    s := lbFaces.Text
  else
  begin
    s := lbFaces.Text + '-' + lbSize.Text;
    // Do NOT localize these!
    if cbBold.Checked then
      s := s + ':bold';

    if cbItalic.Checked then
      s := s + ':italic';

    if cbAntiAlias.Checked then
      s := s + ':antialias=true'
    else
      s := s + ':antialias=false';

    if cbUnderline.Checked then
      s := s + ':underline';
  end;
  result := s;
end;

procedure TfpgFontSelectDialog.SetFontDesc(Desc: string);
var
  cp: integer;
  c: char;
  i: integer;
  token: string;
  prop: string;
  propval: string;

  function NextC: char;
  begin
    inc(cp);
    if cp > length(Desc) then
      c := #0
    else
      c := Desc[cp];
    result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ','a'..'z','A'..'Z','_','0'..'9']) do
    begin
      token := token + c;
      NextC;
    end;
  end;
  
  procedure ProcessAliasFont;
  var
    i: integer;
  begin
    lbCollection.FocusItem := lbCollection.ItemCount;
    for i := 1 to lbFaces.ItemCount do
    begin
      if SameText(lbFaces.Items[i-1], Desc) then
      begin
        lbFaces.FocusItem := i;
        Exit; //==>
      end;
    end;
  end;

begin
  cp := 1;
  c  := Desc[1];

  if Desc[1] = '#' then
    FMode := 2
  else
    FMode := 1;
  SetupUI(FMode);

  if FMode = 2 then
  begin
    ProcessAliasFont;
    Exit; //==>
  end;

  cbBold.Checked      := False;
  cbItalic.Checked    := False;
  cbUnderline.Checked := False;
  cbAntiAlias.Checked := True;

  NextToken;
  i := lbFaces.Items.IndexOf(token);
  if i >= 0 then
    lbFaces.FocusItem := i+1;
  if c = '-' then
  begin
    NextC;
    NextToken;
    i := lbSize.Items.IndexOf(token);
    if i >= 0 then
      lbSize.FocusItem := i+1;
  end;

  while c = ':' do
  begin
    NextC;
    NextToken;

    prop := UpperCase(token);
    propval := '';

    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;

    // Do NOT localize these!
    if prop = 'BOLD' then
    begin
      cbBold.Checked := True;
    end
    else if prop = 'ITALIC' then
    begin
      cbItalic.Checked := True;
    end
    else if prop = 'ANTIALIAS' then
    begin
      if propval = 'FALSE' then
        cbAntialias.Checked := False;
    end
    else if prop = 'UNDERLINE' then
    begin
      cbUnderline.Checked := True;
    end;

  end;

  OnParamChange(self);
end;

procedure TfpgFontSelectDialog.SetupCaptions;
begin
  inherited SetupCaptions;
end;

constructor TfpgFontSelectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := rsSelectAFont;
  Width       := 600;
  MinWidth    := Width;
  MinHeight   := Height;
  FSampleText := 'The quick brown fox jumps over the lazy dog. 0123456789 [oO0,ilLI]';
  FMode       := 1; // normal fonts
  
  btnCancel.Left := Width - FDefaultButtonWidth - FSpacing;
  btnOK.Left     := btnCancel.Left - FDefaultButtonWidth - FSpacing;

  lblLabel5 := TfpgLabel.Create(self);
  with lblLabel5 do
  begin
    SetPosition(8, 8, 73, 16);
    Text := fpgAddColon(rsCollection);
  end;

  { TODO : This need to be implemented at some stage. }
  lbCollection := TfpgListBox.Create(self);
  with lbCollection do
  begin
    Name := 'lbCollection';
    SetPosition(8, 28, 145, 236);
    Items.Add(rsCollectionAllFonts);
    // These should be stored in <users config path>/fpgui directory
    Items.Add(rsCollectionRecentlyUsed);
    Items.Add(rsCollectionFavourites);
    // From here onwards, these should be created automatically.
    Items.Add(rsCollectionFixedWidth);
    Items.Add(rsCollectionSans);
    Items.Add(rsCollectionSerif);
    Items.Add(rsCollectionFontAliases);
    FocusItem := 1;
    OnChange := @OnCollectionChanged;
//    Enabled := False;
  end;

  lblLabel1 := TfpgLabel.Create(self);
  with lblLabel1 do
  begin
    SetPosition(161, 8, 73, 16);
    Text := fpgAddColon(rsName);
  end;

  lbFaces := TfpgListBox.Create(self);
  with lbFaces do
  begin
    Name := 'lbFaces';
    SetPosition(161, 28, 232, 236);
    Items.Add(' ');
    OnChange := @OnParamChange;
  end;

  lblSize := TfpgLabel.Create(self);
  with lblSize do
  begin
    Name := 'lblSize';
    SetPosition(401, 8, 54, 16);
    Text := fpgAddColon(rsSize);
  end;

  lbSize := TfpgListBox.Create(self);
  with lbSize do
  begin
    Name := 'lbSize';
    SetPosition(401, 28, 52, 236);
    { We need to improve this! }
    Items.Add('6');
    Items.Add('7');
    Items.Add('8');
    Items.Add('9');
    Items.Add('10');
    Items.Add('11');
    Items.Add('12');
    Items.Add('13');
    Items.Add('14');
    Items.Add('15');
    Items.Add('16');
    Items.Add('18');
    Items.Add('20');
    Items.Add('24');
    Items.Add('28');
    Items.Add('32');
    Items.Add('48');
    Items.Add('64');
    Items.Add('72');
    OnChange  := @OnParamChange;
  end;

  lblTypeface := TfpgLabel.Create(self);
  with lblTypeface do
  begin
    Name := 'lblTypeface';
    SetPosition(461, 8, 54, 16);
    Text := fpgAddColon(rsTypeface);
  end;

  cbBold := TfpgCheckBox.Create(self);
  with cbBold do
  begin
    SetPosition(461, 32, 110, 20);
    Text := rsBold;
    OnChange := @OnParamChange;
  end;

  cbItalic := TfpgCheckBox.Create(self);
  with cbItalic do
  begin
    SetPosition(461, 56, 110, 20);
    Text := rsItalic;
    OnChange := @OnParamChange;
  end;

  cbUnderline := TfpgCheckBox.Create(self);
  with cbUnderline do
  begin
    SetPosition(461, 80, 110, 20);
    Text := rsUnderScore;
    OnChange := @OnParamChange;
  end;

  cbAntiAlias := TfpgCheckBox.Create(self);
  with cbAntiAlias do
  begin
    SetPosition(461, 124, 110, 20);
    Text := rsAntiAliasing;
    OnChange := @OnParamChange;
    Checked := True;
  end;

  lblLabel4 := TfpgLabel.Create(self);
  with lblLabel4 do
  begin
    SetPosition(8, 268, 584, 16);
    Text := fpgAddColon(rsExampleText);
  end;

  memSample := TfpgMemo.Create(self);
  with memSample do
  begin
    SetPosition(8, 288, 584, 65);
    Text := FSampleText;
    Anchors := [anLeft, anTop, anRight, anBottom];
  end;

  CreateFontList;
end;

procedure TfpgFontSelectDialog.SetSampleText(AText: string);
begin
  if FSampleText = AText then
    Exit; //==>
  if AText = '' then
    Exit; //==>
    
  FSampleText := AText;
  memSample.Text := FSampleText;
end;


{ TfpgFileDialog }

procedure TfpgFileDialog.ListChanged(Sender: TObject; ARow: integer);
var
  s: string;
begin
  if grid.CurrentEntry = nil then
    Exit; //==>
  s := grid.CurrentEntry.Name;

  if grid.CurrentEntry.IsLink then
    s := s + ' -> ' + grid.CurrentEntry.LinkTarget;

  if grid.CurrentEntry.EntryType <> etDir then
    edFileName.Text := grid.CurrentEntry.Name;

  UpdateButtonState;
  lbFileInfo.Text := s;
end;

procedure TfpgFileDialog.GridDblClicked(Sender: TObject; AButton: TMouseButton;
  AShift: TShiftState; const AMousePos: TPoint);
var
  e: TFileEntry;
begin
  e := grid.CurrentEntry;
  if (e = nil) then
    Exit; //==>

  if (e.EntryType = etDir) then
    SetCurrentDirectory(e.Name)
  else if (e.EntryType = etFile) then
    btnOKClick(Sender);
end;

procedure TfpgFileDialog.SetFilter(const Value: string);
begin
  FFilter := Value;
  ProcessFilterString;
end;

function TfpgFileDialog.GetShowHidden: boolean;
begin
  Result := btnShowHidden.Down;
end;

procedure TfpgFileDialog.SetShowHidden(const Value: boolean);
begin
  btnShowHidden.Down := Value;
end;

procedure TfpgFileDialog.InitializeComponents;
begin
  chlDir := TfpgComboBox.Create(self);
  with chlDir do
  begin
    SetPosition(8, 12, 526, 22);
    Anchors := [anLeft, anRight, anTop];
    FontDesc := '#List';
    OnChange := @DirChange;
  end;

  grid := TfpgFileGrid.Create(self);
  with grid do
  begin
    SetPosition(8, 44, 622, 200);
    Anchors := [anLeft, anRight, anTop, anBottom];
    OnRowChange := @ListChanged;
    OnDoubleClick := @GridDblClicked;
  end;

  btnUpDir := TfpgButton.Create(self);
  with btnUpDir do
  begin
    SetPosition(540, 11, 26, 24);
    Anchors := [anRight, anTop];
    Text := '';
    FontDesc := '#Label1';
    ImageName := 'stdimg.folderup';   // Do NOT localize
    ModalResult := 0;
    Focusable := False;
    OnClick := @UpDirClick;
  end;

  btnDirNew := TfpgButton.Create(self);
  with btnDirNew do
  begin
    SetPosition(572, 11, 26, 24);
    Anchors := [anRight, anTop];
    Text := '';
    FontDesc := '#Label1';
    ImageName := 'stdimg.foldernew';    // Do NOT localize
    ModalResult := 0;
    Focusable := False;
    OnClick := @btnDirNewClicked;
  end;

  btnShowHidden := TfpgButton.Create(self);
  with btnShowHidden do
  begin
    SetPosition(604, 11, 26, 24);
    Anchors := [anRight, anTop];
    Text := '';
    FontDesc := '#Label1';
    ImageName := 'stdimg.hidden';   // Do NOT localize
    ModalResult := 0;
    Focusable := False;
    GroupIndex := 1;
    AllowAllUp := True;
    OnClick := @DirChange;
  end;

  { Create lower Panel details }
  
  panel1 := TfpgBevel.Create(self);
  with panel1 do
  begin
    SetPosition(8, 253, 622, 25);
    Anchors := [anLeft, anRight, anBottom];
    Shape := bsBox;
    Style := bsLowered;
  end;

  lbFileInfo := TfpgLabel.Create(panel1);
  with lbFileInfo do
  begin
    SetPosition(5, 4, 609, 16);
    Anchors := [anLeft, anRight, anTop];
    Text := ' ';
    FontDesc := '#Label1';
  end;

  edFilename := TfpgEdit.Create(self);
  with edFilename do
  begin
    SetPosition(8, 301, 622, 22);
    Anchors := [anLeft, anRight, anBottom];
    Text := '';
    FontDesc := '#Edit1';
    OnChange := @edFilenameChanged;
  end;
  
  { Filter section }

  chlFilter := TfpgComboBox.Create(self);
  with chlFilter do
  begin
    SetPosition(8, 345, 622, 22);
    Anchors := [anLeft, anRight, anBottom];
    FontDesc := '#List';
    OnChange := @FilterChange;
  end;

  lb1 := TfpgLabel.Create(self);
  with lb1 do
  begin
    SetPosition(8, 283, 80, 16);
    Anchors := [anLeft, anBottom];
    Text := fpgAddColon(rsFileName);
    FontDesc := '#Label1';
  end;

  lb2 := TfpgLabel.Create(self);
  with lb2 do
  begin
    SetPosition(8, 327, 64, 16);
    Anchors := [anLeft, anBottom];
    Text := fpgAddColon(rsFileType);
    FontDesc := '#Label1';
  end;

  ActiveWidget := grid;
  FileName := '';
  Filter := rsAllFiles + ' (*)|*';
  chlFilter.FocusItem := 1;
end;

procedure TfpgFileDialog.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
var
  e: TFileEntry;
begin
  if not consumed then
  begin
    if (ActiveWidget = grid) then
    begin
      case keycode of
        keyReturn:
          begin
            e := grid.CurrentEntry;
            if (e <> nil) then
            begin
              if (e.EntryType = etDir) then
                SetCurrentDirectory(e.Name)
              else if (e.EntryType = etFile) then
                btnOKClick(btnOK);
              consumed := True;
            end;
          end;
        keyBackSpace:
          begin
             SetCurrentDirectory('..');
             consumed := True;
          end;
      end;
    end;
  end;
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgFileDialog.btnOKClick(Sender: TObject);
var
  e: TFileEntry;
begin
  e := grid.CurrentEntry;
  if e.EntryType = etDir then
  begin
    SetCurrentDirectory(e.Name);
    Exit; //==>
  end;

  if not FOpenMode or fpgFileExists(edFileName.Text) then
  begin
    ModalResult := 1;
  end;

  if ModalResult = 1 then
    // FileName := fpgExpandFileName(edFileName.Text);
    FileName := grid.FileList.DirectoryName + edFileName.Text;
end;

constructor TfpgFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := rsFileSelection;
  Width       := 640;
  Height      := 410;
  WindowPosition := wpScreenCenter;
  FSpacing    := 10;

  FFilterList := TStringList.Create;
  
  InitializeComponents;
  
  // position standard dialog buttons
  btnCancel.Left  := Width - FDefaultButtonWidth - FSpacing;
  btnCancel.Top   := Height - btnCancel.Height - FSpacing;
  btnOK.Left      := btnCancel.Left - FDefaultButtonWidth - 6;
  btnOK.Top       := btnCancel.Top;
end;

destructor TfpgFileDialog.Destroy;
begin
  FFilterList.Free;
  inherited Destroy;
end;

procedure TfpgFileDialog.DirChange(Sender: TObject);
begin
  SetCurrentDirectory(chlDir.Text);
end;

procedure TfpgFileDialog.FilterChange(Sender: TObject);
begin
  SetCurrentDirectory('.');
end;

procedure TfpgFileDialog.UpDirClick(Sender: TObject);
begin
  SetCurrentDirectory('..');
end;

procedure TfpgFileDialog.btnDirNewClicked(Sender: TObject);
var
  dlg: TfpgNewDirDialog;
begin
  dlg := TfpgNewDirDialog.Create(nil);
  try
    if dlg.ShowModal = 1 then
    begin
      if dlg.Directory <> '' then
      begin
        mkdir(dlg.Directory);
        grid.FileList.FileMask := GetFileFilter;
        grid.FileList.ShowHidden := ShowHidden;
        grid.FileList.ReadDirectory();
        grid.FileList.Sort(soFileName);
        grid.Invalidate;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfpgFileDialog.edFilenameChanged(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfpgFileDialog.UpdateButtonState;
begin
  if FOpenMode then
    // btnOK.Enabled := grid.CurrentEntry.EntryType = etFile
  else
    btnOK.Enabled := edFileName.Text <> '';
end;

procedure TfpgFileDialog.SetCurrentDirectory(const ADir: string);
var
  fsel: string;
begin
  if ADir = '..' then
    fsel := ExtractFileName(
      ExcludeTrailingPathDelimiter(grid.FileList.DirectoryName))
  else
    fsel := '';
    
  grid.FileList.FileMask := GetFileFilter;
  grid.FileList.ShowHidden := ShowHidden;

  if not grid.FileList.ReadDirectory(ADir) then
  begin
    ShowMessage(Format(rsErrCouldNotOpenDir, [ADir]), rsError);
    Exit; //==>
  end;
  
  grid.FileList.Sort(soFileName);
  
  chlDir.Items.Assign(grid.FileList.SpecialDirs);
  chlDir.FocusItem := grid.FileList.CurrentSpecialDir + 1;

  if fsel <> '' then
    SelectFile(fsel)
  else
    grid.FocusRow := 1;
    
  grid.Update;
  grid.SetFocus;
end;

function TfpgFileDialog.SelectFile(const AFilename: string): boolean;
var
  n: integer;
begin
  for n := 1 to grid.FileList.Count do
  begin
    if grid.FileList.Entry[n].Name = AFilename then
    begin
      grid.FocusRow := n;
      Result := True;
      Exit; //==>
    end;
  end;
  Result := False;
end;

procedure TfpgFileDialog.ProcessFilterString;
var
  p: integer;
  s: string;
  fs: string;
  fm: string;
begin
  s := FFilter;
  FFilterList.Clear;
  chlFilter.Items.Clear;

  repeat
    fs  := '';
    fm  := '';
    p   := pos('|', s);
    if p > 0 then
    begin
      fs := Copy(s, 1, p-1);
      Delete(s, 1, p);
      p := pos('|', s);
      if p > 0 then
      begin
        fm := Copy(s, 1, p-1);
        Delete(s, 1, p);
      end
      else
      begin
        fm  := s;
        s   := '';
      end;
    end;

    if (fs <> '') and (fm <> '') then
    begin
      chlFilter.Items.Add(fs);
      FFilterList.Add(fm);
    end;
  until (fs = '') or (fm = ''); { repeat/until }
end;

function TfpgFileDialog.GetFileFilter: string;
var
  i: integer;
begin
  i := chlFilter.FocusItem;
  if (i > 0) and (i <= FFilterList.Count) then
    Result := FFilterList[i-1]
  else
    Result := '*';
end;

function TfpgFileDialog.RunOpenFile: boolean;
var
  sdir: string;
  fname: string;
begin
  FOpenMode := True;
  sdir := ExtractFileDir(FileName);
  if sdir = '' then
    sdir := '.';
  SetCurrentDirectory(sdir);
  fname := ExtractFileName(FileName);
  if not SelectFile(fname) then
    edFilename.Text := fname;
    
  WindowTitle     := rsOpenAFile;
  btnOK.ImageName := 'stdimg.open';   // Do NOT localize
  btnOK.Text      := rsOpen;

  if ShowModal = 1 then
    Result := True
  else
    Result := False;
end;

function TfpgFileDialog.RunSaveFile: boolean;
var
  sdir: string;
  fname: string;
begin
  FOpenMode := False;
  sdir := ExtractFileDir(FileName);
  if sdir = '' then
    sdir := '.';
  SetCurrentDirectory(sdir);
  fname := ExtractFileName(FileName);
  if not SelectFile(fname) then
    edFilename.Text := fname;

  WindowTitle     := rsSaveAFile;
  btnOK.ImageName := 'stdimg.save';   // Do NOT localize
  btnOK.Text      := rsSave;

  if ShowModal = 1 then
    Result := True
  else
    Result := False;
end;


{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$undef read_interface}
{$define read_implementation}


{$I messagedialog.inc}
{$I newdirdialog.inc}
{$I promptuserdialog.inc}



end.

