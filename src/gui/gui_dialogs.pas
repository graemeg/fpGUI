{
  General dialogs used by fpGUI based applications
}

unit gui_dialogs;

{$mode objfpc}{$H+}

{
  TODO:
    * Try and abstract the code to remove all IFDEF's
    * Combobox dropdown in FileDialog doesn't function to do ModolForm being used.
    * File Selection not working 100% in FileDialog
    * Moving through directories not working 100% yet in FileDialog.
}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gui_form,
  gui_button,
  gui_label,
  gui_listbox,
  gui_checkbox,
  gui_edit,
  gui_grid,
  gui_combobox,
  gui_bevel;

type

  { @abstract(A standard message box dialog.) It is used by the global @link(ShowMessage)
    function. }
  TfpgMessageBox = class(TfpgForm)
  private
    FLines: TStringList;
    FFont: TfpgFont;
    FTextY: integer;
    FLineHeight: integer;
    FMaxLineWidth: integer;
    FButton: TfpgButton;
    procedure   ButtonClick(Sender: TObject);
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    { This sets the message to be displayed. }
    procedure   SetMessage(AMessage: string);
  end;
  

  { @abstract(A abstract dialog which forms the basis of other dialogs.) This
    dialog implements the two basic buttons (OK, Cancel) and also some keyboard
    support like Escape to close the dialog.}
  TfpgBaseDialog = class(TfpgForm)
  protected
    FSpacing: integer;
    FDefaultButtonWidth: integer;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure   btnOKClick(Sender: TObject); virtual;
    procedure   btnCancelClick(Sender: TObject); virtual;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  { @abstract(A standard font selection dialog.) It also contains a Collection
    listbox which gets automatically populated based on the available fonts.
    There are two custom collections called Favourites and Recently Used which
    list you own selection of fonts.}
  TfpgFontSelectDialog = class(TfpgBaseDialog)
  private
    FSampleText: string;
    lblLabel1: TfpgLabel;
    lblLabel2: TfpgLabel;
    lblLabel3: TfpgLabel;
    lblLabel4: TfpgLabel;
    lblLabel5: TfpgLabel;
    lbCollection: TfpgListBox;
    lbFaces: TfpgListBox;
    lbSize: TfpgListBox;
    cbBold: TfpgCheckBox;
    cbItalic: TfpgCheckBox;
    cbUnderline: TfpgCheckBox;
    cbAntiAlias: TfpgCheckBox;
    edSample: TfpgEdit;
    procedure   OnParamChange(Sender: TObject);
    procedure   CreateFontList;
  protected
    function    GetFontDesc: string;
    procedure   SetFontDesc(Desc: string);
  public
    constructor Create(AOwner: TComponent); override;
    { This well set the sample text or font preview text to AText.}
    procedure   SetSampleText(AText: string);
  end;
  
  
  TfpgFileDialog = class(TfpgBaseDialog)
  private
    FOpenMode: boolean;
    FFilterList: TStringList;
    FFilter: string;
    procedure   SetFilter(const Value: string);
    function    GetShowHidden: boolean;
    procedure   SetShowHidden(const Value: boolean);
    procedure   ListChanged(Sender: TObject; ARow: integer);
    procedure   InitializeComponents;
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   btnOKClick(Sender: TObject); override;
    procedure   SetCurrentDirectory(const ADir: string);
  public
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
    FileName: string;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   DirChange(Sender: TObject);
    procedure   FilterChange(Sender: TObject);
    procedure   GridDblClick(Sender: TObject; x, y: integer; var btnstate, shiftstate: word);
    procedure   UpDirClick(Sender: TObject);
    function    SelectFile(const AFilename: string): boolean;
    procedure   ProcessFilterString;
    function    GetFileFilter: string;
    property    Filter: string read FFilter write SetFilter;
    function    RunOpenFile: boolean;
    function    RunSaveFile: boolean;
    property    ShowHidden: boolean read GetShowHidden write SetShowHidden;
  end;


{ A convenience function to show a message using the TfpgMessageBox class.}
procedure ShowMessage(AMessage, ATitle: string); overload;
{ A convenience function to show a message using the TfpgMessageBox class.}
procedure ShowMessage(AMessage: string); overload;

{ A convenience function to show the font selection dialog (TfpgFontSelectDialog).}
function SelectFontDialog(var FontDesc: string): boolean;


implementation

uses
  gfxbase,
  gfx_widget,
  gfx_utf8utils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ;
  
  
procedure ShowMessage(AMessage, ATitle: string);
var
  frm: TfpgMessageBox;
begin
  frm := TfpgMessageBox.Create(nil);
  try
    frm.WindowTitle := ATitle;
    frm.SetMessage(AMessage);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure ShowMessage(AMessage: string);
begin
  ShowMessage(AMessage, 'Message');
end;

function SelectFontDialog(var FontDesc: string): boolean;
var
  frm: TfpgFontSelectDialog;
begin
  Result := False;
  frm := TfpgFontSelectDialog.Create(nil);
  frm.SetFontDesc(FontDesc);
  if frm.ShowModal > 0 then
  begin
    FontDesc := frm.GetFontDesc;
    Result := True;
  end;
  frm.Free;
end;


{ TfpgMessageBox }

procedure TfpgMessageBox.ButtonClick(Sender: TObject);
begin
  ModalResult := 1;
end;

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
    Canvas.DrawString(Width div 2 - tw div 2, y, FLines[n]);
    Inc(y, FLineHeight);
  end;
  Canvas.EndDraw;
end;

constructor TfpgMessageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowAttributes := [waAutoPos];
  
  FLines        := TStringList.Create;
  FFont         := fpgGetFont('#Label1');
  FTextY        := 10;
  FLineHeight   := FFont.Height + 4;
  MinWidth      := 200;
  FMaxLineWidth := 500;
  
  FButton := TfpgButton.Create(self);
  FButton.text    := 'OK';   // We must localize this
  FButton.Width   := 75;
  FButton.OnClick := @ButtonClick;
  
end;

destructor TfpgMessageBox.Destroy;
begin
  FFont.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TfpgMessageBox.SetMessage(AMessage: string);
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
    w   := FFont.TextWidth(s2);
    if w > FMaxLineWidth then
    begin
      while w > FMaxLineWidth do
      begin
        m := UTF8Length(s);
        repeat
          Dec(m);
          s2  := UTF8Copy(s,1,m);
          w   := FFont.TextWidth(s2);
        until w <= FMaxLineWidth;
        if w > maxw then
          maxw := w;

        // are we in the middle of a word. If so find the beginning of word.
        while UTF8Copy(s2, m, m+1) <> ' ' do
        begin
          Dec(m);
          s2  := UTF8Copy(s,1,m);
        end;

        FLines.Add(s2);
        s   := UTF8Copy(s, m+1, UTF8length(s));
        s2  := s;
        w   := FFont.TextWidth(s2);
      end; { while }
      if all then
      begin
        FLines.Add(s2);
        s := '';
      end;
    end
    else
    begin
      FLines.Add(s2);
      s := '';
    end; { if/else }

    if w > maxw then
      maxw := w;
  end;

begin
  s := '';
  FLines.Clear;
  n := 1;
  maxw := 0;
  while n <= length(AMessage) do
  begin
    c := AMessage[n];
    if (c = #13) or (c = #10) then
    begin
      AddLine(false);
      if (c = #13) and (n < length(AMessage)) and (AMessage[n+1] = #10) then
        Inc(n);
    end
    else
      s := s + c;
    Inc(n);
  end; { while }

  AddLine(true);

  // dialog width with 10 pixel border on both sides
  Width := maxw + 2*10;

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
  ModalResult := 0;
  Close;
end;

procedure TfpgBaseDialog.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEscape then   // Esc cancels the dialog
    Close
  else
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

constructor TfpgBaseDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$Note We need to localize this dialog }
  Width     := 500;
  Height    := 400;
  WindowPosition := wpScreenCenter;
  FSpacing  := 6;
  FDefaultButtonWidth := 80;

  btnCancel := CreateButton(self, Width-FDefaultButtonWidth-FSpacing, 370, FDefaultButtonWidth, 'Cancel', @btnCancelClick);
  btnCancel.ImageName := 'stdimg.Cancel';
  btnCancel.ShowImage := True;
  btnCancel.Anchors   := [anRight, anBottom];

  btnOK := CreateButton(self, btnCancel.Left-FDefaultButtonWidth-FSpacing, 370, FDefaultButtonWidth, 'OK', @btnOKClick);
  btnOK.ImageName := 'stdimg.OK';
  btnOK.ShowImage := True;
  btnOK.Anchors   := [anRight, anBottom];
end;


{ TfpgFontSelectDialog }

procedure TfpgFontSelectDialog.OnParamChange(Sender: TObject);
var
  fdesc: string;
begin
  fdesc := GetFontDesc;
  {$IFDEF DEBUG} Writeln(fdesc); {$ENDIF}
  edSample.FontDesc := fdesc;
end;

procedure TfpgFontSelectDialog.CreateFontList;
var
  fl: TStringList;
  i: integer;
begin
  lbFaces.Items.Clear;
  fl := fpgApplication.GetFontFaceList;
  for i := 0 to fl.Count-1 do
    lbFaces.Items.Add(fl.Strings[i]);
  fl.Free;
end;

function TfpgFontSelectDialog.GetFontDesc: string;
var
  s: string;
begin
  s := lbFaces.Text + '-' + lbSize.Text;
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

  function NextC : char;
  begin
    inc(cp);
    if cp > length(desc) then
      c := #0
    else
      c := desc[cp];
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

begin
  cp := 1;
  c  := desc[1];

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

constructor TfpgFontSelectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$Note We need to localize this dialog }
  WindowTitle := 'Select Font...';
  Width       := 600;
  MinWidth    := Width;
  MinHeight   := Height;
  FSampleText := 'The quick brown fox jumps over the lazy dog. 0123456789 [oO0,ilLI]';

  btnCancel.Left := Width - FDefaultButtonWidth - FSpacing;
  btnOK.Left     := btnCancel.Left - FDefaultButtonWidth - FSpacing;

  lblLabel5 := TfpgLabel.Create(self);
  with lblLabel5 do
  begin
    SetPosition(8, 8, 73, 16);
    Text := 'Collection:';
  end;

  {$Note This need to be implemented at some stage. }
  lbCollection := TfpgListBox.Create(self);
  with lbCollection do
  begin
    SetPosition(8, 28, 145, 236);
    Items.Add('All Fonts');
    // These should be stored in <users config path>/fpgui directory
    Items.Add('Recently Used');
    Items.Add('Favourites');
    // From here onwards, these should be created automatically.
    Items.Add('Fixed Width');
    Items.Add('Sans');
    Items.Add('Serif');
//    OnChange := @OnParamChange;
    FocusItem := 1;
    Enabled := False;
  end;

  lblLabel1 := TfpgLabel.Create(self);
  with lblLabel1 do
  begin
    SetPosition(161, 8, 73, 16);
    Text := 'Font:';
  end;

  lbFaces := TfpgListBox.Create(self);
  with lbFaces do
  begin
    SetPosition(161, 28, 232, 236);
    Items.Add(' ');
    OnChange := @OnParamChange;
  end;

  lblLabel3 := TfpgLabel.Create(self);
  with lblLabel3 do
  begin
    SetPosition(401, 8, 54, 16);
    Text := 'Size:';
  end;

  lbSize := TfpgListBox.Create(self);
  with lbSize do
  begin
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
    FocusItem := 5;
  end;

  lblLabel2 := TfpgLabel.Create(self);
  with lblLabel2 do
  begin
    SetPosition(461, 8, 54, 16);
    Text := 'Typeface:';
  end;

  cbBold := TfpgCheckBox.Create(self);
  with cbBold do
  begin
    SetPosition(461, 32, 87, 20);
    Text := 'Bold';
    OnChange := @OnParamChange;
  end;

  cbItalic := TfpgCheckBox.Create(self);
  with cbItalic do
  begin
    SetPosition(461, 56, 87, 20);
    Text := 'Italic';
    OnChange := @OnParamChange;
  end;

  cbUnderline := TfpgCheckBox.Create(self);
  with cbUnderline do
  begin
    SetPosition(461, 80, 87, 20);
    Text := 'Underline';
    OnChange := @OnParamChange;
  end;

  cbAntiAlias := TfpgCheckBox.Create(self);
  with cbAntiAlias do
  begin
    SetPosition(461, 124, 99, 20);
    Text := 'Anti aliasing';
    OnChange := @OnParamChange;
    Checked := True;
  end;

  lblLabel4 := TfpgLabel.Create(self);
  with lblLabel4 do
  begin
    SetPosition(8, 268, 55, 16);
    Text := 'Sample:';
  end;

  edSample := TfpgEdit.Create(self);
  with edSample do
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
  edSample.Text := FSampleText;
end;

{ TfpgFileDialog }

procedure TfpgFileDialog.ListChanged(Sender: TObject; ARow: integer);
var
  s : string;
begin
  if grid.CurrentEntry = nil then
    Exit; //==>
  s := grid.CurrentEntry.Name;

  if grid.CurrentEntry.IsLink then
    s := s + ' -> ' + grid.CurrentEntry.LinkTarget;

  if grid.CurrentEntry.EntryType <> etDir then
    edFileName.Text := grid.CurrentEntry.Name;
    
  btnOK.Enabled := grid.CurrentEntry.EntryType = etFile;

  lbFileInfo.Text := s;
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
//    OnDoubleClick := @GridDblClick;
  end;

  btnUpDir := TfpgButton.Create(self);
  with btnUpDir do
  begin
    SetPosition(540, 11, 26, 24);
    Anchors := [anRight, anTop];
    Text := '';
    FontDesc := '#Label1';
    ImageName := 'stdimg.folderup';
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
    ImageName := 'stdimg.foldernew';
    ModalResult := 0;
    Focusable := False;
  end;

  btnShowHidden := TfpgButton.Create(self);
  with btnShowHidden do
  begin
    SetPosition(604, 11, 26, 24);
    Anchors := [anRight, anTop];
    Text := '';
    FontDesc := '#Label1';
    ImageName := 'stdimg.hidden';
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
    Text := 'Filename:';
    FontDesc := '#Label1';
  end;

  lb2 := TfpgLabel.Create(self);
  with lb2 do
  begin
    SetPosition(8, 327, 64, 16);
    Anchors := [anLeft, anBottom];
    Text := 'File type:';
    FontDesc := '#Label1';
  end;

  ActiveWidget := grid;
  FileName := '';
  Filter := 'All Files (*)|*';
end;

procedure TfpgFileDialog.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
var
  e: TFileEntry;
begin
  if not consumed then
  begin
    if (keycode = keyReturn) and (ActiveWidget = grid) then
    begin
      e := grid.CurrentEntry;
      if (e <> nil) and (e.EntryType = etDir) then
      begin
        SetCurrentDirectory(e.Name);
        consumed := True;
      end;
    end;
  end;
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgFileDialog.btnOKClick(Sender: TObject);
begin
  if not FOpenMode or SysUtils.FileExists(edFileName.Text) then
  begin
    ModalResult := 1;
  end;

  if ModalResult > 0 then
    FileName := ExpandFileName(edFileName.Text);
end;

constructor TfpgFileDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'File Selection';
  Width := 640;
  Height := 410; // 460;
  WindowPosition := wpScreenCenter;
  FSpacing := 10;

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

procedure TfpgFileDialog.GridDblClick(Sender: TObject; x, y: integer;
  var btnstate, shiftstate: word);
begin

end;

procedure TfpgFileDialog.UpDirClick(Sender: TObject);
begin
  SetCurrentDirectory('..');
end;

procedure TfpgFileDialog.SetCurrentDirectory(const ADir: string);
var
  ds: string;
  n: integer;
  rootadd: integer;
  fsel: string;
{$ifdef Win32}
  drvind: integer;
  drvs: string;
{$endif}
begin
  GetDir(0, ds);
  fsel := ExtractFileName(ds);

  if not SetCurrentDir(ADir) then
  begin
    ShowMessage('Could not open the directory ' + ADir, 'Error');
    Exit; //==>
  end;

  chlDir.Items.Clear;
  if ADir <> '..' then
    fsel := '';

  rootadd := 1;

  {$IFDEF MSWINDOWS}
  // making drive list 1
  drvind := -1;
  if Copy(ds, 2, 1) = ':' then
    drvind := ord(UpCase(ds[1]))-ord('A');
  n := 0;
  while n < drvind do
  begin
    drvs := chr(n+ord('A'))+':\';
    if Windows.GetDriveType(PChar(drvs)) <> 1 then
    begin
      chlDir.Items.Add(drvs);
    end;
    inc(n);
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  if Copy(ds, 1, 1) <> DirectorySeparator then
    ds := DirectorySeparator + ds;
  {$ENDIF}

  n := 1;
  while n < Length(ds) do
  begin
    if ds[n] = DirectorySeparator then
    begin
      chlDir.Items.Add(Copy(ds, 1, n-1+rootadd));
      rootadd := 0;
    end;
    inc(n);
  end;

  chlDir.Items.Add(ds);
  chlDir.FocusItem := chlDir.Items.Count;

  {$IFDEF MSWINDOWS}
  // making drive list 2
  n := drvind+1;
  if n < 0 then n := 0;
  while n <= 25 do
  begin
    drvs := chr(n+ord('A'))+':\';
    if Windows.GetDriveType(PChar(drvs)) <> 1 then
    begin
      chlDir.Items.Add(drvs);
    end;
    inc(n);
  end;
  {$ENDIF}

  grid.FileList.ReadDirectory(GetFileFilter, ShowHidden);
  grid.FileList.Sort(soFileName);
  grid.Update;

  if fsel <> '' then
    SelectFile(fsel)
  else
    grid.FocusRow := 1;
end;

function TfpgFileDialog.SelectFile(const AFilename: string): boolean;
var
  n : integer;
begin
  for n:=1 to grid.FileList.Count do
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
    
  WindowTitle     := 'Open File...';
  btnOK.ImageName := 'stdimg.open';
  btnOK.Text      := 'Open';

  if ShowModal > 0 then
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

  WindowTitle     := 'Save File...';
  btnOK.ImageName := 'stdimg.save';
  btnOK.Text      := 'Save';

  if ShowModal > 0 then
    Result := True
  else
    Result := False;
end;

end.

