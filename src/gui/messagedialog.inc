{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit contains the Message Dialog used throughout fpGUI Toolkit.
}

{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}

type

  TfpgMessageDialog = class(TfpgForm)
  private
    FInformativeText: TStringList;
    FText: string;
    FButtons: TfpgMsgDlgButtons;
    FDefaultButton: TfpgMsgDlgBtn;
    FDialogType: TfpgMsgDlgType;
    FButtonList: TList; // pointers to buttons
    FMaxLineWidth: Integer;
    FFont: TfpgFont;
    FTextY: Integer;
    FLineHeight: Integer;
    function    GetInformativeText: string;
    procedure   SetButtons(const AValue: TfpgMsgDlgButtons);
    procedure   SetDefaultButton(const AValue: TfpgMsgDlgBtn);
    procedure   SetInformativeText(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   pnlIconPaint(Sender: TObject);
    procedure   PrepareIcon;
    procedure   PrepareText;
    procedure   PrepareButtons;
  protected
    procedure   SetWindowTitle(const ATitle: string); override;
    procedure   HandleClose; override;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   PrepareLayout;
  public
    {@VFD_HEAD_BEGIN: fpgMessageDialog}
    lblName1: TfpgLabel;
    pnlIcon: TfpgBevel;
    {@VFD_HEAD_END: fpgMessageDialog}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    class procedure About(const ATitle: string; const AText: string);
    class procedure AboutFPGui(const ATitle: string = '');
    { ACloseButton is when the user cancels the dialog via the Esc key or the X window button }
    class function Critical(const ATitle: string; const AText: string; AButtons: TfpgMsgDlgButtons = [mbOK]; ADefaultButton: TfpgMsgDlgBtn = mbNoButton; ACloseButton: TfpgMsgDlgBtn = mbCancel): TfpgMsgDlgBtn;
    { ACloseButton is when the user cancels the dialog via the Esc key or the X window button }
    class function Information(const ATitle: string; const AText: string; AButtons: TfpgMsgDlgButtons = [mbOK]; ADefaultButton: TfpgMsgDlgBtn = mbNoButton; ACloseButton: TfpgMsgDlgBtn = mbCancel): TfpgMsgDlgBtn;
    { ACloseButton is when the user cancels the dialog via the Esc key or the X window button }
    class function Question(const ATitle: string; const AText: string; AButtons: TfpgMsgDlgButtons = [mbYes, mbNo]; ADefaultButton: TfpgMsgDlgBtn = mbNo; ACloseButton: TfpgMsgDlgBtn = mbCancel): TfpgMsgDlgBtn;
    { ACloseButton is when the user cancels the dialog via the Esc key or the X window button }
    class function Warning(const ATitle: string; const AText: string; AButtons: TfpgMsgDlgButtons = [mbOK]; ADefaultButton: TfpgMsgDlgBtn = mbNoButton; ACloseButton: TfpgMsgDlgBtn = mbCancel): TfpgMsgDlgBtn;
    property    InformativeText: string read GetInformativeText write SetInformativeText;
    property    Text: string read FText write SetText;
    property    Buttons: TfpgMsgDlgButtons read FButtons write SetButtons;
    property    DefaultButton: TfpgMsgDlgBtn read FDefaultButton write SetDefaultButton;
    property    DialogType: TfpgMsgDlgType read FDialogType;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}

{ TfpgMessageDialog }

procedure TfpgMessageDialog.SetInformativeText(const AValue: string);
var
  outw: integer;
begin
  if not Assigned(FInformativeText) then
    FInformativeText := TStringList.Create;

  WrapText(AValue, FInformativeText, FFont, FMaxLineWidth, outw);
  // adjust dialog's height
  Height := lblName1.Bottom + (FLineHeight*FInformativeText.Count) + FTextY + 35;
end;

procedure TfpgMessageDialog.SetButtons(const AValue: TfpgMsgDlgButtons);
begin
  if FButtons = AValue then
    Exit; //==>
  FButtons := AValue;
end;

function TfpgMessageDialog.GetInformativeText: string;
begin
  if Assigned(FInformativeText) then
    Result := FInformativeText.Text
  else
    Result := '';
end;

procedure TfpgMessageDialog.SetDefaultButton(const AValue: TfpgMsgDlgBtn);
begin
  if FDefaultButton = AValue then
    Exit; //==>
  FDefaultButton := AValue;
  // Remember to check if button exists in FButtons
end;

procedure TfpgMessageDialog.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  lblName1.Text := FText;
end;

procedure TfpgMessageDialog.pnlIconPaint(Sender: TObject);
var
  wg: TfpgBevel;
begin
  wg := TfpgBevel(Sender);
  wg.Visible := False;
end;

procedure TfpgMessageDialog.PrepareIcon;
begin
  // do nothing yet
end;

procedure TfpgMessageDialog.PrepareText;
begin
  { todo: Implement this correctly }
//  Height := 150;
end;

procedure TfpgMessageDialog.PrepareButtons;
const
  cSpacing = 4;
var
  i: integer;
  b: TfpgButton;
  lcount: integer;
  lwidth: integer;
  ltop: integer;
  lleft: integer;
  sl: TStringList;
  lDefault: integer;

  function GetButtonCount: integer;
  var
    i: TfpgMsgDlgBtn;
  begin
    Result := 0;

    // try known sets first
    if Buttons = mbYesNoCancel then
    begin
      { TODO : At some stage the StyleManager can give us the correct button
        order based on the OS and Window Manager. }
      Result := 3;
      sl.Add(cMsgDlgBtnText[mbYes] + '=' + IntToStr(Integer(mrYes)));
      sl.Add(cMsgDlgBtnText[mbNo] + '=' + IntToStr(Integer(mrNo)));
      sl.Add(cMsgDlgBtnText[mbCancel] + '=' + IntToStr(Integer(mrCancel)));
      case DefaultButton of
        mbYes:    lDefault := 0;
        mbNo:     lDefault := 1;
        mbCancel: lDefault := 2;
      end;
      Exit; //==>
    end;

    // if we got here, try all known buttons.
    for i := Low(TfpgMsgDlgBtn) to High(TfpgMsgDlgBtn) do
    begin
      if i in Buttons then
      begin
        inc(Result);
        { TODO : How could we improve this to stay in sync with TfpgModalResult values
          like mrOK, mrCancel etc... }
        sl.Add(cMsgDlgBtnText[i] + '=' + IntToStr(Ord(i)));
        if i = DefaultButton then
          lDefault := Result-1;
      end;
    end;
  end;

begin
  sl      := TStringList.Create;  // holds button captions
  lcount  := GetButtonCount;
  lwidth  := 0;

  // create buttons
  for i := 0 to lcount-1 do
  begin
    b       := TfpgButton.Create(self);
    b.Name  := 'DlgButton' + IntToStr(i+1);
    b.Text  := sl.Names[i];
    b.ModalResult   := TfpgModalResult(StrToInt(sl.ValueFromIndex[i]));
    if (i = lDefault) or (lcount = 1) then
      b.Default := True;
    FButtonList.Add(b);
    lwidth  := lwidth + b.Width
  end;
  lwidth := lwidth + (cSpacing * (lcount-1));

  // position buttons
  if Assigned(FInformativeText) then
    ltop := lblName1.Bottom + 50 + (FLineHeight*FInformativeText.Count) + 15
  else
    ltop := lblName1.Bottom + 15;

//  ltop := Height - TfpgButton(FButtonList[0]).Height - cSpacing;
  lleft := (Width - lwidth) div 2;
  for i := 0 to lcount-1 do
  begin
    b := TfpgButton(FButtonList[i]);
    b.SetPosition(lleft, ltop, b.Width, b.Height);
    lleft := lleft + b.Width + cSpacing;
  end;

  sl.Free;
end;

procedure TfpgMessageDialog.SetWindowTitle(const ATitle: string);
begin
  if ATitle = '' then
  begin
    case DialogType of
      mtAbout:        SetWindowTitle(Format(rsAbout, [fpGUIName]));
      mtError:        SetWindowTitle(rsError);
      mtWarning:      SetWindowTitle(rsWarning);
      mtConfirmation: SetWindowTitle(rsConfirmation);
    end;
  end
  else
    inherited SetWindowTitle(ATitle);
end;

procedure TfpgMessageDialog.HandleClose;
begin
  if ModalResult = mrNone then      // Form was close via the X (window frame) button
    ModalResult := mrCancel;
  inherited HandleClose;
end;

procedure TfpgMessageDialog.HandlePaint;
var
  logo: TfpgImage;
  i: integer;
  y: integer;
  tw: integer;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  case FDialogType of
    mtAbout:
        begin
          Canvas.Clear(BackgroundColor);
          logo := CreateImage_BMP(@stdimg_fpgui_logo, SizeOf(stdimg_fpgui_logo));
          Canvas.SetColor(clBlack);
          Canvas.DrawRectangle(12, 12, logo.Width+2, logo.Height+2);
          Canvas.DrawImage(13, 13, logo);
          logo.free;
        end;

    mtError:
        begin
          Canvas.DrawImage(12, 12, fpgImages.GetImage('stdimg.dlg.critical'));    // Do NOT localize
        end;

    mtConfirmation:
        begin
          Canvas.DrawImage(12, 12, fpgImages.GetImage('stdimg.dlg.help'));        // Do NOT localize
        end;

    mtWarning:
        begin
          Canvas.DrawImage(12, 12, fpgImages.GetImage('stdimg.dlg.warning'));     // Do NOT localize
        end;

    else
        begin // default to Information
          Canvas.DrawImage(12, 12, fpgImages.GetImage('stdimg.dlg.info'));        // Do NOT localize
        end;
  end;

  // paint informative text
  if Assigned(FInformativeText) then
  begin
    Canvas.SetFont(FFont);
    y := FTextY;
    for i := 0 to FInformativeText.Count-1 do
    begin
      // centre text
//      tw := FFont.TextWidth(FInformativeText[i]);
//      Canvas.DrawString(Width div 2 - tw div 2, y, FInformativeText[i]);
      // left align text
      if FInformativeText[i] <> '' then
        Canvas.DrawString(70, y, FInformativeText[i]);
      Inc(y, FLineHeight);
    end;
  end;
  Canvas.EndDraw;
end;

procedure TfpgMessageDialog.HandleShow;
var
  i: integer;
begin
  inherited HandleShow;

  for i := 0 to ComponentCount-1 do
    if Components[i] is TfpgButton then
      if TfpgButton(Components[i]).Default then
        TfpgButton(Components[i]).SetFocus;
end;

procedure TfpgMessageDialog.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  case CheckClipBoardKey(keycode, shiftstate) of
    ckCopy:
        begin
          fpgClipboard.Text := Text + LineEnding + LineEnding + InformativeText;
          Consumed := True;
        end;
  else
    if keycode = keyEscape then   // Esc cancels the dialog
      ModalResult := mrCancel
    else
      inherited HandleKeyPress(keycode, shiftstate, consumed);
  end;
end;

procedure TfpgMessageDialog.PrepareLayout;
begin
  PrepareIcon;
  PrepareText;
  PrepareButtons;
end;

constructor TfpgMessageDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FButtonList := TList.Create;
  FMaxLineWidth := 400;
  FFont         := fpgGetFont('#Label1');
  FTextY        := 60;
  FLineHeight   := FFont.Height + 4;
end;

destructor TfpgMessageDialog.Destroy;
begin
  FFont.Free;
  FButtonList.Clear;
  FButtonList.Free;
  FInformativeText.Free;
  inherited Destroy;
end;

class procedure TfpgMessageDialog.About(const ATitle: string; const AText: string);
begin
  writeln('** Implement TfpgMessageDialog.About');
end;

class procedure TfpgMessageDialog.AboutFPGui(const ATitle: string);
var
  dlg: TfpgMessageDialog;
begin
  dlg := TfpgMessageDialog.Create(nil);
  try
    dlg.FDialogType := mtAbout;
    if ATitle = '' then
      dlg.WindowTitle := Format(rsAbout, [fpGUIName])
    else
      dlg.WindowTitle := ATitle;
    dlg.Buttons := [mbOK];
    dlg.DefaultButton := mbOK;
    dlg.Text := dlg.WindowTitle;
    dlg.InformativeText := LineEnding + LineEnding
                           + 'This program uses ' + fpGUIName + ' version ' + fpGUI_Version + '.'
                           + LineEnding + LineEnding
                           + fpGUIName + ' is intended for Open Source and Commercial applications. fpGUI '
                           + 'uses the LGPL 2 license with a static linking exception - the same as the Free '
                           + 'Pascal Compiler''s RTL.'
                           + LineEnding + LineEnding
                           + 'fpGUI is an Object Pascal toolkit for cross-platform application development. '
                           + 'It provides single-source portability across Linux, MS Windows, FreeBSD, '
                           + 'OpenSolaris and embedded operating systems like Embedded Linux and Windows CE.'
                           + LineEnding + LineEnding
                           + 'For more information, see the ' + fpGUIName + ' website at: '
                           + fpGUIWebsite;
    dlg.PrepareLayout;
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;

class function TfpgMessageDialog.Critical(const ATitle: string;
  const AText: string; AButtons: TfpgMsgDlgButtons;
  ADefaultButton: TfpgMsgDlgBtn; ACloseButton: TfpgMsgDlgBtn): TfpgMsgDlgBtn;
var
  dlg: TfpgMessageDialog;
  mr: TfpgModalResult;
begin
  dlg := TfpgMessageDialog.Create(nil);
  try
    dlg.FDialogType     := mtError;
    dlg.FButtons        := AButtons;
    dlg.Text            := ATitle;
    dlg.InformativeText := AText;
    dlg.WindowTitle     := ATitle;
    dlg.FDefaultButton  := ADefaultButton;
    dlg.PrepareLayout;
    mr := dlg.ShowModal;
    // if there is a Cancel button, ignore ACloseButton.
    if (mr = mrCancel) and (not (mbCancel in AButtons)) then
      Result := ACloseButton
    else
      Result := TfpgMsgDlgBtn(mr);
  finally
    dlg.Free;
  end;
end;

class function TfpgMessageDialog.Information(const ATitle: string;
  const AText: string; AButtons: TfpgMsgDlgButtons;
  ADefaultButton: TfpgMsgDlgBtn; ACloseButton: TfpgMsgDlgBtn): TfpgMsgDlgBtn;
var
  dlg: TfpgMessageDialog;
  mr: TfpgModalResult;
begin
  dlg := TfpgMessageDialog.Create(nil);
  try
    dlg.FDialogType     := mtInformation;
    dlg.FButtons        := AButtons;
    dlg.Text            := ATitle;
    dlg.InformativeText := AText;
    dlg.WindowTitle     := ATitle;
    dlg.FDefaultButton  := ADefaultButton;
    dlg.PrepareLayout;
    mr := dlg.ShowModal;
    // if there is a Cancel button, ignore ACloseButton.
    if (mr = mrCancel) and (not (mbCancel in AButtons)) then
      Result := ACloseButton
    else
      Result := TfpgMsgDlgBtn(mr);
  finally
    dlg.Free;
  end;
end;

class function TfpgMessageDialog.Question(const ATitle: string;
  const AText: string; AButtons: TfpgMsgDlgButtons;
  ADefaultButton: TfpgMsgDlgBtn; ACloseButton: TfpgMsgDlgBtn): TfpgMsgDlgBtn;
var
  dlg: TfpgMessageDialog;
  mr: TfpgModalResult;
begin
  dlg := TfpgMessageDialog.Create(nil);
  try
    dlg.FDialogType     := mtConfirmation;
    dlg.FButtons        := AButtons;
    dlg.Text            := ATitle;
    dlg.InformativeText := AText;
    dlg.WindowTitle     := ATitle;
    dlg.FDefaultButton  := ADefaultButton;
    dlg.PrepareLayout;
    mr := dlg.ShowModal;
    // if there is a Cancel button, ignore ACloseButton.
    if (mr = mrCancel) and (not (mbCancel in AButtons)) then
      Result := ACloseButton
    else
      Result := TfpgMsgDlgBtn(mr);
  finally
    dlg.Free;
  end;
end;

class function TfpgMessageDialog.Warning(const ATitle: string;
  const AText: string; AButtons: TfpgMsgDlgButtons;
  ADefaultButton: TfpgMsgDlgBtn; ACloseButton: TfpgMsgDlgBtn): TfpgMsgDlgBtn;
var
  dlg: TfpgMessageDialog;
  mr: TfpgModalResult;
begin
  dlg := TfpgMessageDialog.Create(nil);
  try
    dlg.FDialogType     := mtWarning;
    dlg.FButtons        := AButtons;
    dlg.Text            := ATitle;
    dlg.InformativeText := AText;
    dlg.WindowTitle     := ATitle;
    dlg.FDefaultButton  := ADefaultButton;
    dlg.PrepareLayout;
    mr := dlg.ShowModal;
    // if there is a Cancel button, ignore ACloseButton.
    if (mr = mrCancel) and (not (mbCancel in AButtons)) then
      Result := ACloseButton
    else
      Result := TfpgMsgDlgBtn(mr);
  finally
    dlg.Free;
  end;
end;

procedure TfpgMessageDialog.AfterCreate;
begin
  {@VFD_BODY_BEGIN: fpgMessageDialog}
  Name := 'fpgMessageDialog';
  WindowTitle := rsMessage;
  Sizeable  := False;
  Width     := 500;
  Height    := 400;
  MinWidth  := 300;
  MinHeight := 160;
  WindowPosition := wpOneThirdDown;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(116, 20, 312, Font.Height);
    Text := '';
    FontDesc := '#Label2';
  end;

  pnlIcon := TfpgBevel.Create(self);
  with pnlIcon do
  begin
    Name := 'pnlIcon';
    SetPosition(12, 12, 80, 80);
    Shape := bsSpacer;
    OnPaint := @pnlIconPaint;
  end;

  {@VFD_BODY_END: fpgMessageDialog}
end;

{$ENDIF read_implementation}


