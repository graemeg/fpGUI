unit tiDialogs;

{$mode objfpc}{$H+}

{ TODO: Port tiProcessing and tiEndProcessing }

interface

uses
  Classes,
  SysUtils,
  Variants;

  
  // Call showMessage, but accepts a variant. Good for debugging.
  procedure tiShowMessage(const AArray: Array of Const); overload;
  procedure tiShowMessage(const AValue: variant); overload;

  
  // Show the contents of a TStringList - for debugging
  procedure tiShowStringList(const pStringList: TStringList; const pHeading: string = 'Show string list');
  // Show the contents of a TStrings - for debugging
  procedure tiShowStrings(const AStrings: TStrings; const pHeading: string = 'Show strings');
  // Show a long string - for debugging
  procedure tiShowString(const AStr: string; const pHeading: string = 'Show string');
  // Show a variant array of variants - for debugging
  procedure tiShowVariant(AValue: Variant; pHeading: string = 'Show variant');
  // Show the contents of a stream
  procedure tiShowStream(const AValue: TStream; const pHeading: string = 'Show stream');
  // Show a <Yes>, <No> dialog box, and return true if <Yes> was selected
  function tiAppConfirmation(const AMessage: string; ATitle: string = ''): boolean; overload;
  function tiAppConfirmation(const AMessage: string; const AValues: array of const): boolean; overload;
  // Show a message
  procedure tiAppMessage(const AMessage: string; ATitle: string = '');
  // Show a warning
  procedure tiAppWarning(const AMessage: string; ATitle: string = '');
  // Show a error message
  procedure tiAppError(const AMessage: string; ATitle: string = '');

  // A type of notification window that will disappear by it self
  procedure tiProcessing(const AMessage: TfpgString);
  procedure tiEndProcessing;

implementation

uses
  fpg_main,
  fpg_form,
  fpg_memo,
  fpg_label,
  fpg_dialogs,
  fpg_panel,
  tiGUIINI,
  tiUtils;

var
  pWorkingForm: TfpgForm;

type
  TProcessingForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ProcessingForm}
    Bevel1: TfpgBevel;
    lblMessage: TfpgLabel;
    {@VFD_HEAD_END: ProcessingForm}
  public
    procedure AfterCreate; override;
  end;

{ TProcessingForm }

procedure TProcessingForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ProcessingForm}
  Name := 'ProcessingForm';
  SetPosition(317, 177, 400, 150);
  WindowTitle := 'Processing...';
  WindowPosition := wpScreenCenter;
  BackgroundColor := clHilite1;
  WindowType := wtPopup;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(8, 4, 232, 80);
    Align := alClient;
  end;

  lblMessage := TfpgLabel.Create(Bevel1);
  with lblMessage do
  begin
    Name := 'lblMessage';
    SetPosition(32, 28, 108, 32);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlCenter;
    Text := '...';
    WrapText := True;
    Align := alClient;
    MouseCursor := mcHourGlass;
  end;

  {@VFD_BODY_END: ProcessingForm}
  {%endregion}
end;



procedure tiShowMessage(const AArray: array of const);
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  i: Integer;
  lsLine: string;
begin
  lsLine := '';
  for I := 0 to High(AArray) do begin
    if lsLine <> '' then
      lsLine := lsLine + Cr;
    with AArray[i] do
      case VType of
        vtInteger:    lsLine := lsLine + IntToStr(VInteger);
        vtBoolean:    lsLine := lsLine + BoolChars[VBoolean];
        vtChar:       lsLine := lsLine + VChar;
        vtExtended:   lsLine := lsLine + FloatToStr(VExtended^);
        vtString:     lsLine := lsLine + VString^;
        vtPChar:      lsLine := lsLine + VPChar;
        vtObject:     lsLine := lsLine + VObject.ClassName;
        vtClass:      lsLine := lsLine + VClass.ClassName;
        vtAnsiString: lsLine := lsLine + string(VAnsiString);
        vtCurrency:   lsLine := lsLine + CurrToStr(VCurrency^);
        vtVariant:    lsLine := lsLine + string(VVariant^);
        vtInt64:      lsLine := lsLine + IntToStr(VInt64^);
    end;
  end;
  tiShowMessage(lsLine);
end;

procedure tiShowMessage(const AValue: variant);
begin
  ShowMessage(VarToStr(AValue));
end;

procedure tiShowStringList(const pStringList: TStringList; const pHeading: string);
begin
  tiShowStrings(pStringList, pHeading);
end;

procedure tiShowStrings(const AStrings: TStrings; const pHeading: string);
var
  lForm: TfpgForm;
  lMemo: TfpgMemo;
begin
  lForm := TfpgForm.Create(nil);
  lMemo := TfpgMemo.Create(lForm);
  try
    lForm.WindowTitle := pHeading;
    lForm.Width       := 300;
    lForm.Height      := 300;
    lForm.WindowPosition := wpScreenCenter;
    lForm.Name        := 'FormShowStrings';
    lMemo.Lines.Assign(AStrings);
    lMemo.FontDesc    := 'Courier New-10';
    gGUIINI.ReadFormState(lForm);
    lMemo.SetPosition(0, 0, lForm.Width, lForm.Height);
    lMemo.Align       := alClient;
    lForm.ShowModal;
    gGUIINI.WriteFormState(lForm);
  finally
    lForm.free;
  end;
end;

procedure tiShowString(const AStr: string; const pHeading: string);
var
  lSL: TStringList;
begin
  lSL := TStringList.Create;
  try
    lSL.Text := AStr;
    tiShowStringList(lSL, pHeading);
  finally
    lSL.Free;
  end;
end;

procedure tiShowVariant(AValue: Variant; pHeading: string);
var
  ls: string;
begin
  ls := tiVariantArrayToString(AValue);
  tiShowString(ls, pHeading);
end;

procedure tiShowStream(const AValue: TStream; const pHeading: string);
var
  lStringStream: TStringStream;
begin
  lStringStream := TStringStream.Create('');
  try
    AValue.Position := 0;
    lStringStream.CopyFrom(AValue, AValue.Size);
    tiShowString(lStringStream.DataString, pHeading);
  finally
    lStringStream.Free;
  end;
end;

function tiAppConfirmation(const AMessage: string; ATitle: string = ''): boolean;
begin
  Result := TfpgMessageDialog.Question(ATitle, AMessage) = mbYes
end;

function tiAppConfirmation(const AMessage: string;
  const AValues: array of const): boolean;
begin
  Result := tiAppConfirmation(Format(AMessage, AValues));
end;

procedure tiAppMessage(const AMessage: string; ATitle: string = '');
begin
  TfpgMessageDialog.Information(ATitle, AMessage);
end;

procedure tiAppWarning(const AMessage: string; ATitle: string = '');
begin
  TfpgMessageDialog.Warning(ATitle, AMessage);
end;

procedure tiAppError(const AMessage: string; ATitle: string = '');
begin
  TfpgMessageDialog.Critical(ATitle, AMessage);
end;

procedure tiProcessing(const AMessage: TfpgString);
begin
  if not Assigned(pWorkingForm) then
  begin
    pWorkingForm := TProcessingForm.Create(nil);
    TProcessingForm(pWorkingForm).lblMessage.Text := AMessage;
    pWorkingForm.Show;
  end
  else
    TProcessingForm(pWorkingForm).lblMessage.Text := AMessage;
  fpgApplication.ProcessMessages;
end;

procedure tiEndProcessing;
begin
  if Assigned(pWorkingForm) then
    pWorkingForm.Close;
  FreeAndNil(pWorkingForm);
end;

end.

