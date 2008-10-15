unit tiDialogs;

{$mode objfpc}{$H+}

{
  TODO:
    * Port tiProcessing and tiEndProcessing
}

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


implementation

uses
  fpg_main,
  fpg_form,
  fpg_memo,
  fpg_dialogs,
  tiGUIINI,
  tiUtils;


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

end.

