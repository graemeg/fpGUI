unit tiGUIINI;

{$mode objfpc}{$H+}

{
  TODO:
    * When TfpgForm supports FormState property, implement the remaining functions
}

interface
uses
  tiINI
  ,fpg_form
  ;

type

  TtiGuiINIFile = class(TtiINIFile)
  public
    procedure   ReadFormState(AForm: TfpgForm; AHeight: integer = -1; AWidth: integer = -1);
    procedure   WriteFormState(AForm : TfpgForm);
  end;

function gGUIINI(const AFileName: string = ''): TtiGuiINIFile;


implementation
uses
  fpg_main
  ;

var
  uGuiINI : TtiGuiINIFile;


function gGUIINI(const AFileName: string = ''): TtiGuiINIFile;
begin
  if uGuiINI = nil then
  begin
    uGuiINI := TtiGuiINIFile.CreateExt(AFileName);
    uGuiINI.CacheUpdates := False;
  end;
  result := uGuiINI;
end;

procedure TtiGuiINIFile.ReadFormState(AForm: TfpgForm; AHeight : integer = -1; AWidth : integer = -1);
var
  LINISection: string;
  LTop: integer;
  LLeft: integer;
  LHeight: integer;
  LWidth: integer;
begin
  Assert(AForm <> nil, 'AForm not assigned');
  LINISection := AForm.Name + 'State';
  // Read form position, -1 if not stored in registry
  LTop := readInteger(LINISection, 'Top', -1);
  LLeft := readInteger(LINISection, 'Left', -1);
  // The form pos was found in the ini file
  if (LTop <> -1) and (LLeft <> -1) then
  begin
    AForm.Top   := readInteger(LINISection, 'Top',    AForm.Top);
    AForm.Left  := readInteger(LINISection, 'Left',   AForm.Left);
    AForm.WindowPosition := wpUser;
  end
  else
  begin  // No form pos in the ini file, so default to screen center
    if Assigned(fpgApplication.MainForm) and (fpgApplication.MainForm <> AForm) then
      AForm.WindowPosition := wpAuto
    else
      AForm.WindowPosition := wpScreenCenter;
  end;
  // Only set the form size if a bsSizable window
  if AForm.Sizeable then
  begin
    if AHeight = -1 then
      LHeight := AForm.Height
    else
      LHeight := AHeight;
    if AWidth = -1 then
      LWidth := AForm.Width
    else
      LWidth := AWidth;
    AForm.Height  := readInteger(LINISection, 'Height', LHeight);
    AForm.Width   := readInteger(LINISection, 'Width',  LWidth);
  end;

  // If the form is off screen (positioned outside all monitor screens) then
  // center the form on screen.
  if AForm.WindowPosition = wpUser then
  begin
    if (AForm.Top < 0) or (AForm.Top > fpgApplication.ScreenHeight) or
       (AForm.Left < 0) or (AForm.Left > fpgApplication.ScreenWidth) then
      AForm.WindowPosition := wpScreenCenter;
  end;
end;

procedure TtiGuiINIFile.WriteFormState(AForm: TfpgForm);
var
  LINISection: string;
begin
  LINISection := AForm.Name + 'State';
  WriteInteger(LINISection, 'Top', AForm.Top);
  WriteInteger(LINISection, 'Left', AForm.Left);
  if AForm.Sizeable then
  begin
    WriteInteger(LINISection, 'Height', AForm.Height);
    WriteInteger(LINISection, 'Width', AForm.Width);
  end;
end;

initialization
  uGuiINI := nil;

finalization
  uGuiINI.Free;

end.








