unit tiGUIINI;

{$mode objfpc}{$H+}

{
  TODO:
    * When TfpgForm supports FormState property, implement the remaining functions
}

interface
uses
  tiINI
  ,gui_form
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
  fpgfx
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
  LTop := readInteger(LINISection, 'Top',    -1);
  LLeft := readInteger(LINISection, 'Left',   -1);
  // The form pos was found in the registr
  if (LTop <> -1) and (LLeft <> -1) then
  begin
    AForm.Top   := readInteger(LINISection, 'Top',    AForm.Top);
    AForm.Left  := readInteger(LINISection, 'Left',   AForm.Left);
    AForm.WindowPosition := wpUser;
  // No form pos in the registry, so default to screen center
  end
  else
  begin
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
//  AForm.WindowState := TWindowState(ReadInteger(LINISection, 'WindowState', ord(wsNormal)));

  // If the form is off screen (positioned outside all monitor screens) then
  // center the form on screen.
  //{$IFDEF MSWINDOWS}
  //if (AForm.FormStyle <> fsMDIChild) {$IFNDEF FPC} and tiFormOffScreen(AForm) {$ENDIF} then
  //begin
    //if Assigned(Application.MainForm) and (Application.MainForm <> AForm) then
      //AForm.Position := poMainFormCenter
    //else
      //AForm.Position:= poScreenCenter;
  //end;
  //{$ENDIF MSWINDOWS}
end;

procedure TtiGuiINIFile.WriteFormState(AForm: TfpgForm);
var
  LINISection: string;
begin
  LINISection := AForm.Name + 'State';
//  writeInteger(LINISection, 'WindowState', ord(AForm.WindowState));
//  if AForm.WindowState = wsNormal then
//  begin
    WriteInteger(LINISection, 'Top', AForm.Top);
    WriteInteger(LINISection, 'Left', AForm.Left);
    if AForm.Sizeable then
    begin
      WriteInteger(LINISection, 'Height', AForm.Height);
      WriteInteger(LINISection, 'Width', AForm.Width);
    end;
//  end;
end;

initialization
  uGuiINI := nil;

finalization
  uGuiINI.Free;

end.








