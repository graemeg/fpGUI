{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This descendant adds ReadOnly support and can read/write Form state
      information.
}

unit fpg_iniutils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  fpg_base,
  fpg_form;

type

  TfpgINIFile = class(TINIFile)
  private
    FReadOnly: Boolean;
  public
    constructor CreateExt(const AFileName: TfpgString = ''; AReadOnly: Boolean = False);
    function    ReadString(const ASection, AIdent, ADefault: string): string; override;
    function    ReadInteger(const ASection, AIdent: string; ADefault: longint): longint; override;
    function    ReadBool(const ASection, AIdent: string; ADefault: Boolean): Boolean; override;
    function    ReadDate(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    function    ReadDateTime(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    function    ReadFloat(const ASection, AName: string; ADefault: double): double; override;
    function    ReadTime(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    procedure   ReadFormState(AForm: TfpgForm; AHeight: integer = -1; AWidth: integer = -1; const ASkipDimensions: Boolean = False);
    procedure   WriteFormState(AForm: TfpgForm);
  end;

// singleton
function gINI(const AFileName: TfpgString = ''): TfpgINIFile;

implementation

uses
  fpg_main,
  fpg_constants,
  fpg_utils;

var
  uINI: TfpgINIFile;


function gINI(const AFileName: TfpgString): TfpgINIFile;
begin
  if uINI = nil then
    uINI := TfpgINIFile.CreateExt(AFileName);
  Result := uINI;
end;

{ TfpgINIFile }

constructor TfpgINIFile.CreateExt(const AFileName: TfpgString; AReadOnly: Boolean);
var
  lDir: TfpgString;
  lFileName: TfpgString;
begin
  FReadOnly := AReadOnly;
  lDir      := fpgExtractFileDir(AFileName);
  lFileName := fpgExtractFileName(AFileName);

  if lDir = '' then
    lDir := GetAppConfigDir(False);
  if not (lDir[Length(lDir)] = PathDelim) then
    lDir := lDir + PathDelim;

  { We used a non-Global config dir, so should be able to create the dir }
  if not fpgForceDirectories(lDir) then
    raise Exception.CreateFmt(rsErrFailedToCreateDir, [lDir]);


  if lFileName = '' then
    lFileName := ApplicationName + '.ini'
  else if fpgExtractFileExt(lFileName) = '' then
    lFileName := lFileName + '.ini';

  lFileName := lDir + lFileName;
  Create(lFileName);
end;

function TfpgINIFile.ReadString(const ASection, AIdent, ADefault: string): string;
begin
  Result := inherited ReadString(ASection, AIdent, ADefault);
  if (not ValueExists(ASection, AIdent)) and
    (not FReadOnly) then
    WriteString(ASection, AIdent, ADefault);
end;

function TfpgINIFile.ReadInteger(const ASection, AIdent: string; ADefault: longint): longint;
begin
  if (not ValueExists(ASection, AIdent)) and
    (not FReadOnly) then
    WriteInteger(ASection, AIdent, ADefault);
  Result := inherited ReadInteger(ASection, AIdent, ADefault);
end;

function TfpgINIFile.ReadBool(const ASection, AIdent: string; ADefault: Boolean): Boolean;
var
  lValueExists: Boolean;
begin
  lValueExists := ValueExists(ASection, AIdent);
  if (not lValueExists) and
    (not FReadOnly) then
    WriteBool(ASection, AIdent, ADefault);
  Result := inherited ReadBool(ASection, AIdent, ADefault);
end;

function TfpgINIFile.ReadDate(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  if (not ValueExists(ASection, AName)) and
    (not FReadOnly) then
    WriteDate(ASection, AName, ADefault);
  Result := inherited ReadDate(ASection, AName, ADefault);
end;

function TfpgINIFile.ReadDateTime(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  if (not ValueExists(ASection, AName)) and
    (not FReadOnly) then
    WriteDateTime(ASection, AName, ADefault);
  Result := inherited ReadDateTime(ASection, AName, ADefault);
end;

function TfpgINIFile.ReadFloat(const ASection, AName: string; ADefault: double): double;
begin
  if (not ValueExists(ASection, AName)) and
    (not FReadOnly) then
    WriteFloat(ASection, AName, ADefault);
  Result := inherited ReadFloat(ASection, AName, ADefault);
end;

function TfpgINIFile.ReadTime(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  if (not ValueExists(ASection, AName)) and
    (not FReadOnly) then
    WriteTime(ASection, AName, ADefault);
  Result := inherited ReadTime(ASection, AName, ADefault);
end;

// Do NOT localize
procedure TfpgINIFile.ReadFormState(AForm: TfpgForm; AHeight: integer; AWidth: integer; const ASkipDimensions: Boolean = False);
var
  LINISection: string;
  LTop: integer;
  LLeft: integer;
  LHeight: integer;
  LWidth: integer;
begin
  Assert(AForm <> nil, Format(rsErrNotAssigned, ['pForm']));
  LINISection := AForm.Name + 'State';
  // Read form position, -1 if not stored in ini file
  LTop        := readInteger(LINISection, 'Top', -1);
  LLeft       := readInteger(LINISection, 'Left', -1);
  // The form pos was found in the ini file
  if (LTop <> -1) and (LLeft <> -1) then
  begin
    AForm.Top  := readInteger(LINISection, 'Top', AForm.Top);
    AForm.Left := readInteger(LINISection, 'Left', AForm.Left);
    AForm.WindowPosition := wpUser;
    // No form pos in the ini file, so default to screen center
  end
  else if Assigned(fpgApplication.MainForm) and (fpgApplication.MainForm <> AForm) then
    AForm.WindowPosition := wpAuto
  else
    AForm.WindowPosition := wpScreenCenter;
  // Only set the form size if a bsSizable window
  if AForm.Sizeable and (not ASkipDimensions) then
  begin
    if AHeight = -1 then
      LHeight := AForm.Height
    else
      LHeight := AHeight;
    if AWidth = -1 then
      LWidth  := AForm.Width
    else
      LWidth  := AWidth;
    AForm.Height := readInteger(LINISection, 'Height', LHeight);
    AForm.Width := readInteger(LINISection, 'Width', LWidth);
  end;
  AForm.UpdateWindowPosition;

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

// Do NOT localize
procedure TfpgINIFile.WriteFormState(AForm: TfpgForm);
var
  LINISection: string;
begin
  LINISection := AForm.Name + 'State';
  //  writeInteger(LINISection, 'WindowState', ord(AForm.WindowState));
  //  if AForm.WindowState = wsNormal then
  //  begin
  
  // A work-around while WindowState is not implemented
  if (AForm.Top >= 0) or (AForm.Left >= 0) then
  begin
    writeInteger(LINISection, 'Top', AForm.Top);
    writeInteger(LINISection, 'Left', AForm.Left);
  end;
  if AForm.Sizeable then
  begin
    writeInteger(LINISection, 'Height', AForm.Height);
    WriteInteger(LINISection, 'Width', AForm.Width);
  end;
  //  end;
end;


initialization
  uINI := nil;

finalization
  uINI.Free;

end.

