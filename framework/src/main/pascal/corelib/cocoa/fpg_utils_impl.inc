{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2019 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

{%mainunit fpg_utils.pas}

uses
  Unix,
  BaseUnix,
  fpg_constants,
  fpg_stringutils;

// MacOS specific filesystem implementations of encoding functions

function fpgToOSEncoding(aString: TfpgString): string;
begin
  Result := aString;
end;

function fpgFromOSEncoding(aString: string): TfpgString;
begin
  Result := aString;
end;

procedure fpgSpawnDetached(const AExecutable: TfpgString; const AParams: array of TfpgString);
var
  pid: TPid;
  status: cInt;
  i: integer;
  argv: array of PChar;
begin
  SetLength(argv, Length(AParams) + 2);
  argv[0] := PChar(AExecutable);
  for i := 0 to High(AParams) do
    argv[i + 1] := PChar(AParams[i]);
  argv[High(argv)] := nil;

  pid := FpFork;
  if pid < 0 then
    raise Exception.Create('fpgSpawnDetached: fork failed');

  if pid = 0 then
  begin
    if FpFork <> 0 then
      FpExit(0);
    FpExecVP(PChar(AExecutable), PPChar(@argv[0]));
    FpExit(127);
  end;

  FpWaitPid(pid, @status, 0);
end;

// TODO: This still needs a proper macOS implementation (use 'open' command)
procedure fpgOpenURL(const aURL: TfpgString);
var
  Helper: string;
begin
  Helper := '';
  if fpsystem('which xdg-open') = 0 then
    Helper := 'xdg-open'
  else if FileExists('/usr/bin/sensible-browser') then
    Helper := '/usr/bin/sensible-browser'
  else if FileExists('/etc/alternatives/x-www-browser') then
    Helper := '/etc/alternatives/x-www-browser'
  else if fpsystem('which firefox') = 0 then
    Helper := 'firefox'
  else if fpsystem('which konqueror') = 0 then
    Helper := 'konqueror'
  else if fpsystem('which opera') = 0 then
    Helper := 'opera'
  else if fpsystem('which mozilla') = 0 then
    Helper := 'mozilla'
  else if fpsystem('which chrome') = 0 then
    Helper := 'chrome'
  else if fpsystem('which chromium') = 0 then
    Helper := 'chromium';

  if Helper <> '' then
    fpgSpawnDetached(Helper, [aURL]);
end;

function fpgFileSize(const AFilename: TfpgString): integer;
var
  st: baseunix.stat;
begin
  if not fpstat(pointer(AFilename),st) >= 0 then
    exit(-1);
  Result := st.st_size;
end;
