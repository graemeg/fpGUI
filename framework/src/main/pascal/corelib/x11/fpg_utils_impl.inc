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

// X11 specific filesystem implementations of encoding functions

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
    // Intermediate child: fork again to orphan the grandchild so it is
    // reparented to init and won't zombie when it exits.
    if FpFork <> 0 then
      FpExit(0);
    // Grandchild only: exec the target; FpExecVP searches PATH for bare names.
    FpExecVP(PChar(AExecutable), PPChar(@argv[0]));
    FpExit(127);
  end;

  // Parent: reap the intermediate child synchronously (exits in microseconds).
  FpWaitPid(pid, @status, 0);
end;

var
  fpgOpenURLhelper: string = '';
procedure fpgOpenURL(const aURL: TfpgString);
var
  s: string;
  function which(const name: string; var pname: string): integer;
  var
    pipe: TextFile;
  begin
    result:=-1; // indicates failure to shell out
    try
        popen(pipe, 'which '''+StringReplace(name, '''', '''\''', [rfReplaceAll])+'''', 'r');
        try
          ReadLn(pipe, pname);
        finally
          result := pclose(pipe);
        end;
    except end;
  end;
begin
  if fpgOpenURLhelper='' then begin
    // if cache not set then determine helper
    s := '';
    if which('xdg-open', s) = 0 then
      fpgOpenURLhelper := s
    else if FileExists('/usr/bin/sensible-browser') then
      fpgOpenURLhelper := '/usr/bin/sensible-browser'
    else if FileExists('/etc/alternatives/x-www-browser') then
      fpgOpenURLhelper := '/etc/alternatives/x-www-browser'
    else if which('firefox', s) = 0 then
      fpgOpenURLhelper := s
    else if which('konqueror', s) = 0 then
      fpgOpenURLhelper := s
    else if which('opera', s) = 0 then
      fpgOpenURLhelper := s
    else if which('mozilla', s) = 0 then
       fpgOpenURLhelper := s
    else if which('chrome', s) = 0 then
       fpgOpenURLhelper := s
    else if which('chromium', s) = 0 then
       fpgOpenURLhelper := s;
  end;
  if fpgOpenURLhelper <> '' then
    fpgSpawnDetached(fpgOpenURLhelper, [aURL]);
end;

function fpgFileSize(const AFilename: TfpgString): integer;
var
  st: baseunix.stat;
begin
  if not fpstat(pointer(AFilename),st) >= 0 then
    exit(-1);
  Result := st.st_size;
end;
