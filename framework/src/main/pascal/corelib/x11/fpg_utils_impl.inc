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
  process,
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

var
  fpgOpenURLhelper: string = '';
procedure fpgOpenURL(const aURL: TfpgString);
var
  p: TProcess;
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
  begin
    p := TProcess.create(nil);
    try
      p.Executable := fpgOpenURLhelper;
      p.Parameters.Add(aURL);
      p.Execute;
    finally
      p.Free;
    end;
  end
end;

function fpgFileSize(const AFilename: TfpgString): integer;
var
  st: baseunix.stat;
begin
  if not fpstat(pointer(AFilename),st) >= 0 then
    exit(-1);
  Result := st.st_size;
end;
