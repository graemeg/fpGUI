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

// MacOS specific filesystem implementations of encoding functions

function fpgToOSEncoding(aString: TfpgString): string;
begin
  Result := aString;
end;

function fpgFromOSEncoding(aString: string): TfpgString;
begin
  Result := aString;
end;

// TODO: This still needs a MacOS implementation
procedure fpgOpenURL(const aURL: TfpgString);
var
  Helper: string;
  p: TProcess;
begin
  Helper := '';
  //TODO: Catch "which" command output to run the browser from there
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
  begin
    p := TProcess.create(nil);
    try
      p.Executable := Helper;
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
