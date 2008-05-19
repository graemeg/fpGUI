{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The starting unit for the UI Designer project.
}

program uidesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpgfx, vfdmain, vfdresizer, vfdforms,
  vfdfile, newformdesigner, vfdwidgets, vfdformparser, vfdeditors,
  vfdwidgetclass, vfdutils, vfdprops, vfddesigner, vfdpropeditgrid;


procedure MainProc;
begin
  fpgApplication.Initialize;
  try
    RegisterWidgets;
    PropList := TPropertyList.Create;
    maindsgn := TMainDesigner.Create;
    maindsgn.CreateWindows;
    maindsgn.EditedFileName := ParamStr(1);
    if FileExists(maindsgn.EditedFileName) then
      maindsgn.OnLoadFile(maindsgn);

    // Note:  This needs improving!!
    fpgApplication.MainForm := frmMain;
    fpgApplication.Run;
    
  finally
    maindsgn.Free;
  end;
end;

begin
  MainProc;
end.


