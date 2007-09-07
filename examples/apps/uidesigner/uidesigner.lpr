{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The starting unit for the uiDesigner project.
}

program uidesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpgfx, vfdmain, vfdresizer, vfdforms,
  vfdfile, newformdesigner, vfdwidgets, vfdformparser, vfdeditors,
  vfdwidgetclass, vfdutils, vfdprops, vfddesigner;


procedure MainProc;
begin
  fpgApplication.Initialize;

  RegisterWidgets;
  PropList := TPropertyList.Create;
  maindsgn := TMainDesigner.Create;
  maindsgn.CreateWindows;
  maindsgn.EditedFileName := ParamStr(1);
  if FileExists(maindsgn.EditedFileName) then
    maindsgn.OnLoadFile(maindsgn);

  // Note:  This needs improving!!
  fpgApplication.ProcessMessages;
  repeat
    fpgWaitWindowMessage;
  until (not frmMain.Visible);
  
{
  repeat
    try
      fpgDoMessageLoop;
      break;
    except
      on e: Exception do
        ShowMessage(e.message, 'Exception');
    end;
  until False;
}

//  fpgApplication.Run;
end;

begin
  MainProc;
end.


