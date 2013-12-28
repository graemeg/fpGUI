{ 
This is the extended version of fpGUI uidesigner.
With run-only-once, window list, undo feature, integration into IDE, editor launcher,...
Fred van Stappen
fiens@hotmail.com
}
{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The starting unit for the UI Designer project.
}

program uidesigner_ext;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  RunOnce_PostIt,
  fpg_cmdlineparams,
  mystyle_systemcolors,
  fpg_stylemanager,
  SysUtils,
  fpg_main,
  vfdmain,
  newformdesigner,
  vfdwidgets;

  procedure MainProc;
  var
    filedir: string;
  begin
    ifonlyone := True;
    filedir := '';

    if (isrunningIDE('typhon') = False) and (isrunningIDE('lazarus') = False) then
    begin
      filedir := 'clear';
      RunOnce(filedir);
    end
    else
    begin
      { If file passed in as clasical first param, load it! }
      if (FileExists(ParamStr(1))) or (ParamStr(1) = 'closeall') or
        (ParamStr(1) = 'quit') then
        filedir := ParamStr(1);

      if gCommandLineParams.IsParam('onlyone') then
      begin
        if StrToInt(copy(gCommandLineParams.GetParam('onlyone'), 1, 1)) > 0 then
          RunOnce(filedir)
        else
          ifonlyone := False;
        ;
      end
      else
        RunOnce(filedir);
    end;

    fpgApplication.Initialize;
    try
      RegisterWidgets;
      if not gCommandLineParams.IsParam('style') then
        if fpgStyleManager.SetStyle('my style system colors') then
          fpgStyle := fpgStyleManager.Style;



      PropList := TPropertyList.Create;
      maindsgn := TMainDesigner.Create;

      maindsgn.CreateWindows;

      // Note:  This needs improving!!
      fpgApplication.MainForm := frmMain;

      fpgApplication.Run;

      PropList.Free;

    finally
      maindsgn.Free;
    end;
  end;

{$R *.res}

begin
  MainProc;
end.
