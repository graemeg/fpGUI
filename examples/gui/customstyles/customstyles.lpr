{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This example demonstrates how to define a new theme/style and how
      to use it. Styles can be changed at runtime via the --style parameter.
      eg:

        $ ./customestyle --style 'Demo Style'
        $ ./customestyle --style Win2000
        $ ./customestyle --style 'demo style'

      Note that the style name is not case sensitive. Also if the style
      name consists of more than one word, in needs to be between single
      quotes.
}


program customstyles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,fpg_main
  ,fpg_cmdlineparams
  ,fpg_stylemanager
  ,frm_test
  ,mystyle
  ;


procedure MainProc;
var
  frm: TTestForm;
begin
  fpgApplication.Initialize;

  { Set our new style as the default (before we create any forms), unless
    a the end-user specified a different style via the command line. }
  if not gCommandLineParams.IsParam('style') then
    if fpgStyleManager.SetStyle('Demo Style') then
      fpgStyle := fpgStyleManager.Style;

  frm := TTestForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

