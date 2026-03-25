{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

program maximus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_stylemanager,
  fpg_cmdlineparams,
  ide.form.main,
  ide.form.configure,
  ide.consts,
  ide.macros,
  ide.form.debug,
  ide.project.backend,
  ide.project,
  ide.project.pasbuild,
  ide.project.unitlist,
  ide.form.projectoptions,
  ide.utils,
  ide.builder.thread,
  ide.images,
  ide.stringhelpers,
  ide.form.procedurelist,
  ide.filemonitor,
  fpg_textedit,
  ide.form.find,
  Sha1,
  ide.editor.undo,
  ide.session;


procedure MainProc;
var
  frm: TMainForm;
  cmd: ICmdLineParams;
begin
//  FPG_DEFAULT_FONT_DESC := 'DejaVu Sans-9';
  fpgApplication.Initialize;
  RegisterIDEImages;

  { Set our new style as the default (before we create any forms), unless
    a the end-user specified a different style via the command line. }
  if Supports(fpgApplication, ICmdLineParams, cmd) and not cmd.HasOption('style') then
  begin
    if fpgStyleManager.SetStyle('Fusion Light') then
      fpgStyle := fpgStyleManager.Style;
  end;

  frm := TMainForm.Create(nil);
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

