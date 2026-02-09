{
    Demo app to show TfpgAbout
    Written by Jonathan A. Foster <jon@jfpossibilities.com>
    Started April 10th, 2019

    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2019 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
program aboutdialog;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fpg_constants,
  fpg_base, fpg_main, fpg_form, fpg_button,
  fAbout;


{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}



{*********************************************************************
 * Main Form                                                         *
 *********************************************************************}
type
  TfrmMain = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: frmMain}
    click_me: TfpgButton;
    {@VFD_HEAD_END: frmMain}

    procedure   AfterCreate; override;
    procedure   clicked(sender: TObject);
  end;



procedure TfrmMain.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: frmMain}
  Name := 'frmMain';
  SetPosition(0, 0, 100, 100);
  WindowPosition:=wpAuto;

  click_me := TfpgButton.create(self);
  with click_me do begin
    name:='click_me';
    SetPosition(0, 0, 100, 100);
    anchors:=[anLeft, anTop, anRight, anBottom];
    text:='Click Me!';
    OnClick:=@clicked;
  end;
  {@VFD_BODY_END: frmMain}
end;



procedure TfrmMain.clicked(sender: TObject);
begin
  if AboutDlg=mrOK then close;
end;



{*********************************************************************
 * Launch control                                                    *
 *********************************************************************}
var
  main: TfrmMain;
  app: TfpgApplication;
begin

  app := fpgApplication;
  app.initialize;
  app.AppTitle     := 'A Demo About Box';
  app.AppVersion   := FPGUI_VERSION;
  app.AppAuthor    := 'jon@jfpossibilties.com';
  app.AppCopyright := '(c) 2019 the fpGUI project GPL2';
  app.AppSiteName  := 'fpgui.SF.net';
  app.AppSiteURL   := fpGUIWebsite;
  app.AppIcon      := 'stdimg.dlg.info';
  { set these for the about box to link to a help topic in your help file
    when the license statement is clicked:
  app.AppLicTopic  := 0;
  app.HelpFile     := 'myhelp.inf'; }
  app.HelpContext := 10;
  main := TfrmMain.create(nil);
  try
    main.show;
    app.run;
  finally
    main.free;
  end;

end.
