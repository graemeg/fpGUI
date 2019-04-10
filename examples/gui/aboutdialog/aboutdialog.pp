{*********************************************************************
 * App meta & about dlg demo                                         *
 * Written by Jonathan A. Foster <jon@jfpossibilities.com>           *
 * Started April 10th, 2019                                          *
 * Copyright JF Possibilities, Inc.                                  *
 *********************************************************************}
{$mode objfpc}{$H+}
program aboutdialog;
uses SysUtils, Classes,
  fpg_base, fpg_main,
  fpg_form, fpg_button, fpg_about;


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
  AboutDlg;
end;



{*********************************************************************
 * Launch control                                                    *
 *********************************************************************}
var
    main: TfrmMain;
begin

    fpgApplication.initialize;
    fpgApplication.app_name:='About Dialog Test';
    fpgApplication.app_ver:='0.1';
    fpgApplication.app_author:='jon@jfpossibilties.com';
    fpgApplication.app_copyright:='(c) 2019 the fpGUI project GPL2';
    fpgApplication.app_site:='fpgui.SF.net';
    fpgApplication.app_url:='http://fpgui.sourceforge.net/';
    fpgApplication.app_icon:='stdimg.dlg.info';
    fpgApplication.app_lic_topic:=0;
    main:=TfrmMain.create(nil);
    try
        main.show;
        fpgApplication.run;
    finally
        main.free;
    end;

end.
