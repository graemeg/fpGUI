{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This program is kindly supplied by Henry Vermaak. It uses the new
      fpmake build system introduced in FPC 2.2.2. fpmake is supposed to
      replace the Makefile build system.

    Usage:
      - Compile fpmake.pas as follows:
             fpc fpmake.pas

      - To find out more about what fpmake can do and see some help
             fpmake --help

      - Build and install fpGUI
             fpmake install

        Note that if you installed FPC in a non-standard location on you
        system you need to tell fpmake which compiler and units to use. The
        following is all in one line:
             fpmake.exe -r c:\fpc\2.2.3\bin\i386-win32\ppc386.exe
                -UG c:\fpc\2.2.3\units\i386-win32
          or
             ./fpmake -v build -UG /opt/fpc/lib/fpc/2.2.3/units/i386-linux/
}

program fpmake;

{$mode objfpc}
{$h+}

uses sysutils, fpmkunit;

var
  T: TTarget;
  P: TPackage;
begin
  with Installer do begin
    P := AddPackage('fpgui');
    P.Version := '0.6_svn';
    { Fill in more package details here }

    { This shouldn't really be here.  fpmake will install to the local
      fpc installation, i.e. /usr/[local]/lib/fpc/<version>/units/<cpu-os>/fpgui
      if we set the package name to fpgui as above.  This base install dir
      can be overridden by passing -B to fpmake.  The line below will cause
      the units to be output in ../lib/<cpu-os>/fpgui  }
    Defaults.UnitInstallDir := Format('../lib/%s-%s/', [CurrentCPU, CurrentOS]);

    { If you installed FPC to a non-standard location, you need to specify
      where fpmake can find the compiler and RTL units. You can pass that
      information via the command line to fpmake, or specify it here in code. }
//    if Defaults.OS in AllUnixOSes then
//      Defaults.GlobalUnitDir := Format('/opt/fpc/lib/fpc/2.2.3/units/%s-%s/', [CurrentCPU, CurrentOS])
//    else
//      Defaults.GlobalUnitDir := Format('c:\fpc\2.2.3\units\%s-%s', [CurrentCPU, CurrentOS]);

    if Defaults.OS in AllUnixOSes
      then Defaults.Options := Defaults.Options + '-dX11'
      else Defaults.Options := Defaults.Options + '-dGDI';

    P.SourcePath.Add('corelib');
    P.SourcePath.Add('corelib/x11', AllUnixOSes);
    P.SourcePath.Add('corelib/gdi', AllWindowsOSes);
    P.SourcePath.Add('gui');
    P.SourcePath.Add('gui/db');

    P.UnitPath.Add('corelib');
    P.UnitPath.Add('corelib/x11', AllUnixOSes);
    P.UnitPath.Add('corelib/gdi', AllWindowsOSes);
    P.UnitPath.Add('gui');
    P.UnitPath.Add('gui/db');

    P.IncludePath.Add('corelib');
    P.IncludePath.Add('corelib/x11', AllUnixOSes);
    P.IncludePath.Add('corelib/gdi', AllWindowsOSes);
    P.IncludePath.Add('gui');

    { todo: add unit and include dependency for all }

    { x11 and gdi common }
    T := P.Targets.AddUnit('fpg_impl.pas');

    { corelib/x11 }
    T := P.Targets.AddUnit('fpg_keyconv_x11.pas', AllUnixOSes);
    T := P.Targets.AddUnit('fpg_netlayer_x11.pas', AllUnixOSes);
    T := P.Targets.AddUnit('fpg_x11.pas', AllUnixOSes);
{    with T.Dependencies do begin
      AddUnit('fpg_xft_x11');
      AddUnit('fpg_netlayer_x11');
      AddUnit('fpg_base');
      AddUnit('fpg_impl');
    end;  }
    T := P.Targets.AddUnit('fpg_xft_x11.pas', AllUnixOSes);

    { corelib/gdi }
    T := P.Targets.AddUnit('fpg_gdi.pas', AllWindowsOSes);

    { corelib }
    T := P.Targets.AddUnit('fpg_base.pas');
    T := P.Targets.AddUnit('fpg_imagelist.pas');
    T := P.Targets.AddUnit('fpg_popupwindow.pas');
    T := P.Targets.AddUnit('fpg_translations.pas');
    T := P.Targets.AddUnit('fpg_cmdlineparams.pas');
    T := P.Targets.AddUnit('fpg_imgfmt_bmp.pas');
    T := P.Targets.AddUnit('fpg_stdimages.pas');
    T := P.Targets.AddUnit('fpg_utils.pas');
    T := P.Targets.AddUnit('fpg_command_intf.pas');
    T := P.Targets.AddUnit('fpg_main.pas');
    T := P.Targets.AddUnit('fpg_stringhashlist.pas');
    T := P.Targets.AddUnit('fpg_widget.pas');
    T := P.Targets.AddUnit('fpg_constants.pas');
      T.ResourceStrings := True;
    T := P.Targets.AddUnit('fpg_strings.pas');
    T := P.Targets.AddUnit('fpg_wuline.pas');
    T := P.Targets.AddUnit('fpg_extinterpolation.pas');
    T := P.Targets.AddUnit('fpg_pofiles.pas');
    T := P.Targets.AddUnit('fpg_stringutils.pas');

    { gui/db }
    T := P.Targets.AddUnit('fpgui_db.pas');

    { gui }
    T := P.Targets.AddUnit('fpg_animation.pas');
    T := P.Targets.AddUnit('fpg_combobox.pas');
    T := P.Targets.AddUnit('fpg_edit.pas');
    T := P.Targets.AddUnit('fpg_hint.pas');
    T := P.Targets.AddUnit('fpg_listbox.pas');
    T := P.Targets.AddUnit('fpg_mru.pas');
    T := P.Targets.AddUnit('fpg_radiobutton.pas');
    T := P.Targets.AddUnit('fpg_tab.pas');
    T := P.Targets.AddUnit('fpg_basegrid.pas');
    T := P.Targets.AddUnit('fpg_customgrid.pas');
    T := P.Targets.AddUnit('fpg_form.pas');
    T := P.Targets.AddUnit('fpg_hyperlink.pas');
    T := P.Targets.AddUnit('fpg_listview.pas');
    T := P.Targets.AddUnit('fpg_panel.pas');
    T := P.Targets.AddUnit('fpg_scrollbar.pas');
    T := P.Targets.AddUnit('fpg_trackbar.pas');
    T := P.Targets.AddUnit('fpg_button.pas');
    T := P.Targets.AddUnit('fpg_dialogs.pas');
    T := P.Targets.AddUnit('fpg_gauge.pas');
    T := P.Targets.AddUnit('fpg_iniutils.pas');
    T := P.Targets.AddUnit('fpg_memo.pas');
    T := P.Targets.AddUnit('fpg_popupcalendar.pas');
    T := P.Targets.AddUnit('fpg_splitter.pas');
    T := P.Targets.AddUnit('fpg_tree.pas');
    T := P.Targets.AddUnit('fpg_checkbox.pas');
    T := P.Targets.AddUnit('fpg_editcombo.pas');
    T := P.Targets.AddUnit('fpg_grid.pas');
    T := P.Targets.AddUnit('fpg_label.pas');
    T := P.Targets.AddUnit('fpg_menu.pas');
    T := P.Targets.AddUnit('fpg_progressbar.pas');
    T := P.Targets.AddUnit('fpg_style.pas');

    Run;
  end;
end.
