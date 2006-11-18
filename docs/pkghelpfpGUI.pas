unit pkghelpfpGUI;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, HelpFPDoc;

procedure Register;

implementation

procedure Register;
begin
 // for Online help files
{
  RegisterFPDocHTMLHelpForPackage('fpGUI Help','fpGUI Help Database',
              'http://opensoft.homeip.net/fpgui/docs/','fpGUI');
}
 // for local help files
 RegisterFPDocHTMLHelpForPackage('fpGUI Help','fpGUI Help Database',
             'file://$PkgDir(fpGUIHelpIntegration)/html','fpGUIHelpIntegration', '../src');
//             'file://$PkgDir(fpGUI)/html','fpGUI','../src');

end;

end.
