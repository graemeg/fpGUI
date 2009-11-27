{
  Dump the controls data
}
unit readcontrols;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper;

procedure ProcessControls(AIn: TFileStream; AOut: TFileTextStream);


implementation

uses
  readheader, IPFFileFormatUnit;

procedure ProcessControls(AIn: TFileStream; AOut: TFileTextStream);
var
  ctrls: TPanelControls;
  i: integer;
begin
  AOut.WriteLn('');
  AOut.WriteLn('Panel Controls (Buttons)');
  if eHdr.CtrlOffset > 0 then
  begin
    AIn.Seek(eHdr.CtrlOffset, soBeginning);
    AIn.Read(ctrls, SizeOf(TControlDef));
    AOut.WriteLn(Format('  PanelControls.ControlCount:  %4.4x (%0:d)', [ctrls.ControlCount]));
    AOut.WriteLn(Format('  PanelControls.GroupCount:    %4.4x (%0:d)', [ctrls.GroupCount]));
    AOut.WriteLn(Format('  PanelControls.GroupIndex:    %4.4x (%0:d)', [ctrls.GroupIndex]));
    AOut.WriteLn(Format('  PanelControls.Reserved:      %4.4x (%0:d)', [ctrls.Reserved]));
    AOut.WriteLn('  *****');
    AOut.WriteLn('  <todo - process CountrolCount and GroupCount data>');
  end
  else
    AOut.WriteLn('  No panel control found');
end;

end.

