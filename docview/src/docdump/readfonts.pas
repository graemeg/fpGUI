{
  Dumps the font data
}
unit readfonts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper;

procedure ProcessFonts(AIn: TFileStream; AOut: TFileTextStream);


implementation

uses
  readheader, IPFFileFormatUnit;

procedure ProcessFonts(AIn: TFileStream; AOut: TFileTextStream);
var
  fnt: THelpFontSpec;
  pData: pointer;
  i: integer;
begin
  AOut.WriteLn('');
  AOut.WriteLn('Font Data');
  if eHdr.NumFontEntry > 0 then
  begin
    AIn.Seek(eHdr.FontTableOffset, soBeginning);
    for i := 0 to eHdr.NumFontEntry-1 do
    begin
      AIn.Read(fnt, SizeOf(THelpFontSpec));
      AOut.WriteLn(Format('  Font Entry #%d', [i]));
      AOut.WriteLn(Format('    FontSpec.FaceName: %s', [fnt.FaceName]));
      AOut.WriteLn(Format('    FontSpec.Height:   %4.4x (%0:d)', [fnt.Height]));
      AOut.WriteLn(Format('    FontSpec.Width:    %4.4x (%0:d)', [fnt.Width]));
      AOut.WriteLn(Format('    FontSpec.CodePage: %4.4x (%0:d)', [fnt.Codepage]));
    end;
  end
  else
    AOut.WriteLn('  No font data is present');
end;

end.

