unit readnlsdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, filestreamhelper;

procedure ProcessNLSData(AIn: TFileStream; AOut: TFileTextStream);

implementation

uses
  readheader;

procedure ProcessNLSData(AIn: TFileStream; AOut: TFileTextStream);
begin
  AOut.WriteLn('');
  AOut.WriteLn('NLS Data');
  if hdr.nlslen > 0 then
  begin
    AOut.WriteLn('  <todo - process NLS data>');

  end
  else
    AOut.WriteLn('NLS Data is not present');
end;

end.

