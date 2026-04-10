{
    svg2hvif — SVG to Haiku Vector Icon Format (HVIF) converter.

    This program is part of the fpGUI Toolkit project.

    Usage:
      svg2hvif <input.svg> <output.hvif>

    Converts a subset of SVG to HVIF binary format. See svg2hvif_converter.pas
    for a description of supported SVG features.

    Exit codes:
      0  Success
      1  Wrong argument count or conversion error
}

program svg2hvif;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  svg2hvif_converter;

var
  srcFile, dstFile: string;

begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: svg2hvif <input.svg> <output.hvif>');
    WriteLn;
    WriteLn('Converts a subset of SVG to Haiku Vector Icon Format (HVIF).');
    WriteLn('Supported: <path> elements, fill colours, cubic/quadratic beziers.');
    WriteLn('Not supported: <rect>, <circle>, gradients, stroke, transforms.');
    ExitCode := 1;
    Exit;
  end;

  srcFile := ParamStr(1);
  dstFile := ParamStr(2);

  if not FileExists(srcFile) then
  begin
    WriteLn('Error: input file not found: ', srcFile);
    ExitCode := 1;
    Exit;
  end;

  try
    TfpgSvgToHvif.Convert(srcFile, dstFile);
    WriteLn('Converted: ', srcFile, ' -> ', dstFile);
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
