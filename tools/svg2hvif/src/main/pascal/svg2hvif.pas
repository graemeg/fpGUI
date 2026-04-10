{
    svg2hvif — SVG to Haiku Vector Icon Format (HVIF) converter.

    Copyright (c) 2026 Graeme Geldenhuys

    This program is part of the fpGUI Toolkit project.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Usage:
      svg2hvif [options] <input.svg> [<output.hvif>]
      svg2hvif [options] -o <dir> <input1.svg> [<input2.svg> ...]

    Options:
      -o <dir>     Write output file(s) to <dir> instead of alongside the source
      -v           Verbose: print each conversion as it happens
      --overwrite  Overwrite existing output files (default: skip if present)
      --help       Show this help and exit

    Output filename:
      When no explicit output file is given, the output file is placed in the
      same directory as the input (or -o <dir>) with the .svg extension replaced
      by .hvif.

    Exit codes:
      0  All conversions succeeded
      1  One or more conversions failed or bad arguments
}

program svg2hvif;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes,
  svg2hvif_converter;

{ =========================================================
  CLI helpers
  ========================================================= }

procedure PrintUsage;
begin
  WriteLn('Usage: svg2hvif [options] <input.svg> [<output.hvif>]');
  WriteLn('       svg2hvif [options] -o <dir> <input1.svg> [<input2.svg> ...]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -o <dir>     Write output(s) to <dir>');
  WriteLn('  -v           Verbose output');
  WriteLn('  --overwrite  Overwrite existing output files');
  WriteLn('  --help       Show this help');
  WriteLn;
  WriteLn('Converts SVG files to HVIF binary format.');
  WriteLn('Supported: path, rect, circle, ellipse, polygon, polyline, line,');
  WriteLn('           groups with transform, gradients, arc commands.');
end;

{ Replace the extension of AFileName with ANewExt (including the dot). }
function ChangeFileExtension(const AFileName, ANewExt: string): string;
begin
  Result := ChangeFileExt(AFileName, ANewExt);
end;

{ Build output path for ASrc under AOutDir (or alongside ASrc if AOutDir=''). }
function BuildOutputPath(const ASrc, AOutDir: string): string;
var
  baseName: string;
begin
  baseName := ChangeFileExtension(ExtractFileName(ASrc), '.hvif');
  if AOutDir <> '' then
    Result := IncludeTrailingPathDelimiter(AOutDir) + baseName
  else
    Result := ChangeFileExtension(ASrc, '.hvif');
end;


var
  outDir: string;
  verbose, overwrite: Boolean;
  inputFiles: TStringList;
  explicitOutput: string;
  i: Integer;
  arg, srcFile, dstFile: string;
  anyError: Boolean;
  nextIsOutDir: Boolean;
begin
  outDir        := '';
  verbose       := False;
  overwrite     := False;
  anyError      := False;
  nextIsOutDir  := False;
  explicitOutput := '';

  inputFiles := TStringList.Create;
  try

    { ---- Parse command line ---- }
    for i := 1 to ParamCount do
    begin
      arg := ParamStr(i);

      if nextIsOutDir then
      begin
        outDir := arg;
        nextIsOutDir := False;
        Continue;
      end;

      if (arg = '--help') or (arg = '-h') then
      begin
        PrintUsage;
        ExitCode := 0;
        Exit;
      end
      else if arg = '--overwrite' then
        overwrite := True
      else if arg = '-v' then
        verbose := True
      else if arg = '-o' then
        nextIsOutDir := True
      else if (arg <> '') and (arg[1] = '-') then
      begin
        WriteLn('Error: unknown option: ', arg);
        ExitCode := 1;
        Exit;
      end
      else
        inputFiles.Add(arg);
    end;

    if nextIsOutDir then
    begin
      WriteLn('Error: -o requires a directory argument');
      ExitCode := 1;
      Exit;
    end;

    { Validate argument combinations }
    if inputFiles.Count = 0 then
    begin
      PrintUsage;
      ExitCode := 1;
      Exit;
    end;

    { Two positional args without -o: first is input, second is output }
    if (inputFiles.Count = 2) and (outDir = '') then
    begin
      explicitOutput := inputFiles[1];
      inputFiles.Delete(1);
    end
    else if (inputFiles.Count > 1) and (outDir = '') then
    begin
      WriteLn('Error: multiple input files require -o <dir>');
      ExitCode := 1;
      Exit;
    end;

    { Validate output directory exists when given }
    if (outDir <> '') and not DirectoryExists(outDir) then
    begin
      WriteLn('Error: output directory does not exist: ', outDir);
      ExitCode := 1;
      Exit;
    end;

    { ---- Process each input file ---- }
    for i := 0 to inputFiles.Count - 1 do
    begin
      srcFile := inputFiles[i];

      if not FileExists(srcFile) then
      begin
        WriteLn('Error: input file not found: ', srcFile);
        anyError := True;
        Continue;
      end;

      if explicitOutput <> '' then
        dstFile := explicitOutput
      else
        dstFile := BuildOutputPath(srcFile, outDir);

      if FileExists(dstFile) and not overwrite then
      begin
        if verbose then
          WriteLn('Skipping (already exists): ', dstFile);
        Continue;
      end;

      try
        TfpgSvgToHvif.Convert(srcFile, dstFile);
        if verbose then
          WriteLn('Converted: ', srcFile, ' -> ', dstFile);
      except
        on E: Exception do
        begin
          WriteLn('Error converting ', srcFile, ': ', E.Message);
          anyError := True;
        end;
      end;
    end;

  finally
    inputFiles.Free;
  end;

  if anyError then
    ExitCode := 1;
end.
