{
  This program searches for *.bmp files in the current directory and
  outputs to stdout the bmp files found as byte array constants.

  Here are a few usage examples:

  1)
  ./updatestdimgs > ../src/corelib/stdimages.inc

  This takes whatever .bmp file are in the current directory. Creates
  byte array constants and outputs them to an include file.


  2)
  ./updatestdimgs -v -u myimages -m 0,0 -p myapp > /tmp/myimages.pas

  This takes all .bmp images in current directory. (-v) Produces
  verbose output at images are processed. (-u) generate a complete
  unit for the images including registration code. (-m) if the images
  have a transparency mask, what pixel should be used by default as
  the transparency color. (-p) Use the "myapp" as the prefix to all
  image constants. And lastly take all the stdout output and pipe it
  into a file called myimages.pas

  PS:
  -o for the output file parameter still has some bugs, so I would
  suggest you use piping (.... > outfile.inc) instead.


  3)
  ./updatestdimgs -v -i themes/silver/ > /tmp/mysilvertheme.inc

  This takes all .bmp files in the themes/silver/ directory and generates
  byte array constants using the default "usr" prefix and outputs to
  stdout. We then pipe the output to mysilvertheme.inc file.

}
program updatestdimgs;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ELSE}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  CustApp;

const
{$ifdef unix}
  bin2obj = 'bin2obj';
{$else}
  bin2obj = 'bin2obj.exe';
{$endif}

type
  TConvertApp = class(TCustomApplication)
  private
    FBeVerbose: Boolean;
    FBinary: string;
    FOutputFile: string;
    FInputDir: string;
    FPrefix: string;
    FUnitName: string;
    FUnitCode: string;
    FMaskSamplePos: string;
  public
    procedure   Usage;
    procedure   Verbose(Msg: string; Args: array of const);
    procedure   ConvertImage(FN: string);
    procedure   ConvertImages;
    function    ProcessCommandLine: Boolean;
    procedure   DoRun; override;
    property    BeVerbose: Boolean read FBeVerbose;
    property    InputDir: string read FInputDir;
    property    OutputFile: string read FOutputFile;
    property    Prefix: string read FPrefix;
  end;

  
  procedure TConvertApp.Usage;
  begin
    Writeln('Usage : ', ExtractFileName(ParamStr(0)));
    Writeln(' -h --help           This help screen');
    Writeln(' -i --inputdir=NNN   Search files in dir NNN');
    Writeln(' -o --output=NNN     Write output in file NNN');
    Writeln(' -p --prefix=NNN     Prefix constant names with NNN');
    Writeln(' -u --unit=NNN       Create a complete unit named NNN');
    Writeln(' -m --mask=X,Y       When using -u switch, set the position ');
    Writeln('                     of the pixel containing transparent color');
    Writeln(' -v --verbose        Be verbose');
  end;

  procedure TConvertApp.Verbose(Msg: string; Args: array of const);
  begin
    if BeVerbose then
      Writeln(StdErr, Format(Msg, Args));
  end;

  procedure TConvertApp.ConvertImage(FN: string);
  var
    S: string;
    iname: string;
  begin
    Verbose('Converting image : %s', [FN]);
    iname := ChangeFileExt(FN, '');
    S := FPrefix + '_' + iname;
    if (FOutputFile <> '') then
      ExecuteProcess(FBinary, ['-o', FOutputFile, '-c', S, FN])
    else
      ExecuteProcess(FBinary, ['-c', S, FN]);

    if FUnitName <> '' then
      FUnitCode := FUnitCode +
        '  fpgImages.AddMaskedBMP('                 +LineEnding+
        '      '''+FPrefix+'.'+iname+''','          +LineEnding+
        '      @'+S+','                             +LineEnding+
        '      sizeof('+S+'), '+FMaskSamplePos+');' +LineEnding+LineEnding;
  end;

  function TConvertApp.ProcessCommandLine: Boolean;
  const
    Longopts: array[1..7] of string = (
      'help', 'verbose', 'inputdir:', 'output:', 'prefix:', 'unit:', 'mask:');
  var
    S: string;
  begin
    S      := CheckOptions('hvi:o:p:u:m:', Longopts);
    Result := (S = '') and not HasOption('h', 'help');
    if not Result then
    begin
      if (S <> '') then
        Writeln(StdErr, 'Error in options: ', S);
      Usage;
      Exit;
    end;
    FBeVerbose := HasOption('v', 'verbose');
    if HasOption('i', 'inputdir') then
      FInputDir := GetOptionValue('i', 'inputdir');
    if HasOption('o', 'output') then
      FOutputFile := GetOptionValue('o', 'output');
    if HasOption('p', 'prefix') then
      FPrefix     := GetOptionValue('p', 'prefix')
    else
      FPrefix     := 'usr';

    if HasOption('u', 'unit') then
      if FOutputFile <> '' then
        FUnitName := ChangeFileExt(FOutputFile, '')
      else
        FUnitName := GetOptionValue('u', 'unit');
      
    if HasOption('m', 'mask') then
      FMaskSamplePos := GetOptionValue('m', 'mask')
    else
      FMaskSamplePos := '0,0';
  end;

  procedure TConvertApp.DoRun;
  begin
    StopOnException := True;
    if ProcessCommandLine then
      ConvertImages;
    Terminate;
  end;

  procedure TConvertApp.ConvertImages;
  var
    Info: TSearchRec;
    f: TextFile;
  begin
    if (FBinary = '') then
      FBinary   := FileSearch(bin2obj, GetEnvironmentVariable('PATH'));    
    if (FInputDir <> '') then
      FInputDir := IncludeTrailingPathDelimiter(FInputDir);
      
    if FUnitName <> '' then
    begin
      FUnitCode := 'unit '+ FUnitName +';'            +LineEnding+LineEnding+
                   '{$mode objfpc}{$H+}'              +LineEnding+LineEnding+
                   'interface'                        +LineEnding+LineEnding+
                   'uses'                             +LineEnding+
                   '  fpg_main;'                      +LineEnding+LineEnding+
                   'procedure InitializeCustomImages;'+LineEnding+LineEnding+
                   'implementation';
      AssignFile(f, FOutputFile);
      Rewrite(f);
      WriteLn(f, FUnitCode);
      CloseFile(f);
      FUnitCode := LineEnding+
                   'procedure InitializeCustomImages;'+LineEnding+
                   'begin'                            +LineEnding;
    end;
      
    if FindFirst(FInputDir + '*.bmp', faAnyFile, Info) = 0 then
      try
        repeat
          ConvertImage(FInputDir + Info.Name);
        until FindNext(Info) <> 0;
      finally
        FindClose(Info);
      end;

    if FUnitName <> '' then
    begin
      FUnitCode := FUnitCode + 'end;'+LineEnding+LineEnding+'end.';
      Append(f);
      WriteLn(f, FUnitCode);
      CloseFile(f);
    end;
  end;

begin
  with TConvertApp.Create(nil) do
    try
      Run
    finally
      Free;
    end;
end.


