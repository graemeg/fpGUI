{
  This program searches for *.bmp files in the current directory and
  outputs to stdout the bmp files found as byte array constants.
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
  public
    procedure Usage;
    procedure Verbose(Msg: string; Args: array of const);
    procedure ConvertImage(FN: string);
    procedure ConvertImages;
    function ProcessCommandLine: Boolean;
    procedure DoRun; override;
    property BeVerbose: Boolean read FBeVerbose;
    property InputDir: string read FInputDir;
    property OutputFile: string read FOutputFile;
    property Prefix: string read FPrefix;
  end;

  
  procedure TConvertApp.Usage;
  begin
    Writeln('Usage : ', ExtractFileName(ParamStr(0)));
    Writeln(' -h --help           This help screen');
    Writeln(' -i --inputdir=NNN   Search files in dir NNN');
    Writeln(' -o --output=NNN     Write output in file NNN');
    Writeln(' -p --prefix=NNN     Prefix constant names with  NNN');
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
  begin
    Verbose('Converting image : %s', [FN]);
    S := FPrefix + ChangeFileExt(FN, '');
    if (FOutputFile <> '') then
      ExecuteProcess(FBinary, ['-o', FOutputFile, '-c', S, FN])
    else
      ExecuteProcess(FBinary, ['-c', S, FN]);
  end;

  function TConvertApp.ProcessCommandLine: Boolean;
  const
    Longopts: array[1..5] of string = (
      'help', 'verbose', 'inputdir', 'output:', 'prefix:');
  var
    S: string;
  begin
    S      := CheckOptions('hvi:o:p:', Longopts);
    Result := (S = '') and not HasOption('h', 'help');
    if not Result then
    begin
      if (S <> '') then
        Writeln(StdErr, 'Error in options: ', S);
      Usage;
      Exit;
    end;
    FBeVerbose := HasOption('v');
    if HasOption('i', 'inputdir') then
      FInputDir := GetOptionValue('i', 'inputdir');
    if HasOption('o', 'output') then
      FOutputFile := GetOptionValue('o', 'output');
    if HasOption('p', 'prefix') then
      FPrefix     := GetOptionValue('p', 'prefix')
    else
      FPrefix     := 'stdimg_';
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
  begin
    if (FBinary = '') then
      FBinary   := FileSearch('bin2obj', GetEnvironmentVariable('PATH'));
    if (FInputDir <> '') then
      FInputDir := IncludeTrailingPathDelimiter(FInputDir);
    if FindFirst(FInputDir + '*.bmp', faAnyFile, Info) = 0 then
      try
        repeat
          ConvertImage(FInputDir + Info.Name);
        until FindNext(Info) <> 0;
      finally
        FindClose(Info);
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


