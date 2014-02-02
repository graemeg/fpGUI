{
  Dumps the structure of an OS/2 IPF help file
}
program docdump;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, IPFFileFormatUnit, IPFEscapeCodes, CustApp, readheader,
  filestreamhelper, readextfiles, readstrings, iterator_intf, iterator_impl,
  readnlsdata, readfonts, readcontrols, readtoc, u_Tools, readdictionary;

type

  { TDocDump }

  TDocDump = class(TCustomApplication)
  private
    FIn: TFileStream;
    FOut: TFileTextStream;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TDocDump }

procedure TDocDump.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  FIn   := TFileStream.Create(ParamStr(1), fmOpenRead);
  FOut  := TFileTextStream.Create(ExtractFileName(ParamStr(1))+'.txt', fmCreate);
  try
    FOut.WriteLn(Format('File name: %s (%d bytes)', [ExtractFileName(ParamStr(1)), FIn.Size]));
    ProcessHeader(FIn, FOut);
    ProcessExtFiles(FIn, FOut);
    ProcessStringsTable(FIn, FOut);
    ProcessNLSData(FIn, FOut);
    ProcessFonts(FIn, FOut);
    ProcessControls(FIn, FOut);
    ProcessTOC(FIn, FOut);
    ProcessDictionary(FIn, FOut);
  finally
    FIn.Free;
    FOut.Free;
  end;
  // stop program loop
  Terminate;
end;

constructor TDocDump.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDocDump.Destroy;
begin
  inherited Destroy;
end;

procedure TDocDump.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TDocDump;


begin
  Application:=TDocDump.Create(nil);
  Application.Run;
  Application.Free;
end.

