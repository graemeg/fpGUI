{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Runner thread for executing compiled programs. Launches an
      executable, captures stdout/stderr line-by-line, and supports
      cross-thread termination from the main thread.
}

unit ide.runner.thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, ide.builder.thread;

type

  TRunnerThread = class(TThread)
  private
    FExecutablePath: string;
    FWorkingDirectory: string;
    FParameters: string;
    FExitCode: Integer;
    FWasTerminated: Boolean;
    FOnAvailableOutput: TOutputLineEvent;
    FOutputQueue: TStringList;
    FOutputLock: TCriticalSection;
    FProcessLock: TCriticalSection;
    FProcess: TObject;  { TProcess — stored as TObject to avoid exposing process unit in interface }
    procedure DoFlushOutput;
    procedure SendOutput(const ALine: string);
  protected
    procedure Execute; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    { Call from main thread to kill the running process. Thread-safe. }
    procedure TerminateProcess;
    property ExecutablePath: string read FExecutablePath write FExecutablePath;
    property WorkingDirectory: string read FWorkingDirectory write FWorkingDirectory;
    property Parameters: string read FParameters write FParameters;
    property ExitCode: Integer read FExitCode;
    property WasTerminated: Boolean read FWasTerminated;
    property OnAvailableOutput: TOutputLineEvent read FOnAvailableOutput write FOnAvailableOutput;
  end;


{ Resolve the full path to the project's built executable.
  Returns empty string if it cannot be determined. }
function ResolveProjectExecutablePath: string;


implementation

uses
  process,
  ide.project,
  ide.project.backend,
  ide.project.pasbuild,
  ide.macros;


{ TRunnerThread }

procedure TRunnerThread.AfterConstruction;
begin
  inherited AfterConstruction;
  FreeOnTerminate := True;
  FExitCode := -1;
  FWasTerminated := False;
  FOutputQueue := TStringList.Create;
  FOutputLock := TCriticalSection.Create;
  FProcessLock := TCriticalSection.Create;
end;

destructor TRunnerThread.Destroy;
begin
  FProcessLock.Free;
  FOutputLock.Free;
  FOutputQueue.Free;
  inherited Destroy;
end;

procedure TRunnerThread.SendOutput(const ALine: string);
begin
  FOutputLock.Acquire;
  try
    FOutputQueue.Add(ALine);
  finally
    FOutputLock.Release;
  end;
  Queue(@DoFlushOutput);
end;

procedure TRunnerThread.DoFlushOutput;
var
  i: Integer;
  snapshot: TStringList;
begin
  if not Assigned(FOnAvailableOutput) then
    Exit;
  FOutputLock.Acquire;
  try
    if FOutputQueue.Count = 0 then
      Exit;
  finally
    FOutputLock.Release;
  end;
  snapshot := TStringList.Create;
  try
    FOutputLock.Acquire;
    try
      snapshot.Assign(FOutputQueue);
      FOutputQueue.Clear;
    finally
      FOutputLock.Release;
    end;
    for i := 0 to snapshot.Count - 1 do
      FOnAvailableOutput(Self, snapshot[i]);
  finally
    snapshot.Free;
  end;
end;

procedure TRunnerThread.TerminateProcess;
var
  p: TProcess;
begin
  FProcessLock.Acquire;
  try
    if FProcess <> nil then
    begin
      p := TProcess(FProcess);
      if p.Running then
      begin
        p.Terminate(0);
        FWasTerminated := True;
      end;
    end;
  finally
    FProcessLock.Release;
  end;
end;

procedure TRunnerThread.Execute;
const
  BufSize = 1024;
var
  p: TProcess;
  Buf: string;
  Count: Integer;
  i: Integer;
  LineStart: Integer;
  CurrentLine: string;
begin
  p := TProcess.Create(nil);
  try
    FProcessLock.Acquire;
    try
      FProcess := p;
    finally
      FProcessLock.Release;
    end;

    p.Executable := FExecutablePath;
    if FParameters <> '' then
      p.Parameters.DelimitedText := FParameters;
    if FWorkingDirectory <> '' then
      p.CurrentDirectory := FWorkingDirectory;
    p.Options := [poUsePipes, poStdErrToOutput];
    p.ShowWindow := swoShowNormal;

    try
      p.Execute;
    except
      on E: Exception do
      begin
        SendOutput('Error launching process: ' + E.Message);
        FExitCode := -1;
        Exit;
      end;
    end;

    { Read output line by line }
    CurrentLine := '';
    SetLength(Buf, BufSize);
    repeat
      if (p.Output <> nil) then
        Count := p.Output.Read(Buf[1], Length(Buf))
      else
        Count := 0;
      LineStart := 1;
      i := 1;
      while i <= Count do
      begin
        if Buf[i] in [#10, #13] then
        begin
          CurrentLine := CurrentLine + Copy(Buf, LineStart, i - LineStart);
          SendOutput(CurrentLine);
          CurrentLine := '';
          if (i < Count) and (Buf[i+1] in [#10, #13]) and (Buf[i] <> Buf[i+1]) then
            Inc(i);
          LineStart := i + 1;
        end;
        Inc(i);
      end;
      CurrentLine := Copy(Buf, LineStart, Count - LineStart + 1);
    until Count = 0;
    if CurrentLine <> '' then
      SendOutput(CurrentLine);

    p.WaitOnExit;
    FExitCode := p.ExitCode;
  finally
    FProcessLock.Acquire;
    try
      FProcess := nil;
    finally
      FProcessLock.Release;
    end;
    FreeAndNil(p);
  end;
end;


{ Executable path resolution }

function ResolveProjectExecutablePath: string;
var
  pb: TPasBuildProjectBackend;
begin
  Result := '';
  if GProject = nil then
    Exit;

  if GProject.ProjectFormat = pfPasBuild then
  begin
    pb := TPasBuildProjectBackend(GProject);
    if pb.ActiveModule <> nil then
    begin
      { projectDir + output.directory + executableName }
      if (pb.ActiveModule.ProjectDir <> '') and (pb.ActiveModule.ExecutableName <> '') then
      begin
        Result := IncludeTrailingPathDelimiter(pb.ActiveModule.ProjectDir);
        if pb.ActiveModule.OutputDir <> '' then
          Result := Result + IncludeTrailingPathDelimiter(pb.ActiveModule.OutputDir);
        Result := Result + pb.ActiveModule.ExecutableName;
      end;
    end;
  end
  else
  begin
    { Legacy projects: ProjectDir + TargetFile with macro expansion }
    if GProject.TargetFile <> '' then
    begin
      Result := GProject.TargetFile;
      Result := GMacroList.ExpandMacro(Result);
      if (Result <> '') and (Result[1] <> PathDelim) then
        Result := IncludeTrailingPathDelimiter(GProject.ProjectDir) + Result;
    end;
  end;
end;


end.
