{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Builder thread for compiling projects. Supports both legacy
      FPC-direct builds and PasBuild goal execution (compile, clean,
      test, rebuild).
}

unit ide.builder.thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TOutputLineEvent = procedure(Sender: TObject; const ALine: string) of object;

  TBuilderThread = class(TThread)
  private
    FBuildMode: integer;
    FBuildGoal: string;
    FBuildModule: string;
    FOnAvailableOutput: TOutputLineEvent;
    FOutputQueue: TStringList;
    FOutputLock: TCriticalSection;
    procedure DoFlushOutput;
    procedure SendOutput(const ALine: string);
    function  RunCommand(const ACmd: string; const AWorkDir: string): Integer;
  protected
    procedure Execute; override;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property  BuildMode: integer read FBuildMode write FBuildMode;
    { PasBuild goal: 'compile', 'clean', 'test', 'rebuild'.
      Empty string means default compile (backward compatible). }
    property  BuildGoal: string read FBuildGoal write FBuildGoal;
    { For aggregator projects: build a specific module. Empty = build all. }
    property  BuildModule: string read FBuildModule write FBuildModule;
    property  OnAvailableOutput: TOutputLineEvent read FOnAvailableOutput write FOnAvailableOutput;
  end;

implementation

uses
  ide.project.backend
  ,ide.project
  ,ide.project.pasbuild
  ,process
  ,fpg_base
  ,fpg_iniutils
  ,fpg_utils
  ,ide.consts
  ,ide.macros
  ;

{ TBuilderThread }

procedure TBuilderThread.AfterConstruction;
begin
  inherited AfterConstruction;
  FBuildMode := -1;  // signals use of project's default build mode
  FBuildGoal := '';
  FBuildModule := '';
  FreeOnTerminate := True;
  FOutputQueue := TStringList.Create;
  FOutputLock  := TCriticalSection.Create;
end;

destructor TBuilderThread.Destroy;
begin
  FOutputLock.Free;
  FOutputQueue.Free;
  inherited Destroy;
end;

{ Push a line into the queue and schedule a non-blocking flush on the main
  thread. Queue() returns immediately so the builder thread is never blocked
  waiting for the GUI to catch up. }
procedure TBuilderThread.SendOutput(const ALine: string);
begin
  FOutputLock.Acquire;
  try
    FOutputQueue.Add(ALine);
  finally
    FOutputLock.Release;
  end;
  Queue(@DoFlushOutput);
end;

{ Called on the main thread by CheckSynchronize. Drains all lines that have
  accumulated since the last flush and fires the output event for each one.
  Because every SendOutput() call queues a DoFlushOutput, many of these
  invocations will find the queue already drained by an earlier call in the
  same CheckSynchronize batch — so we exit early before allocating. }
procedure TBuilderThread.DoFlushOutput;
var
  i: integer;
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

function TBuilderThread.RunCommand(const ACmd: string; const AWorkDir: string): Integer;
const
  BufSize = 1024;
var
  p: TProcess;
  Buf: string;
  Count: integer;
  i: integer;
  LineStart: integer;
  CurrentLine: string;  { accumulates partial lines across reads }
begin
  p := TProcess.Create(nil);
  try
    p.Options := [poUsePipes, poStdErrToOutPut];
    p.ShowWindow := swoShowNormal;
    p.CurrentDirectory := AWorkDir;
    p.CommandLine := ACmd;
    p.Execute;

    { Process output line by line }
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
            inc(i);
          LineStart := i + 1;
        end;
        inc(i);
      end;
      CurrentLine := Copy(Buf, LineStart, Count - LineStart + 1);
    until Count = 0;
    if CurrentLine <> '' then
      SendOutput(CurrentLine);
    p.WaitOnExit;
    Result := p.ExitCode;
  finally
    FreeAndNil(p);
  end;
end;

procedure TBuilderThread.Execute;
var
  c: TfpgString;
  unitdir: TfpgString;
  Goal: string;
  ExitCode: Integer;
  pb: TPasBuildProjectBackend;
begin
  Goal := FBuildGoal;

  { PasBuild projects with a specific goal }
  if (GProject.ProjectFormat = pfPasBuild) and (Goal <> '') then
  begin
    pb := TPasBuildProjectBackend(GProject);

    if Goal = 'rebuild' then
    begin
      { Rebuild = clean then compile as two separate executions }
      if FBuildModule <> '' then
        c := pb.GenerateModuleGoalCmdLine('clean', FBuildModule)
      else
        c := GProject.GenerateGoalCmdLine('clean');
      SendOutput('Cleaning: ' + c);
      ExitCode := RunCommand(c, pb.GetBuildDir);
      if ExitCode <> 0 then
      begin
        SendOutput('Clean failed (exit code ' + IntToStr(ExitCode) + ')');
        Exit;
      end;
      SendOutput('');
      { Now compile }
      if FBuildModule <> '' then
        c := pb.GenerateModuleGoalCmdLine('compile', FBuildModule)
      else
        c := GProject.GenerateGoalCmdLine('compile');
      SendOutput('Compiling: ' + c);
      RunCommand(c, pb.GetBuildDir);
    end
    else
    begin
      { Single goal: clean, test, compile }
      if FBuildModule <> '' then
        c := pb.GenerateModuleGoalCmdLine(Goal, FBuildModule)
      else
        c := GProject.GenerateGoalCmdLine(Goal);
      SendOutput('Running: ' + c);
      RunCommand(c, pb.GetBuildDir);
    end;
    Exit;
  end;

  { Default path: compile (backward compatible) }
  if GProject.ProjectFormat = pfPasBuild then
  begin
    pb := TPasBuildProjectBackend(GProject);
    if FBuildModule <> '' then
    begin
      c := pb.GenerateModuleGoalCmdLine('compile', FBuildModule);
      SendOutput('Compiling module ' + FBuildModule + ': ' + c);
      RunCommand(c, pb.GetBuildDir);
    end
    else
    begin
      c := GProject.GenerateCmdLine(False, BuildMode);
      SendOutput('Compiling: ' + c);
      RunCommand(c, pb.GetBuildDir);
    end;
  end
  else
  begin
    { Legacy projects create unit output dir and invoke FPC directly }
    unitdir := GProject.ProjectDir + GProject.UnitOutputDir;
    unitdir := GMacroList.ExpandMacro(unitdir);
    if not fpgDirectoryExists(unitdir) then
    begin
      {$IFDEF DEBUG}
      writeln('DEBUG:  TBuilderThread.Execute - Creating dir: ' + unitdir);
      {$ENDIF}
      fpgForceDirectories(unitDir);
    end;
    c := gINI.ReadString(cEnvironment, 'Compiler', '');
    c := c + GProject.GenerateCmdLine(False, BuildMode);
    c := GMacroList.ExpandMacro(c);
    SendOutput('Compiling: ' + c);
    RunCommand(c, GProject.ProjectDir);
  end;
end;

end.
