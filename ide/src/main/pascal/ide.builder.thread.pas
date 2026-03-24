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
  Classes, SysUtils;

type
  TOutputLineEvent = procedure(Sender: TObject; const ALine: string) of object;

  TBuilderThread = class(TThread)
  private
    FBuildMode: integer;
    FBuildGoal: string;
    FOnAvailableOutput: TOutputLineEvent;
    OutputLine: string;
    procedure DoOutputLine;
    procedure SendOutput(const ALine: string);
    function  RunCommand(const ACmd: string; const AWorkDir: string): Integer;
  protected
    procedure Execute; override;
  public
    procedure AfterConstruction; override;
    property  BuildMode: integer read FBuildMode write FBuildMode;
    { PasBuild goal: 'compile', 'clean', 'test', 'rebuild'.
      Empty string means default compile (backward compatible). }
    property  BuildGoal: string read FBuildGoal write FBuildGoal;
    property  OnAvailableOutput: TOutputLineEvent read FOnAvailableOutput write FOnAvailableOutput;
  end;

implementation

uses
  ide.project.backend
  ,ide.project
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
  FreeOnTerminate := True;
end;

procedure TBuilderThread.SendOutput(const ALine: string);
begin
  OutputLine := ALine;
  Synchronize(@DoOutputLine);
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
begin
  p := TProcess.Create(nil);
  try
    p.Options := [poUsePipes, poStdErrToOutPut];
    p.ShowWindow := swoShowNormal;
    p.CurrentDirectory := AWorkDir;
    p.CommandLine := ACmd;
    p.Execute;

    { Process output line by line }
    OutputLine := '';
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
          OutputLine := OutputLine + Copy(Buf, LineStart, i - LineStart);
          Synchronize(@DoOutputLine);
          OutputLine := '';
          if (i < Count) and (Buf[i+1] in [#10, #13]) and (Buf[i] <> Buf[i+1]) then
            inc(i);
          LineStart := i + 1;
        end;
        inc(i);
      end;
      OutputLine := Copy(Buf, LineStart, Count - LineStart + 1);
    until Count = 0;
    if OutputLine <> '' then
      Synchronize(@DoOutputLine);
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
begin
  Goal := FBuildGoal;

  { PasBuild projects with a specific goal }
  if (GProject.ProjectFormat = pfPasBuild) and (Goal <> '') then
  begin
    if Goal = 'rebuild' then
    begin
      { Rebuild = clean then compile as two separate executions }
      c := GProject.GenerateGoalCmdLine('clean');
      SendOutput('Cleaning: ' + c);
      ExitCode := RunCommand(c, GProject.ProjectDir);
      if ExitCode <> 0 then
      begin
        SendOutput('Clean failed (exit code ' + IntToStr(ExitCode) + ')');
        Exit;
      end;
      SendOutput('');
      { Now compile }
      c := GProject.GenerateGoalCmdLine('compile');
      SendOutput('Compiling: ' + c);
      RunCommand(c, GProject.ProjectDir);
    end
    else
    begin
      { Single goal: clean, test, compile }
      c := GProject.GenerateGoalCmdLine(Goal);
      SendOutput('Running: ' + c);
      RunCommand(c, GProject.ProjectDir);
    end;
    Exit;
  end;

  { Default path: compile (backward compatible) }
  if GProject.ProjectFormat = pfPasBuild then
  begin
    c := GProject.GenerateCmdLine(False, BuildMode);
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
  end;

  SendOutput('Compiling: ' + c);
  RunCommand(c, GProject.ProjectDir);
end;

procedure TBuilderThread.DoOutputLine;
begin
  if Assigned(FOnAvailableOutput) then
    FOnAvailableOutput(self, OutputLine);
end;

end.
