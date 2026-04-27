{
    fpGUI IDE - Build Dispatch

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Extracted from ide.form.main.pas — separates build dispatch logic
      from the main form.

      DetectModuleFromTreeNode is a pure function that validates a tree
      node and returns the module name if it holds a TAggregatorModuleInfo.

      StartModuleBuild creates and starts a TBuilderThread for a given
      goal and module name.
}
unit ide.build.dispatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_tree,
  ide.builder.thread,
  ide.project.pasbuild;

{ Pure function: given a tree node, return the module name if the node
  holds a TAggregatorModuleInfo. Returns '' for nil, nil data, or
  category tag nodes. }
function DetectModuleFromTreeNode(ANode: TfpgTreeNode): string;

{ Create and start a build thread for a specific module.
  Returns the created thread (caller does not need to free it —
  TThread.FreeOnTerminate is not set; the form's BuildTerminated
  handler manages cleanup). }
function StartModuleBuild(
  const AGoal: string;
  const AModuleName: string;
  AOnTerminate: TNotifyEvent;
  AOnOutput: TOutputLineEvent
): TBuilderThread;

implementation

function DetectModuleFromTreeNode(ANode: TfpgTreeNode): string;
begin
  Result := '';
  if ANode = nil then
    Exit;
  if ANode.Data = nil then
    Exit;
  if PtrInt(ANode.Data) <= 4 then
    Exit;
  if not (TObject(ANode.Data) is TAggregatorModuleInfo) then
    Exit;
  Result := TAggregatorModuleInfo(ANode.Data).Name;
end;

function StartModuleBuild(
  const AGoal: string;
  const AModuleName: string;
  AOnTerminate: TNotifyEvent;
  AOnOutput: TOutputLineEvent
): TBuilderThread;
begin
  Result := TBuilderThread.Create(True);
  Result.BuildGoal := AGoal;
  Result.BuildModule := AModuleName;
  Result.OnTerminate := AOnTerminate;
  Result.OnAvailableOutput := AOnOutput;
  Result.Resume;
end;

end.
