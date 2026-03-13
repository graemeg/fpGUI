{
    fpGUI IDE — Undo/Redo System

    Command-pattern based undo/redo for the text editor widget.
    Each text mutation is represented as a TUndoAction subclass that
    can execute, undo, and redo itself against a TStringList model.

    TUndoManager orchestrates the undo/redo stacks, action merging
    (consecutive keystrokes), and optional max-depth limiting.
}
unit ide.editor.undo;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  { TUndoAction — abstract base for all undoable actions }

  TUndoAction = class
  public
    procedure Execute; virtual; abstract;
    procedure Undo; virtual; abstract;
    procedure Redo; virtual;
    { Returns True if AOther was merged into this action }
    function TryMerge(AOther: TUndoAction): Boolean; virtual;
  end;


  { TInsertTextAction — inserts text at a given line/column position }

  TInsertTextAction = class(TUndoAction)
  private
    FLines: TStringList;
    FLineIndex: Integer;
    FColPos: Integer;
    FText: string;
  public
    constructor Create(ALines: TStringList; ALineIndex, AColPos: Integer; const AText: string);
    procedure Execute; override;
    procedure Undo; override;
    function TryMerge(AOther: TUndoAction): Boolean; override;
  end;


  { TDeleteTextAction — deletes ACount characters starting at line/column }

  TDeleteTextAction = class(TUndoAction)
  private
    FLines: TStringList;
    FLineIndex: Integer;
    FColPos: Integer;
    FCount: Integer;
    FDeletedText: string;  { saved on Execute for Undo }
  public
    constructor Create(ALines: TStringList; ALineIndex, AColPos, ACount: Integer);
    procedure Execute; override;
    procedure Undo; override;
    procedure Redo; override;
    function TryMerge(AOther: TUndoAction): Boolean; override;
  end;


  { TSplitLineAction — splits a line at a given column, optionally adding indent }

  TSplitLineAction = class(TUndoAction)
  private
    FLines: TStringList;
    FLineIndex: Integer;
    FColPos: Integer;
    FIndent: string;
  public
    constructor Create(ALines: TStringList; ALineIndex, AColPos: Integer); overload;
    constructor Create(ALines: TStringList; ALineIndex, AColPos: Integer; const AIndent: string); overload;
    procedure Execute; override;
    procedure Undo; override;
  end;


  { TJoinLinesAction — joins ALineIndex onto the previous line }

  TJoinLinesAction = class(TUndoAction)
  private
    FLines: TStringList;
    FLineIndex: Integer;
    FJoinCol: Integer;     { column where the join happened (length of previous line) }
    FJoinedText: string;   { text of the line that was removed }
  public
    constructor Create(ALines: TStringList; ALineIndex: Integer);
    procedure Execute; override;
    procedure Undo; override;
  end;


  { TCompoundAction — groups multiple actions as a single undo step }

  TCompoundAction = class(TUndoAction)
  private
    FActions: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AAction: TUndoAction);
    procedure Execute; override;
    procedure Undo; override;
    procedure Redo; override;
  end;


  { TUndoManager — manages undo/redo stacks with action merging }

  TUndoManager = class
  private
    FUndoStack: TList;
    FRedoStack: TList;
    FMaxUndoLevels: Integer;
    FMergeBroken: Boolean;
    procedure ClearStack(AStack: TList);
    procedure TrimUndoStack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteAction(AAction: TUndoAction);
    procedure Undo;
    procedure Redo;
    procedure Clear;
    procedure BreakMerge;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    function UndoCount: Integer;
    function RedoCount: Integer;
    property MaxUndoLevels: Integer read FMaxUndoLevels write FMaxUndoLevels;
  end;


implementation


{ =========================================================================
    TUndoAction
  ========================================================================= }

procedure TUndoAction.Redo;
begin
  Execute;
end;

function TUndoAction.TryMerge(AOther: TUndoAction): Boolean;
begin
  Result := False;
end;


{ =========================================================================
    TInsertTextAction
  ========================================================================= }

constructor TInsertTextAction.Create(ALines: TStringList; ALineIndex, AColPos: Integer;
  const AText: string);
begin
  inherited Create;
  FLines := ALines;
  FLineIndex := ALineIndex;
  FColPos := AColPos;
  FText := AText;
end;

procedure TInsertTextAction.Execute;
var
  Line: string;
begin
  Line := FLines[FLineIndex];
  Insert(FText, Line, FColPos + 1);  { FColPos is 0-based, Insert is 1-based }
  FLines[FLineIndex] := Line;
end;

procedure TInsertTextAction.Undo;
var
  Line: string;
begin
  Line := FLines[FLineIndex];
  Delete(Line, FColPos + 1, Length(FText));
  FLines[FLineIndex] := Line;
end;

function TInsertTextAction.TryMerge(AOther: TUndoAction): Boolean;
var
  Other: TInsertTextAction;
begin
  Result := False;
  if not (AOther is TInsertTextAction) then
    Exit;
  Other := TInsertTextAction(AOther);
  { Merge if same line and the new insert follows immediately after this one }
  if (Other.FLineIndex = FLineIndex) and
     (Other.FColPos = FColPos + Length(FText)) and
     (Length(Other.FText) = 1) then
  begin
    FText := FText + Other.FText;
    Result := True;
  end;
end;


{ =========================================================================
    TDeleteTextAction
  ========================================================================= }

constructor TDeleteTextAction.Create(ALines: TStringList; ALineIndex, AColPos, ACount: Integer);
begin
  inherited Create;
  FLines := ALines;
  FLineIndex := ALineIndex;
  FColPos := AColPos;
  FCount := ACount;
  FDeletedText := '';
end;

procedure TDeleteTextAction.Execute;
var
  Line: string;
begin
  Line := FLines[FLineIndex];
  FDeletedText := Copy(Line, FColPos + 1, FCount);
  Delete(Line, FColPos + 1, FCount);
  FLines[FLineIndex] := Line;
end;

procedure TDeleteTextAction.Undo;
var
  Line: string;
begin
  Line := FLines[FLineIndex];
  Insert(FDeletedText, Line, FColPos + 1);
  FLines[FLineIndex] := Line;
end;

procedure TDeleteTextAction.Redo;
var
  Line: string;
begin
  { On redo we already know what was deleted, just delete again }
  Line := FLines[FLineIndex];
  Delete(Line, FColPos + 1, Length(FDeletedText));
  FLines[FLineIndex] := Line;
end;

function TDeleteTextAction.TryMerge(AOther: TUndoAction): Boolean;
var
  Other: TDeleteTextAction;
begin
  Result := False;
  if not (AOther is TDeleteTextAction) then
    Exit;
  Other := TDeleteTextAction(AOther);
  { Merge consecutive backspaces: other deletes at position just before this one }
  if (Other.FLineIndex = FLineIndex) and
     (Other.FCount = 1) and
     (Other.FColPos = FColPos - 1) then
  begin
    { Prepend the newly deleted char — backspace goes backwards }
    FDeletedText := Other.FDeletedText + FDeletedText;
    FColPos := Other.FColPos;
    FCount := FCount + 1;
    Result := True;
  end;
end;


{ =========================================================================
    TSplitLineAction
  ========================================================================= }

constructor TSplitLineAction.Create(ALines: TStringList; ALineIndex, AColPos: Integer);
begin
  inherited Create;
  FLines := ALines;
  FLineIndex := ALineIndex;
  FColPos := AColPos;
  FIndent := '';
end;

constructor TSplitLineAction.Create(ALines: TStringList; ALineIndex, AColPos: Integer;
  const AIndent: string);
begin
  inherited Create;
  FLines := ALines;
  FLineIndex := ALineIndex;
  FColPos := AColPos;
  FIndent := AIndent;
end;

procedure TSplitLineAction.Execute;
var
  Line, Tail: string;
begin
  Line := FLines[FLineIndex];
  Tail := FIndent + Copy(Line, FColPos + 1, MaxInt);
  FLines[FLineIndex] := Copy(Line, 1, FColPos);
  FLines.Insert(FLineIndex + 1, Tail);
end;

procedure TSplitLineAction.Undo;
var
  Top, Bottom, Joined: string;
begin
  Top := FLines[FLineIndex];
  Bottom := FLines[FLineIndex + 1];
  { Remove the indent that was added, then rejoin }
  Joined := Top + Copy(Bottom, Length(FIndent) + 1, MaxInt);
  FLines[FLineIndex] := Joined;
  FLines.Delete(FLineIndex + 1);
end;


{ =========================================================================
    TJoinLinesAction
  ========================================================================= }

constructor TJoinLinesAction.Create(ALines: TStringList; ALineIndex: Integer);
begin
  inherited Create;
  FLines := ALines;
  FLineIndex := ALineIndex;
  FJoinCol := 0;
  FJoinedText := '';
end;

procedure TJoinLinesAction.Execute;
begin
  FJoinedText := FLines[FLineIndex];
  FJoinCol := Length(FLines[FLineIndex - 1]);
  FLines[FLineIndex - 1] := FLines[FLineIndex - 1] + FJoinedText;
  FLines.Delete(FLineIndex);
end;

procedure TJoinLinesAction.Undo;
var
  Line: string;
begin
  Line := FLines[FLineIndex - 1];
  FLines[FLineIndex - 1] := Copy(Line, 1, FJoinCol);
  FLines.Insert(FLineIndex, FJoinedText);
end;


{ =========================================================================
    TCompoundAction
  ========================================================================= }

constructor TCompoundAction.Create;
begin
  inherited Create;
  FActions := TList.Create;
end;

destructor TCompoundAction.Destroy;
var
  i: Integer;
begin
  for i := 0 to FActions.Count - 1 do
    TUndoAction(FActions[i]).Free;
  FActions.Free;
  inherited Destroy;
end;

procedure TCompoundAction.Add(AAction: TUndoAction);
begin
  FActions.Add(AAction);
end;

procedure TCompoundAction.Execute;
var
  i: Integer;
begin
  for i := 0 to FActions.Count - 1 do
    TUndoAction(FActions[i]).Execute;
end;

procedure TCompoundAction.Undo;
var
  i: Integer;
begin
  { Undo in reverse order }
  for i := FActions.Count - 1 downto 0 do
    TUndoAction(FActions[i]).Undo;
end;

procedure TCompoundAction.Redo;
var
  i: Integer;
begin
  for i := 0 to FActions.Count - 1 do
    TUndoAction(FActions[i]).Redo;
end;


{ =========================================================================
    TUndoManager
  ========================================================================= }

constructor TUndoManager.Create;
begin
  inherited Create;
  FUndoStack := TList.Create;
  FRedoStack := TList.Create;
  FMaxUndoLevels := 0;  { 0 = unlimited }
  FMergeBroken := False;
end;

destructor TUndoManager.Destroy;
begin
  ClearStack(FUndoStack);
  ClearStack(FRedoStack);
  FUndoStack.Free;
  FRedoStack.Free;
  inherited Destroy;
end;

procedure TUndoManager.ClearStack(AStack: TList);
var
  i: Integer;
begin
  for i := 0 to AStack.Count - 1 do
    TUndoAction(AStack[i]).Free;
  AStack.Clear;
end;

procedure TUndoManager.TrimUndoStack;
begin
  if FMaxUndoLevels <= 0 then
    Exit;
  while FUndoStack.Count > FMaxUndoLevels do
  begin
    TUndoAction(FUndoStack[0]).Free;
    FUndoStack.Delete(0);
  end;
end;

procedure TUndoManager.ExecuteAction(AAction: TUndoAction);
var
  Top: TUndoAction;
begin
  AAction.Execute;

  { Try to merge with the top of the undo stack }
  if (not FMergeBroken) and (FUndoStack.Count > 0) then
  begin
    Top := TUndoAction(FUndoStack[FUndoStack.Count - 1]);
    if Top.TryMerge(AAction) then
    begin
      AAction.Free;
      { Clear redo stack — new action invalidates redo history }
      ClearStack(FRedoStack);
      Exit;
    end;
  end;

  FMergeBroken := False;
  FUndoStack.Add(AAction);
  TrimUndoStack;

  { Clear redo stack — new action invalidates redo history }
  ClearStack(FRedoStack);
end;

procedure TUndoManager.Undo;
var
  Action: TUndoAction;
begin
  if FUndoStack.Count = 0 then
    Exit;
  Action := TUndoAction(FUndoStack[FUndoStack.Count - 1]);
  FUndoStack.Delete(FUndoStack.Count - 1);
  Action.Undo;
  FRedoStack.Add(Action);
  FMergeBroken := True;
end;

procedure TUndoManager.Redo;
var
  Action: TUndoAction;
begin
  if FRedoStack.Count = 0 then
    Exit;
  Action := TUndoAction(FRedoStack[FRedoStack.Count - 1]);
  FRedoStack.Delete(FRedoStack.Count - 1);
  Action.Redo;
  FUndoStack.Add(Action);
  FMergeBroken := True;
end;

procedure TUndoManager.Clear;
begin
  ClearStack(FUndoStack);
  ClearStack(FRedoStack);
  FMergeBroken := False;
end;

procedure TUndoManager.BreakMerge;
begin
  FMergeBroken := True;
end;

function TUndoManager.CanUndo: Boolean;
begin
  Result := FUndoStack.Count > 0;
end;

function TUndoManager.CanRedo: Boolean;
begin
  Result := FRedoStack.Count > 0;
end;

function TUndoManager.UndoCount: Integer;
begin
  Result := FUndoStack.Count;
end;

function TUndoManager.RedoCount: Integer;
begin
  Result := FRedoStack.Count;
end;

end.
