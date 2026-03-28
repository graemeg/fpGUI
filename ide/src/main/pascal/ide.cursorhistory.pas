{
    fpGUI IDE - Cursor History

    Back/forward navigation stack for cursor positions.
    Records (filename, line, col) on navigation jumps and
    supports Alt+Left / Alt+Right traversal.

    Pure logic unit — no GUI dependencies.
}
unit ide.cursorhistory;

{$mode objfpc}{$H+}

interface

type

  TCursorLocation = record
    Filename: string;
    Line: Integer;      // 0-based (matches CaretPos_V)
    Col: Integer;       // 0-based (matches CaretPos_H)
  end;

  { TCursorHistory }

  TCursorHistory = class
  private
    FItems: array of TCursorLocation;
    FCount: Integer;
    FIndex: Integer;     // points to current position, -1 when empty
    FCapacity: Integer;
    function IsDuplicate(const ALoc: TCursorLocation): Boolean;
    procedure DropOldest;
  public
    constructor Create(ACapacity: Integer = 50);
    { Unconditional push. Truncates forward history, deduplicates same file+line. }
    procedure RecordLocation(const ALoc: TCursorLocation);
    { Only records if at the tip (not mid-traversal). Used by back handler
      to capture departure point on first GoBack without corrupting the
      stack during subsequent traversal. }
    procedure RecordBeforeJump(const ACurrent: TCursorLocation);
    function GoBack(out ALoc: TCursorLocation): Boolean;
    function GoForward(out ALoc: TCursorLocation): Boolean;
    function CanGoBack: Boolean;
    function CanGoForward: Boolean;
    { True when FIndex < FCount - 1 (we have gone back from the tip). }
    function IsTraversing: Boolean;
    procedure Clear;
    property Count: Integer read FCount;
  end;


implementation

{ TCursorHistory }

constructor TCursorHistory.Create(ACapacity: Integer);
begin
  inherited Create;
  FCapacity := ACapacity;
  SetLength(FItems, FCapacity);
  FCount := 0;
  FIndex := -1;
end;

function TCursorHistory.IsDuplicate(const ALoc: TCursorLocation): Boolean;
begin
  Result := False;
  if FIndex < 0 then
    Exit;
  Result := (FItems[FIndex].Filename = ALoc.Filename)
        and (FItems[FIndex].Line = ALoc.Line);
end;

procedure TCursorHistory.DropOldest;
var
  i: Integer;
begin
  for i := 1 to FCount - 1 do
    FItems[i - 1] := FItems[i];
  Dec(FCount);
  Dec(FIndex);
end;

procedure TCursorHistory.RecordLocation(const ALoc: TCursorLocation);
begin
  if IsDuplicate(ALoc) then
    Exit;

  { Truncate forward history if we are mid-traversal }
  if FIndex >= 0 then
    FCount := FIndex + 1;

  { Handle capacity overflow }
  if FCount >= FCapacity then
    DropOldest;

  { Append new entry }
  FItems[FCount] := ALoc;
  FIndex := FCount;
  Inc(FCount);
end;

procedure TCursorHistory.RecordBeforeJump(const ACurrent: TCursorLocation);
begin
  if not IsTraversing then
    RecordLocation(ACurrent);
end;

function TCursorHistory.GoBack(out ALoc: TCursorLocation): Boolean;
begin
  Result := CanGoBack;
  if not Result then
    Exit;
  Dec(FIndex);
  ALoc := FItems[FIndex];
end;

function TCursorHistory.GoForward(out ALoc: TCursorLocation): Boolean;
begin
  Result := CanGoForward;
  if not Result then
    Exit;
  Inc(FIndex);
  ALoc := FItems[FIndex];
end;

function TCursorHistory.CanGoBack: Boolean;
begin
  Result := FIndex > 0;
end;

function TCursorHistory.CanGoForward: Boolean;
begin
  Result := (FIndex >= 0) and (FIndex < FCount - 1);
end;

function TCursorHistory.IsTraversing: Boolean;
begin
  Result := (FCount > 0) and (FIndex >= 0) and (FIndex < FCount - 1);
end;

procedure TCursorHistory.Clear;
begin
  FCount := 0;
  FIndex := -1;
end;

end.
