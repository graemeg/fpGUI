{
    fpGUI  -  Free Pascal Graphical User Interface

    GelDirty  -  Window dirty list (redrawing queue)

    Copyright (C) 2000 - 2006 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit GELDirty;

{$IFDEF Debug}
  {$ASSERTIONS On}
{$ENDIF}

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface
uses
  Classes,
  GfxBase;		// fpGFX units


type

  PDirtyListEntry = ^TDirtyListEntry;
  TDirtyListEntry = record
    Prev, Next: PDirtyListEntry;
    Window: TFCustomWindow;
    Rect: TRect;
  end;


  TDirtyList = class
  private
    FFirst, FLast: PDirtyListEntry;
  protected
    procedure   RemoveEntry(AEntry: PDirtyListEntry);
  public
    destructor  Destroy; override;
    procedure   AddRect(AWindow: TFCustomWindow; const ARect: TRect);
    procedure   ClearQueueForWindow(AWindow: TFCustomWindow);
    procedure   PaintQueueForWindow(AWindow: TFCustomWindow);
    procedure   PaintAll;
    property    First: PDirtyListEntry read FFirst write FFirst;  // !!!
  end;


implementation


destructor TDirtyList.Destroy;
var
  Entry, NextEntry: PDirtyListEntry;
begin
  Entry := FFirst;
  while Assigned(Entry) do
  begin
    NextEntry := Entry^.Next;
    Dispose(Entry);
    Entry := NextEntry;
  end;
  inherited Destroy;
end;


procedure TDirtyList.AddRect(AWindow: TFCustomWindow; const ARect: TRect);
var
  Entry, NextEntry: PDirtyListEntry;
begin
  // Check for empty or invalid update rectangle
  if (ARect.Left >= ARect.Right) or (ARect.Top >= ARect.Bottom) or
    (ARect.Right < 0) or (ARect.Top < 0) or
    (ARect.Left >= AWindow.ClientWidth) or
    (ARect.Top >= AWindow.ClientHeight) then
    exit;

  { Check if the new rectangle is already contained in some other rectangle
    in the dirty list for the same window }
  Entry := FFirst;
  while Assigned(Entry) do
  begin
    NextEntry := Entry^.Next;
    with Entry^.Rect do
      if AWindow = Entry^.Window then
        if (ARect.Left >= Left) and (ARect.Top >= Top) and
          (ARect.Right <= Right) and (ARect.Bottom <= Bottom) then
	  // Rectangle is already contained in dirt list -> do nothing
	  exit
	else if (Left >= ARect.Left) and (Top >= ARect.Top) and
	  (Right <= ARect.Right) and (Bottom <= ARect.Bottom) then
	begin
	  // The new rectangle contains the currently checked rectangle
	  Entry^.Rect := ARect;
	  exit;
	end;
    Entry := NextEntry;
  end;


  // If we got this far, then we really have to add the rectangle to our list

  New(Entry);
  Entry^.Window := AWindow;
  Entry^.Rect := ARect;
  Entry^.Next := nil;

  if Assigned(FFirst) then
  begin
    Entry^.Prev := FLast;
    FLast^.Next := Entry;
    FLast := Entry;
  end else
  begin
    Entry^.Prev := nil;
    FFirst := Entry;
    FLast := Entry;
  end;
end;


procedure TDirtyList.ClearQueueForWindow(AWindow: TFCustomWindow);
var
  Entry, NextEntry: PDirtyListEntry;
begin
  Entry := FFirst;
  while Assigned(Entry) do
  begin
    NextEntry := Entry^.Next;
    if Entry^.Window = AWindow then
      RemoveEntry(Entry);
    Entry := NextEntry;
  end;
end;


procedure TDirtyList.PaintQueueForWindow(AWindow: TFCustomWindow);
var
  IsNotEmpty: Boolean;
  Entry, NextEntry: PDirtyListEntry;
begin
  IsNotEmpty := False;
  AWindow.Canvas.SaveState;
  AWindow.Canvas.EmptyClipRect;

  Entry := First;
  while Assigned(Entry) do
  begin
    NextEntry := Entry^.Next;
    if Entry^.Window = AWindow then
    begin
      IsNotEmpty := AWindow.Canvas.UnionClipRect(Entry^.Rect);
      RemoveEntry(Entry);
    end;
    Entry := NextEntry;
  end;

  if IsNotEmpty and Assigned(AWindow.OnPaint) then
    AWindow.OnPaint(AWindow);

  AWindow.Canvas.RestoreState;
end;


procedure TDirtyList.PaintAll;
begin
  while Assigned(FFirst) do
    PaintQueueForWindow(FFirst^.Window);
end;


procedure TDirtyList.RemoveEntry(AEntry: PDirtyListEntry);
begin
  if Assigned(AEntry^.Prev) then
    AEntry^.Prev^.Next := AEntry^.Next
  else
    FFirst := AEntry^.Next;
  if Assigned(AEntry^.Next) then
    AEntry^.Next^.Prev := AEntry^.Prev
  else
    FLast := AEntry^.Prev;
  Dispose(AEntry);
end;


end.

