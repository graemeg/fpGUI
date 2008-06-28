{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Splitter control.
}

unit gui_splitter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gfxbase,
  gfx_widget;

type

  NaturalNumber = 1..High(Integer);

  { TfpgSplitter }

  TfpgSplitter = class(TfpgWidget)
  private
    FAutoSnap: Boolean;
    FControl: TfpgWidget;
    FDownPos: TPoint;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldSize: Integer;
    FSplit: Integer;
    procedure   CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    function    FindControl: TfpgWidget;
    procedure   UpdateControlSize;
    procedure   UpdateSize(X, Y: Integer);
  protected
    function    DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandlePaint; override;
    procedure   StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

function CreateSplitter(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
         AnAlign: TAlign): TfpgSplitter;

implementation

function CreateSplitter(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
         AnAlign: TAlign): TfpgSplitter;
begin
  Result        := TfpgSplitter.Create(AOwner);
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Align  := AnAlign;
end;

{ TfpgSplitter }

procedure TfpgSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft:   S := FControl.Width  + Split;
    alRight:  S := FControl.Width  - Split;
    alTop:    S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

function TfpgSplitter.FindControl: TfpgWidget;
var
  i: Integer;
  wg: TfpgWidget;
  p: TPoint;
  r: TfpgRect;
begin
  Result := nil;
  p := Point(Left, Top);
  case Align of
    alLeft: Dec(p.X);
    alRight: Inc(p.X, Width);
    alTop: Dec(p.Y);
    alBottom: Inc(p.Y, Height);
  else
    Exit;
  end;

  for i := 0 to Parent.ComponentCount-1 do
  begin
    wg := TfpgWidget(Parent.Components[i]);
    if (wg <> nil) and wg.Visible and wg.Enabled then
    begin
      Result := wg;
      r := Result.GetBoundsRect;
      if (r.Width = 0) then
        if Align in [alTop, alLeft] then
          Dec(r.Left)
        else
          Inc(r.Width);
      if (r.Height = 0) then
        if Align in [alTop, alLeft] then
          Dec(r.Top)
        else
          Inc(r.Height);
      if PtInRect(r, p) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TfpgSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft, alRight: // FControl.Width  := FNewSize; // (1)
         FControl.MoveAndResize(FControl.Left, FControl.Top, FNewSize, FControl.Height); // (2)
      alTop, alBottom: // FControl.Height := FNewSize; // (1)
         FControl.MoveAndResize(FControl.Left, FControl.Top, FControl.Width, FNewSize);  // (2)
    end;
    // FControl.UpdateWindowPosition; // (1)
    // vvzh:
    // Lines marked with (1) work wrong under Linux (e.g. folding/unfolding Memo1)
    // Lines marked with (2) work OK under both platforms. Why?
    Parent.Realign;
    // if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TfpgSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

function TfpgSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  // Result := CanResize(NewSize); // omit onCanResize call
  Result := True;
  if Result and (NewSize <= FMinSize) and FAutoSnap then
    NewSize := 0;
end;

procedure TfpgSplitter.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  i: integer;
  wg: TfpgWidget;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  FControl := FindControl;
  FDownPos := Point(X, Y);
  
  if Assigned(FControl) then
  begin
    if Align in [alLeft, alRight] then
    begin
      FMaxSize := Parent.Width - FMinSize;
      for i := 0 to Parent.ComponentCount-1 do
      begin
        wg := TfpgWidget(Parent.Components[i]);
        if wg.Visible and (wg.Align in [alLeft, alRight]) then
          Dec(FMaxSize, Width);
      end;
      Inc(FMaxSize, FControl.Width);
    end
    else
    begin
      FMaxSize := Parent.Height - FMinSize;
      for i := 0 to Parent.ComponentCount-1 do
      begin
        wg := TfpgWidget(Parent.Components[i]);
        if (wg.Align in [alTop, alBottom]) then
          Dec(FMaxSize, Height);
      end;
      Inc(FMaxSize, FControl.Height);
    end;
    UpdateSize(X, Y);
    CaptureMouse;
    {AllocateLineDC;
    with ValidParentForm(Self) do
      if ActiveControl <> nil then
      begin
        FActiveControl := ActiveControl;
        FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
        TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
      end;
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;}
  end;
end;

procedure TfpgSplitter.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  if Assigned(FControl) then
  begin
    ReleaseMouse;
    // if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    {writeln('LT: ', FControl.Left, ':', FControl.Width, '  ', Self.Left, ':', Self.Width);
    writeln('RB: ', FControl.Top, ':', FControl.Height, '  ', Self.Top, ':', Self.Height);}
    StopSizing;
  end;
end;

procedure TfpgSplitter.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
var
  NewSize, Split: Integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  if (ssLeft in shiftstate) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      // if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      // if ResizeStyle = rsUpdate then
      UpdateControlSize;
      // if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TfpgSplitter.HandleMouseEnter;
begin
  if Align in [alBottom, alTop] then
    MouseCursor := mcSizeNS
  else
    MouseCursor := mcSizeEW;
end;

procedure TfpgSplitter.HandleMouseExit;
begin
  if FControl = nil then
    MouseCursor := mcDefault;
end;

procedure TfpgSplitter.HandlePaint;
begin
  Canvas.SetColor(clWindowBackground);
  Canvas.FillRectangle(GetClientBounds);
end;

procedure TfpgSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    // if FLineVisible then DrawLine;
    FControl := nil;
    {ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;}
  end;
  {if Assigned(FOnMoved) then
    FOnMoved(Self);}
end;

constructor TfpgSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Height := 100;
  Align := alLeft;
  Width := 2;
  FMinSize := 30;
  // FResizeStyle := rsPattern;
  FOldSize := -1;
end;

destructor TfpgSplitter.Destroy;
begin
  inherited Destroy;
end;

end.
