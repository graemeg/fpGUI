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
  
const
  clColorGrabBar = $839EFE; // Pale navy blue
  cSplitterWidth = 8;

type

  NaturalNumber = 1..High(Integer);


  TfpgSplitter = class(TfpgWidget)
  private
    FAutoSnap: Boolean;
    FColorGrabBar: TfpgColor;
    FControl: TfpgWidget;
    FDownPos: TPoint;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldSize: Integer;
    FSplit: Integer;
    FMouseOver: Boolean;
    procedure   CalcSplitSize(X, Y: Integer; out NewSize, Split: Integer);
    function    FindControl: TfpgWidget;
    procedure   SetColorGrabBar(const AValue: TfpgColor);
    procedure   UpdateControlSize;
    procedure   UpdateSize(const X, Y: Integer);
  protected
    function    DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandlePaint; override;
    procedure   StopSizing; dynamic;
    Procedure   DrawGrabBar(ARect: TfpgRect); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    ColorGrabBar: TfpgColor read FColorGrabBar write SetColorGrabBar default clColorGrabBar;
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

procedure TfpgSplitter.CalcSplitSize(X, Y: Integer; out NewSize, Split: Integer);
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

procedure TfpgSplitter.SetColorGrabBar(const AValue: TfpgColor);
begin
  if FColorGrabBar = AValue then
    Exit; //==>
  FColorGrabBar := AValue;
  Repaint;
end;

procedure TfpgSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft, alRight:
//          FControl.Width  := FNewSize; // (1)
         FControl.SetPosition(FControl.Left, FControl.Top, FNewSize, FControl.Height); // (2)
      alTop, alBottom:
//          FControl.Height := FNewSize; // (1)
         FControl.SetPosition(FControl.Left, FControl.Top, FControl.Width, FNewSize);  // (2)
    end;
//    FControl.UpdateWindowPosition; // (1)
    // vvzh:
    // Lines marked with (1) work wrong under Linux (e.g. folding/unfolding Memo1)
    // Lines marked with (2) work OK under both platforms. Why?
    Parent.Realign;
    // if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TfpgSplitter.UpdateSize(const X, Y: Integer);
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
  FMouseOver := True;
  if Align in [alBottom, alTop] then
    MouseCursor := mcSizeNS
  else
    MouseCursor := mcSizeEW;
  Repaint;
end;

procedure TfpgSplitter.HandleMouseExit;
begin
  FMouseOver := False;
  if FControl = nil then
    MouseCursor := mcDefault;
  Repaint;
end;

procedure TfpgSplitter.HandlePaint;
var
  lRect: TfpgRect;
begin
  Canvas.SetColor(clWindowBackground);
  Canvas.FillRectangle(GetClientRect);
  
  case Align of
    alRight,
    alLeft:
        begin
          lRect.Top    := Height div 4;
          lRect.SetBottom(Height div 4 * 3);
          lRect.Left   := 1;
          lRect.SetRight(6);
        end;

    alTop,
    alBottom:
        begin
          lRect.Left   := Width div 4;
          lRect.SetRight(Width div 4 * 3);
          lRect.Top    := 1;
          lRect.SetBottom(6);
        end;
    end;
  DrawGrabBar(lRect);
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

procedure TfpgSplitter.DrawGrabBar(ARect: TfpgRect);
var
  lFillRect: TfpgRect;
  lSaveColor: TfpgColor;
begin
  lSaveColor := Canvas.Color;

  // Draw the outline of the rectangle
  Canvas.Color := clGray;
  Canvas.DrawRectangle(ARect);

  // If the mouse is over the splitter bar, then fill the grab bar part
  // with colour.
  if FMouseOver then
  begin
    lFillRect := ARect;
    InflateRect(lFillRect, -1, -1);
    Canvas.Color := FColorGrabBar;
    Canvas.FillRectangle(lFillRect);
  end;

  // Draw a shadow around the inside of the grab bar
  Canvas.Color := clWhite;
  Canvas.DrawLine(ARect.Left+1, ARect.Top+1, ARect.Right, ARect.Top+1);
  Canvas.DrawLine(ARect.Left+1, ARect.Top+1, ARect.Left+1, ARect.Bottom);

  // Draw some texture inside the grab bar
  Canvas.SetLineStyle(1, lsDot);
  if Align in [alLeft, alRight] then
  begin
    Canvas.DrawLine(ARect.Left+3, ARect.Top+15, ARect.Left+3, ARect.Bottom-15);
    Canvas.Color := clGray;
    Canvas.DrawLine(ARect.Left+4, ARect.Top+16, ARect.Left+4, ARect.Bottom-16);
  end
  else
  begin
    Canvas.DrawLine(ARect.Left+15, ARect.Top+3, ARect.Right-15, ARect.Top+3);
    Canvas.Color := clGray;
    Canvas.DrawLine(ARect.Left+16, ARect.Top+4, ARect.Right-16, ARect.Top+4);
  end;

  Canvas.SetLineStyle(1, lsSolid);
  Canvas.Color := clBlack;

  { TODO : Improve the look of the triangles }
  case Align of
    alRight:
        begin
          // Draw the top triangle
          Canvas.FillTriangle(ARect.Left+2, ARect.Top+5,
                              ARect.Left+2, ARect.Top+10,
                              ARect.Left+4, ARect.Top+7);
          // Draw the bottom triangle
          Canvas.FillTriangle(ARect.Left+2, ARect.Bottom-5,
                              ARect.Left+2, ARect.Bottom-10,
                              ARect.Left+4, ARect.Bottom-7);
        end;

    alLeft:
        begin
          // Draw the top triangle
          Canvas.FillTriangle(ARect.Right-2, ARect.Top+5,
                              ARect.Right-2, ARect.Top+10,
                              ARect.Right-4, ARect.Top+7);
          // Draw the bottom triangle
          Canvas.FillTriangle(ARect.Right-2, ARect.Bottom-5,
                              ARect.Right-2, ARect.Bottom-10,
                              ARect.Right-4, ARect.Bottom-7);
        end;

    alBottom:
        begin
          // Draw the left triangle
          Canvas.FillTriangle(ARect.Left+5,   ARect.Top+2,
                              ARect.Left+10,  ARect.Top+2,
                              ARect.Left+7,   ARect.Top+4);
          // Draw the right triangle
          Canvas.FillTriangle(ARect.Right-5,  ARect.Top+2,
                              ARect.Right-10, ARect.Top+2,
                              ARect.Right-7,  ARect.Top+4);
        end;

    alTop:
        begin
          // Draw the left triangle
          Canvas.FillTriangle(ARect.Left+5,   ARect.Bottom-1,
                              ARect.Left+10,  ARect.Bottom-1,
                              ARect.Left+7,   ARect.Bottom-4);
          // Draw the right triangle
          Canvas.FillTriangle(ARect.Right-5,  ARect.Bottom-1,
                              ARect.Right-10, ARect.Bottom-1,
                              ARect.Right-7,  ARect.Bottom-4);
        end;
  end;

  Canvas.Color := lSaveColor;
end;

constructor TfpgSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Height := 100;
  Align := alLeft;
  Width := cSplitterWidth;
  FMinSize := 30;
  // FResizeStyle := rsPattern;
  FOldSize := -1;
  FMouseOver := False;
  FColorGrabBar := clColorGrabBar;
end;

destructor TfpgSplitter.Destroy;
begin
  inherited Destroy;
end;

end.
