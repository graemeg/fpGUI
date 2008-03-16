{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a PopupWindow control. It gets used for things like PopupMenu,
      ComboBox and Calendar controls.
}

unit gfx_popupwindow;

{$mode objfpc}{$H+}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gfx_impl;
  
type

  TfpgPopupWindow = class(TfpgWidget)
  private
    FDontCloseWidget: TfpgWidget;
    FPopupFrame: boolean;
    procedure   SetPopupFrame(const AValue: boolean);
  protected
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
    procedure   AdjustWindowStyle; override;
    procedure   HandleClose; virtual;
    procedure   ProcessPopupFrame; virtual;
    procedure   DoPaintPopupFrame; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   ShowAt(AWidget: TfpgWidget; x, y: TfpgCoord);
    procedure   Close; virtual;
    property    DontCloseWidget: TfpgWidget read FDontCloseWidget write FDontCloseWidget;
    property    PopupFrame: boolean read FPopupFrame write SetPopupFrame;
  end;


procedure ClosePopups;
function  PopupListFirst: TfpgPopupWindow;
function  PopupListFind(AWinHandle: TfpgWinHandle): TfpgPopupWindow;
function  PopupDontCloseWidget(AWidget: TfpgWidget): boolean;


implementation


type
  // Popup window linked list. Maybe we can implement it via a TList as well.
  PPopupListRec = ^PopupListRec;
  PopupListRec = record
    Widget: TfpgPopupWindow;
    Next: PPopupListRec;
  end;

var
  uOriginalFocusRoot: TfpgWidget;
  uFirstPopup: PPopupListRec;
  uLastPopup: PPopupListRec;
  
  
// local helper functions

procedure ClosePopups;
begin
  while uFirstPopup <> nil do
  begin
    {$IFDEF DEBUG}
    writeln('...closing ', uFirstPopup^.Widget.Name);
    {$ENDIF}
    TfpgPopupWindow(uFirstPopup^.Widget).Close;
  end;
end;

procedure PopupListAdd(pw: TfpgPopupWindow);
var
  p: PPopupListRec;
begin
  if pw = nil then
    Exit; //==>

  if uFirstPopup = nil then
    uOriginalFocusRoot := FocusRootWidget;

  FocusRootWidget := pw;

  New(p);
  p^.Widget := pw;
  p^.Next := nil;
  if uFirstPopup = nil then
    uFirstPopup := p
  else
    uLastPopup^.Next := p;
  uLastPopup := p;
end;

procedure PopupListRemove(pw: TfpgPopupWindow);
var
  prevp: PPopupListRec;
  p: PPopupListRec;
  px: PPopupListRec;
begin
  p := uFirstPopup;
  prevp := nil;

  while p <> nil do
  begin
    if p^.Widget = pw then
    begin
      if prevp = nil then
        uFirstPopup := p^.Next
      else
        prevp^.Next := p^.Next;
      if uLastPopup = p then
        uLastPopup := prevp;
      px := p;
      p := p^.Next;
      Dispose(px);
    end
    else
    begin
      prevp := p;
      p := p^.Next;
    end;
  end;

  if uLastPopup <> nil then
    FocusRootWidget := uLastPopup^.Widget
  else
    FocusRootWidget := uOriginalFocusRoot;
end;

function PopupListFirst: TfpgPopupWindow;
begin
  if uFirstPopup <> nil then
    Result := uFirstPopup^.Widget
  else
    Result := nil;
end;


function PopupListFind(AWinHandle: TfpgWinHandle): TfpgPopupWindow;
var
  p: PPopupListRec;
begin
  p := uFirstPopup;
  while p <> nil do
  begin
    if p^.Widget.WinHandle = AWinHandle then
    begin
      Result := p^.Widget;
      Exit; //==>
    end;
    p := p^.Next;
  end;
  Result := nil;
end;

function PopupDontCloseWidget(AWidget: TfpgWidget): boolean;
var
  p: PPopupListRec;
begin
  Result := False;
  if AWidget = nil then
    Exit; //==>

  p := uFirstPopup;
  while p <> nil do
  begin
    if p^.Widget.DontCloseWidget = AWidget then
    begin
      Result := True;
      Exit; //==>
    end;
    p := p^.Next;
  end;
end;


{ TfpgPopupWindow }

procedure TfpgPopupWindow.SetPopupFrame(const AValue: boolean);
begin
  if FPopupFrame <> AValue then
  begin
    FPopupFrame := AValue;
    ProcessPopupFrame;
  end;
end;

procedure TfpgPopupWindow.MsgClose(var msg: TfpgMessageRec);
begin
  {$IFDEF DEBUG}
  writeln('TfpgPopupWindow.MsgClose [', Classname, ']');
  {$ENDIF}
  HandleClose;
end;

procedure TfpgPopupWindow.AdjustWindowStyle;
begin
  inherited AdjustWindowStyle;
  // We could possibly change this later
  Exclude(FWindowAttributes, waSizeable);
end;

procedure TfpgPopupWindow.HandleClose;
begin
  HandleHide;
end;

procedure TfpgPopupWindow.ProcessPopupFrame;
var
  i: integer;
begin
  if PopupFrame then
  begin
    for i := 0 to ComponentCount-1 do
    begin
      if Components[i] is TfpgWidget then
        TfpgWidget(Components[i]).Anchors := [anRight, anBottom];
    end;
    // make space for the frame
    HandleResize(Width+2, Height+2);
    UpdateWindowPosition;

    for i := 0 to ComponentCount-1 do
    begin
      if Components[i] is TfpgWidget then
        TfpgWidget(Components[i]).Anchors := [anLeft, anTop];
    end;
    HandleResize(Width+2, Height+2);
    UpdateWindowPosition;

    Canvas.BeginDraw;
    DoPaintPopupFrame;
    Canvas.EndDraw;
  end;
end;

procedure TfpgPopupWindow.DoPaintPopupFrame;
var
  lColor: TfpgColor;
begin
  lColor := fpgColorToRGB(BackgroundColor);
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.SetColor(clWidgetFrame);
  Canvas.DrawRectangle(0, 0, Width, Height);
  Canvas.SetColor(lColor);
end;

constructor TfpgPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowType := wtPopup;
  FDontCloseWidget := nil;
  Parent := nil;
  FPopupFrame := False;
end;

procedure TfpgPopupWindow.ShowAt(AWidget: TfpgWidget; x, y: TfpgCoord);
var
  pt: TPoint;
begin
  PopupListAdd(self);
  DontCloseWidget := nil;
  // translate coordinates
  pt    := WindowToScreen(AWidget, Point(x, y));
  // reposition
  Left  := pt.X;
  Top   := pt.Y;
  // and show
  HandleShow;
end;

procedure TfpgPopupWindow.Close;
begin
  HandleClose;
  PopupListRemove(self);
  { TODO : Move this out to the GDI specific unit. }
  {$IFDEF MSWINDOWS}
  if uFirstPopup <> nil then
    uFirstPopup^.Widget.CaptureMouse;
  {$ENDIF}
end;


initialization
  uFirstPopup := nil;
  uLastPopup  := nil;

end.

