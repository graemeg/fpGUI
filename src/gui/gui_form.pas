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
      Defines a Form control. Also known as a Window which holds other
      controls.
}

unit gui_form;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  gfx_widget;

type
  TWindowPosition = (wpUser, wpAuto, wpScreenCenter);


  TfpgForm = class(TfpgWidget)
  private
    FOnActivate: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure   SetBackgroundColor(const AValue: TfpgColor);
  protected
    FModalResult: integer;
    FParentForm: TfpgForm;
    FWindowPosition: TWindowPosition;
    FWindowTitle: string;
    FSizeable: boolean;
    FBackgroundColor: TfpgColor;
    procedure   AdjustWindowStyle; override;
    procedure   SetWindowParameters; override;
    procedure   SetWindowTitle(const ATitle: string); override;
    procedure   MsgActivate(var msg: TfpgMessageRec); message FPGM_ACTIVATE;
    procedure   MsgDeActivate(var msg: TfpgMessageRec); message FPGM_DEACTIVATE;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
    procedure   HandlePaint; override;
    procedure   HandleClose; virtual;
    procedure   HandleHide; override;
    procedure   HandleShow; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   AfterConstruction; override;
    procedure   BeforeDestruction; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; virtual;
    procedure   Show;
    procedure   Hide;
    function    ShowModal: integer;
    procedure   Close;
    property    Sizeable: boolean read FSizeable write FSizeable;
    property    ModalResult: integer read FModalResult write FModalResult;
  published
    {$Note Refactor this to a TfpgCustomForm and only surface it here }
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor default clWindowBackground;
    property    WindowPosition: TWindowPosition read FWindowPosition write FWindowPosition default wpAuto;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    property    OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property    OnClose: TNotifyEvent read FOnClose write FOnClose;
    property    OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property    OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property    OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property    OnHide: TNotifyEvent read FOnHide write FOnHide;
    property    OnPaint;
    property    OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;


function WidgetParentForm(wg: TfpgWidget): TfpgForm;


implementation

uses
  fpgfx,
  gui_menu;
  
type
  // to access protected methods
  TfpgMenuBarFriend = class(TfpgMenuBar)
  end;


function WidgetParentForm(wg: TfpgWidget): TfpgForm;
var
  w: TfpgWidget;
begin
  w := wg;
  while w <> nil do
  begin
    if w is TfpgForm then
    begin
      Result := TfpgForm(w);
      Exit; //==>
    end;
    w := w.Parent;
  end;
  Result := nil;
end;

{ TfpgForm }

procedure TfpgForm.SetWindowTitle(const ATitle: string);
begin
  FWindowTitle := ATitle;
  inherited SetWindowTitle(ATitle);
end;

procedure TfpgForm.MsgActivate(var msg: TfpgMessageRec);
begin
  if (fpgApplication.TopModalForm = nil) or (fpgApplication.TopModalForm = self) then
  begin
    FocusRootWidget := self;
    
    if FFormDesigner <> nil then
    begin
      FFormDesigner.Dispatch(msg);
      Exit;
    end;

    if ActiveWidget = nil then
      ActiveWidget := FindFocusWidget(nil, fsdFirst)
    else
      ActiveWidget.SetFocus;
  end;
  if Assigned(FOnActivate) then
    FOnActivate(self);
end;

procedure TfpgForm.MsgDeActivate(var msg: TfpgMessageRec);
begin
  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(self);
end;

procedure TfpgForm.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited;
  Canvas.Clear(FBackgroundColor);
  Canvas.EndDraw(0, 0, FWidth, FHeight);
end;

procedure TfpgForm.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit; //==>
  FBackgroundColor := AValue;
  RePaint;
end;

procedure TfpgForm.AdjustWindowStyle;
begin
  if fpgApplication.MainForm = nil then
    fpgApplication.MainForm := self;

  if FWindowPosition = wpAuto then
    Include(FWindowAttributes, waAutoPos)
  else
    Exclude(FWindowAttributes, waAutoPos);

  if FWindowPosition = wpScreenCenter then
    Include(FWindowAttributes, waScreenCenterPos)
  else
    Exclude(FWindowAttributes, waScreenCenterPos);

  if FSizeable then
    Include(FWindowAttributes, waSizeable)
  else
    Exclude(FWindowAttributes, waSizeable);
end;

procedure TfpgForm.SetWindowParameters;
begin
  inherited;
  DoSetWindowTitle(FWindowTitle);
end;

constructor TfpgForm.Create(AOwner: TComponent);
begin
  inherited;
  FWindowPosition  := wpAuto;
  FWindowTitle     := '';
  FSizeable        := True;
  FParentForm      := nil;
  FBackgroundColor := clWindowBackground;
  FMinWidth        := 32;
  FMinHeight       := 32;
  FModalResult     := 0;

  AfterCreate;
end;

procedure TfpgForm.AfterCreate;
begin
  // for the user
end;

procedure TfpgForm.Show;
begin
  FVisible := True;
  HandleShow;
end;

function TfpgForm.ShowModal: integer;
begin
  FWindowType := wtModalForm;
  fpgApplication.PushModalForm(self);
  ModalResult     := 0;

  Show;
  // processing messages until this form ends.
  // delivering the remaining messages
  fpgApplication.ProcessMessages;
  repeat
    fpgWaitWindowMessage;
  until (ModalResult <> 0) or (not Visible);

  fpgApplication.PopModalForm;
  Result := ModalResult;
end;

procedure TfpgForm.MsgClose(var msg: TfpgMessageRec);
var
  tmp: IInterface;
begin
  tmp := PrintCallTrace(Classname, 'MsgClose');
  if Assigned(FOnClose) then
    FOnClose(self);
  HandleClose;
end;

procedure TfpgForm.HandleClose;
begin
  Close;
end;

procedure TfpgForm.HandleHide;
begin
  if Assigned(FOnHide) then
    FOnHide(self);
  inherited HandleHide;
end;

procedure TfpgForm.HandleShow;
begin
  inherited HandleShow;
  if Assigned(FOnShow) then
    FOnShow(self);
end;

procedure TfpgForm.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  i: integer;
  wg: TfpgWidget;
begin
//  writeln(Classname, '.Keypress');
  // find the TfpgMenuBar
  if not consumed then
  begin
    for i := 0 to ComponentCount-1 do
    begin
      wg := TfpgWidget(Components[i]);
      if (wg <> nil) and (wg <> self) and (wg is TfpgMenuBar) then
      begin
        TfpgMenuBarFriend(wg).HandleKeyPress(keycode, shiftstate, consumed);
        Break; //==>
      end;
    end;
  end;  { if }

  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TfpgForm.AfterConstruction;
begin
  inherited AfterConstruction;
  if Assigned(FOnCreate) then
    FOnCreate(self);
end;

procedure TfpgForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(FOnDestroy) then
    FOnDestroy(self);
end;

procedure TfpgForm.Hide;
begin
  if (fpgApplication.TopModalForm = self) then
    fpgApplication.PopModalForm;
  HandleHide;
  if ModalResult = 0 then
    ModalResult := -1;
end;

procedure TfpgForm.Close;
begin
  Hide;
  fpgApplication.RemoveComponent(self);
  if fpgApplication.MainForm = self then
    fpgApplication.Terminated := True;
end;


end.

