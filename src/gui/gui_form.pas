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
  TCloseAction = (caNone, caHide, caFree{, caMinimize});
  
  TFormCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TFormCloseQueryEvent = procedure(Sender: TObject; var CanClose: boolean) of object;

  TfpgForm = class(TfpgWidget)
  private
    FFullScreen: boolean;
    FOnActivate: TNotifyEvent;
    FOnClose: TFormCloseEvent;
    FOnCloseQuery: TFormCloseQueryEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
  protected
    FModalResult: integer;
    FParentForm: TfpgForm;
    FWindowPosition: TWindowPosition;
    FWindowTitle: string;
    FSizeable: boolean;
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
    procedure   HandleMove(x, y: TfpgCoord); override;
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   DoOnClose(var CloseAction: TCloseAction); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterConstruction; override;
    procedure   BeforeDestruction; override;
    procedure   AfterCreate; virtual;
    procedure   Show;
    procedure   Hide;
    function    ShowModal: integer;
    procedure   Close;
    function    CloseQuery: boolean; virtual;
    property    Sizeable: boolean read FSizeable write FSizeable;
    property    ModalResult: integer read FModalResult write FModalResult;
    property    FullScreen: boolean read FFullScreen write FFullScreen default False;
  published
    { TODO : Refactor this to a TfpgCustomForm and only surface it here }
    property    BackgroundColor;
    property    TextColor;
    property    WindowPosition: TWindowPosition read FWindowPosition write FWindowPosition default wpAuto;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    // events
    property    OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property    OnClose: TFormCloseEvent read FOnClose write FOnClose;
    property    OnCloseQuery: TFormCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property    OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property    OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property    OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property    OnHide: TNotifyEvent read FOnHide write FOnHide;
    property    OnPaint;
    property    OnResize;
    property    OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;


function WidgetParentForm(wg: TfpgWidget): TfpgForm;


implementation

uses
  fpgfx,
  gfx_popupwindow,
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
  ClosePopups;
  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(self);
end;

procedure TfpgForm.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
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
    
  if FFullScreen then
    Include(FWindowAttributes, waFullScreen)
  else
    Exclude(FWindowAttributes, waFullScreen);
end;

procedure TfpgForm.SetWindowParameters;
begin
  inherited;
  DoSetWindowTitle(FWindowTitle);
end;

constructor TfpgForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindowPosition  := wpAuto;
  FWindowTitle     := '';
  FSizeable        := True;
  FParentForm      := nil;
  FBackgroundColor := clWindowBackground;
  FTextColor       := clText1;
  FMinWidth        := 32;
  FMinHeight       := 32;
  FModalResult     := 0;
  FFullScreen      := False;
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
var
  lCloseAction: TCloseAction;
begin
  FWindowType := wtModalForm;
  fpgApplication.PushModalForm(self);
  ModalResult     := 0;

  Show;

  // processing messages until this form ends.
  // delivering the remaining messages
  fpgApplication.ProcessMessages;
  try
    repeat
      fpgWaitWindowMessage;
    until (ModalResult <> 0) or (not Visible);
  except
    on E: Exception do
    begin
      ModalResult := -1;
      Visible := False;
      fpgApplication.HandleException(self);
    end;
  end;

  fpgApplication.PopModalForm;
  Result := ModalResult;
  
  if ModalResult <> 0 then
  begin
    lCloseAction := caFree; // Dummy variable - we do nothing with it
    DoOnClose(lCloseAction); // Simply so the OnClose event fires.
  end;
end;

procedure TfpgForm.MsgClose(var msg: TfpgMessageRec);
begin
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
  {$IFDEF UNIX}
  { TODO : A temporary work-around because XLib doesn't sent a Resize event
    when the form is created. It's clever enough to size the form beforehand,
    but without this call the Alignment code doesn't execute. }
  inherited HandleResize(FWidth, FHeight);
  {$ENDIF}
end;

procedure TfpgForm.HandleMove(x, y: TfpgCoord);
begin
  ClosePopups;
  inherited HandleMove(x, y);
end;

procedure TfpgForm.HandleResize(awidth, aheight: TfpgCoord);
begin
  ClosePopups;
  inherited HandleResize(awidth, aheight);
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
  AfterCreate;
  if Assigned(FOnCreate) then
    FOnCreate(self);
end;

procedure TfpgForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(FOnDestroy) then
    FOnDestroy(self);
end;

procedure TfpgForm.DoOnClose(var CloseAction: TCloseAction);
begin
  if Assigned(FOnClose) then
    OnClose(self, CloseAction);
end;

procedure TfpgForm.Hide;
begin
  Visible := False;
//  HandleHide;
  if ModalResult = 0 then
    ModalResult := -1;
end;

procedure TfpgForm.Close;
var
  CloseAction: TCloseAction;
  IsMainForm: Boolean;
begin
  if CloseQuery then  // May we close the form? User could override decision
  begin
    IsMainForm := fpgApplication.MainForm = self;
    if IsMainForm then
      CloseAction := caFree
    else
      CloseAction := caHide;

    // execute event handler - maybe user wants to modify it.
    DoOnClose(CloseAction);
    // execute action according to close action
    case CloseAction of
      caHide:
        begin
          Hide;
        end;
        // fpGUI Forms don't have a WindowState property yet!
//      caMinimize: WindowState := wsMinimized;
      caFree:
        begin
          HandleHide;
          if IsMainForm then
            fpgApplication.Terminate
          else
            // We can't free ourselves, somebody else needs to do it
            fpgPostMessage(Self, fpgApplication, FPGM_CLOSE);
        end;
    end;  { case CloseAction }
  end;  { if CloseQuery }
end;

function TfpgForm.CloseQuery: boolean;
begin
  Result := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(self, Result);
end;


end.

