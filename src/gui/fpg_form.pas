{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
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

unit fpg_form;

{$mode objfpc}{$H+}

{.$Define CStackDebug}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_widget;

type
  TWindowPosition = (wpUser, wpAuto, wpScreenCenter, wpOneThirdDown);
  TCloseAction = (caNone, caHide, caFree{, caMinimize});
  
  TFormCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TFormCloseQueryEvent = procedure(Sender: TObject; var CanClose: boolean) of object;
  TfpgHelpEvent = function(AHelpType: THelpType; AHelpContext: THelpContext;
       const AHelpKeyword: String; const AHelpFile: String;
       var AHandled: Boolean): Boolean of object;


  TfpgBaseForm = class(TfpgWidget)
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
    FOnHelp: TfpgHelpEvent;
    FDNDEnabled: boolean;
    procedure   SetDNDEnabled(const AValue: boolean);
  protected
    FModalResult: TfpgModalResult;
    FParentForm: TfpgBaseForm;
    FWindowPosition: TWindowPosition;
    FWindowTitle: string;
    FSizeable: boolean;
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
    procedure   DoOnClose(var CloseAction: TCloseAction); virtual;
    function    DoOnHelp(AHelpType: THelpType; AHelpContext: THelpContext; const AHelpKeyword: String; const AHelpFile: String; var AHandled: Boolean): Boolean; virtual;
    procedure   DoKeyShortcut(const AOrigin: TfpgWidget; const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False); override;
    { -- properties -- }
    property    DNDEnabled: boolean read FDNDEnabled write SetDNDEnabled default False;
    property    Sizeable: boolean read FSizeable write FSizeable;
    property    ModalResult: TfpgModalResult read FModalResult write FModalResult;
    property    FullScreen: boolean read FFullScreen write FFullScreen default False;
    property    WindowPosition: TWindowPosition read FWindowPosition write FWindowPosition default wpAuto;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    { -- events -- }
    property    OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property    OnClose: TFormCloseEvent read FOnClose write FOnClose;
    property    OnCloseQuery: TFormCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property    OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property    OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property    OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property    OnHelp: TfpgHelpEvent read FOnHelp write FOnHelp;
    property    OnHide: TNotifyEvent read FOnHide write FOnHide;
    property    OnShow: TNotifyEvent read FOnShow write FOnShow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    procedure   BeforeDestruction; override;
    procedure   AfterCreate; virtual;
    procedure   AdjustWindowStyle; override;
    procedure   SetWindowParameters; override;
    procedure   InvokeHelp; override;
    procedure   Show;
    procedure   Hide;
    function    ShowModal: TfpgModalResult;
    procedure   Close;
    function    CloseQuery: boolean; virtual;
  end;
  
  
  TfpgForm = class(TfpgBaseForm)
  published
    property    BackgroundColor;
    property    DNDEnabled;
    property    FullScreen;
    property    Height;
    property    Hint;
    property    Left;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ModalResult;
    property    ShowHint;
    property    Sizeable;
    property    TextColor;
    property    Top;
    property    Width;
    property    WindowPosition;
    property    WindowState;
    property    WindowTitle;
    property    OnActivate;
    property    OnClick;
    property    OnClose;
    property    OnCloseQuery;
    property    OnCreate;
    property    OnDeactivate;
    property    OnDestroy;
    property    OnDoubleClick;
    property    OnEnter;
    property    OnExit;
    property    OnHide;
    property    OnKeyPress;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnMouseScroll;
    property    OnMouseUp;
    property    OnPaint;
    property    OnResize;
    property    OnShow;
    property    OnShowHint;
  end;


function WidgetParentForm(wg: TfpgWidget): TfpgForm;


implementation

uses
  fpg_main,
  fpg_popupwindow,
  fpg_menu;
  
type
  // to access protected methods
  TfpgMenuBarFriend = class(TfpgMenuBar)
  end;

  TfpgWidgetFriend = class(TfpgWidget)
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

{ TfpgBaseForm }

procedure TfpgBaseForm.SetDNDEnabled(const AValue: boolean);
begin
  if FDNDEnabled = AValue then exit;
  FDNDEnabled := AValue;
  DoDNDEnabled(AValue);
end;

procedure TfpgBaseForm.SetWindowTitle(const ATitle: string);
begin
  FWindowTitle := ATitle;
  inherited SetWindowTitle(ATitle);
end;

procedure TfpgBaseForm.MsgActivate(var msg: TfpgMessageRec);
begin
  {$IFDEF DEBUG}
  DebugLn(Classname + ' ' + Name + '.BaseForm - MsgActivate');
  {$ENDIF}
  if (fpgApplication.TopModalForm = nil) or (fpgApplication.TopModalForm = self) then
  begin
    {$IFDEF DEBUG}
    DebugLn('Inside if block');
    {$ENDIF}
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

procedure TfpgBaseForm.MsgDeActivate(var msg: TfpgMessageRec);
begin
  ClosePopups;
  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(self);
end;

procedure TfpgBaseForm.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
end;

procedure TfpgBaseForm.AdjustWindowStyle;
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

  if FWindowPosition = wpOneThirdDown then
    Include(FWindowAttributes, waOneThirdDownPos)
  else
    Exclude(FWindowAttributes, waOneThirdDownPos);

  if FSizeable then
    Include(FWindowAttributes, waSizeable)
  else
    Exclude(FWindowAttributes, waSizeable);
    
  if FFullScreen then
    Include(FWindowAttributes, waFullScreen)
  else
    Exclude(FWindowAttributes, waFullScreen);
end;

procedure TfpgBaseForm.SetWindowParameters;
begin
  inherited;
  DoSetWindowTitle(FWindowTitle);
end;

constructor TfpgBaseForm.Create(AOwner: TComponent);
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
  FModalResult     := mrNone;
  FFullScreen      := False;
  FIsContainer     := True;
  FDNDEnabled      := False;
end;

destructor TfpgBaseForm.Destroy;
begin
  fpgApplication.RemoveWindowFromModalStack(Self);
  inherited Destroy;
end;

procedure TfpgBaseForm.AfterCreate;
begin
  // for the user
end;

procedure TfpgBaseForm.InvokeHelp;
var
  lEventHandled: Boolean;
  lSucceeded: Boolean;
begin
  lEventHandled := False;
  lSucceeded := False;
  lSucceeded := DoOnHelp(HelpType, HelpContext, HelpKeyword, fpgApplication.HelpFile, lEventHandled);
  if (not lSucceeded) or (not lEventHandled) then
    inherited InvokeHelp;
end;

procedure TfpgBaseForm.Show;
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgBaseForm.Show - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  FVisible := True;
  HandleShow;
end;

function TfpgBaseForm.ShowModal: TfpgModalResult;
var
  lCloseAction: TCloseAction;
begin
  FWindowType := wtModalForm;
  fpgApplication.PushModalForm(self);
  ModalResult := mrNone;

  try
    Show;
    // processing messages until this form ends.
    // delivering the remaining messages
    fpgApplication.ProcessMessages;
    try
      repeat
        fpgWaitWindowMessage;
      until (ModalResult <> mrNone) or (not Visible);
    except
      on E: Exception do
      begin
        Visible := False;
        fpgApplication.HandleException(self);
      end;
    end;  { try..except }
  finally
    { we have to pop the form even in an error occurs }
    fpgApplication.PopModalForm;
    Result := ModalResult;
  end;  { try..finally }
  
  if ModalResult <> mrNone then
  begin
    lCloseAction := caHide; // Dummy variable - we do nothing with it
    DoOnClose(lCloseAction); // Simply so the OnClose event fires.
  end;
end;

procedure TfpgBaseForm.MsgClose(var msg: TfpgMessageRec);
begin
  HandleClose;
end;

procedure TfpgBaseForm.HandleClose;
begin
  Close;
end;

procedure TfpgBaseForm.HandleHide;
begin
  if Assigned(FOnHide) then
    FOnHide(self);
  inherited HandleHide;
end;

procedure TfpgBaseForm.HandleShow;
begin
  inherited HandleShow;
  HandleAlignments(0, 0);
  if Assigned(FOnShow) then
    FOnShow(self);
end;

procedure TfpgBaseForm.HandleMove(x, y: TfpgCoord);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TfpgBaseForm.HandleMove - ' + ClassName + ' ('+Name+')');
  DebugLn(Format('x:%d  y:%d', [x, y]));
  {$ENDIF}
  ClosePopups;
  inherited HandleMove(x, y);
end;

procedure TfpgBaseForm.HandleResize(awidth, aheight: TfpgCoord);
{$IFDEF CStackDebug}
var
  i: iinterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  DebugMethodEnter('TfpgBaseForm.HandleResize - ' + ClassName + ' ('+Name+')');
  DebugLn(Format('w:%d  h:%d', [awidth, aheight]));
  {$ENDIF}
  ClosePopups;
  inherited HandleResize(awidth, aheight);
end;

procedure TfpgBaseForm.AfterConstruction;
begin
  AfterCreate;
  inherited AfterConstruction;
  if Assigned(FOnCreate) then
    FOnCreate(self);
end;

procedure TfpgBaseForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(FOnDestroy) then
    FOnDestroy(self);
end;

procedure TfpgBaseForm.DoOnClose(var CloseAction: TCloseAction);
begin
  if Assigned(FOnClose) then
    OnClose(self, CloseAction);
end;

function TfpgBaseForm.DoOnHelp(AHelpType: THelpType; AHelpContext: THelpContext;
  const AHelpKeyword: String; const AHelpFile: String; var AHandled: Boolean): Boolean;
begin
  if Assigned(FOnHelp) then
    Result := FOnHelp(AHelpType, AHelpContext, AHelpKeyword, AHelpFile, AHandled);
end;

procedure TfpgBaseForm.DoKeyShortcut(const AOrigin: TfpgWidget;
  const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False);
var
  wg: TfpgWidget;
  menu: TfpgMenuBar;
  i: integer;
  ss: TShiftState;
  key: word;
  c: TfpgComponent;

  function FindMenuBar(AWidget: TfpgWidget): TfpgWidget;
  var
    n: integer;
    w: TfpgWidget;
  begin
    Result := nil;
    for n := 0 to AWidget.ComponentCount-1 do
    begin
      w := TfpgWidget(AWidget.Components[n]);
      if (w <> nil) and (w <> self) and (w <> AOrigin) and (w is TfpgMenuBar) then
      begin
        Result := w;
        exit;
      end;
      if w.ComponentCount > 0 then
        Result := FindMenuBar(w);
      if Result <> nil then
        exit;
    end;
  end;

begin
  // find the first TfpgMenuBar - if it exits
  wg := FindMenuBar(self);
  if (wg <> nil) then
  begin
    menu := wg as TfpgMenuBar;
    key := keycode;
    ss := shiftstate;
    TfpgMenuBarFriend(wg).HandleKeyPress(key, ss, consumed);
  end;

  if consumed then
    Exit;
  // now send to each widget on the form - excluding AOrigin and MenuBar widgets
  for i := 0 to ComponentCount-1 do
  begin
    c := TfpgComponent(Components[i]);
    if c is TfpgWidget then
      wg := TfpgWidget(c)
    else
      wg := nil;
    if (wg <> nil) and (wg <> self) and (wg <> AOrigin) and (wg <> menu) and (not (wg is TfpgPopupMenu)) then
    begin
      if (not wg.Visible) or (not wg.Enabled) then
        continue
      else
      begin
        TfpgWidgetFriend(wg).DoKeyShortcut(AOrigin, keycode, shiftstate, consumed);
        if consumed then
          Exit;
      end;
    end;
  end;
end;

procedure TfpgBaseForm.Hide;
begin
  Visible := False;
end;

procedure TfpgBaseForm.Close;
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
            fpgPostMessage(Self, fpgApplication, FPGM_FREEME);
        end;
    end;  { case CloseAction }
  end;  { if CloseQuery }
end;

function TfpgBaseForm.CloseQuery: boolean;
begin
  Result := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(self, Result);
end;


end.

