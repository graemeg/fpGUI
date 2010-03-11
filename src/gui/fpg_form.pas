{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
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
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   DoOnClose(var CloseAction: TCloseAction); virtual;
    function    DoOnHelp(AHelpType: THelpType; AHelpContext: THelpContext; const AHelpKeyword: String; const AHelpFile: String; var AHandled: Boolean): Boolean; virtual;
    // properties
    property    Sizeable: boolean read FSizeable write FSizeable;
    property    ModalResult: TfpgModalResult read FModalResult write FModalResult;
    property    FullScreen: boolean read FFullScreen write FFullScreen default False;
    property    WindowPosition: TWindowPosition read FWindowPosition write FWindowPosition default wpAuto;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    // events
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
    function    ShowModal: integer;
    procedure   Close;
    function    CloseQuery: boolean; virtual;
  end;
  
  
  TfpgForm = class(TfpgBaseForm)
  published
    property    BackgroundColor;
    property    FullScreen;
    property    Hint;
    property    ModalResult;
    property    Sizeable;
    property    ShowHint;
    property    TextColor;
    property    WindowPosition;
    property    WindowTitle;
    property    OnActivate;
    property    OnClose;
    property    OnCloseQuery;
    property    OnCreate;
    property    OnDeactivate;
    property    OnDestroy;
    property    OnHide;
    property    OnPaint;
    property    OnResize;
    property    OnShow;
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

procedure TfpgBaseForm.SetWindowTitle(const ATitle: string);
begin
  FWindowTitle := ATitle;
  inherited SetWindowTitle(ATitle);
end;

procedure TfpgBaseForm.MsgActivate(var msg: TfpgMessageRec);
begin
//  writeln('BaseForm - MsgActivate');
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
begin
  FVisible := True;
  HandleShow;
end;

function TfpgBaseForm.ShowModal: integer;
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
        ModalResult := mrError;
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
    lCloseAction := caFree; // Dummy variable - we do nothing with it
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
begin
  ClosePopups;
  inherited HandleMove(x, y);
end;

procedure TfpgBaseForm.HandleResize(awidth, aheight: TfpgCoord);
begin
  ClosePopups;
  inherited HandleResize(awidth, aheight);
end;

procedure TfpgBaseForm.HandleKeyPress(var keycode: word;
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

procedure TfpgBaseForm.AfterConstruction;
begin
  inherited AfterConstruction;
  AfterCreate;
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

procedure TfpgBaseForm.Hide;
begin
  Visible := False;
  if ModalResult = mrNone then  // No modal Result was set, and we need something
    ModalResult := mrError;     // other than mrNone to exit modal event loop
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

