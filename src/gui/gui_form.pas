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
  protected
    FPrevModalForm: TfpgWindowBase;
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
    property    WindowPosition: TWindowPosition read FWindowPosition write FWindowPosition;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    property    ModalResult: integer read FModalResult write FModalResult;
  published
    {$Note Refactor this to a TfpgCustomForm and only surface it here }
    property    OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property    OnClose: TNotifyEvent read FOnClose write FOnClose;
    property    OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property    OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property    OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property    OnHide: TNotifyEvent read FOnHide write FOnHide;
    property    OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;


var
  // Don't like this. It's a bit of a hack. Possibly move this into
  // fpgApplication, but do we want fpgApplication to have that dependency??
  fpgMainForm: TfpgForm;

function WidgetParentForm(wg: TfpgWidget): TfpgForm;


implementation

uses
  fpgfx;

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

procedure TfpgForm.AdjustWindowStyle;
begin
  if fpgMainForm = nil then
    fpgMainForm := self;

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
  FWindowPosition  := wpUser;
  FWindowTitle     := '';
  FSizeable        := True;
  FParentForm      := nil;
  FBackgroundColor := clWindowBackground;
  FMinWidth        := 32;
  FMinHeight       := 32;
  FModalResult     := 0;
  FPrevModalForm   := nil;

  AfterCreate;
end;

procedure TfpgForm.AfterCreate;
begin
  // for the user
end;

procedure TfpgForm.Show;
begin
  HandleShow;
end;

function TfpgForm.ShowModal: integer;
begin
  FPrevModalForm  := fpgApplication.TopModalForm;
  fpgApplication.TopModalForm := self;
  ModalResult     := 0;

  Show;
  // processing messages until this form ends.
  // delivering the remaining messages
  fpgApplication.ProcessMessages;
  repeat
    fpgWaitWindowMessage;
  until (ModalResult <> 0) or (not Visible);

  fpgApplication.TopModalForm := FPrevModalForm;
  Result := ModalResult;
end;

procedure TfpgForm.MsgClose(var msg: TfpgMessageRec);
begin
  HandleClose;
  if Assigned(FOnClose) then
    FOnClose(self);
end;

procedure TfpgForm.HandleClose;
begin
  Close;
end;

procedure TfpgForm.HandleHide;
begin
  inherited HandleHide;
  if Assigned(FOnHide) then
    FOnHide(self);
end;

procedure TfpgForm.HandleShow;
begin
  inherited HandleShow;
  if Assigned(FOnShow) then
    FOnShow(self);
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
    fpgApplication.TopModalForm := FPrevModalForm;
  HandleHide;
  if ModalResult = 0 then
    ModalResult := -1;
end;

procedure TfpgForm.Close;
begin
  Hide;
  if fpgMainForm = self then
    Halt(0);
end;

initialization
  fpgMainForm     := nil;

end.

