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
  protected
    FPrevModalForm: TfpgForm;
    FModalResult: integer;
    FParentForm: TfpgForm;
    FWindowPosition: TWindowPosition;
    FWindowTitle: string;
    FSizeable: boolean;
    FBackgroundColor: TfpgColor;
    procedure   AdjustWindowStyle; override;
    procedure   SetWindowParameters; override;
    procedure   SetWindowTitle(const AValue: string);
    procedure   MsgActivate(var msg: TfpgMessageRec); message FPGM_ACTIVATE;
    procedure   MsgDeActivate(var msg: TfpgMessageRec); message FPGM_DEACTIVATE;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
    procedure   HandlePaint; override;
    procedure   HandleClose; virtual;
  public
    constructor Create(aowner: TComponent); override;
    procedure   AfterCreate; virtual;
    procedure   Show;
    procedure   Hide;
    function    ShowModal: integer;
    procedure   Close;
    property    Sizeable: boolean read FSizeable write FSizeable;
    property    WindowPosition: TWindowPosition read FWindowPosition write FWindowPosition;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    property    ModalResult: integer read FModalResult write FModalResult;
  end;


var
  // Don't like this. It's a bit of a hack. Possibly move this into
  // fpgApplication, but do we want fpgApplication to have that dependency??
  fpgMainForm: TfpgForm;
  fpgTopModalForm: TfpgForm;

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
      Exit;
    end;
    w := w.Parent;
  end;
  Result := nil;
end;

{ TfpgForm }

procedure TfpgForm.SetWindowTitle(const AValue: string);
begin
  FWindowTitle := avalue;
  inherited DoSetWindowTitle(FWindowTitle);
end;

procedure TfpgForm.HandlePaint;
begin
  canvas.BeginDraw;
  canvas.Clear(FBackgroundColor);
  canvas.EndDraw(0, 0, FWidth, FHeight);
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

constructor TfpgForm.Create(aowner: TComponent);
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
  FPrevModalForm  := fpgTopModalForm;
  fpgTopModalForm := self;
  ModalResult     := 0;

  Show;
  // processing messages until this form ends.
  // delivering the remaining messages
  fpgApplication.ProcessMessages;
  repeat
    fpgWaitWindowMessage;
  until (ModalResult <> 0) or (not Visible);

  fpgTopModalForm := FPrevModalForm;
  Result := ModalResult;
end;

procedure TfpgForm.MsgActivate(var msg: TfpgMessageRec);
begin
  if (fpgTopModalForm = nil) or (fpgTopModalForm = self) then
  begin
    FocusRootWidget := self;
    if ActiveWidget = nil then
      ActiveWidget := FindFocusWidget(nil, fsdFirst)
    else
      ActiveWidget.SetFocus;
  end;
end;

procedure TfpgForm.MsgDeActivate(var msg: TfpgMessageRec);
begin
  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
end;

procedure TfpgForm.MsgClose(var msg: TfpgMessageRec);
begin
  HandleClose;
end;

procedure TfpgForm.HandleClose;
begin
  Close;
end;

procedure TfpgForm.Hide;
begin
  if (fpgTopModalForm = self) then
    fpgTopModalForm := self.FPrevModalForm;
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
  fpgTopModalForm := nil;

end.

