program eventtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;
  

const
  ButtonNames: array[TMouseButton] of PChar =
    ('Left', 'Right', 'Middle');

type

  { TMainForm }

  TMainForm = class(TfpgWindow)
  private
    FMoveEventCount: integer;
    function    ShiftStateToStr(Shift: TShiftState): string;
    function    MouseState(AShift: TShiftState; const AMousePos: TPoint): string;
    procedure   MsgActivate(var msg: TfpgMessageRec); message FPGM_ACTIVATE;
    procedure   MsgDeActivate(var msg: TfpgMessageRec); message FPGM_DEACTIVATE;
    procedure   MsgClose(var msg: TfpgMessageRec); message FPGM_CLOSE;
    procedure   MsgPaint(var msg: TfpgMessageRec); message FPGM_PAINT;
    procedure   MsgResize(var msg: TfpgMessageRec); message FPGM_RESIZE;
    procedure   MsgMove(var msg: TfpgMessageRec); message FPGM_MOVE;
    procedure   MsgKeyChar(var msg: TfpgMessageRec); message FPGM_KEYCHAR;
    procedure   MsgKeyPress(var msg: TfpgMessageRec); message FPGM_KEYPRESS;
    procedure   MsgKeyRelease(var msg: TfpgMessageRec); message FPGM_KEYRELEASE;
    procedure   MsgMouseDown(var msg: TfpgMessageRec); message FPGM_MOUSEDOWN;
    procedure   MsgMouseUp(var msg: TfpgMessageRec); message FPGM_MOUSEUP;
    procedure   MsgMouseMove(var msg: TfpgMessageRec); message FPGM_MOUSEMOVE;
    procedure   MsgDoubleClick(var msg: TfpgMessageRec); message FPGM_DOUBLECLICK;
    procedure   MsgMouseEnter(var msg: TfpgMessageRec); message FPGM_MOUSEENTER;
    procedure   MsgMouseExit(var msg: TfpgMessageRec); message FPGM_MOUSEEXIT;
    procedure   MsgScroll(var msg: TfpgMessageRec); message FPGM_SCROLL;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Show;
  end;

{ TMainForm }

function TMainForm.ShiftStateToStr(Shift: TShiftState): string;
begin
  SetLength(Result, 0);
  if ssShift in Shift then
    Result := 'Shift ';
  if ssAlt in Shift then
    Result := Result + 'Alt ';
  if ssCtrl in Shift then
    Result := Result + 'Ctrl ';
  if ssMeta in Shift then
    Result := Result + 'Meta ';
  if ssSuper in Shift then
    Result := Result + 'Super ';
  if ssHyper in Shift then
    Result := Result + 'Hyper ';
  if ssAltGr in Shift then
    Result := Result + 'AltGr ';
  if ssCaps in Shift then
    Result := Result + 'Caps ';
  if ssNum in Shift then
    Result := Result + 'Num ';
  if ssScroll in Shift then
    Result := Result + 'Scroll ';
  if ssLeft in Shift then
    Result := Result + 'Left ';
  if ssRight in Shift then
    Result := Result + 'Right ';
  if ssMiddle in Shift then
    Result := Result + 'Middle ';
  if ssDouble in Shift then
    Result := Result + 'Double ';
  if Length(Result) > 0 then
    SetLength(Result, Length(Result) - 1);

end;

function TMainForm.MouseState(AShift: TShiftState; const AMousePos: TPoint): string;
var
  ShiftStateStr: String;
begin
  ShiftStateStr := ShiftStateToStr(AShift);
  Result := '[X=' + IntToStr(AMousePos.x) + ' Y=' + IntToStr(AMousePos.y);
  if Length(ShiftStateStr) > 0 then
    Result := Result + ' ' + ShiftStateStr;
  Result := Result + '] ';
end;

procedure TMainForm.MsgActivate(var msg: TfpgMessageRec);
begin
  Writeln('Window Activate message');
end;

procedure TMainForm.MsgDeActivate(var msg: TfpgMessageRec);
begin
  Writeln('Window Deactivate message');
end;

procedure TMainForm.MsgClose(var msg: TfpgMessageRec);
begin
  Writeln('Window Close message');
  Halt(0);
end;

procedure TMainForm.MsgPaint(var msg: TfpgMessageRec);
var
  h: integer;
begin
  Writeln('Paint message');
  Canvas.BeginDraw;
  h := Canvas.Font.Height;
  Canvas.SetColor(clWhite);
  Canvas.FillRectangle(0, 0, Width, Height);
  Canvas.SetTextColor(clBlack);
  Canvas.DrawString(0, 0, 'Event test');
  Canvas.DrawString(0, h, 'Do something interactive (move mouse, press keys...)');
  Canvas.DrawString(0, h*2, 'and watch the output on the console.');
  Canvas.EndDraw;
end;

procedure TMainForm.MsgResize(var msg: TfpgMessageRec);
begin
  Writeln('Resize message');
  FWidth  := msg.Params.rect.Width;
  FHeight := msg.Params.rect.Height;
  WriteLn('  Window has been resized. New size: ', Width, ' x ', Height);
end;

procedure TMainForm.MsgMove(var msg: TfpgMessageRec);
begin
  Writeln('Move message');
  WriteLn('  Window has been moved to (', msg.Params.rect.Left, ',', msg.Params.rect.Top, ')');
end;

procedure TMainForm.MsgKeyChar(var msg: TfpgMessageRec);
var
  AKeyChar: TfpgChar;
begin
  Write('Character generated: ');
  AKeyChar := msg.Params.keyboard.keychar;
  if AKeyChar >= ' ' then
    WriteLn('''', AKeyChar, '''')
  else
    WriteLn('#', Ord(AKeyChar[1]));
end;

procedure TMainForm.MsgKeyPress(var msg: TfpgMessageRec);
begin
  WriteLn('[', ShiftStateToStr(msg.Params.keyboard.shiftstate), '] Key pressed: ',
    KeycodeToText(msg.Params.keyboard.keycode, []));
end;

procedure TMainForm.MsgKeyRelease(var msg: TfpgMessageRec);
begin
  WriteLn('[', ShiftStateToStr(msg.Params.keyboard.shiftstate), '] Key released: ',
    KeycodeToText(msg.Params.keyboard.keycode, []));
end;

procedure TMainForm.MsgMouseDown(var msg: TfpgMessageRec);
begin
  WriteLn(MouseState(msg.Params.mouse.shiftstate, Point(msg.Params.mouse.x, msg.Params.mouse.y)),
    'Mouse button pressed: ', ' button=' + IntToStr(msg.Params.mouse.Buttons));
//    ButtonNames[msg.Params.mouse.Buttons]);
end;

procedure TMainForm.MsgMouseUp(var msg: TfpgMessageRec);
begin
  WriteLn(MouseState(msg.Params.mouse.shiftstate, Point(msg.Params.mouse.x, msg.Params.mouse.y)),
    'Mouse button released: ', ' button=' + IntToStr(msg.Params.mouse.Buttons));
//    ButtonNames[msg.Params.mouse.Buttons]);
end;

procedure TMainForm.MsgMouseMove(var msg: TfpgMessageRec);
begin
  inc(FMoveEventCount);
  // only report mouse moves every 10 messages - just to limit the output a bit
  if (FMoveEventCount mod 10) = 0 then
  begin
    WriteLn(MouseState(msg.Params.mouse.shiftstate, Point(msg.Params.mouse.x, msg.Params.mouse.y)), 'Mouse moved');
  end;
end;

procedure TMainForm.MsgDoubleClick(var msg: TfpgMessageRec);
begin
  Writeln('Mouse doubleclick message');
end;

procedure TMainForm.MsgMouseEnter(var msg: TfpgMessageRec);
begin
  WriteLn(MouseState(msg.Params.mouse.shiftstate, Point(msg.Params.mouse.x, msg.Params.mouse.y)), 'Mouse entered window');
end;

procedure TMainForm.MsgMouseExit(var msg: TfpgMessageRec);
begin
  WriteLn('Mouse left window');
end;

procedure TMainForm.MsgScroll(var msg: TfpgMessageRec);
var
  delta: Integer;
begin
  delta := msg.Params.mouse.delta;
  Writeln('Mouse scroll delta=' + IntToStr(delta) + ' button=' + IntToStr(msg.Params.mouse.Buttons));
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(aowner);
  FMoveEventCount := 0;
  FWidth    := 400;
  FHeight   := 100;
  WindowAttributes := [waSizeable, waScreenCenterPos];
end;

procedure TMainForm.Show;
begin
  AllocateWindowHandle;
  DoSetWindowVisible(True);
  // We can't set a title if we don't have a window handle. So we do that here
  // and not in the constructor.
  SetWindowTitle('fpGFX event test');
end;
  
  
procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.

