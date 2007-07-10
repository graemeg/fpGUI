program eventtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, GFXBase, fpGFX, gfx_widget;
  
type

  { TMainForm }

  TMainForm = class(TfpgWindow)
  private
    FMoveEventCount: integer;
    function    ShiftStateToStr(AShift: word): string;
    function    MouseState(AShift: word; const AMousePos: TPoint): string;
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
    constructor Create(aowner: TComponent); override;
    procedure   Show;
  end;

{ TMainForm }

function TMainForm.ShiftStateToStr(AShift: word): string;
begin
  Result := '';
  {$Note This must move into gfx_XXX units and return TShiftState enum}
  if (AShift and ss_Shift) <> 0 then
    Result := 'Shift ';
  if (AShift and ss_Alt) <> 0 then
    Result := Result + 'Alt ';
  if (AShift and ss_Control) <> 0 then
    Result := Result + 'Ctrl ';
end;

function TMainForm.MouseState(AShift: word; const AMousePos: TPoint): string;
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
  Writeln('Window is Deactivate message');
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
  Writeln('Resize');
  FWidth  := msg.Params.rect.Width;
  FHeight := msg.Params.rect.Height;
end;

procedure TMainForm.MsgMove(var msg: TfpgMessageRec);
begin
  Writeln('Window Move');
end;

procedure TMainForm.MsgKeyChar(var msg: TfpgMessageRec);
begin
  Write('Keychar - Character generated: ');
//  if Char(keycode) >= ' ' then
//    WriteLn('''', Char(keycode), '''')
//  else
//    WriteLn('#', Ord(keycode));
end;

procedure TMainForm.MsgKeyPress(var msg: TfpgMessageRec);
begin
  Writeln('KeyPress');
end;

procedure TMainForm.MsgKeyRelease(var msg: TfpgMessageRec);
begin
  Writeln('KeyRelease');
end;

procedure TMainForm.MsgMouseDown(var msg: TfpgMessageRec);
begin
  Writeln('Mouse button down.' + ' button=' + IntToStr(msg.Params.mouse.Buttons));
end;

procedure TMainForm.MsgMouseUp(var msg: TfpgMessageRec);
begin
  Writeln('Mouse button up.' + ' button=' + IntToStr(msg.Params.mouse.Buttons));
end;

procedure TMainForm.MsgMouseMove(var msg: TfpgMessageRec);
var
  s: string;
begin
  inc(FMoveEventCount);
  // only report mouse moves every 10 messages - just to limit the output a bit
  if (FMoveEventCount mod 10) = 0 then
  begin
    s := Format('[%d,%d] ', [msg.Params.mouse.x, msg.Params.mouse.y]);
    WriteLn(s + 'Mouse move message');
//    WriteLn(MouseState(shiftstate, Point(x, y)), 'Mouse moved');
  end;
end;

procedure TMainForm.MsgDoubleClick(var msg: TfpgMessageRec);
begin
  Writeln('Mouse doubleclick');
end;

procedure TMainForm.MsgMouseEnter(var msg: TfpgMessageRec);
begin
  Writeln('Mouse enter');
end;

procedure TMainForm.MsgMouseExit(var msg: TfpgMessageRec);
begin
  Writeln('Mouse exit');
end;

procedure TMainForm.MsgScroll(var msg: TfpgMessageRec);
var
  delta: smallint;
begin
  delta := msg.Params.mouse.delta;
  Writeln('Mouse scroll delta=' + IntToStr(delta) + ' button=' + IntToStr(msg.Params.mouse.Buttons));
end;

constructor TMainForm.Create(aowner: TComponent);
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
  // We can't set a title if we don't have a window handle. So we do that here
  // and not in the constructor.
  DoSetWindowTitle('fpGFX event test');
end;
  
  
procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;

begin
  MainProc;
end.

