program eventtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, GFXBase, fpGFX, gui_form, gfx_widget;
  
type
  TMainForm = class(TfpgForm)
  private
    function    ShiftStateToStr(AShift: word): string;
    function    MouseState(AShift: word; const AMousePos: TPoint): string;
  protected
    procedure   HandleClose; override;
    procedure   HandlePaint; override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: word); override;
    procedure   HandleKeyChar(var keycode: word; var shiftstate: word; var consumed: boolean); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: word); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: word); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: word); override;
    procedure   HandleRMouseDown(x, y: integer; shiftstate: word); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: word); override;
  public
    constructor Create(aowner: TComponent); override;
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

procedure TMainForm.HandleClose;
begin
  WriteLn('HandleClose');
  inherited HandleClose;
end;

procedure TMainForm.HandlePaint;
begin
  WriteLn('HandlePaint');
  inherited HandlePaint;
end;

procedure TMainForm.HandleDoubleClick(x, y: integer; button: word;
  shiftstate: word);
begin
  WriteLn('HandleDoubleClick');
  inherited HandleDoubleClick(x, y, button, shiftstate);
end;

procedure TMainForm.HandleKeyChar(var keycode: word; var shiftstate: word;
  var consumed: boolean);
begin
  Write('Character generated: ');
  if Char(keycode) >= ' ' then
    WriteLn('''', Char(keycode), '''')
  else
    WriteLn('#', Ord(keycode));

  inherited HandleKeyChar(keycode, shiftstate, consumed);
end;

procedure TMainForm.HandleMouseEnter;
begin
  WriteLn('Mouse entered window');
  inherited HandleMouseEnter;
end;

procedure TMainForm.HandleMouseExit;
begin
  WriteLn('Mouse left window');
  inherited HandleMouseExit;
end;

procedure TMainForm.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: word);
begin
  WriteLn(MouseState(shiftstate, Point(x, y)), 'Mouse moved');
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
end;

procedure TMainForm.HandleLMouseDown(x, y: integer; shiftstate: word);
begin
  WriteLn('Left mouse button down');
  inherited HandleLMouseDown(x, y, shiftstate);
end;

procedure TMainForm.HandleLMouseUp(x, y: integer; shiftstate: word);
begin
  Writeln('Left mouse button up');
  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TMainForm.HandleRMouseDown(x, y: integer; shiftstate: word);
begin
  Writeln('Right mouse button down');
  inherited HandleRMouseDown(x, y, shiftstate);
end;

procedure TMainForm.HandleRMouseUp(x, y: integer; shiftstate: word);
begin
  WriteLn('Right mouse button up');
  inherited HandleRMouseUp(x, y, shiftstate);
end;

constructor TMainForm.Create(aowner: TComponent);
begin
  inherited Create(aowner);
  SetPosition(100, 100, 500, 100);
  WindowTitle := 'fpGFX event test';
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

