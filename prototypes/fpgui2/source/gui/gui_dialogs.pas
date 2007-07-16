unit gui_dialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpgfx, gui_form, gui_button, gui_label;

type

  { TfpgMessageBox }

  TfpgMessageBox = class(TfpgForm)
  private
    FLines: TStringList;
    FFont: TfpgFont;
    FTextY: integer;
    FLineHeight: integer;
    FMaxLineWidth: integer;
    FButton: TfpgButton;
    procedure   ButtonClick(Sender: TObject);
  protected
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   SetMessage(AMessage: string);
  end;


procedure ShowMessage(AMessage, ATitle: string); overload;
procedure ShowMessage(AMessage: string); overload;


implementation

uses
  gfxbase, gfx_utf8utils;


procedure ShowMessage(AMessage, ATitle: string);
var
  frm: TfpgMessageBox;
begin
  frm := TfpgMessageBox.Create(nil);
  try
    frm.WindowTitle := ATitle;
    frm.SetMessage(AMessage);
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure ShowMessage(AMessage: string);
begin
  ShowMessage(AMessage, 'Message');
end;


{ TfpgMessageBox }

procedure TfpgMessageBox.ButtonClick(Sender: TObject);
begin
  ModalResult := 1;
end;

procedure TfpgMessageBox.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if keycode = keyEscape then
    Close;
end;

procedure TfpgMessageBox.HandlePaint;
var
  n, y: integer;
  tw: integer;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;

//  canvas.Clear(FBackgroundColor);
  Canvas.SetFont(FFont);

  y := FTextY;
  for n := 0 to FLines.Count-1 do
  begin
    tw := FFont.TextWidth(FLines[n]);
    Canvas.DrawString(Width div 2 - tw div 2, y, FLines[n]);
    Inc(y, FLineHeight);
  end;
  Canvas.EndDraw;
end;

constructor TfpgMessageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowAttributes := [waAutoPos];
  
  FLines := TStringList.Create;
  FFont := fpgGetFont('#Label1');
  FTextY := 10;
  FLineHeight := FFont.Height + 4;
  MinWidth := 200;
  FMaxLineWidth := 500;
  
  FButton := TfpgButton.Create(self);
  FButton.text := 'OK';   // We must localize this
  FButton.Width := 75;
  FButton.OnClick := @ButtonClick;
  
end;

destructor TfpgMessageBox.Destroy;
begin
  FFont.Free;
  FLines.Free;
  inherited Destroy;
end;

procedure TfpgMessageBox.SetMessage(AMessage: string);
var
  maxw: integer;
  n: integer;
  s, s2: string;
  c: char;

  // -----------------
  procedure AddLine(all: boolean);
  var
    w: integer;
    m: integer;
  begin
    s2  := s;
    w   := FFont.TextWidth(s2);
    if w > FMaxLineWidth then
    begin
      while w > FMaxLineWidth do
      begin
        m := UTF8Length(s);
        repeat
          Dec(m);
          s2  := UTF8Copy(s,1,m);
          w   := FFont.TextWidth(s2);
        until w <= FMaxLineWidth;
        if w > maxw then
          maxw := w;

        // are we in the middle of a word. If so find the beginning of word.
        while UTF8Copy(s2, m, m+1) <> ' ' do
        begin
          Dec(m);
          s2  := UTF8Copy(s,1,m);
        end;

        FLines.Add(s2);
        s   := UTF8Copy(s, m+1, UTF8length(s));
        s2  := s;
        w   := FFont.TextWidth(s2);
      end; { while }
      if all then
      begin
        FLines.Add(s2);
        s := '';
      end;
    end
    else
    begin
      FLines.Add(s2);
      s := '';
    end; { if/else }

    if w > maxw then
      maxw := w;
  end;

begin
  s := '';
  FLines.Clear;
  n := 1;
  maxw := 0;
  while n <= length(AMessage) do
  begin
    c := AMessage[n];
    if (c = #13) or (c = #10) then
    begin
      AddLine(false);
      if (c = #13) and (n < length(AMessage)) and (AMessage[n+1] = #10) then
        Inc(n);
    end
    else
      s := s + c;
    Inc(n);
  end; { while }

  AddLine(true);

  // dialog width with 10 pixel border on both sides
  Width := maxw + 2*10;

  if Width < FMinWidth then
    Width := FMinWidth;

  // center button
  FButton.Top   := FTextY + FLineHeight*FLines.Count + FTextY;
  FButton.Left  := (Width div 2) - (FButton.Width div 2);

  // adjust dialog's height
  Height := FButton.Top + FButton.Height + FTextY;
end;


end.

