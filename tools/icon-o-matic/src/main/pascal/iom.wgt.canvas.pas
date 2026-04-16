unit iom.wgt.canvas;

{
  TIomCanvasWidget — renders a loaded THvifIcon centred on the widget,
  with a checkerboard background to show alpha.

  Step #4: read-only viewer only; no mouse interaction yet.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  fpg_base, fpg_main, fpg_widget,
  fpg_hvif;


type
  TIomCanvasWidget = class(TfpgWidget)
  private
    FIcon:      THvifIcon;    { owned — nil when nothing loaded }
    FFilePath:  string;

    function  RenderSize: Integer;
    procedure DrawCheckerboard(AX, AY, ASize: Integer);
    procedure DrawEmptyHint;

  protected
    procedure HandlePaint; override;
    procedure HandleResize(awidth, aheight: TfpgCoord); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { Load an HVIF file into the widget; raises on parse error. }
    procedure LoadFromFile(const AFileName: string);
    { Clear the current icon back to the empty state. }
    procedure ClearIcon;

    property FilePath: string read FFilePath;
  end;


implementation

const
  CHECKER_CELL = 8;
  COL_CHECKER1: TfpgColor = $FFCCCCCC;
  COL_CHECKER2: TfpgColor = $FFAAAAAA;
  COL_BORDER:   TfpgColor = $FF999999;
  COL_BG:       TfpgColor = $FFE8E8E8;


{ ── TIomCanvasWidget ─────────────────────────────────────────────────────── }

constructor TIomCanvasWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := nil;
end;

destructor TIomCanvasWidget.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TIomCanvasWidget.LoadFromFile(const AFileName: string);
var
  newIcon: THvifIcon;
begin
  newIcon := THvifIcon.CreateFromFile(AFileName);
  FIcon.Free;
  FIcon     := newIcon;
  FFilePath := AFileName;
  Repaint;
end;

procedure TIomCanvasWidget.ClearIcon;
begin
  FreeAndNil(FIcon);
  FFilePath := '';
  Repaint;
end;

{ The render size: square that fits inside the widget with 20px margins. }
function TIomCanvasWidget.RenderSize: Integer;
begin
  Result := Min(Width - 40, Height - 40);
  if Result < 16 then
    Result := 16;
  { Round down to a multiple of 4 so HVIF coordinates stay clean. }
  Result := (Result div 4) * 4;
end;

procedure TIomCanvasWidget.DrawCheckerboard(AX, AY, ASize: Integer);
var
  col, row: Integer;
begin
  col := 0;
  while col < ASize do
  begin
    row := 0;
    while row < ASize do
    begin
      if Odd((col div CHECKER_CELL) + (row div CHECKER_CELL)) then
        Canvas.SetColor(COL_CHECKER2)
      else
        Canvas.SetColor(COL_CHECKER1);
      Canvas.FillRectangle(AX + col, AY + row,
                           Min(CHECKER_CELL, ASize - col),
                           Min(CHECKER_CELL, ASize - row));
      Inc(row, CHECKER_CELL);
    end;
    Inc(col, CHECKER_CELL);
  end;
end;

procedure TIomCanvasWidget.DrawEmptyHint;
begin
  Canvas.SetColor($FF000000);
  Canvas.SetTextColor($FF888888);
  Canvas.DrawString(Width div 2 - 80, Height div 2 - 8,
      'File > Open to load an HVIF icon');
end;

procedure TIomCanvasWidget.HandlePaint;
var
  sz, ox, oy: Integer;
  img: TfpgImage;
begin
  Canvas.BeginDraw;
  try
    { Widget background }
    Canvas.SetColor(COL_BG);
    Canvas.FillRectangle(0, 0, Width, Height);

    if FIcon = nil then
    begin
      DrawEmptyHint;
      Exit;
    end;

    sz := RenderSize;
    ox := (Width  - sz) div 2;
    oy := (Height - sz) div 2;

    { Checkerboard behind the icon to reveal alpha }
    DrawCheckerboard(ox, oy, sz);

    { Render the icon at the computed size }
    img := FIcon.GetImage(sz, sz);
    if img <> nil then
      Canvas.DrawImage(ox, oy, img);

    { Border around the icon area }
    Canvas.SetColor(COL_BORDER);
    Canvas.DrawRectangle(ox - 1, oy - 1, sz + 2, sz + 2);

  finally
    Canvas.EndDraw;
  end;
end;

procedure TIomCanvasWidget.HandleResize(awidth, aheight: TfpgCoord);
begin
  inherited HandleResize(awidth, aheight);
  { Clear cached rendered image so the next paint uses the new size. }
  if FIcon <> nil then
    FIcon.ClearCache;
  Repaint;
end;

end.
