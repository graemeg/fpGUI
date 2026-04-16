unit iom.wgt.previewbar;

{
  TIomPreviewBar — renders the HVIF icon simultaneously at 16, 32, 48 and 64 px.

  Sits at the top of the right panel.  Rebuilds the icon from the document
  whenever DocumentChanged is called (same trigger as the main canvas).

  Each cell draws a checkerboard background (to show transparency) with the
  rendered icon composited on top, followed by a 1px border.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget,
  fpg_hvif, fpg_hvif_writer,
  fpg_iom_document;


type
  TIomPreviewBar = class(TfpgWidget)
  private
    FDocument:  TIomDocument;   { not owned }
    FIcon:      THvifIcon;      { owned; rebuilt when FIconDirty is True }
    FIconDirty: Boolean;

    procedure RebuildIcon;
    procedure DrawCell(AX, AY, ASize: Integer);

  protected
    procedure HandlePaint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { Connect to the document. Pass nil to disconnect. }
    procedure SetDocument(ADoc: TIomDocument);

    { Mark icon as dirty and repaint — call whenever the document changes. }
    procedure DocumentChanged;
  end;


implementation

const
  PREVIEW_SIZES: array[0..3] of Integer = (16, 32, 48, 64);
  CELL_GAP    = 8;   { horizontal gap between cells }
  MARGIN_X    = 6;   { left margin }
  CHECKER     = 4;   { checkerboard cell size in pixels }
  BAR_HEIGHT  = 76;  { 64 + 6 top + 6 bottom padding }

  COL_CHECKER1: TfpgColor = $FFDDDDDD;
  COL_CHECKER2: TfpgColor = $FFAAAAAA;
  COL_CELL_BDR: TfpgColor = $FF888888;
  COL_BG:       TfpgColor = $FFD0D0D0;


{ ── TIomPreviewBar ────────────────────────────────────────────────────────── }

constructor TIomPreviewBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument  := nil;
  FIcon      := nil;
  FIconDirty := False;
  PreferredSize := fpgSize(220, BAR_HEIGHT);
end;

destructor TIomPreviewBar.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TIomPreviewBar.SetDocument(ADoc: TIomDocument);
begin
  FDocument  := ADoc;
  FIconDirty := (ADoc <> nil);
  FreeAndNil(FIcon);
  Repaint;
end;

procedure TIomPreviewBar.DocumentChanged;
begin
  FIconDirty := True;
  Repaint;
end;

procedure TIomPreviewBar.RebuildIcon;
var
  writer: THvifWriter;
  ms:     TMemoryStream;
begin
  FreeAndNil(FIcon);
  if FDocument = nil then
    Exit;
  writer := FDocument.BuildWriter;
  try
    ms := TMemoryStream.Create;
    try
      writer.SaveToStream(ms);
      ms.Position := 0;
      FIcon := THvifIcon.CreateFromStream(ms);
    finally
      ms.Free;
    end;
  finally
    writer.Free;
  end;
end;

procedure TIomPreviewBar.DrawCell(AX, AY, ASize: Integer);
var
  col, row: Integer;
  img:      TfpgImage;
begin
  { Checkerboard background }
  col := 0;
  while col < ASize do
  begin
    row := 0;
    while row < ASize do
    begin
      if Odd((col div CHECKER) + (row div CHECKER)) then
        Canvas.SetColor(COL_CHECKER2)
      else
        Canvas.SetColor(COL_CHECKER1);
      Canvas.FillRectangle(AX + col, AY + row, CHECKER, CHECKER);
      Inc(row, CHECKER);
    end;
    Inc(col, CHECKER);
  end;
  { Icon render }
  if FIcon <> nil then
  begin
    img := FIcon.GetImage(ASize, ASize);
    if img <> nil then
      Canvas.DrawImage(AX, AY, img);
  end;
  { Cell border }
  Canvas.SetColor(COL_CELL_BDR);
  Canvas.DrawRectangle(AX, AY, ASize, ASize);
end;

procedure TIomPreviewBar.HandlePaint;
var
  i, cx, cy: Integer;
begin
  Canvas.BeginDraw;
  try
    Canvas.SetColor(COL_BG);
    Canvas.FillRectangle(0, 0, Width, Height);

    if FIconDirty then
    begin
      RebuildIcon;
      FIconDirty := False;
    end;

    cx := MARGIN_X;
    for i := 0 to High(PREVIEW_SIZES) do
    begin
      cy := (Height - PREVIEW_SIZES[i]) div 2;
      DrawCell(cx, cy, PREVIEW_SIZES[i]);
      Inc(cx, PREVIEW_SIZES[i] + CELL_GAP);
    end;
  finally
    Canvas.EndDraw;
  end;
end;

end.
