unit vertex.wgt.previewbar;

{
  TVertexPreviewBar — renders the HVIF icon simultaneously at 16, 32, 48 and 64 px.

  Sits at the top of the right panel.  Rebuilds the icon from the document
  whenever DocumentChanged is called (same trigger as the main canvas).

  Layout (top → bottom):
    ┌───────────────────────────────────────┐  ← CELLS_H = 76 px
    │  [16]  [32]   [48]    [  64  ]        │    cells centred vertically
    ├───────────────────────────────────────┤  ← 1px separator
    │  ☑ Checker                            │  ← FOOTER_H = 22 px
    └───────────────────────────────────────┘

  The "Checker" checkbox toggles the checkerboard transparency background on
  all four cells.  When unchecked, cells use a plain white background instead.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_widget,
  fpg_checkbox,
  fpg_hvif, fpg_hvif_writer,
  fpg_vertex_document;


type
  TVertexPreviewBar = class(TfpgWidget)
  private
    FDocument:   TVertexDocument;   { not owned }
    FIcon:       THvifIcon;      { owned; rebuilt when FIconDirty is True }
    FIconDirty:  Boolean;
    FChkChecker: TfpgCheckBox;   { owned; toggles checkerboard background }

    procedure RebuildIcon;
    procedure DrawCell(AX, AY, ASize: Integer);
    procedure ChkCheckerChanged(Sender: TObject);

  protected
    procedure HandlePaint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { Connect to the document. Pass nil to disconnect. }
    procedure SetDocument(ADoc: TVertexDocument);

    { Mark icon as dirty and repaint — call whenever the document changes. }
    procedure DocumentChanged;
  end;


implementation

const
  PREVIEW_SIZES: array[0..3] of Integer = (16, 32, 48, 64);
  CELL_GAP    = 8;    { horizontal gap between cells }
  MARGIN_X    = 6;    { left margin before first cell }
  CHECKER     = 4;    { checkerboard tile size in pixels }
  CELLS_H     = 76;   { cell zone: 64 (largest icon) + 6 top + 6 bottom }
  FOOTER_H    = 22;   { checkbox strip at the bottom }
  BAR_HEIGHT  = CELLS_H + FOOTER_H;   { = 98 }
  CHK_W       = 80;   { checkbox widget width }
  CHK_H       = 18;   { checkbox widget height }

  COL_CHECKER1: TfpgColor = $FFDDDDDD;
  COL_CHECKER2: TfpgColor = $FFAAAAAA;
  COL_PLAIN:    TfpgColor = $FFFFFFFF;
  COL_CELL_BDR: TfpgColor = $FF888888;
  COL_SEP:      TfpgColor = $FFBBBBBB;
  COL_BG:       TfpgColor = $FFD0D0D0;


{ ── TVertexPreviewBar ────────────────────────────────────────────────────────── }

constructor TVertexPreviewBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument  := nil;
  FIcon      := nil;
  FIconDirty := False;
  PreferredSize := fpgSize(220, BAR_HEIGHT);

  FChkChecker := TfpgCheckBox.Create(Self);
  FChkChecker.Text     := 'Checker';
  FChkChecker.Checked  := True;
  FChkChecker.OnChange := @ChkCheckerChanged;
  FChkChecker.SetPosition(MARGIN_X, CELLS_H + (FOOTER_H - CHK_H) div 2, CHK_W, CHK_H);
end;

destructor TVertexPreviewBar.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;

procedure TVertexPreviewBar.SetDocument(ADoc: TVertexDocument);
begin
  FDocument  := ADoc;
  FIconDirty := (ADoc <> nil);
  FreeAndNil(FIcon);
  Repaint;
end;

procedure TVertexPreviewBar.DocumentChanged;
begin
  FIconDirty := True;
  Repaint;
end;

procedure TVertexPreviewBar.ChkCheckerChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TVertexPreviewBar.RebuildIcon;
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

procedure TVertexPreviewBar.DrawCell(AX, AY, ASize: Integer);
var
  col, row: Integer;
  img:      TfpgImage;
  r: TfpgRect;
begin
  if FChkChecker.Checked then
  begin
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
  end
  else
  begin
    Canvas.SetColor(COL_PLAIN);
    Canvas.FillRectangle(AX, AY, ASize, ASize);
  end;

  if FIcon <> nil then
  begin
    img := FIcon.GetImage(ASize, ASize);
    if img <> nil then
      Canvas.DrawImage(AX, AY, img);
  end;

  Canvas.SetColor(COL_CELL_BDR);
  r.SetRect(AX, AY, ASize, ASize);
  r.InflateRect(1, 1);
  Canvas.DrawRectangle(r);
end;

procedure TVertexPreviewBar.HandlePaint;
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

    { Draw the four preview cells, centred in the CELLS_H zone }
    cx := MARGIN_X;
    for i := 0 to High(PREVIEW_SIZES) do
    begin
      cy := (CELLS_H - PREVIEW_SIZES[i]) div 2;
      DrawCell(cx, cy, PREVIEW_SIZES[i]);
      Inc(cx, PREVIEW_SIZES[i] + CELL_GAP);
    end;

    { Separator between cell zone and footer }
    Canvas.SetColor(COL_SEP);
    Canvas.DrawLine(0, CELLS_H, Width, CELLS_H);
  finally
    Canvas.EndDraw;
  end;
  { FChkChecker is a child widget and paints itself }
end;

end.
