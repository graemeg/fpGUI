unit fpreport_export_fpgui;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  fpreport_export_aggpas,
  fpg_base,
  fpg_main,
  fpg_form;

type

  TFPReportExportCanvas = class(TFPReportExportAggPas)
  private
    FCanvas: TfpgCanvas;
    FPageNumber: Integer;
    procedure   SetPageNumber(AValue: Integer);
  protected
    procedure   BufferToFile(const APageNo: integer); override;
    procedure   SetupRenderBuffer(const APage: TFPReportPage); override;
  public
    constructor Create(AOwner: TComponent); override;
    property    Canvas: TfpgCanvas read FCanvas write FCanvas;
    property    PageNumber: Integer read FPageNumber write SetPageNumber;
  end;


implementation

const
  RGBA_Width = 4;


{ TFPReportExportCanvas }

procedure TFPReportExportCanvas.SetPageNumber(AValue: Integer);
begin
  if FPageNumber = AValue then
    Exit;
  FPageNumber := AValue;
end;

procedure TFPReportExportCanvas.BufferToFile(const APageNo: integer);
var
  image: TfpgImage;
  p: Pointer;
begin
  // based on APageNo, do an early exit or not
  if APageNo <> FPageNumber then
    Exit;

  image := TfpgImage.Create;
  image.AllocateImage(32, ImageWidth, ImageHeight);

  try
    p := image.ImageData;
    Move(RenderBuffer[0], p^, image.ImageDataSize);
    image.UpdateImage;
    Canvas.DrawImage(0, 0, image);
  finally
    image.Free;
  end;
end;

procedure TFPReportExportCanvas.SetupRenderBuffer(const APage: TFPReportPage);
var
  lStride: Integer;
begin
  SetLength(FRenderBuffer, 0);

  ImageWidth := Round(mmToPixels(APage.PageSize.Width));
  ImageHeight := Round(mmToPixels(APage.PageSize.Height));

  SetLength(FRenderBuffer, ImageWidth * ImageHeight * RGBA_Width);

  // we use the -1 multiplier to flip the AggPas image co-ordinate system
  lStride := (ImageWidth * RGBA_Width);
  FAgg.Attach(@(RenderBuffer[0]), ImageWidth, ImageHeight, lStride);

  FAgg.clearAll(255, 255, 255);
  FAgg.flipText(True);
end;

constructor TFPReportExportCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageNumber := 1;
end;

end.
