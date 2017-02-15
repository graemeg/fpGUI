{ Same as pipeline7 example, but where warp lens is controlled with the
  mouse click location. }
program example_pipeline9;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  ,SysUtils
  ,fpg_base
  ,fpg_main
  ,fpg_form
  ,fpg_dialogs
//  ,Agg2D
  ,agg_basics
  ,agg_color
  ,agg_rendering_buffer
  ,agg_pixfmt
  ,agg_pixfmt_rgba  // ,agg_pixfmt_rgb
  ,agg_renderer_base
  ,agg_renderer_scanline
  ,agg_scanline_u
  ,agg_rasterizer_scanline_aa
  ,agg_path_storage
  ,agg_render_scanlines
  ,agg_conv_curve
  ,agg_conv_stroke
  ,agg_trans_affine
  ,agg_conv_transform
  ,agg_conv_segmentator
  ,agg_trans_warp_magnifier
  ;

type
  TMainForm = class(TfpgForm)
  private
    FTrackMouse: boolean;
    Fpt: TPoint;
    FImage: TfpgImage;
    procedure   FormShow(Sender: TObject);
    procedure   FormMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure   FormMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   FormMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   FormPaint(Sender: TObject);
    procedure   DoAggPainting;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  ShowMessage('This example is exactly the same as ''pipeline7'', but ' +
    'this one tracks the mouse position when you hold down the left mouse button. ' +
    'The warp lense will be rendered at the mouse position', 'Quick Help', true);
end;

procedure TMainForm.FormMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
begin
  if not FTrackMouse then
    exit;

  Fpt := AMousePos;
  DoAggPainting;
  Repaint;
end;

procedure TMainForm.FormMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
  const AMousePos: TPoint);
begin
  FTrackMouse := False;
end;

procedure TMainForm.FormMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
  const AMousePos: TPoint);
begin
  FTrackMouse := True;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  Canvas.DrawImage(0, 0 , FImage);
end;

procedure TMainForm.DoAggPainting;
var
  rbuf: rendering_buffer;
  pixf: pixel_formats;
  ren_base: renderer_base;
  clr: aggclr;
  sl: scanline_u8;
  ras: rasterizer_scanline_aa;
  ren_sl: renderer_scanline_aa_solid;
  path: path_storage;
  curve: conv_curve;
  stroke: conv_stroke;
  matrix: trans_affine;
  trans_curve: conv_transform;
  trans_stroke: conv_transform;
  trans_warp: conv_transform;
  trans_warp_stroke: conv_transform;
  lens: trans_warp_magnifier;
  segm: conv_segmentator;

  buffer: pointer;
begin

  // create the rendering buffer object and attach it to the image data of the TfpgImage instance
  //--------------------
  buffer := FImage.ScanLine[0];
  rbuf.Construct;
  rbuf.attach(
      buffer ,
      FImage.Width ,
      FImage.Height ,
      FImage.Width * 4 {4 bytes per pixel} );

  // Create Pixel Format and Basic renderers
  //--------------------
  pixfmt_bgra32(pixf, @rbuf);
  ren_base.Construct(@pixf);

  // At last we do some very simple things, like clear
  //--------------------
  clr.ConstrInt(255, 250, 230);
  ren_base.clear(@clr);

  // Create Scanline Container, Scanline Rasterizer,
  // and Scanline Renderer for solid fill.
  //--------------------
  sl.Construct;
  ras.Construct;
  ren_sl.Construct(@ren_base);

  // Create Vertex Source (path) object, in our case it's
  // path_storage and form the path.
  //--------------------
  path.Construct;
  path.remove_all;  // Not obligatory in this case
  path.move_to(10, 10);
  path.line_to(FImage.Width-10, 10);
  path.line_to(FImage.Width-10, FImage.Height-10);
  path.line_to(10, FImage.Height-10);
  path.line_to(10, FImage.Height-20);
  path.curve4(FImage.Width-20, FImage.Height-20,
              FImage.Width-20, 20,
              10, 20);
  path.close_polygon;

  // The vectorial pipeline
  //-----------------------
  matrix.Construct;
  matrix.translate(-FImage.Width/2, -FImage.Height/2);
  matrix.rotate(deg2rad(35.0));
  matrix.scale(0.3, 0.45);
  matrix.translate(FImage.Width/2, FImage.Height/2);

  lens.Construct;
  // should we use the mouse pointer position or not
  if (Fpt.X = 0) and (Fpt.Y = 0) then
    lens.center(120, 100)
  else
    lens.center(Fpt.X, Fpt.Y);

  lens.magnification(3.0);
  lens.radius(18);

  curve.Construct(@path);  // generates the curve line segments
  segm.Construct(@curve);

  trans_curve.Construct(@segm, @matrix);  // apply transformation against the curve
  trans_warp.Construct(@trans_curve, @lens);
  ras.add_path(@trans_warp);

  // Set the fill color and render the polygon:
  //-----------------------
  clr.ConstrInt(160, 180, 80);
  ren_sl.color_(@clr);
  render_scanlines(@ras, @sl, @ren_sl);

  stroke.Construct(@segm); // generates the stroke rendering
  trans_stroke.Construct(@stroke, @matrix);  // apply transformation against the stroke
  trans_warp_stroke.Construct(@trans_stroke, @lens);

  stroke.width_(6.0);

  // Set the stroke color and render the stroke:
  //-----------------------
  ras.add_path(@trans_warp_stroke);
  clr.ConstrInt(120, 100, 0);
  ren_sl.color_(@clr);
  render_scanlines(@ras, @sl, @ren_sl);
  //==========[ end pipeline ]============

  trans_warp_stroke.Destruct;
  trans_warp.Destruct;
  trans_stroke.Destruct;
  trans_curve.Destruct;
  stroke.Destruct;
  curve.Destruct;
  path.Destruct;
  ras.Destruct;
  sl.Destruct;
  rbuf.Destruct;

end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'AggPas Tutorial';
  WindowPosition := wpOneThirdDown;
  SetPosition(100, 100, 200, 200);
  OnPaint := @FormPaint;
  OnMouseDown := @FormMouseDown;
  OnMouseUp := @FormMouseUp;
  OnMouseMove := @FormMouseMove;
  OnShow := @FormShow;

  FTrackMouse := False;

  FImage := TfpgImage.Create;
  FImage.AllocateImage(32, Width, Height);

  DoAggPainting;
  FImage.UpdateImage;
end;

destructor TMainForm.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


