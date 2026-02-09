{ Just for fun we added an extra "effect", by applying a very
  fast blur algorithm at the end. }
program example_runtime_renderpath_alteration;

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
  ,fpg_checkbox
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
  ,agg_blur
  ;

type
  TMainForm = class(TfpgForm)
  private
    cb: TfpgCheckbox;
    FImage: TfpgImage;
    FHasCurve: boolean;
    procedure   CheckBoxChecked(Sender: TObject);
    procedure   FormPaint(Sender: TObject);
    procedure   DoAggPainting;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

{ TMainForm }

procedure TMainForm.CheckBoxChecked(Sender: TObject);
begin
  FHasCurve := cb.Checked;
  DoAggPainting;
  Repaint;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  Canvas.DrawImage(0, 0 , FImage);
end;

procedure TMainForm.DoAggPainting;
const
  cx = 100.0;
  cy = 100.0;
  rx = 25.0;
  ry = 50.0;
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
  path.move_to(cx - rx, cy);
  path.curve4(cx - rx, cy + ry * 0.56,
              cx - rx * 0.56, cy + ry,
              cx, cy + ry);
  path.curve4(cx + rx * 0.56, cy + ry,
              cx + rx, cy + ry * 0.56,
              cx + rx, cy);
  path.curve4(cx + rx, cy - ry * 0.56,
              cx + rx * 0.56, cy - ry,
              cx, cy - ry);
  path.curve4(cx - rx * 0.56, cy - ry,
              cx - rx, cy - ry * 0.56,
              cx - rx, cy);
  path.close_polygon();


  // The vectorial pipeline
  //-----------------------
  if FHasCurve then
  begin
    curve.Construct(@path);
    stroke.Construct(@curve);  // generates the stroke rendering
  end
  else
  begin
    stroke.Construct(@path);  // generates the stroke rendering
  end;
  stroke.width_(4.0);

  // Set the stroke color and render the stroke:
  //-----------------------
  ras.add_path(@stroke);
  clr.ConstrInt(120, 100, 0);
  ren_sl.color_(@clr);
  render_scanlines(@ras, @sl, @ren_sl);
  //==========[ end pipeline ]============

  stroke.Destruct;
  if FHasCurve then
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

  FHasCurve := True;

  FImage := TfpgImage.Create;
  FImage.AllocateImage(32, Width, Height);

  DoAggPainting;
  FImage.UpdateImage;

  cb := CreateCheckBox(self, 2, 2, 'Include Curve?');
  cb.Checked := FHasCurve;
  cb.OnChange := @CheckBoxChecked;
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


