unit rptshapes;


{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
{$IFDEF USEPOLYGON}
  reportpolygon,
{$ENDIF}
  udapp;

type
  TShapesDemo = class(TReportDemoApp)
  private
    lReportData: TFPReportUserData;
    sl: TStringList;
    procedure   GetReportDataFirst(Sender: TObject);
    procedure   GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
  protected
    procedure   InitialiseData; override;
    procedure   CreateReportDesign; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;


implementation

uses
  fpTTF;

{ TShapesDemo }

procedure TShapesDemo.GetReportDataFirst(Sender: TObject);
begin
  {$IFDEF gdebug}
  writeln('GetReportDataFirst');
  {$ENDIF}
end;

procedure TShapesDemo.GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataValue - %d', [lReportData.RecNo]));
  {$ENDIF}
  if AValueName = 'element' then
  begin
    AValue := sl[lReportData.RecNo-1];
  end;
end;

procedure TShapesDemo.GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataEOF - %d', [lReportData.RecNo]));
  {$ENDIF}
  if lReportData.RecNo > sl.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TShapesDemo.InitialiseData;
var
  i: integer;
begin
  sl := TStringList.Create;
  for i := 1 to 10 do
    sl.Add(Format('Item %d', [i]));
end;

procedure TShapesDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  Memo: TFPReportMemo;
  Shape: TFPReportShape;
{$IFDEF USEPOLYGON}
  Poly : TReportPolygon;
{$ENDIF}
begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 6 - Shapes';

  p := TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 30;
  p.Margins.Top := 20;
  p.Margins.Right := 30;
  p.Margins.Bottom := 20;
  p.Data := lReportData;
  p.Font.Name := 'LiberationSans';

  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Layout.Height := 200;
  {$ifdef ColorBands}
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clReportTitleSummary;
  {$endif}

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 40;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 100;
  Memo.Layout.Height := 20;
  Memo.Text := 'Demo 6 - Shapes';
  Memo.UseParentFont := False;
  Memo.Font.Size := 24;

  Shape := TFPReportShape.Create(TitleBand);
  Shape.Layout.Left := 0;
  Shape.Layout.Top := 40;
  Shape.Layout.Width := 40;
  Shape.Layout.Height := 25;
  Shape.ShapeType := stEllipse;
  Shape.Frame.Shape := fsRectangle;
  Shape.Frame.BackgroundColor := TFPReportColor($E0E0E0);

  Shape := TFPReportShape.Create(TitleBand);
  Shape.Layout.Left := 55;
  Shape.Layout.Top := 40;
  Shape.Layout.Width := 40;
  Shape.Layout.Height := 25;
  Shape.ShapeType := stCircle;
  Shape.Color:=clRed;
  Shape.Frame.Shape := fsRectangle;
  Shape.Frame.BackgroundColor := TFPReportColor($E0E0E0);

  Shape := TFPReportShape.Create(TitleBand);
  Shape.Layout.Left := 110;
  Shape.Layout.Top := 40;
  Shape.Layout.Width := 40;
  Shape.Layout.Height := 25;
  Shape.ShapeType := stLine;
//  Shape.Orientation := orNorth;
//  Shape.Orientation := orSouth;
//  Shape.Orientation := orEast;
//  Shape.Orientation := orWest;
//  Shape.Orientation := orNorthWest;
//  Shape.Orientation := orSouthEast;
//  Shape.Orientation := orNorthEast;
  Shape.Orientation := orSouthWest;
  Shape.Frame.Shape := fsRectangle;
  Shape.Frame.BackgroundColor := TFPReportColor($E0E0E0);

  Shape := TFPReportShape.Create(TitleBand);
  Shape.Layout.Left := 0;
  Shape.Layout.Top := 75;
  Shape.Layout.Width := 40;
  Shape.Layout.Height := 25;
  Shape.ShapeType := stSquare;
  Shape.Frame.Shape := fsRectangle;
  Shape.Frame.BackgroundColor := TFPReportColor($E0E0E0);

  Shape := TFPReportShape.Create(TitleBand);
  Shape.Layout.Left := 55;
  Shape.Layout.Top := 75;
  Shape.Layout.Width := 40;
  Shape.Layout.Height := 25;
  Shape.ShapeType := stTriangle;
//  Shape.Orientation := orNorth;
//  Shape.Orientation := orSouth;
//  Shape.Orientation := orEast;
//  Shape.Orientation := orWest;
  Shape.Orientation := orNorthEast;
//  Shape.Orientation := orSouthWest;
//  Shape.Orientation := orSouthEast;
//  Shape.Orientation := orNorthWest;
  Shape.Frame.Shape := fsRectangle;
  Shape.Frame.BackgroundColor := TFPReportColor($E0E0E0);

  Shape := TFPReportShape.Create(TitleBand);
  Shape.Layout.Left := 110;
  Shape.Layout.Top := 75;
  Shape.Layout.Width := 40;
  Shape.Layout.Height := 25;
  Shape.ShapeType := stRoundedRect;
  Shape.Color:=clBlue;
//  Shape.CornerRadius := 2;
  Shape.Frame.Shape := fsRectangle;
  Shape.Frame.BackgroundColor := TFPReportColor($E0E0E0);

{$IFDEF USEPOLYGON}
  Poly := TReportPolygon.Create(TitleBand);
  Poly.Layout.Left := 0;
  Poly.Layout.Top := 110;
  Poly.Layout.Width := 40;
  Poly.Layout.Height := 25;
  Poly.Corners:=5;
  Poly.Color:=clRed;
  Poly.LineWidth:=2;
  Poly.Frame.Shape := fsRectangle;
  Poly.Frame.BackgroundColor := TFPReportColor($E0E0E0);
{$ENDIF}

end;

constructor TShapesDemo.Create(AOwner : TComponent);
begin
  Inherited;
  lReportData := TFPReportUserData.Create(self);
  lReportData.OnGetValue := @GetReportDataValue;
  lReportData.OnGetEOF := @GetReportDataEOF;
  lReportData.OnFirst := @GetReportDataFirst;
end;

destructor TShapesDemo.Destroy;
begin
  FreeAndNil(lReportData);
  FreeAndNil(sl);
  inherited Destroy;
end;



end.

