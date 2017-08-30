unit rptimages;

{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  SysUtils,
  Classes,
  fpreport,
  udapp;

type
  TImagesDemo = class(TReportDemoApp)
  private
    lReportData: TFPReportUserData;
    sl: TStringList;
    procedure   GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
  protected
    procedure   InitialiseData; override;
    procedure   CreateReportDesign; override;
  public
    constructor Create(AOWner : TComponent); override;
    destructor  Destroy; override;
    procedure GetReportDataNames(Sender: TObject; List: TStrings);
  end;


implementation

uses
  fpTTF,
  FPCanvas;

{ TImagesDemo }

procedure TImagesDemo.GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
begin
  if AValueName = 'country' then
  begin
    AValue := sl.Names[lReportData.RecNo-1];
  end
  else if AValueName = 'population' then
  begin
    AValue := sl.Values[sl.Names[lReportData.RecNo-1]];
  end;
end;

procedure TImagesDemo.GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  if lReportData.RecNo > sl.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TImagesDemo.InitialiseData;
begin
  sl := TStringList.Create;
  {$I countries.inc}
  sl.Sort;
end;

procedure TImagesDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  PageHeader: TFPReportPageHeaderBand;
  Image: TFPReportImage;
  Checkbox: TFPReportCheckbox;
begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 7 - Images and Checkboxes';

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
  TitleBand.Layout.Height := 40;
  {$ifdef ColorBands}
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clReportTitleSummary;
  {$endif}

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 140;
  Memo.Layout.Height := 15;
  Memo.Text := 'Country and Population as of 2014';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.UseParentFont := False;
  Memo.Font.Color := TFPReportColor($000080);
  Memo.Font.Name := 'LiberationSerif';
  Memo.Font.Size := 24;

  PageHeader := TFPReportPageHeaderBand.Create(p);
  PageHeader.Layout.Height := 8;
  PageHeader.Frame.BackgroundColor := TFPReportColor($000080);
  PageHeader.Frame.Shape := fsRectangle;
  PageHeader.VisibleOnPage := vpNotOnFirst;
  {$ifdef ColorBands}
  PageHeader.Frame.Shape := fsRectangle;
  PageHeader.Frame.BackgroundColor := clPageHeaderFooter;
  {$endif}
  PageHeader.UseParentFont := False;
  PageHeader.Font.Color := clWhite;

  Memo := TFPReportMemo.Create(PageHeader);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 1.5;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 5;
  Memo.Text := 'Country';
  Memo.TextAlignment.Vertical := tlCenter;
  {$ifdef ColorBands}
  // just so the text is visible in this situation
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor := clNavy;
  {$endif}

  Memo := TFPReportMemo.Create(PageHeader);
  Memo.Layout.Left := 70;
  Memo.Layout.Top := 1.5;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 5;
  Memo.Text := 'Population';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taRightJustified;
  {$ifdef ColorBands}
  // just so the text is visible in this situation
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor := clNavy;
  {$endif}

  Memo := TFPReportMemo.Create(PageHeader);
  Memo.Layout.Left := 115;
  Memo.Layout.Top := 1.5;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 5;
  Memo.Text := '> 30 million';
  Memo.TextAlignment.Vertical := tlCenter;
  {$ifdef ColorBands}
  // just so the text is visible in this situation
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor := clNavy;
  {$endif}


  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 8;
  DataBand.Data := lReportData;
  {$ifdef ColorBands}
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.BackgroundColor := clDataBand;
  {$endif}

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := '[country]';
  Memo.TextAlignment.Vertical := tlCenter;

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 70;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 5;
  Memo.Text := '[formatfloat(''#,##0'', StrToFloat(population))]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taRightJustified;

  Checkbox := TFPReportCheckbox.Create(DataBand);
  Checkbox.Layout.Left := 120;
  Checkbox.Layout.Top := 1;
  Checkbox.Expression := '[population] > 30000000';

  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 30;
  {$ifdef ColorBands}
  PageFooter.Frame.Shape := fsRectangle;
  PageFooter.Frame.BackgroundColor := clPageHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 130;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := 20;
  Memo.Layout.Height := 5;
  Memo.Text := 'Page [PageNo]';
  Memo.TextAlignment.Vertical := tlBottom;
  Memo.TextAlignment.Horizontal := taRightJustified;

  Image := TFPReportImage.Create(PageFooter);
  Image.Layout.Left := 0;
  Image.Layout.Top := 0;
  Image.Layout.Width := 40;
  Image.Layout.Height := 30;
  Image.LoadFromFile('company-logo.png');
  Image.Stretched := True;
end;

constructor TImagesDemo.Create(AOwner : TComponent);
begin
  Inherited;
  lReportData := TFPReportUserData.Create(Self);
  lReportData.OnGetValue := @GetReportDataValue;
  lReportData.OnGetEOF := @GetReportDataEOF;
  lReportData.OnGetNames := @GetReportDataNames;
end;

destructor TImagesDemo.Destroy;
begin
  FreeAndNil(lReportData);
  FreeAndNil(sl);
  inherited Destroy;
end;

procedure TImagesDemo.GetReportDataNames(Sender: TObject; List: TStrings);
begin
  List.Add('country');
  List.Add('population');
end;



end.

