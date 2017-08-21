unit rptjson;

{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  fpreportjson,
  udapp;

type

  TJSONDemo = class(TReportDemoApp)
  private
    FReportData : TFPReportJSONData;
  Protected
    procedure   InitialiseData; override;
    procedure   CreateReportDesign;override;
    procedure   LoadDesignFromFile(const AFilename: string);
    procedure   HookupData(const AComponentName: string; const AData: TFPReportData);
  public
    constructor Create(AOWner :TComponent); override;
    destructor  Destroy; override;
  end;


implementation

uses
  fpReportStreamer,
  fpTTF,
  fpJSON,
  jsonparser;

{ TJSONDemo }

procedure TJSONDemo.InitialiseData;
begin
  FReportData.Path:='Data';
  FReportData.LoadFromFile('countries.json');
end;

procedure TJSONDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  GroupHeader: TFPReportGroupHeaderBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;

begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 12 - JSON Data';

  p :=  TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 30;
  p.Margins.Top := 20;
  p.Margins.Right := 30;
  p.Margins.Bottom := 20;
  p.Data := FReportData;
  p.Font.Name := 'LiberationSans';

  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Layout.Height := 40;
  {$ifdef ColorBands}
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clReportTitleSummary;
  {$endif}

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 35;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := 80;
  Memo.Layout.Height := 10;
  Memo.Text := 'COUNTRY AND POPULATION AS OF 2014';

  GroupHeader := TFPReportGroupHeaderBand.Create(p);
  GroupHeader.Layout.Height := 15;
  GroupHeader.GroupCondition := 'copy(''[Name]'',1,1)';
  {$ifdef ColorBands}
  GroupHeader.Frame.Shape := fsRectangle;
  GroupHeader.Frame.BackgroundColor := clGroupHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(GroupHeader);
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 5;
  Memo.Layout.Width := 10;
  Memo.Layout.Height := 8;
  Memo.UseParentFont := False;
  Memo.Text := '[copy(Name,1,1)]';
  Memo.Font.Size := 16;

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 8;
  {$ifdef ColorBands}
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.BackgroundColor := clDataBand;
  {$endif}

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := '[Name]';

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 70;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 5;
  Memo.Text := '[formatfloat(''#,##0'', Population)]';


  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 20;
  {$ifdef ColorBands}
  PageFooter.Frame.Shape := fsRectangle;
  PageFooter.Frame.BackgroundColor := clPageHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 130;
  Memo.Layout.Top := 13;
  Memo.Layout.Width := 20;
  Memo.Layout.Height := 5;
  Memo.Text := 'Page [PageNo]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taRightJustified;
end;

procedure TJSONDemo.LoadDesignFromFile(const AFilename: string);
var
  rs: TFPReportJSONStreamer;
  fs: TFileStream;
  lJSON: TJSONObject;
begin
  if AFilename = '' then
    Exit;
  if not FileExists(AFilename) then
    raise Exception.CreateFmt('The file "%s" can not be found', [AFilename]);

  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    lJSON := TJSONObject(GetJSON(fs));
  finally
    fs.Free;
  end;

  rs := TFPReportJSONStreamer.Create(nil);
  rs.JSON := lJSON; // rs takes ownership of lJSON
  try
    rpt.ReadElement(rs);
  finally
    rs.Free;
  end;
end;

procedure TJSONDemo.HookupData(const AComponentName: string; const AData: TFPReportData);
var
  b: TFPReportCustomBandWithData;
begin
  b := TFPReportCustomBandWithData(rpt.FindRecursive(AComponentName));
  if Assigned(b) then
    b.Data := AData;
end;

constructor TJSONDemo.Create(AOwner: TComponent);
begin
  inherited;
  FReportData := TFPReportJSONData.Create(nil);
end;

destructor TJSONDemo.Destroy;
begin
  FreeAndNil(FReportData);
  inherited Destroy;
end;

end.

