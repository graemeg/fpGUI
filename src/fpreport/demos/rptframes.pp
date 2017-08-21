unit rptframes;

{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  udapp;

type

  TFramesDemo = class(TReportDemoApp)
  private
    lReportData: TFPReportUserData;
    sl: TStringList;
    procedure   GetReportDataFirst(Sender: TObject);
    procedure   GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure   GetReportFieldNames(Sender: TObject; List: TStrings);

  protected
    procedure   InitialiseData; override;
    procedure   CreateReportDesign; override;
  public
    constructor Create(AOwner : TComponent) ; override;
    destructor  Destroy; override;
  end;


implementation

uses
  FPCanvas,
  fpTTF;

{ TFramesDemo }

procedure TFramesDemo.GetReportDataFirst(Sender: TObject);
begin
  {$IFDEF gdebug}
  writeln('GetReportDataFirst');
  {$ENDIF}
end;

procedure TFramesDemo.GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataValue - %d', [lReportData.RecNo]));
  {$ENDIF}
  if AValueName = 'country' then
  begin
    AValue := sl.Names[lReportData.RecNo-1];
  end
  else if AValueName = 'population' then
  begin
    AValue := sl.Values[sl.Names[lReportData.RecNo-1]];
  end;
end;

procedure TFramesDemo.GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataEOF - %d', [lReportData.RecNo]));
  {$ENDIF}
  if lReportData.RecNo > sl.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TFramesDemo.GetReportFieldNames(Sender: TObject; List: TStrings);
begin
  {$IFDEF gdebug}
  writeln('********** GetReportFieldNames');
  {$ENDIF}
  List.Add('country');
  List.Add('population');
end;

procedure TFramesDemo.InitialiseData;
begin
  sl := TStringList.Create;
  {$I countries.inc}
  sl.Sort;
end;

procedure TFramesDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  GroupHeader: TFPReportGroupHeaderBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  ReportSummary: TFPReportSummaryBand;
  PageHeader: TFPReportPageHeaderBand;
begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 4 - Frames and Fonts';

  p :=  TFPReportPage.Create(rpt);
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
  Memo.Font.Color := clNavy;
  Memo.Font.Name := 'LiberationSerif';
  Memo.Font.Size := 24;

  PageHeader := TFPReportPageHeaderBand.Create(p);
  PageHeader.Layout.Height := 30;
  {$ifdef ColorBands}
  PageHeader.Frame.Shape := fsRectangle;
  PageHeader.Frame.BackgroundColor := clPageHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(PageHeader);
  Memo.Layout.Left := 55;
  Memo.Layout.Top := 15;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 10;
  Memo.Text := 'PageHeader band';

  GroupHeader := TFPReportGroupHeaderBand.Create(p);
  GroupHeader.Layout.Height := 15;
  GroupHeader.GroupCondition := 'copy(''[country]'',1,1)';
  GroupHeader.Frame.Color := clNavy;
  GroupHeader.Frame.Pen := psDashDot;
  {$ifdef ColorBands}
  GroupHeader.Frame.Shape := fsRectangle;
  GroupHeader.Frame.BackgroundColor := clGroupHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(GroupHeader);
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 5;
  Memo.Layout.Width := 10;
  Memo.Layout.Height := 8;
  Memo.Text := '[copy(country,1,1)]';
  Memo.UseParentFont := False;
  Memo.Font.Size := 16;
  Memo.Font.Color := TFPReportColor($C00000);
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := TFPReportColor($008080);
  Memo.Frame.Pen := psDot;

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

  { ReportSummary could have been designed before PageFooter. The layouting
    will sort out the order anyway. }
  ReportSummary := TFPReportSummaryBand.Create(p);
  ReportSummary.Layout.Height := 60;
  ReportSummary.StartNewPage := True;
  {$ifdef ColorBands}
  ReportSummary.Frame.Shape := fsRectangle;
  ReportSummary.Frame.BackgroundColor := clReportTitleSummary;
  {$endif}
  ReportSummary.UseParentFont := False;
  ReportSummary.Font.Size := 8;

  Memo := TFPReportMemo.Create(ReportSummary);
  Memo.Layout.Left := 3;
  Memo.Layout.Top := 3;
  Memo.Layout.Width := 100;
  Memo.Layout.Height := 5;
  Memo.Text := 'This block is the ReportSummary band - forced on a new page.';

  Memo := TFPReportMemo.Create(ReportSummary);
  Memo.Layout.Left := 20;
  Memo.Layout.Top := 10;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 15;
  Memo.Text := 'Lines: Left & Bottom';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Color := clNavy;
  Memo.Frame.Lines := [flLeft, flBottom];

  Memo := TFPReportMemo.Create(ReportSummary);
  Memo.Layout.Left := 90;
  Memo.Layout.Top := 10;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 15;
  Memo.Text := 'Lines: Top & Right';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Color := clNavy;
  Memo.Frame.Lines := [flTop, flRight];

  Memo := TFPReportMemo.Create(ReportSummary);
  Memo.Layout.Left := 20;
  Memo.Layout.Top := 40;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 15;
  Memo.Text := 'Lines: Top & Bottom';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Color := clNavy;
  Memo.Frame.Lines := [flTop, flBottom];

  Memo := TFPReportMemo.Create(ReportSummary);
  Memo.Layout.Left := 90;
  Memo.Layout.Top := 40;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 15;
  Memo.Text := 'Lines: Left & Right';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Color := clNavy;
  Memo.Frame.Lines := [flLeft, flRight];
end;

constructor TFramesDemo.Create(AOwner: TComponent);
begin
  inherited;
  lReportData := TFPReportUserData.Create(nil);
  lReportData.OnGetValue := @GetReportDataValue;
  lReportData.OnGetEOF := @GetReportDataEOF;
  lReportData.OnFirst := @GetReportDataFirst;
  lReportData.OnGetNames := @GetReportFieldNames;
end;

destructor TFramesDemo.Destroy;
begin
  FreeAndNil(lReportData);
  FreeAndNil(sl);
  inherited Destroy;
end;


end.

