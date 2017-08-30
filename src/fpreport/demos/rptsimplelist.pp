unit rptsimplelist;


{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  udapp;

type
  TSimpleListDemo = class(TReportDemoApp)
  private
    lReportData: TFPReportUserData;
    sl: TStringList;
    procedure   GetReportDataFirst(Sender: TObject);
    procedure   GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure   GetReportDataNames(Sender: TObject; List: TStrings);
  Protected
    procedure   InitialiseData; override;
    procedure   CreateReportDesign; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;


implementation

uses
  fpTTF;

{ TSimpleListDemo }

procedure TSimpleListDemo.GetReportDataFirst(Sender: TObject);
begin
  {$IFDEF gdebug}
  writeln('GetReportDataFirst');
  {$ENDIF}
end;

procedure TSimpleListDemo.GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataValue - %d', [lReportData.RecNo]));
  {$ENDIF}
  if AValueName = 'element' then
  begin
    AValue := sl[lReportData.RecNo-1];
  end;
end;

procedure TSimpleListDemo.GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  {$IFDEF gdebug}
  writeln(Format('GetReportDataEOF - %d', [lReportData.RecNo]));
  {$ENDIF}
  if lReportData.RecNo > sl.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TSimpleListDemo.GetReportDataNames(Sender: TObject; List: TStrings);
begin
  List.Add('element');
end;

procedure TSimpleListDemo.InitialiseData;
var
  i: integer;
begin
  sl := TStringList.Create;
  for i := 1 to 30 do
    sl.Add(Format('Item %d', [i]));
end;

procedure TSimpleListDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  SummaryBand: TFPReportSummaryBand;
begin
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 1 - Simple Listing';
  PaperManager.RegisterStandardSizes;

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
  Memo.Layout.Left := 55;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 10;
  Memo.Text := 'THE REPORT TITLE';

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 10;
  {$ifdef ColorBands}
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.BackgroundColor := clDataBand;
  {$endif}

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 60;
  Memo.Layout.Height := 5;
  Memo.Text := 'Hello world <[element]>.';

  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 20;
  {$ifdef ColorBands}
  PageFooter.Frame.Shape := fsRectangle;
  PageFooter.Frame.BackgroundColor := clPageHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 135;
  Memo.Layout.Top := 13;
  Memo.Layout.Width := 20;
  Memo.Layout.Height := 5;
  Memo.Text := 'Page [PageNo]';


  SummaryBand := TFPReportSummaryBand.Create(p);
  SummaryBand.Layout.Height := 40;
  {$ifdef ColorBands}
  SummaryBand.Frame.Shape := fsRectangle;
  SummaryBand.Frame.BackgroundColor := clReportTitleSummary;
  {$endif}

  Memo := TFPReportMemo.Create(SummaryBand);
  Memo.Layout.Left := 19;
  Memo.Layout.Top := 10;
  Memo.Layout.Width := 70;
  Memo.Layout.Height := 25;
  Memo.StretchMode := smActualHeight;
  Memo.Text := 'This is some long text that should be wrapping inside the memo. It has a 10mm left margin, and a 7mm right margin. 0mm margin top and bottom.';
  Memo.TextAlignment.LeftMargin := 10;
  Memo.TextAlignment.RightMargin := 7;
  Memo.TextAlignment.Horizontal := taWidth;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor := clLtGray;
end;

constructor TSimpleListDemo.Create(AOwner : TComponent);
begin
  Inherited;
  lReportData := TFPReportUserData.Create(Self);
  lReportData.OnGetValue := @GetReportDataValue;
  lReportData.OnGetEOF := @GetReportDataEOF;
  lReportData.OnFirst := @GetReportDataFirst;
  lReportData.OnGetNames := @GetReportDataNames;
end;

destructor TSimpleListDemo.Destroy;
begin
  FreeAndNil(lReportData);
  FreeAndNil(sl);
  inherited Destroy;
end;


end.

