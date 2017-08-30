unit rptcolumns;


{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  udapp;

type

  TColumnsDemo = class(TReportDemoApp)
  private
    FDataPage1: TFPReportUserData;
    FDataPage2: TFPReportUserData;
    FStringListPage1: TStringList;
    FStringListPage2: TStringList;
    procedure   GetReportDataPage1Value(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   GetReportDataPage1EOF(Sender: TObject; var IsEOF: Boolean);
    procedure   GetReportDataPage1Names(Sender: TObject; List: TStrings);
    procedure   GetReportDataPage2Value(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   GetReportDataPage2EOF(Sender: TObject; var IsEOF: Boolean);
    procedure   GetReportDataPage2Names(Sender: TObject; List: TStrings);
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

{ TColumnsDemo }

procedure TColumnsDemo.GetReportDataPage1Value(Sender: TObject; const AValueName: String; var AValue: Variant);
begin
  if AValueName = 'p1element' then
  begin
    AValue := FStringListPage1[FDataPage1.RecNo-1];
  end;
end;

procedure TColumnsDemo.GetReportDataPage1EOF(Sender: TObject; var IsEOF: Boolean);
begin
  if FDataPage1.RecNo > FStringListPage1.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TColumnsDemo.GetReportDataPage2Value(Sender: TObject; const AValueName: String; var AValue: Variant);
begin
  if AValueName = 'p2element' then
  begin
    AValue := FStringListPage2[FDataPage2.RecNo-1];
  end;
end;

procedure TColumnsDemo.GetReportDataPage2EOF(Sender: TObject; var IsEOF: Boolean);
begin
  if FDataPage2.RecNo > FStringListPage2.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TColumnsDemo.InitialiseData;
var
  i: integer;
begin
  FStringListPage1 := TStringList.Create;
  for i := 1 to 50 do
    FStringListPage1.Add(Format('DataItem %d', [i]));

  FStringListPage2 := TStringList.Create;
  for i := 1 to 77 do
    FStringListPage2.Add(Format('Item %d', [i]));
end;

procedure TColumnsDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  PageHeader: TFPReportPageHeaderBand;
  ColumnHeader: TFPReportColumnHeaderBand;
  ColumnFooter: TFPReportColumnFooterBand;
  DataHeader: TFPReportDataHeaderBand;
  DataFooter: TFPReportDataFooterBand;
  ChildBand: TFPReportChildBand;
begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 9 - Multi Columns';

  { Page 1 }
  p := TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 10;
  p.Margins.Top := 10;
  p.Margins.Right := 10;
  p.Margins.Bottom := 10;
  p.Data := FDataPage1;
  p.Font.Name := 'LiberationSans';

  p.ColumnCount := 2;
  p.ColumnGap := 10;

  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Layout.Height := 277;
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := TFPReportColor($003366);

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 3;
  Memo.Layout.Top := 3;
  Memo.Layout.Width := TitleBand.Layout.Width-6;
  Memo.Layout.Height := TitleBand.Layout.Height-6;
  Memo.Text := 'THE REPORT TITLE' + LineEnding + '(start of designed report page 1)';
  Memo.UseParentFont := False;
  Memo.Font.Name := 'LiberationSans-Bold';
  Memo.Font.Size := 20;
  Memo.Font.Color := TFPReportColor($5B7290);
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := TFPReportColor($663366);
  Memo.Frame.BackgroundColor := TFPReportColor($E7EBF0);
  Memo.Frame.Width := 5;

  PageHeader := TFPReportPageHeaderBand.Create(p);
  PageHeader.Layout.Height := 30;
  PageHeader.VisibleOnPage := vpNotOnFirst;
  PageHeader.Frame.Shape := fsRectangle;
  PageHeader.Frame.BackgroundColor := TFPReportColor($003366);

  Memo := TFPReportMemo.Create(PageHeader);
  Memo.Layout.Left := 55;
  Memo.Layout.Top := 15;
  Memo.Layout.Width := 70;
  Memo.Layout.Height := 10;
  Memo.UseParentFont := False;
  Memo.Font.Color := clWhite;
  Memo.Text := 'PageHeader - designed report page 1';

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 6;
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.Color := TFPReportColor($B1BDCD);
  DataBand.Frame.BackgroundColor := TFPReportColor($E7EBF0);

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 55;
  Memo.Layout.Height := 6;
  Memo.Text := 'DataBand <[p1element]>.';
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.TextAlignment.Vertical := tlCenter;

  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 20;
  PageFooter.VisibleOnPage := vpNotOnFirst;
  PageFooter.Frame.Shape := fsRectangle;
  PageFooter.Frame.BackgroundColor := TFPReportColor($5B7290);

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 135;
  Memo.Layout.Top := 9;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 20;
  Memo.Text := 'PageFooter Band' + LineEnding + '<i>Page [PageNo]</i>';
  Memo.TextAlignment.Horizontal := taRightJustified;
  Memo.Options := [moAllowHTML];

  { Page 2 }
  p := TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 30;
  p.Margins.Top := 20;
  p.Margins.Right := 30;
  p.Margins.Bottom := 20;
  p.Data := FDataPage2;
  p.Font.Name := 'LiberationSans';

  p.ColumnCount := 3;
  p.ColumnGap := 5;

  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Layout.Height := 40;
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clLtGray;

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 55;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := 65;
  Memo.Layout.Height := 10;
  Memo.Text := 'THE REPORT TITLE' + LineEnding + '(Start of Designed Report Page 2)';

  PageHeader := TFPReportPageHeaderBand.Create(p);
  PageHeader.Layout.Height := 20;
  PageHeader.Frame.Shape := fsRectangle;
  PageHeader.Frame.BackgroundColor := clTeal;

  Memo := TFPReportMemo.Create(PageHeader);
  Memo.Layout.Left := 10;
  Memo.Layout.Top := 5;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 10;
  Memo.Text := 'Page Header Band  - designed report page 2';

  ColumnHeader := TFPReportColumnHeaderBand.Create(p);
  ColumnHeader.Layout.Height := 15;
  ColumnHeader.Frame.Shape := fsRectangle;
  ColumnHeader.Frame.BackgroundColor := clDkRed;

  Memo := TFPReportMemo.Create(ColumnHeader);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 2.5;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 10;
  Memo.UseParentFont := False;
  Memo.Font.Name := 'LiberationSans-Bold';
  Memo.Font.Color := clWhite;
  Memo.Text := 'ColumnHeader Band';
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.TextAlignment.Vertical := tlCenter;

  DataHeader := TFPReportDataHeaderBand.Create(p);
  DataHeader.Layout.Height := 10;
  DataHeader.Frame.Shape := fsRectangle;
  DataHeader.Frame.BackgroundColor := TFPReportColor($ffa500);

  Memo := TFPReportMemo.Create(DataHeader);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 1.5;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 8;
  Memo.UseParentFont := False;
  Memo.Font.Name := 'LiberationSans-Bold';
  Memo.Font.Color := clWhite;
  Memo.Text := 'DataHeader Band';
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.TextAlignment.Vertical := tlCenter;

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 10;
  DataBand.Data := FDataPage2;
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.BackgroundColor := clDataBand;
  { associated DataHeader band }
  DataBand.HeaderBand := DataHeader;

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 1;
  Memo.Layout.Width := 40;
  Memo.Layout.Height := 5;
  Memo.Text := 'DataBand <[p2element]>.';

  ChildBand := TFPReportChildBand.Create(p);
  ChildBand.Layout.Height := 9;
  ChildBand.Frame.Shape := fsRectangle;
  ChildBand.Frame.BackgroundColor := clLtGray;

  Memo := TFPReportMemo.Create(ChildBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 1;
  Memo.Layout.Width := 40;
  Memo.Layout.Height := 5;
  Memo.Text := 'ChildBand - [p2element]';

  DataBand.ChildBand := ChildBand;

  DataFooter := TFPReportDataFooterBand.Create(p);
  DataFooter.Layout.Height := 10;
  DataFooter.Frame.Shape := fsRectangle;
  DataFooter.Frame.BackgroundColor := TFPReportColor($ffa500);
  DataFooter.UseParentFont := False;
  DataFooter.Font.Name := 'LiberationSans-Bold';
  DataFooter.Font.Color := clWhite;

  Memo := TFPReportMemo.Create(DataFooter);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 1.5;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 8;
  Memo.Text := 'DataFooter Band';
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.TextAlignment.Vertical := tlCenter;

  ColumnFooter := TFPReportColumnFooterBand.Create(p);
  ColumnFooter.Layout.Height := 15;
  ColumnFooter.Frame.Shape := fsRectangle;
  ColumnFooter.Frame.BackgroundColor := clGreen;
//  ColumnFooter.FooterPosition := fpAfterLast;

  Memo := TFPReportMemo.Create(ColumnFooter);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 2.5;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 10;
  Memo.Text := 'ColumnFooter Band';
  Memo.TextAlignment.Horizontal := taLeftJustified;
  Memo.TextAlignment.Vertical := tlCenter;

  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 20;
  PageFooter.Frame.Shape := fsRectangle;
  PageFooter.Frame.BackgroundColor := clLtGray;

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 135;
  Memo.Layout.Top := 13;
  Memo.Layout.Width := 20;
  Memo.Layout.Height := 5;
  Memo.Text := 'Page [PageNo]';

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 10;
  Memo.Layout.Top := 5;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 10;
  Memo.Text := 'PageFooter Band';
end;

constructor TColumnsDemo.Create(AOwner: TComponent);
begin
  Inherited;
  FDataPage1 := TFPReportUserData.Create(nil);
  FDataPage1.OnGetValue := @GetReportDataPage1Value;
  FDataPage1.OnGetEOF := @GetReportDataPage1EOF;
  FDataPage1.OnGetNames := @GetReportDataPage1Names;
  FDataPage2 := TFPReportUserData.Create(nil);
  FDataPage2.OnGetValue := @GetReportDataPage2Value;
  FDataPage2.OnGetEOF := @GetReportDataPage2EOF;
  FDataPage2.OnGetNames := @GetReportDataPage2Names;
end;

destructor TColumnsDemo.Destroy;
begin
  FreeAndNil(FDataPage1);
  FreeAndNil(FDataPage2);
  FreeAndNil(FStringListPage1);
  FreeAndNil(FStringListPage2);
  inherited Destroy;
end;

procedure TColumnsDemo.GetReportDataPage2Names(Sender: TObject; List: TStrings);
begin
  List.Add('p2element');
end;

procedure TColumnsDemo.GetReportDataPage1Names(Sender: TObject; List: TStrings);
begin
  List.Add('p1element');
end;


end.

