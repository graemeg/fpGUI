unit rptmasterdetail;


{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  udapp;

type
  TMasterDetailDemo = class(TReportDemoApp)
  private
    FMasterData: TFPReportUserData;
    FDetailData: TFPReportUserData;
    FDetail2Data: TFPReportUserData;
    FMasterNo: Integer;
    FDetailNo: Integer;
    FDetail2No: Integer;
    procedure   MasterDataFirst(Sender: TObject);
    procedure   MasterDataGetValue(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure   MasterDataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure   MasterDataNext(Sender: TObject);
    procedure   DetailDataFirst(Sender: TObject);
    procedure   DetailDataGetValue(Sender: TObject; const AValueName: string; var AValue: variant);
    procedure   DetailDataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure   DetailDataNext(Sender: TObject);
    procedure   Detail2DataFirst(Sender: TObject);
    procedure   Detail2DataGetValue(Sender: TObject; const AValueName: string; var AValue: variant);
    procedure   Detail2DataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure   Detail2DataNext(Sender: TObject);
  Public
    procedure   CreateReportDesign; override;
    procedure MasterDataGetNames(Sender: TObject; List: TStrings);
    procedure DetailDataGetNames(Sender: TObject; List: TStrings);
    procedure Detail2DataGetNames(Sender: TObject; List: TStrings);
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;


implementation

uses
  fpTTF;

{ Our user defined data stored in arrays. }
const
  Master: array[1..3, 1..2] of String = ( // master ID, master name
    ('1', 'master 1'),
    ('2', 'master 2'),
    ('3', 'master 3'));

  Detail: array[1..12, 1..2] of String = ( // master ID, detail name
    ('1', 'detail 1.1'), ('1', 'detail 1.2'), ('1', 'detail 1.3'),
    ('1', 'detail 1.4'), ('1', 'detail 1.5'),

    ('2', 'detail 2.1'), ('2', 'detail 2.2'), ('2', 'detail 2.3'),

    ('3', 'detail 3.1'), ('3', 'detail 3.2'), ('3', 'detail 3.3'),
    ('3', 'detail 3.4'));

  D2: array[1..4, 1..2] of String = ( // detail #1 name, detail #2 name
    ('detail 1.2', 'detail 1.2.1'),
    ('detail 1.4', 'detail 1.4.1'),
    ('detail 1.4', 'detail 1.4.2'),
    ('detail 2.2', 'detail 2.2.1'));

{ TMasterDetailDemo }

procedure TMasterDetailDemo.MasterDataFirst(Sender: TObject);
begin
  {$IFDEF gdebug}
  writeln('MasterDataFirst');
  {$ENDIF}
  FMasterNo := 1;
end;

procedure TMasterDetailDemo.MasterDataGetValue(Sender: TObject; const AValueName: String; var AValue: Variant);
begin
  {$IFDEF gdebug}
  writeln(Format('MasterDataGetValue - %d', [FMasterData.RecNo]));
  {$ENDIF}
  if AValueName  = 'mastername' then
    AValue := Master[FMasterNo][2];
end;

procedure TMasterDetailDemo.MasterDataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  {$IFDEF gdebug}
  writeln(Format('MasterDataEOF - %d', [FMasterData.RecNo]));
  {$ENDIF}
  IsEOF := FMasterNo > High(Master);
end;

procedure TMasterDetailDemo.MasterDataNext(Sender: TObject);
begin
  Inc(FMasterNo);
end;

procedure TMasterDetailDemo.DetailDataFirst(Sender: TObject);
begin
  {$IFDEF gdebug}
  writeln('DetailDataFirst');
  {$ENDIF}
  FDetailNo := 1;
  while (not FDetailData.EOF) and (Detail[FDetailNo][1] <> Master[FMasterNo][1]) do
    Inc(FDetailNo);
end;

procedure TMasterDetailDemo.DetailDataGetValue(Sender: TObject; const AValueName: string; var AValue: variant);
begin
  {$IFDEF gdebug}
  writeln('DetailDataGetValue');
  {$ENDIF}
  if AValueName = 'detailname' then
    AValue := Detail[FDetailNo][2];
end;

procedure TMasterDetailDemo.DetailDataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  {$IFDEF gdebug}
  writeln('DetailDataEOF');
  {$ENDIF}
  IsEOF := FDetailNo > High(Detail);
end;

procedure TMasterDetailDemo.DetailDataNext(Sender: TObject);
begin
  Inc(FDetailNo);
  while (not FDetailData.EOF) and (Detail[FDetailNo][1] <> Master[FMasterNo][1]) do
    Inc(FDetailNo);
end;

procedure TMasterDetailDemo.Detail2DataFirst(Sender: TObject);
begin
  FDetail2No := 1;
  while (not FDetail2Data.EOF) and (D2[FDetail2No][1] <> Detail[FDetailNo][2]) do
    Inc(FDetail2No);
end;

procedure TMasterDetailDemo.Detail2DataGetValue(Sender: TObject; const AValueName: string; var AValue: variant);
begin
  if AValueName = 'detail2name' then
    AValue := D2[FDetail2No][2]
end;

procedure TMasterDetailDemo.Detail2DataEOF(Sender: TObject; var IsEOF: Boolean);
begin
  IsEOF := FDetail2No > High(D2);
end;

procedure TMasterDetailDemo.Detail2DataNext(Sender: TObject);
begin
  Inc(FDetail2No);
  while (not FDetail2Data.EOF) and (D2[FDetail2No][1] <> Detail[FDetailNo][2]) do
    Inc(FDetail2No);
end;

procedure TMasterDetailDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  MasterDataBand: TFPReportDataBand;
  DetailDataBand: TFPReportDataBand;
  Detail2DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 10 - Master/Detail using userdata';

  p := TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 30;
  p.Margins.Top := 20;
  p.Margins.Right := 30;
  p.Margins.Bottom := 20;
  p.Data := FMasterData;
  p.Font.Name := 'LiberationSans';

  TitleBand := TFPReportTitleBand.Create(p);
  TitleBand.Layout.Height := 20;
  {$ifdef ColorBands}
  TitleBand.Frame.Shape := fsRectangle;
  TitleBand.Frame.BackgroundColor := clReportTitleSummary;
  {$endif}

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 0;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := TitleBand.Layout.Width;
  Memo.Layout.Height := 15;
  Memo.UseParentFont := False;
  Memo.Font.Name := 'LiberationSans-Bold';
  Memo.Font.Size := 18;
  Memo.Text := 'FPReport Demo 10' + LineEnding + 'Master/Detail using userdata';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;


  MasterDataBand := TFPReportDataBand.Create(p);
  MasterDataBand.Layout.Height := 8;
  {$ifdef ColorBands}
  MasterDataBand.Frame.Shape := fsRectangle;
  MasterDataBand.Frame.BackgroundColor := clDataBand;
  {$endif}

  Memo := TFPReportMemo.Create(MasterDataBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := '[mastername]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor := clLtGray;


  DetailDataBand := TFPReportDataBand.Create(p);
  DetailDataBand.Layout.Height := 8;
  DetailDataBand.Data := FDetailData;
  {$ifdef ColorBands}
  DetailDataBand.Frame.Shape := fsRectangle;
  DetailDataBand.Frame.BackgroundColor := clChildBand;
  {$endif}

  { associate with Master band }
  DetailDataBand.MasterBand := MasterDataBand;

  Memo := TFPReportMemo.Create(DetailDataBand);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 40;
  Memo.Layout.Height := 5;
  Memo.Text := '[detailname]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;

  Detail2DataBand := TFPReportDataBand.Create(p);
  Detail2DataBand.Layout.Height := 8;
  Detail2DataBand.Data := FDetail2Data;
  {$ifdef ColorBands}
  Detail2DataBand.Frame.Shape := fsRectangle;
  Detail2DataBand.Frame.BackgroundColor := TFPReportColor($FFFFA9);
  {$endif}

  { associate with Master band }
  Detail2DataBand.MasterBand := DetailDataBand;

  Memo := TFPReportMemo.Create(Detail2DataBand);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 40;
  Memo.Layout.Height := 5;
  Memo.Text := '[detail2name]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clNavy;
end;

procedure TMasterDetailDemo.MasterDataGetNames(Sender: TObject; List: TStrings);
begin
  List.Add('mastername');
end;

procedure TMasterDetailDemo.DetailDataGetNames(Sender: TObject; List: TStrings);
begin
  List.Add('detailname');
end;

procedure TMasterDetailDemo.Detail2DataGetNames(Sender: TObject; List: TStrings);
begin
  List.Add('detail2name');
end;

constructor TMasterDetailDemo.Create(AOwner : TComponent);
begin
  Inherited;
  FMasterData := TFPReportUserData.Create(nil);
  FMasterData.OnGetValue := @MasterDataGetValue;
  FMasterData.OnGetEOF := @MasterDataEOF;
  FMasterData.OnFirst := @MasterDataFirst;
  FMasterData.OnNext := @MasterDataNext;
  FMasterData.OnGetNames := @MasterDataGetNames;

  FDetailData := TFPReportUserData.Create(nil);
  FDetailData.OnGetValue := @DetailDataGetValue;
  FDetailData.OnGetEOF := @DetailDataEOF;
  FDetailData.OnFirst := @DetailDataFirst;
  FDetailData.OnNext := @DetailDataNext;
  FDetailData.OnGetNames := @DetailDataGetNames;

  FDetail2Data := TFPReportUserData.Create(nil);
  FDetail2Data.OnGetValue := @Detail2DataGetValue;
  FDetail2Data.OnGetEOF := @Detail2DataEOF;
  FDetail2Data.OnFirst := @Detail2DataFirst;
  FDetail2Data.OnNext := @Detail2DataNext;
  FDetail2Data.OnGetNames := @Detail2DataGetNames;
end;

destructor TMasterDetailDemo.Destroy;
begin
  FreeAndNil(FMasterData);
  FreeAndNil(FDetailData);
  FreeAndNil(FDetail2Data);
  inherited Destroy;
end;


end.

