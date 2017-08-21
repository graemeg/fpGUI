unit rptdataset;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  fpreportdb,
  db,
  dbf,
  udapp;

type
  TDatasetDemo = class(TReportDemoApp)
  private
    lReportData: TFPReportDatasetData;
    DataSet: TDBF;
  Public
    procedure   CreateReportDesign; override;
    procedure   InitialiseData; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;


implementation

uses
  fpTTF,
  FPCanvas,
  dbf_fields;

{ TDatasetDemo }

procedure TDatasetDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  Image: TFPReportImage;
  PageFooter: TFPReportPageFooterBand;
begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 8 - Datasets';

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

  Memo := TFPReportMemo.Create(TitleBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 140;
  Memo.Layout.Height := 15;
  Memo.Text := 'Dataset Demo';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.UseParentFont := False;
  Memo.Font.Color := TFPReportColor($000080);
  Memo.Font.Size := 24;

  DataBand := TFPReportDataBand.Create(p);
  DataBand.Layout.Height := 30;

  Image := TFPReportImage.Create(DataBand);
  Image.Layout.Top := 0;
  Image.Layout.Left := 10;
  Image.Layout.Height := 20;
  Image.Layout.Width := 14.8;
  Image.FieldName := 'Photo';
  Image.Stretched := True;

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := 'Name: [name]';

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 5;
  Memo.Layout.Width := 80;
  Memo.Layout.Height := 5;
  Memo.Text := 'Email: [Address]';

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 10;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := 'Age: [Age]';

  PageFooter := TFPReportPageFooterBand.Create(p);
  PageFooter.Layout.Height := 30;

  Memo := TFPReportMemo.Create(PageFooter);
  Memo.Layout.Left := 130;
  Memo.Layout.Top := 20;
  Memo.Layout.Width := 20;
  Memo.Layout.Height := 5;
  Memo.Text := 'Page [PageNo]';
  Memo.TextAlignment.Vertical := tlBottom;
  Memo.TextAlignment.Horizontal := taRightJustified;
end;

procedure TDatasetDemo.InitialiseData;

var
  fields: TDbfFieldDefs;
  lDataSet : TDBF;
begin
  lDataSet := TDBF.Create(Self);
  lDataSet.TableName := 'test.dbf';
  Dataset:=lDataset;
  lReportData.DataSet:= DataSet;

  if FileExists('test.dbf') then
    exit;
  // If you wanted to create a new DBF table
  fields := TDbfFieldDefs.Create(nil);
  fields.Add('Name', ftString, 50);
  fields.Add('Address', ftString, 150);
  fields.Add('Age', ftInteger);
  fields.Add('Photo', ftBlob);
  lDataSet.CreateTableEx(fields); // <== Now we have an empty db table

  lDataSet.Open;

  lDataSet.Insert;
  lDataSet.FieldByName('Name').AsString := 'Kimi Raikkonen';
  lDataSet.FieldByName('Address').AsString := 'kimi@nospam.net';
  lDataSet.FieldByName('Age').AsInteger := 35;
  TBlobField(lDataSet.FieldByName('Photo')).LoadFromFile(ExpandFileName('../common/pictures/man01.png'));
  lDataSet.Post;

  lDataSet.Insert;
  lDataSet.FieldByName('Name').AsString := 'Michael Schumacher';
  lDataSet.FieldByName('Address').AsString := 'michael@schumacher.org';
  lDataSet.FieldByName('Age').AsInteger := 28;
  TBlobField(lDataSet.FieldByName('Photo')).LoadFromFile(ExpandFileName('../common/pictures/man02.png'));
  lDataSet.Post;

  lDataSet.Insert;
  lDataSet.FieldByName('Name').AsString := 'Alain Prost';
  lDataSet.FieldByName('Address').AsString := 'alain@prost.com';
  lDataSet.FieldByName('Age').AsInteger := 64;
  TBlobField(lDataSet.FieldByName('Photo')).LoadFromFile(ExpandFileName('../common/pictures/man03.png'));
  lDataSet.Post;

  lDataSet.Insert;
  lDataSet.FieldByName('Name').AsString := 'Jenson Button';
  lDataSet.FieldByName('Address').AsString := 'jenson@button.info';
  lDataSet.FieldByName('Age').AsInteger := 50;
  TBlobField(lDataSet.FieldByName('Photo')).LoadFromFile(ExpandFileName('../common/pictures/man04.png'));
  lDataSet.Post;

  lDataSet.Insert;
  lDataSet.FieldByName('Name').AsString := 'Fernando Allonso';
  lDataSet.FieldByName('Address').AsString := 'fernando@allonso-team.net';
  lDataSet.FieldByName('Age').AsInteger := 47;
  TBlobField(lDataSet.FieldByName('Photo')).LoadFromFile(ExpandFileName('../common/pictures/man05.png'));
  lDataSet.Post;

  fields.Free;
end;

constructor TDatasetDemo.Create(AOwner : TComponent);
begin
  Inherited;
  lReportData := TFPReportDatasetData.Create(nil);
end;

destructor TDatasetDemo.Destroy;
begin
  FreeAndNil(lReportData);
  FreeAndNil(DataSet);
  inherited Destroy;
end;

end.

