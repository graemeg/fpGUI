unit rptQRCode;


{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  fpreportcontnr,
  fpqrcodegen,
  fpreportqrcode,
  contnrs,
  udapp;

type

  { TCountry }

  TCountry = Class(TCollectionItem)
  private
    FName: String;
    FPopulation: Int64;
  Published
    Property Name : String Read FName Write FName;
    Property Population : Int64 Read FPopulation Write FPopulation;
  end;

  { TCollectionDemo }

  { TQRCodeDemo }

  TQRCodeDemo = class(TReportDemoApp)
  private
    procedure SetQRCodeValue(Sender: TFPReportElement);
  Protected
    FReportData : TFPReportObjectData;
    FQRCode: TFPReportQRcode;
  public
    procedure   InitialiseData; override;
    constructor Create(AOWner :TComponent); override;
    Class function Description : string; override;
    procedure   CreateReportDesign;override;
    procedure   LoadDesignFromFile(const AFilename: string);
    procedure   HookupData(const AComponentName: string; const AData: TFPReportData);
    destructor  Destroy; override;
  end;



implementation

uses
  fpReportStreamer,
  fpTTF,
  fpJSON,
  jsonparser;

procedure TQRCodeDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  GroupHeader: TFPReportGroupHeaderBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  QR : TFPReportQRcode;
  
begin
  Inherited;
  rpt.Author := 'Michael Van Canneyt';
  rpt.Title := 'FPReport Demo : QR Codes';

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

  QR:= TFPReportQRcode.Create(TitleBand);
  QR.Layout.Left := 1;
  QR.Layout.Top := 1;
  QR.Layout.Width := 34;
  QR.Layout.Height := 34;
  QR.Value:='https://www.nayuki.io/page/qr-code-generator-library/';
  QR.Center:=True;

  QR:= TFPReportQRcode.Create(TitleBand);
  QR.Layout.Left := 115;
  QR.Layout.Top := 1;
  QR.Layout.Width := 34;
  QR.Layout.Height := 34;
  QR.Value:='https://freepascal.org/';
  QR.Center:=True;

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
  DataBand.Layout.Height := 35;
  {$ifdef ColorBands}
  DataBand.Frame.Shape := fsRectangle;
  DataBand.Frame.BackgroundColor := clDataBand;
  {$endif}

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 1;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 20;
  Memo.Text := '[Name]';

  Memo := TFPReportMemo.Create(DataBand);
  Memo.Layout.Left := 70;
  Memo.Layout.Top := 1;
  Memo.Layout.Width := 30;
  Memo.Layout.Height := 5;
  Memo.Text := '[formatfloat(''#,##0'', Population)]';

  FQRCode := TFPReportQRCode.Create(DataBand);
  FQRCode.Layout.Left := 100;
  FQRCode.Layout.Top := 1;
  FQRCode.Layout.Width := 32;
  FQRCode.Layout.Height := 32;
  FQRCode.Center:=True;
  // Only one of the 2 ways must be used: either set expression, either use callback.
  FQRCode.Expression:='''http://en.wikipedia.org/wiki/''+Name';
  // Databand.OnBeforePrint:=@SetQRCodeValue;


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

procedure TQRCodeDemo.LoadDesignFromFile(const AFilename: string);
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

procedure TQRCodeDemo.HookupData(const AComponentName: string; const AData: TFPReportData);
var
  b: TFPReportCustomBandWithData;
begin
  b := TFPReportCustomBandWithData(rpt.FindRecursive(AComponentName));
  if Assigned(b) then
    b.Data := AData;
end;

destructor TQRCodeDemo.Destroy;
begin
  FreeAndNil(FReportData);
  inherited Destroy;
end;

constructor TQRCodeDemo.Create(AOWner: TComponent);
begin
  inherited;
  FReportData := TFPReportCollectionData.Create(nil);
  TFPReportCollectionData(FReportData).OwnsCollection:=True;
end;

class function TQRCodeDemo.Description: string;
begin
  Result:='Demo showing native support for QRCodes';
end;

{ TQRCodeDemo }

procedure TQRCodeDemo.SetQRcodeValue(Sender: TFPReportElement);

begin
  FQRCode.Value:='http://en.wikipedia.org/wiki/'+FReportData.FieldValues['Name'];
end;

procedure TQRCodeDemo.InitialiseData;

var
  SL : TStringList;
  i : Integer;
  N,V : String;
  C : TCountry;
  Coll : TCollection;

begin
  Coll:=TCollection.Create(TCountry);
  TFPReportCollectionData(FReportData).Collection:=coll;
  SL:=TStringList.Create;
  try
    {$I countries.inc}
    SL.Sort;
    For I:=0 to SL.Count-1 do
      begin
      C:=Coll.Add As TCountry;
      SL.GetNameValue(I,N,V);
      C.Name:=N;
      C.Population:=StrToInt64Def(V,0);
      end;
  finally
    SL.Free;
  end;
end;

end.

