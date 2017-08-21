unit rptmasterdetaildataset;


{$mode objfpc}{$H+}
{$I demos.inc}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  fpreportdb,
  db,
  sqldb,
  IBConnection,
  udapp;

type

  TMasterDetailDatasetDemo = class(TReportDemoApp)
  private
    IBConnection1: TIBConnection;
    SQLTransaction1: TSQLTransaction;
    ProjectDS: TDataSource;
    qryProject: TSQLQuery;
    qryEmployee: TSQLQuery;
    qryProjBudget: TSQLQuery;
    ReportMasterData: TFPReportDatasetData;
    ReportDetailData: TFPReportDatasetData;
    ReportBudgetData: TFPReportDatasetData;
  Protected
    procedure   CreateReportDesign; override;
    procedure   InitialiseData; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;


implementation

uses
  fpTTF;

const
  cDatabase = 'localhost:/usr/share/doc/firebird2.5-common-doc/examples/empbuild/employee.fdb';
//  cDatabase = '/opt/firebird/examples/empbuild/employee.fdb';

{ TMasterDetailDatasetDemo }

procedure TMasterDetailDatasetDemo.CreateReportDesign;
var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  MasterDataBand: TFPReportDataBand;
  DetailDataBand: TFPReportDataBand;
  ProjBudgetBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  DataHeader: TFPReportDataHeaderBand;
  BudgetDataHeader: TFPReportDataHeaderBand;
begin
  PaperManager.RegisterStandardSizes;
  rpt.Author := 'Graeme Geldenhuys';
  rpt.Title := 'FPReport Demo 11 - Master/Detail using datasets';
  p := TFPReportPage.Create(rpt);
  p.Orientation := poPortrait;
  p.PageSize.PaperName := 'A4';
  { page margins }
  p.Margins.Left := 30;
  p.Margins.Top := 20;
  p.Margins.Right := 30;
  p.Margins.Bottom := 20;
  p.Data := ReportMasterData;
  p.Font.Name := 'LiberationSans';

  // ======== ReportTitle band ===========
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
  Memo.Text := 'FPReport Demo 11' + LineEnding + 'Master/Detail using datasets';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;

  // ======== MasterData band ===========
  MasterDataBand := TFPReportDataBand.Create(p);
  MasterDataBand.Layout.Height := 8;
  {$ifdef ColorBands}
  MasterDataBand.Frame.Shape := fsRectangle;
  MasterDataBand.Frame.BackgroundColor := clDataBand;
  {$endif}

  Memo := TFPReportMemo.Create(MasterDataBand);
  Memo.Layout.Left := 5;
  Memo.Layout.Top := 2;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := '[reportmasterdata.proj_id] - [reportmasterdata.proj_name]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.BackgroundColor := clLtGray;

  // ======== DataHeader band for DetailData ===========
  DataHeader := TFPReportDataHeaderBand.Create(p);
  DataHeader.Layout.Height := 8;
  {$ifdef ColorBands}
  DataHeader.Frame.Shape := fsRectangle;
  DataHeader.Frame.BackgroundColor := clDataHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(DataHeader);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 3;
  Memo.Layout.Width := 15;
  Memo.Layout.Height := 5;
  Memo.Text := 'Emp No.';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Options := [moDisableWordWrap];
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;
  Memo.Frame.BackgroundColor := clLtGray;

  Memo := TFPReportMemo.Create(DataHeader);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 3;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := 'Employee Name';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;
  Memo.Frame.BackgroundColor := clLtGray;

  // ======== DetailData band ===========
  DetailDataBand := TFPReportDataBand.Create(p);
  DetailDataBand.Layout.Height := 5;
  DetailDataBand.Data := ReportDetailData;
  { associate this band with the MasterData band }
  DetailDataBand.MasterBand := MasterDataBand;
  { associate DataHeader band }
  DetailDataBand.HeaderBand := DataHeader;
  DetailDataBand.DisplayPosition := 0;
  {$ifdef ColorBands}
  DetailDataBand.Frame.Shape := fsRectangle;
  DetailDataBand.Frame.BackgroundColor := clChildBand;
  {$endif}

  Memo := TFPReportMemo.Create(DetailDataBand);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 15;
  Memo.Layout.Height := 5;
  Memo.Text := '[reportdetaildata.emp_no]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;

  Memo := TFPReportMemo.Create(DetailDataBand);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := '[reportdetaildata.first_name] [reportdetaildata.last_name]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;


  // ======== DataHeader band for DetailData ===========
  BudgetDataHeader := TFPReportDataHeaderBand.Create(p);
  BudgetDataHeader.Layout.Height := 8;
  {$ifdef ColorBands}
  BudgetDataHeader.Frame.Shape := fsRectangle;
  BudgetDataHeader.Frame.BackgroundColor := clDataHeaderFooter;
  {$endif}

  Memo := TFPReportMemo.Create(BudgetDataHeader);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 3;
  Memo.Layout.Width := 15;
  Memo.Layout.Height := 5;
  Memo.Text := 'Year';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;
  Memo.Frame.BackgroundColor := clLtGray;

  Memo := TFPReportMemo.Create(BudgetDataHeader);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 3;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := 'Budget';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;
  Memo.Frame.BackgroundColor := clLtGray;


  // ======== Project Budget band ===========
  ProjBudgetBand := TFPReportDataBand.Create(p);
  ProjBudgetBand.Layout.Height := 5;
  ProjBudgetBand.Data := ReportBudgetData;
  { associate this band with the MasterData band }
  ProjBudgetBand.MasterBand := MasterDataBand;
  ProjBudgetBand.HeaderBand := BudgetDataHeader;
  ProjBudgetBand.DisplayPosition := 1;
  {$ifdef ColorBands}
  ProjBudgetBand.Frame.Shape := fsRectangle;
  ProjBudgetBand.Frame.BackgroundColor := clChildBand;
  {$endif}

  Memo := TFPReportMemo.Create(ProjBudgetBand);
  Memo.Layout.Left := 15;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 15;
  Memo.Layout.Height := 5;
  Memo.Text := '[ReportBudgetData.FISCAL_YEAR]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taCentered;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;

  Memo := TFPReportMemo.Create(ProjBudgetBand);
  Memo.Layout.Left := 30;
  Memo.Layout.Top := 0;
  Memo.Layout.Width := 50;
  Memo.Layout.Height := 5;
  Memo.Text := '[formatfloat(''#,##0'', ReportBudgetData.PROJECTED_BUDGET)]';
  Memo.TextAlignment.Vertical := tlCenter;
  Memo.TextAlignment.Horizontal := taRightJustified;
  Memo.Frame.Shape := fsRectangle;
  Memo.Frame.Color := clBlack;
end;

procedure TMasterDetailDatasetDemo.InitialiseData;
begin
  SQLTransaction1 := TSQLTransaction.Create(Self);

  IBConnection1 := TIBConnection.Create(Self);
  with IBConnection1 do
  begin
    LoginPrompt := False;
    DatabaseName := cDatabase;
    KeepConnection := False;
    Password := 'masterkey';
    Transaction := SQLTransaction1;
    UserName := 'sysdba';
    Connected := True;
  end;

  qryProject := TSQLQuery.Create(Self);
  with qryProject do
  begin
    Database := IBConnection1;
    Transaction := SQLTransaction1;
    ReadOnly := True;
    SQL.Text := 'select * from PROJECT order by PROJ_NAME';
    Active := True
  end;

  ProjectDS := TDataSource.Create(Self);
  ProjectDS.DataSet := qryProject;

  qryEmployee := TSQLQuery.Create(Self);
  with qryEmployee do
  begin
    Database := IBConnection1;
    Transaction := SQLTransaction1;
    ReadOnly := True;
    SQL.Text :=
          'SELECT ' +
          '  e.* ' +
          'FROM EMPLOYEE e ' +
          '  INNER JOIN EMPLOYEE_PROJECT ep on e.EMP_NO = ep.EMP_NO ' +
          'where ep.proj_id = :proj_id ' +
          'order by e.EMP_NO';
    DataSource := ProjectDS;
    Active := True
  end;

  qryProjBudget := TSQLQuery.Create(Self);
  with qryProjBudget do
  begin
    Database := IBConnection1;
    Transaction := SQLTransaction1;
    ReadOnly := True;
    SQL.Text :=
          'SELECT ' +
          '  b.FISCAL_YEAR, b.PROJECTED_BUDGET ' +
          'FROM PROJ_DEPT_BUDGET b ' +
          'where b.proj_id = :proj_id ' +
          'order by b.FISCAL_YEAR';
    DataSource := ProjectDS;
    Active := True
  end;

  ReportMasterData.DataSet:= qryProject;
  ReportDetailData.DataSet:= qryEmployee;
  ReportBudgetData.DataSet:= qryProjBudget;
end;

constructor TMasterDetailDatasetDemo.Create(AOwner: TComponent);
begin
  Inherited;
  ReportMasterData := TFPReportDatasetData.Create(Self);
  ReportMasterData.Name := 'ReportMasterData';
  ReportDetailData := TFPReportDatasetData.Create(Self);
  ReportDetailData.Name := 'ReportDetailData';
  ReportBudgetData := TFPReportDatasetData.Create(Self);
  ReportBudgetData.Name := 'ReportBudgetData';
end;

destructor TMasterDetailDatasetDemo.Destroy;
begin
  IBConnection1.Close();
  FreeAndNil(qryEmployee);
  FreeAndNil(qryProject);
  FreeAndNil(ReportMasterData);
  FreeAndNil(ReportDetailData);
  FreeAndNil(ReportBudgetData);
  FreeAndNil(ProjectDS);
  FreeAndNil(SQLTransaction1);
  FreeAndNil(IBConnection1);
  inherited Destroy;
end;



end.

