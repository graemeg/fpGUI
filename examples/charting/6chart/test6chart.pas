program testthumbchart;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form,
  fpg_button,
  fpg_chart,
  fpg_chart_pie,
  fpg_chart_bar;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnmake:  TfpgButton;
    btnclear: TfpgButton;
    btnquit:  TfpgButton;
    chart1: TfpgChart;       // Pie
    chart2: TfpgChart;       // Bar
    chart3: TfpgChart;       // Line
    chart4: TfpgChart;       // Scatter
    chart5: TfpgChart;       // xyLine
    chart6: TfpgChart;       // MultiLine
    {@VFD_HEAD_END: MainForm}
  public
    procedure AfterCreate; override;
    procedure makechart(Sender: TObject);
    procedure clearchart(Sender: TObject);
    procedure quitclick(Sender: TObject);
  end;

var
  data1: array[0..6,0..0] of integer = (
    (20),(10),(33),(80),(87),(57),(73)
    );     // take care: total<=360 degrees.
  data2: array[0..10,0..1] of integer = (
    (0,10), (12,10), (24,30), (36,80), (48,70),
    (60,60), (72,55), (84,30), (96,45), (110,10),
    (125,3)   );
  data3: array [0..10,0..2] of integer =(
    (0,45,25),
    (12,40,15),
    (24,30,20),  // first nr is X, others are Y coordinates
    (36,70,20),
    (48,65,40),
    (60,60,50),
    (72,55,70),
    (84,30,70),
    (96,45,80),
    (110,10,60),
    (125,10,60)   );
  data4: array [0..19,0..1] of integer =(
    ( 2, 13),(86, 23),(12, 56),(71, 58),
    (62, 38),(70, 76),( 3, 22),(47, 60),
    (31, 29),(40, 37),(39, 64),(29, 43),
    (26, 35),(59, 50),(39, 10),(61, 30),
    (23, 25),(100, 79),( 0, 26),(63, 68)   );

//var
  frm: TMainForm;

{@VFD_NEWFORM_DECL}

procedure TMainForm.makechart(Sender: TObject);
var
  i,j: integer;
  ds1, ds2, ds3, ds4: TDataset;
begin
  Setlength(ds1,length(data1),length(data1[0]));
  For i:=0 to high(data1) do
    for j:=0 to high(data1[0]) do
      ds1[i,j]:=data1[i,j];
  Chart1.Dataset:=ds1;  // pie
  Chart1.Update;
  Chart2.Dataset:=ds1;  // bar
  Chart2.Update;
  Chart3.Dataset:=ds1;  // line bold
  Chart3.Update;
  Setlength(ds2,length(data2),length(data2[0]));
  For i:=0 to high(data2) do
    for j:=0 to high(data2[0]) do
      ds2[i,j]:=data2[i,j];
  Chart4.Dataset:=ds2;  // xyLine
  Chart4.Update;
  Setlength(ds3,length(data3),length(data3[0]));
  For i:=0 to high(data3) do
    for j:=0 to high(data3[0]) do
      ds3[i,j]:=data3[i,j];
  Chart5.Dataset:=ds3;  // two xy lines
  Chart5.Update;
  Setlength(ds4,length(data4),length(data4[0]));
  For i:=0 to high(data4) do
    for j:=0 to high(data4[0]) do
      ds4[i,j]:=data4[i,j];
  Chart6.Dataset:=ds4;  // chatter
  Chart6.Update;
end;

procedure TMainForm.clearchart(Sender: TObject);
begin
  Chart1.DataSet:=Nil;
  Chart1.Update;
  Chart2.DataSet:=Nil;
  Chart2.Update;
  Chart3.DataSet:=Nil;
  Chart3.Update;
  Chart4.DataSet:=Nil;
  Chart4.Update;
end;
{ fpgColors: clBlack, clCream, clDkGray, clGray, clLime
  clLtGray, clOlive, clSilver, clWhite, clYellow
}

procedure TMainForm.quitclick(Sender: TObject);
begin
  Close;
end;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: main}
  Name := 'mainform';
  SetPosition(323, 113, 480, 300);
  WindowTitle := 'fpGUI Charting Demo';
  WindowPosition := wpOneThirdDown;
  ShowHint := True;

  btnmake := TfpgButton.Create(self);
  with btnmake do
  begin
    Name := 'make';
    SetPosition(25, 10, 80, 23);
    Text := 'Make Chart';
    FontDesc := '#Label1';
    Hint := 'Click here to fill the charts with data';
    ImageName := '';
    TabOrder := 1;
    OnClick:=@makechart;
  end;

  btnclear := TfpgButton.Create(self);
  with btnclear do
  begin
    Name := 'clear';
    SetPosition(120, 10, 80, 23);
    Text := 'Clear';
    FontDesc := '#Label1';
    Hint := 'Click here to erase the charts';
    ImageName := '';
    TabOrder := 2;
    OnClick:=@clearchart;
  end;

  btnquit := TfpgButton.Create(self);
  with btnquit do
  begin
    Name := 'quit';
    SetPosition(217, 10, 80, 23);
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := 'Click to quit.';
    ImageName := '';
    TabOrder := 3;
    OnClick:=@quitclick;
  end;

  Chart1:=TfpgChart.Create(self);
  with Chart1 do
  begin
    Name := 'Chart1';
    SetPosition(25, 55, 128, 96);
    //FontDesc := '#Label1';
    Text := 'Pie Chart';
    ChartType:=ctPie;
    Xaxis.Visible:=False;      // default=True
    Yaxis.Visible:=False;      // default=True
    LineWidth:= 5;             // default=1
    BackgroundColor:= clCream; // default=clCream
  end;

  Chart2:=TfpgChart.Create(self);
  with Chart2 do
  begin
    Name := 'Chart2';
    SetPosition(175, 55, 128, 96);
    Text := 'Bar Chart';
    Xaxis.Visible:=False;
    Yaxis.Visible:=False;
    LineWidth:=1;  // now linewith works as space between bars
    ChartType:=ctBar;
  end;

  Chart3:=TfpgChart.Create(self);  // simple line
  with Chart3 do
  begin
    Name := 'Chart3';
    //SetPosition(25, 175, 128, 96);
    SetPosition(325, 55, 128, 96);
    Text := 'Line Chart';
    LineWidth:=5;
    ChartType:=ctLine;
  end;

  Chart4:=TfpgChart.Create(self);  // xy-line
  with Chart4 do
  begin
    Name := 'Chart4';
    SetPosition(25, 175, 128, 96);
    Text := 'xyLine';
    TextColor := clRed;
    LineWidth:=2;
    Backgroundcolor:=clBlack;
    ChartType:=ctxyLine;
  end;

  Chart5:=TfpgChart.Create(self);  // multi xy-line
  with Chart5 do
  begin
    Name := 'Chart5';
    SetPosition(175, 175, 128, 96);
    Text := 'Multi xyLine';
    LineWidth:=2;
    ChartType:=ctxyLine;
  end;

  Chart6:=TfpgChart.Create(self);  // scatter
  with Chart6 do
  begin
    Name := 'Chart6';
    SetPosition(325, 175, 128, 96);
    Text := 'Scatter';
    Linewidth:=4;  // used to set the size of the dots
    ChartType:=ctScatter;
  end;

  {@VFD_BODY_END: main}
  {%endregion}
end;

begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end.
