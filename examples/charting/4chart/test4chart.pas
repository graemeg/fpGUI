program testthumbchart;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form,
  fpg_button, //fpg_panel,
  fpg_chart;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnmake:  TfpgButton;
    btnclear: TfpgButton;
    btnquit:  TfpgButton;
    chart1: TChart;       // Bar
    chart2: TChart;       // HBar
    chart3: TChart;       // xyLine
    chart4: TChart;       // Area
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

//var
  frm: TMainForm;

{@VFD_NEWFORM_DECL}

procedure TMainForm.makechart(Sender: TObject);
var
  i,j: integer;
  ds1, ds2, ds3: TDataset;
begin
  Setlength(ds1,length(data1),length(data1[0]));
  For i:=0 to high(data1) do
    for j:=0 to high(data1[0]) do
      ds1[i,j]:=data1[i,j];
  Chart1.Dataset:=ds1;  // bar
  Chart1.Update;
  Chart2.Dataset:=ds1;  // horr. bar
  Chart2.Update;

  Setlength(ds2,length(data2),length(data2[0]));
  For i:=0 to high(data2) do
    for j:=0 to high(data2[0]) do
      ds2[i,j]:=data2[i,j];
  Chart3.Dataset:=ds2;  // Area
  Chart3.Update;

  Setlength(ds3,length(data3),length(data3[0]));
  For i:=0 to high(data3) do
    for j:=0 to high(data3[0]) do
      ds3[i,j]:=data3[i,j];
  Chart4.Dataset:=ds3;  // two xy lines
  Chart4.Update;
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
  SetPosition(323, 113, 325, 300);
  WindowTitle := 'Main Chart Test';
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

  Chart1:=TChart.Create(self);
  with Chart1 do
  begin
    Name := 'Chart1';
    SetPosition(25, 55, 128, 96);
    //FontDesc := '#Label1';
    Text := 'Bar Chart';
    ChartType:=ctPie;
    Xaxis.Visible:=False;      // default=True
    Yaxis.Visible:=False;      // default=True
    LineWidth:= 5;             // default=1
    BackgroundColor:= clCream; // default=clCream
  end;

  Chart2:=TChart.Create(self);
  with Chart2 do
  begin
    Name := 'Chart2';
    SetPosition(175, 55, 128, 96);
    Text := 'Horizontal Bar';
    Xaxis.Visible:=False;
    Yaxis.Visible:=False;
    LineWidth:=1;  // now linewith works as space between bars
    ChartType:=ctHBar;
  end;

  Chart3:=TChart.Create(self);  // xy-line
  with Chart3 do
  begin
    Name := 'Chart3';
    SetPosition(25, 175, 128, 96);
    Text := 'Area';
    TextColor := clRed;
    LineWidth:=2;
    Backgroundcolor:=clBlack;
    writeln('clRed: ',clRed);
    ChartType:=ctArea;
  end;

  Chart4:=TChart.Create(self);  // multi xy-line
  with Chart4 do
  begin
    Name := 'Chart4';
    SetPosition(175, 175, 128, 96);
    Text := 'Multi xyLine';
    LineWidth:=2;
    ChartType:=ctxyLine;
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
