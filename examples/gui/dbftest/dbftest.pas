{
    fpGUI  -  Free Pascal GUI Library

    DBF Database example

    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}

program DBFTest;

uses
  SysUtils, Classes, fpGFX, fpGUI, fpGUI_DB, DB, DBF;

type

  { TMainForm }

  TMainForm = class(TFForm)
    DataSet: TDBF;
    DataSource: TDataSource;
    Box: TFBoxLayout;
    ListBox: TFListBox;
    CurDataseTFLabel: TFLabel;
    CurNameText, CurEMailText: TDBText;
    Navi: TFBoxLayout;
    FirstDataset, PrevDataset, NextDataset, LastDataset: TFButton;
    CurEmailEdit: TDBEdit;
    procedure FormCreate(Sender: TObject);
    procedure FirstDatasetClick(Sender: TObject);
    procedure PrevDatasetClick(Sender: TObject);
    procedure NextDatasetClick(Sender: TObject);
    procedure LastDatasetClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  x: Integer;
  s: String;
begin
  DataSet             := TDBF.Create(Self);
  DataSet.TableName   := 'test.dbf';
  DataSource          := TDataSource.Create(Self);
  DataSource.DataSet  := DataSet;

  CurNameText.DataSource    := DataSource;
  CurNameText.DataField     := 'Name';
  CurEMailText.DataSource   := DataSource;
  CurEMailText.DataField    := 'Address';
  CurEmailEdit.DataSource   := DataSource;
  CurEmailEdit.DataField    := 'Address';

  DataSet.Open;
  CurEmailEdit.Field.ReadOnly := True;
  
  while not DataSet.EOF do
  begin
    SetLength(s, 0);
    for x := 0 to DataSet.FieldCount - 2 do
      s := s + DataSet.Fields[x].AsString + ', ';
    s := s + DataSet.Fields[DataSet.FieldCount - 1].AsString;
    ListBox.Items.Add(s);
    DataSet.Next;
  end;
  
  DataSet.First;
end;

procedure TMainForm.FirstDatasetClick(Sender: TObject);
begin
  DataSet.First;
  ReDraw;   // a hack to get around a TFLabel.SetText issue
end;

procedure TMainForm.PrevDatasetClick(Sender: TObject);
begin
  DataSet.Prior;
  ReDraw;   // a hack to get around a TFLabel.SetText issue
end;

procedure TMainForm.NextDatasetClick(Sender: TObject);
begin
  DataSet.Next;
  ReDraw;   // a hack to get around a TFLabel.SetText issue
end;

procedure TMainForm.LastDatasetClick(Sender: TObject);
begin
  DataSet.Last;
  ReDraw;   // a hack to get around a TFLabel.SetText issue
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadForm(self);
end;

destructor TMainForm.Destroy;
begin
  DataSet.Close;
  DataSource.Free;
  DataSet.Free;
  inherited Destroy;
end;


var
  MainForm: TMainForm;
begin
//  WriteLn('Version: ' + {$I %date%} + ' ' + {$I %time%});
  GFApplication.Initialize;

  MainForm := TMainForm.Create(GFApplication);
  try
    MainForm.Show;
    GFApplication.Run;
  finally
    MainForm.Free;
  end;
end.

