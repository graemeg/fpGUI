{
    $Id: dbftest.pp,v 1.2 2001/01/18 12:40:41 sg Exp $

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    DBF database test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program DBFTest;

uses SysUtils, Classes, fpGUI, fpGUI_DB, DB, DBF;

type

  TMainForm = class(TForm)
    DataSet: TDBF;
    DataSource: TDataSource;
    Box: TBoxLayout;
    ListBox: TListBox;
    CurDatasetLabel: TLabel;
    CurNameText, CurEMailText: TDBText;
    Navi: TBoxLayout;
    FirstDataset, PrevDataset, NextDataset, LastDataset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FirstDatasetClick(Sender: TObject);
    procedure PrevDatasetClick(Sender: TObject);
    procedure NextDatasetClick(Sender: TObject);
    procedure LastDatasetClick(Sender: TObject);
  end;


// -------------------------------------------------------------------
//   TMainForm
// -------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
var
  x: Integer;
  s: String;
begin
  DataSet := TDBF.Create(Self);
  DataSet.TableName := 'test.dbf';
  DataSource := TDataSource.Create(Self);
  DataSource.DataSet := DataSet;

  CurNameText.DataSource := DataSource;
  CurNameText.DataField := 'Name';
  CurEMailText.DataSource := DataSource;
  CurEMailText.DataField := 'Address';

  DataSet.Open;

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
end;

procedure TMainForm.PrevDatasetClick(Sender: TObject);
begin
  DataSet.Prior;
end;

procedure TMainForm.NextDatasetClick(Sender: TObject);
begin
  DataSet.Next;
end;

procedure TMainForm.LastDatasetClick(Sender: TObject);
begin
  DataSet.Last;
end;


var
  MainForm: TMainForm;
begin
  Application.Title := 'Interbase Test';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


{
  $Log: dbftest.pp,v $
  Revision 1.2  2001/01/18 12:40:41  sg
  * Now uses the correct field names for the data links ;)

  Revision 1.1  2001/01/17 21:33:28  sg
  * First version

}
