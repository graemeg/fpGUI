{
    $Id: interbasetest.pp,v 1.2 2001/01/17 21:35:45 sg Exp $

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    InterBase database test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program InterBaseTest;

{$linklib dl}
{$linklib crypt}

uses SysUtils, Classes, fpGUI, fpGUI_DB, DB, InterBase;

type

  TMainForm = class(TForm)
    Database: TIBDatabase;
    Transaction: TIBTransaction;
    Query: TIBQuery;
    DataSource: TDataSource;
    Box, ConnectionBox: TFBoxLayout;
    ConnectionLabel, ConnectionStateLabel: TFLabel;
    ListBox: TListBox;
    CurDataseTFLabel: TFLabel;
    CurNameText, CurEMailText: TDBText;
    Navi: TFBoxLayout;
    FirstDataset, PrevDataset, NextDataset, LastDataset: TFButton;
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
  Query := TIBQuery.Create(Self);

  Database.Connected := True;
  Database.Transaction := Transaction;
  Query.Database := Database;
  DataSource.DataSet := Query;
  Transaction.Action := caRollback;
  Transaction.Active := True;

  if Database.Connected then
    ConnectionStateLabel.Text := 'Yes'
  else
    ConnectionStateLabel.Text := 'No';

  CurNameText.DataSource := DataSource;
  CurNameText.DataField := 'UserName';
  CurEMailText.DataSource := DataSource;
  CurEMailText.DataField := 'InstEmail';

  Query.SQL.Add('select * from fpdev');
WriteLn('Query.Active? ', Query.Active);
  Query.Open;

WriteLn('Query.Active? ', Query.Active);

  while not Query.EOF do
  begin
    SetLength(s, 0);
    for x := 0 to Query.FieldCount - 2 do
      s := s + Query.Fields[x].AsString + ', ';
    s := s + Query.Fields[Query.FieldCount - 1].AsString;
    ListBox.Items.Add(s);
    Query.Next;
  end;

  Query.First;
end;

procedure TMainForm.FirstDatasetClick(Sender: TObject);
begin
  Query.First;
end;

procedure TMainForm.PrevDatasetClick(Sender: TObject);
begin
  Query.Prior;
end;

procedure TMainForm.NextDatasetClick(Sender: TObject);
begin
  Query.Next;
end;

procedure TMainForm.LastDatasetClick(Sender: TObject);
begin
  Query.Last;
end;



var
  MainForm: TMainForm;
begin
  Application.Title := 'InterBase Test';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


{
  $Log: interbasetest.pp,v $
  Revision 1.2  2001/01/17 21:35:45  sg
  * Now uses fpGUI_DB unit

}
