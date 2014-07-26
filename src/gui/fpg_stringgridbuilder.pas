{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2014 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit defines a helper class that can populate a StringGrid
      from a CSV file. In future this could be expaned to other file
      types or even data structures.
}
unit fpg_StringGridBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_grid;

type
  TStringGridBuilder = class(TObject)
  private
    FData: TStringList;
    FGrid: TfpgStringGrid;
    FCSVFile: TfpgString;
    FHasHeader: boolean;
  protected
    procedure   InternalSetupColumns; virtual;
    procedure   InternalSetupData; virtual;
    procedure   InternalRepaintRow(const AData: TfpgString; const ARow: integer); virtual;
  public
    constructor Create;
    constructor CreateCustom(const AGrid: TfpgStringGrid; const ACSVFile: TfpgString; const AWithHeader: boolean = True); virtual;
    destructor  Destroy; override;
    procedure   Run;
    property    Grid: TfpgStringGrid read FGrid;
  end;

implementation

uses
  fpg_main,
  fpg_utils,
  fpg_CSVParser;

{ TStringGridBuilder }

procedure TStringGridBuilder.InternalSetupColumns;
var
  x: integer;
  fields: TStringList;
begin
  fields := TStringList.Create;
  try
    gCsvParser.ExtractFields(FData[0], fields);
    // setup correct column count
    FGrid.ColumnCount := fields.Count;
    // initialize columns
    if FHasHeader then
    begin
      for x := 0 to fields.Count-1 do
      begin
        FGrid.ColumnTitle[x] := fields[x];
//        FGrid.ColumnWidth[x] := StrToInt(FColumns.ValueFromIndex[x]);
      end;
    end;
  finally
    fields.Free;
  end;
end;

procedure TStringGridBuilder.InternalSetupData;
var
  y: integer;
begin
  FGrid.BeginUpdate;
  FGrid.MouseCursor := mcHourGlass;
  try
    try
      // set correct row count. Columns have already been handled.
      if FHasHeader then
      begin
        FGrid.RowCount := FData.Count-1;
        for y := 1 to FData.Count-1 do  // rows
        begin
  //        writeln(' Row: ', y, '  Data: ', FData.Strings[y-1]);
          InternalRepaintRow(FData.Strings[y], y-1);
        end;
      end
      else
      begin
        FGrid.RowCount := FData.Count;
        for y := 0 to FData.Count-1 do  // rows
        begin
  //        writeln(' Row: ', y, '  Data: ', FData.Strings[y-1]);
          InternalRepaintRow(FData.Strings[y], y);
        end;
      end;
    except
      fpgApplication.HandleException(self);
    end;
  finally
    if FGrid.RowCount > 0 then
      FGrid.FocusRow := 0;
    FGrid.EndUpdate;
    FGrid.MouseCursor := mcDefault;
  end;
end;

procedure TStringGridBuilder.InternalRepaintRow(const AData: TfpgString; const ARow: integer);
var
  x: integer;
  fields: TStrings;
  value: string;
begin
  fields := TStringList.Create;
  try
    gCsvParser.ExtractFields(AData, fields);
    for x := 0 to FGrid.ColumnCount-1 do
    begin
      if x < fields.Count then
        value := fields.Strings[x]
      else
        value := '';
      FGrid.Cells[x, ARow] := value
    end;
  finally
    fields.Free;
  end;
end;

constructor TStringGridBuilder.Create;
begin
  FData := TStringList.Create;
end;

constructor TStringGridBuilder.CreateCustom(const AGrid: TfpgStringGrid; const ACSVFile: TfpgString; const AWithHeader: boolean);
begin
  Create;
  FGrid := AGrid;
  FCSVFile := ACSVFile;
  FGrid.Clear;
  FHasHeader := AWithHeader;
  FGrid.ShowHeader := AWithHeader;
end;

destructor TStringGridBuilder.Destroy;
begin
  FGrid := nil;
  FData.Free;
  inherited Destroy;
end;

procedure TStringGridBuilder.Run;
begin
  if FCSVFile = '' then
    raise Exception.Create('TStringGridBuilder: CSV filename is empty!');
  if not fpgFileExists(FCSVFile) then
    raise Exception.CreateFmt('TStringGridBuilder: The CSV file <%s> does not exist.', [FCSVFile]);
  FData.LoadFromFile(fpgToOSEncoding(FCSVFile));
  InternalSetupColumns;
  InternalSetupData;
end;


end.

