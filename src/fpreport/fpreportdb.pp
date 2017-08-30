{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Report Data loop classes based on TDataset.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, db;

Type
  TFPReportDatasetData = class(TFPReportData)
  private
    FDataSet: TDataSet;
  protected
    procedure DoGetValue(const AFieldName: string; var AValue: variant); override;
    procedure DoInitDataFields; override;
    procedure DoOpen; override;
    procedure DoFirst; override;
    procedure DoNext; override;
    procedure DoClose; override;
    function  DoEOF: boolean; override;
  Public
    property  DataFields;
  published
    property  DataSet: TDataSet read FDataSet write FDataSet;
  end;

implementation

resourcestring
  SErrNoDataSetAssigned  = 'No dataset has been assigned.';
  SErrDatasetNotOpen = 'Dataset has not been opened yet';


{ TFPReportDatasetData }

procedure TFPReportDatasetData.DoGetValue(const AFieldName: string; var AValue: variant);
var
  ms: TMemoryStream;
begin
  inherited DoGetValue(AFieldName, AValue);
  try
    if FieldTypes[AFieldName] = rfkStream then
    begin
      ms := TMemoryStream.Create;
      try
        TBlobField(FDataSet.FieldByName(AFieldName)).SaveToStream(ms);
        AValue := FPReportStreamToMIMEEncodeString(ms);
      finally
        ms.Free;
      end;
    end
    else
    begin
      AValue := FDataSet.FieldByName(AFieldName).Value;
    end;
  except
    on E: EDatabaseError do
    begin
      // no nothing - it's probably an expression, which will be handled in CustomBand.ExpandMacro()
    end;
  end;
end;

procedure TFPReportDatasetData.DoInitDataFields;
var
  i: integer;

  function DatabaseKindToReportKind(const AType: TFieldType): TFPReportFieldKind;
  begin
    case AType of
      ftUnknown:        Result := rfkString;
      ftString:         Result := rfkString;
      ftSmallint:       Result := rfkInteger;
      ftInteger:        Result := rfkInteger;
      ftWord:           Result := rfkInteger;
      ftBoolean:        Result := rfkBoolean;
      ftFloat:          Result := rfkFloat;
      ftCurrency:       Result := rfkFloat;
      ftBCD:            Result := rfkFloat;
      ftDate:           Result := rfkDateTime;
      ftTime:           Result := rfkDateTime;
      ftDateTime:       Result := rfkDateTime;
      ftBytes:          Result := rfkStream;
      ftVarBytes:       Result := rfkStream;
      ftAutoInc:        Result := rfkInteger;
      ftBlob:           Result := rfkStream;
      ftMemo:           Result := rfkStream;
      ftGraphic:        Result := rfkStream;
      ftFmtMemo:        Result := rfkString;
      //ftParadoxOle:
      //ftDBaseOle:
      ftTypedBinary:    Result := rfkStream;
      //ftCursor:
      ftFixedChar:      Result := rfkString;
      ftWideString:     Result := rfkString;
      ftLargeint:       Result := rfkInteger;
      //ftADT:
      //ftArray:
      //ftReference:
      //ftDataSet:
      ftOraBlob:        Result := rfkStream;
      ftOraClob:        Result := rfkStream;
      ftVariant:        Result := rfkString;
      //ftInterface:
      //ftIDispatch:
      ftGuid:           Result := rfkString;
      ftTimeStamp:      Result := rfkDateTime;
      //ftFMTBcd:
      ftFixedWideChar:  Result := rfkString;
      ftWideMemo:       Result := rfkString;
      else
        Result := rfkString;
    end;
  end;

Var
  B : Boolean;
begin
  inherited DoInitDataFields;
  B:=FDataset.FieldDefs.Count=0;
  if B then
    FDataset.Open;
  try
    DataFields.Clear;
    for i := 0 to FDataSet.FieldDefs.Count-1 do
    begin
      DataFields.AddField(FDataset.FieldDefs[i].Name, DatabaseKindToReportKind(FDataset.FieldDefs[i].DataType));
    end;
  finally
    if B then
      FDataset.Close;
  end;
end;

procedure TFPReportDatasetData.DoOpen;
begin
  inherited DoOpen;
  if not Assigned(FDataSet) then
    ReportError(SErrNoDataSetAssigned);
  FDataSet.Open;
end;

procedure TFPReportDatasetData.DoFirst;
begin
  if not Assigned(FDataSet) then
    ReportError(SErrNoDataSetAssigned);
  if not FDataSet.Active then
    ReportError(SErrDatasetNotOpen);
  inherited DoFirst;
  FDataSet.First;
end;

procedure TFPReportDatasetData.DoNext;
begin
  inherited DoNext;
  FDataSet.Next;
end;

procedure TFPReportDatasetData.DoClose;
begin
  inherited DoClose;
  FDataSet.Close;
end;

function TFPReportDatasetData.DoEOF: boolean;
begin
  Result := FDataSet.EOF;
end;

end.

