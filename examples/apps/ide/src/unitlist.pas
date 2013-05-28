{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit UnitList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base;

type
  TUnit = class(TObject)
  private
    FFilename: TfpgString;
    FOpened: Boolean;
    function GetUnitName: TfpgString;
  public
    constructor Create;
    property FileName: TfpgString read FFilename write FFilename;
    property UnitName: TfpgString read GetUnitName;
    property Opened: Boolean read FOpened write FOpened;
  end;


  TUnitList = class(TObject)
  private
    FList: TList;
    function GetItems(AIndex: integer): TUnit;
    procedure SetItems(AIndex: integer; const AValue: TUnit);
  public
    constructor Create;
    destructor  Destroy; override;
    function    Count: integer;
    function    FindByName(const AUnitName: TfpgString): TUnit;
    function    FileExists(const AFilename: TfpgString): Boolean;
    procedure   Add(NewUnit: TUnit);
    procedure   Clear;
    procedure   Delete(AIndex: integer);
    property    Items[AIndex: integer]: TUnit read GetItems write SetItems; default;
  end;


implementation

uses
  fpg_utils;

{ TUnitList }

function TUnitList.GetItems(AIndex: integer): TUnit;
begin
  Result := TUnit(FList[AIndex]);
end;

procedure TUnitList.SetItems(AIndex: integer; const AValue: TUnit);
begin
  FList[AIndex] := AValue;
end;

constructor TUnitList.Create;
begin
  FList := TList.Create;
  inherited Create;
end;

destructor TUnitList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TUnitList.Count: integer;
begin
  Result := FList.Count;
end;

function TUnitList.FindByName(const AUnitName: TfpgString): TUnit;
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l := 0;
  r := Count-1;
  m := 0;
  while l <= r do
  begin
    m := (l+r) shr 1;
    Result := Items[m];
    cmp := AnsiCompareText(AUnitName, Result.UnitName);
    if cmp < 0 then
      r := m-1
    else if cmp > 0 then
      l := m+1
    else
      exit;
  end;
  Result := nil;
end;

function TUnitList.FileExists(const AFilename: TfpgString): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    Result := Items[i].FileName = AFilename;
    if Result then
      Exit;
  end;
end;

procedure TUnitList.Add(NewUnit: TUnit);
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l := 0;
  r := Count-1;
  m := 0;
  while l <= r do
  begin
    m := (l+r) shr 1;
    cmp := AnsiCompareText(NewUnit.UnitName, Items[m].UnitName);
    if cmp < 0 then
      r := m-1
    else if cmp > 0 then
      l := m + 1
    else
      break;
  end;
  if (m < Count) and (AnsiCompareText(NewUnit.UnitName, Items[m].UnitName) > 0) then
    inc(m);
  FList.Insert(m, NewUnit);
end;

procedure TUnitList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    TUnit(FList[i]).Free;
  FList.Clear;
end;

procedure TUnitList.Delete(AIndex: integer);
begin
  TUnit(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

{ TUnit }

function TUnit.GetUnitName: TfpgString;
begin
  Result := fpgExtractFileName(Filename);
end;

constructor TUnit.Create;
begin
  inherited Create;
  FOpened := False;
end;

end.

