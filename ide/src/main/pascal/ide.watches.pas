(*
    fpGUI IDE - Watches Panel Logic

    Copyright (C) 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Pure logic for the Watches debugger panel: managing a user-defined
      list of watch expressions.

      Holds expression strings only; evaluated results (TVariableValue
      from pdr_ports) are owned by the main form, following the same
      pattern as the Variables panel.

      Persistence is handled externally via GetExpressions / Load.
*)
unit ide.watches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TWatchList — ordered list of user watch expressions.
    Case-sensitive; no duplicates. }
  TWatchList = class
  private
    FItems: TStringList;
    function GetCount: Integer;
    function GetExpression(AIndex: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    { Add expression. Returns the index (existing if already present). }
    function  AddWatch(const AExpr: String): Integer;
    { Remove entry at AIdx. Silently ignores out-of-range values. }
    procedure RemoveWatch(AIdx: Integer);
    { Return index of AExpr, or -1 if not present. Case-sensitive. }
    function  IndexOf(const AExpr: String): Integer;
    { Return all expressions as a plain array for passing to the worker. }
    function  GetExpressions: TStringArray;
    { Replace current list from an expressions array (e.g. on session load). }
    procedure LoadExpressions(const AExprs: TStringArray);
    { Remove all entries. }
    procedure Clear;
    property Count: Integer read GetCount;
    property Expression[I: Integer]: String read GetExpression;
  end;


implementation


{ TWatchList }

constructor TWatchList.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
  FItems.CaseSensitive := True;
end;

destructor TWatchList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TWatchList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TWatchList.GetExpression(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < FItems.Count) then
    Result := FItems[AIndex]
  else
    Result := '';
end;

function TWatchList.AddWatch(const AExpr: String): Integer;
begin
  Result := FItems.IndexOf(AExpr);
  if Result < 0 then
  begin
    Result := FItems.Count;
    FItems.Add(AExpr);
  end;
end;

procedure TWatchList.RemoveWatch(AIdx: Integer);
begin
  if (AIdx >= 0) and (AIdx < FItems.Count) then
    FItems.Delete(AIdx);
end;

function TWatchList.IndexOf(const AExpr: String): Integer;
begin
  Result := FItems.IndexOf(AExpr);
end;

function TWatchList.GetExpressions: TStringArray;
var
  I: Integer;
begin
  SetLength(Result, FItems.Count);
  for I := 0 to FItems.Count - 1 do
    Result[I] := FItems[I];
end;

procedure TWatchList.LoadExpressions(const AExprs: TStringArray);
var
  I: Integer;
begin
  FItems.Clear;
  for I := 0 to High(AExprs) do
    if (AExprs[I] <> '') and (FItems.IndexOf(AExprs[I]) < 0) then
      FItems.Add(AExprs[I]);
end;

procedure TWatchList.Clear;
begin
  FItems.Clear;
end;


end.
