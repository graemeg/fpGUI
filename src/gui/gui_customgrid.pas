{
    fpGUI  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2008 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Custom Grid control and basic Column class.
}

unit gui_customgrid; 

{$mode objfpc}{$H+}

{
  TODO:
    * Column text alignment needs to be implemented. Currently always Centre.
    * AlternateColor for rows need to be implemented.
}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gui_basegrid;
  
type

  // data object for grid columns
  TfpgGridColumn = class(TObject)
  private
    FAlignment: TAlignment;
    FTitle: string;
    FWidth: integer;
    FBackgroundColor: TfpgColor;
    FTextColor: TfpgColor;
  public
    constructor Create; virtual;
    property    Width: integer read FWidth write FWidth;
    property    Title: string read FTitle write FTitle;
    property    Alignment: TAlignment read FAlignment write FAlignment;
    property    BackgroundColor: TfpgColor read FBackgroundColor write FBackgroundColor;
    property    TextColor: TfpgColor read FTextColor write FTextColor;
  end;
  
  
  { TfpgCustomGrid }

  TfpgCustomGrid = class(TfpgBaseGrid)
  protected
    FRowCount: integer;
    FColumns: TList;
    procedure   SetTextColor(const AValue: TfpgColor); override;
    function    GetColumns(AIndex: integer): TfpgGridColumn; virtual;
    procedure   DoDeleteColumn(ACol: integer); virtual;
    procedure   DoSetRowCount(AValue: integer); virtual;
    function    DoCreateColumnClass: TfpgGridColumn; virtual;
    function    GetColumnCount: integer; override;
    procedure   SetColumnCount(const AValue: integer); virtual;
    function    GetRowCount: integer; override;
    procedure   SetRowCount(const AValue: integer); virtual;
    function    GetColumnWidth(ACol: integer): integer; override;
    procedure   SetColumnWidth(ACol: integer; const AValue: integer); override;
    function    GetColumnBackgroundColor(ACol: integer): TfpgColor; override;
    procedure   SetColumnBackgroundColor(ACol: integer; const AValue: TfpgColor); override;
    function    GetColumnTextColor(ACol: integer): TfpgColor; override;
    procedure   SetColumnTextColor(ACol: integer; const AValue: TfpgColor); override;
    function    GetHeaderText(ACol: integer): string; override;
    property    RowCount: integer read GetRowCount write SetRowCount;
    property    ColumnCount: integer read GetColumnCount write SetColumnCount;
    { Columns AIndex is 1-based. }
    property    Columns[AIndex: integer]: TfpgGridColumn read GetColumns;
//    property AlternateColor: TColor read FAlternateColor write SetAlternateColor stored IsAltColorStored;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddColumn(ATitle: string; AWidth: integer): TfpgGridColumn; virtual;
    { AIndex is 1-based. }
    procedure   DeleteColumn(AIndex: integer); virtual;
    procedure   MoveColumn(oldindex, newindex: integer); virtual;
  end;
  
  
implementation

{ TfpgGridColumn }

constructor TfpgGridColumn.Create;
begin
  Width     := 65;
  Title     := '';
  Alignment := taLeftJustify;
end;

{ TfpgCustomGrid }

function TfpgCustomGrid.GetRowCount: integer;
begin
  Result := FRowCount;
end;

procedure TfpgCustomGrid.SetTextColor(const AValue: TfpgColor);
var
  i: integer;
begin
  inherited SetTextColor(AValue);
  for i := 0 to ColumnCount-1 do
  begin
    TfpgGridColumn(FColumns.Items[i]).TextColor := AValue;
  end;
  Repaint;
end;

function TfpgCustomGrid.GetColumns(AIndex: integer): TfpgGridColumn;
begin
  if (AIndex < 1) or (AIndex > FColumns.Count) then
    Result := nil
  else
    Result := TfpgGridColumn(FColumns[AIndex-1]);
end;

procedure TfpgCustomGrid.DoDeleteColumn(ACol: integer);
begin
  TfpgGridColumn(FColumns.Items[ACol-1]).Free;
  FColumns.Delete(ACol-1);
end;

procedure TfpgCustomGrid.DoSetRowCount(AValue: integer);
begin
  // do nothing
end;

function TfpgCustomGrid.DoCreateColumnClass: TfpgGridColumn;
begin
  Result := TfpgGridColumn.Create;
end;

function TfpgCustomGrid.GetColumnCount: integer;
begin
  Result := FColumns.Count;
end;

procedure TfpgCustomGrid.SetColumnCount(const AValue: integer);
var
  n: integer;
begin
  n := FColumns.Count;
  if (n = AValue) or (AValue < 0) then
    Exit; //==>

  if n < AValue then
  begin
    // adding columns
    while n < AValue do
    begin
      AddColumn('', DefaultColWidth);
      inc(n);
    end;
  end
  else
  begin
    while n > AValue do
    begin
      DoDeleteColumn(n);
      dec(n);
    end;
  end;
  UpdateScrollBars;
  RePaint;
end;

procedure TfpgCustomGrid.SetRowCount(const AValue: integer);
begin
  if FRowCount = AValue then
    Exit; //==>
  FRowCount := AValue;
  if FocusRow > FRowCount then
  begin
    FocusRow := FRowCount;
  end;
  DoSetRowCount(AValue);  // could be implemented by descendants
  UpdateScrollBars;
  RePaint;
end;

function TfpgCustomGrid.GetColumnWidth(ACol: integer): integer;
begin
  if (ACol > 0) and (ACol <= ColumnCount) then
    Result := TfpgGridColumn(FColumns[ACol-1]).Width
  else
    result := DefaultColWidth;
end;

procedure TfpgCustomGrid.SetColumnWidth(ACol: integer; const AValue: integer);
var
  lCol: TfpgGridColumn;
begin
  lCol := TfpgGridColumn(FColumns[ACol-1]);
  
  if lCol.Width <> AValue then
  begin
    if AValue < 1 then
      lCol.Width := 1
    else
      lCol.Width := AValue;
    UpdateScrollBars;
    Repaint;
  end;
end;

function TfpgCustomGrid.GetColumnBackgroundColor(ACol: integer): TfpgColor;
begin
  if (ACol > 0) and (ACol <= ColumnCount) then
    Result := TfpgGridColumn(FColumns[ACol-1]).FBackgroundColor
  else
    result := BackgroundColor;
end;

procedure TfpgCustomGrid.SetColumnBackgroundColor(ACol: integer; const AValue: TfpgColor);
var
  lCol: TfpgGridColumn;
begin
  lCol := TfpgGridColumn(FColumns[ACol-1]);

  if lCol.FBackgroundColor <> AValue then
  begin
    lCol.FBackgroundColor := AValue;
//    UpdateScrollBars;
    Repaint;
  end;
end;

function TfpgCustomGrid.GetColumnTextColor(ACol: integer): TfpgColor;
begin
  if (ACol > 0) and (ACol <= ColumnCount) then
    Result := TfpgGridColumn(FColumns[ACol-1]).FTextColor
  else
    result := TextColor;
end;

procedure TfpgCustomGrid.SetColumnTextColor(ACol: integer; const AValue: TfpgColor);
var
  lCol: TfpgGridColumn;
begin
  lCol := TfpgGridColumn(FColumns[ACol-1]);

  if lCol.FTextColor <> AValue then
  begin
    lCol.FTextColor := AValue;
//    UpdateScrollBars;
    Repaint;
  end;
end;

function TfpgCustomGrid.GetHeaderText(ACol: integer): string;
begin
  Result := TfpgGridColumn(FColumns[ACol-1]).Title;
end;

constructor TfpgCustomGrid.Create(AOwner: TComponent);
begin
  FColumns := TList.Create;
  inherited Create(AOwner);
  ColumnCount := 5;
  RowCount    := 5;
end;

destructor TfpgCustomGrid.Destroy;
begin
  while FColumns.Count > 0 do
  begin
    TfpgGridColumn(FColumns.Items[0]).Free;
    FColumns.Delete(0);
  end;

  FColumns.Free;
  inherited Destroy;
end;

function TfpgCustomGrid.AddColumn(ATitle: string; AWidth: integer): TfpgGridColumn;
begin
  Result := DoCreateColumnClass;
  Result.Title := ATitle;
  Result.Width := AWidth;
  Result.Backgroundcolor := clBoxcolor;
  Result.TextColor := TextColor;
  FColumns.Add(Result);
  
  if csUpdating in ComponentState then
    Exit; //==>
    
  UpdateScrollBars;
  RePaint;
end;

procedure TfpgCustomGrid.DeleteColumn(AIndex: integer);
var
  c: TfpgGridColumn;
begin
  c := Columns[AIndex];
  if c <> nil then
  begin
    DoDeleteColumn(AIndex);
    if HasHandle then
      Update;
  end;
end;

procedure TfpgCustomGrid.MoveColumn(oldindex, newindex: integer);
begin
  FColumns.Move(oldindex, newindex);
  if HasHandle then
    Update;
end;

end.

