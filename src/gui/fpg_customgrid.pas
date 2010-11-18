{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Custom Grid control and basic Column class.
}

unit fpg_customgrid;

{$mode objfpc}{$H+}

{
  TODO:
    * Column text alignment needs to be implemented. Currently always Centre.
}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_basegrid;
  
type

  // data object for grid columns
  TfpgGridColumn = class(TObject)
  private
    FAlignment: TAlignment;
    FLayout: TLayout;
    FHMargin: Integer;
    FTitle: string;
    FWidth: integer;
    FBackgroundColor: TfpgColor;
    FTextColor: TfpgColor;
  public
    constructor Create; virtual;
    property    Width: integer read FWidth write FWidth;
    property    Title: string read FTitle write FTitle;
    property    Alignment: TAlignment read FAlignment write FAlignment;
    property    Layout: TLayout read FLayout write FLayout;
    property    BackgroundColor: TfpgColor read FBackgroundColor write FBackgroundColor;
    property    HMargin: Integer read FHMargin write FHMargin;
    property    TextColor: TfpgColor read FTextColor write FTextColor;
  end;
  
  
  TfpgCustomGrid = class(TfpgBaseGrid)
  protected
    FRowCount: Integer;
    FColumns: TFPList;
    procedure   HandleSetFocus; override;
    procedure   SetTextColor(const AValue: TfpgColor); override;
    function    GetColumns(AIndex: integer): TfpgGridColumn; virtual;
    procedure   DoDeleteColumn(ACol: integer); virtual;
    procedure   DoSetRowCount(AValue: integer); virtual;
    procedure   DoAfterAddColumn(ACol: integer); virtual;
    function    DoCreateColumnClass: TfpgGridColumn; virtual;
    function    GetColumnCount: Integer; override;
    procedure   SetColumnCount(const AValue: Integer); virtual;
    function    GetRowCount: Integer; override;
    procedure   SetRowCount(const AValue: Integer); virtual;
    function    GetColumnWidth(ACol: Integer): integer; override;
    procedure   SetColumnWidth(ACol: Integer; const AValue: integer); override;
    function    GetColumnBackgroundColor(ACol: Integer): TfpgColor; override;
    procedure   SetColumnBackgroundColor(ACol: Integer; const AValue: TfpgColor); override;
    function    GetColumnTextColor(ACol: Integer): TfpgColor; override;
    procedure   SetColumnTextColor(ACol: Integer; const AValue: TfpgColor); override;
    function    GetHeaderText(ACol: Integer): string; override;
    property    RowCount: Integer read GetRowCount write SetRowCount;
    property    ColumnCount: Integer read GetColumnCount write SetColumnCount;
    property    Columns[AIndex: integer]: TfpgGridColumn read GetColumns;
//    property AlternateColor: TColor read FAlternateColor write SetAlternateColor stored IsAltColorStored;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddColumn(ATitle: string; AWidth: integer): TfpgGridColumn; virtual;
    procedure   DeleteColumn(AIndex: integer); virtual;
    procedure   DeleteRow(AIndex: integer); virtual;
    procedure   MoveColumn(oldindex, newindex: integer); virtual;
  end;
  
  
implementation

{ TfpgGridColumn }

constructor TfpgGridColumn.Create;
begin
  Width     := 65;
  Title     := '';
  Alignment := taLeftJustify;
  Layout := tlCenter;
  HMargin := 2;
end;

{ TfpgCustomGrid }

function TfpgCustomGrid.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

procedure TfpgCustomGrid.HandleSetFocus;
begin
  inherited HandleSetFocus;
  if (GetRowCount > 0) and (FocusRow = -1) then
  begin
    FocusRow := 0;
    FocusCol := 0;
//    Repaint;
    Update;
  end;
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
//  Repaint;
  Update;
end;

function TfpgCustomGrid.GetColumns(AIndex: integer): TfpgGridColumn;
begin
  if (AIndex < 0) or (AIndex > FColumns.Count-1) then
    Result := nil
  else
    Result := TfpgGridColumn(FColumns[AIndex]);
end;

procedure TfpgCustomGrid.DoDeleteColumn(ACol: integer);
begin
  TfpgGridColumn(FColumns.Items[ACol]).Free;
  FColumns.Delete(ACol);
end;

procedure TfpgCustomGrid.DoSetRowCount(AValue: integer);
begin
  // do nothing yet
end;

procedure TfpgCustomGrid.DoAfterAddColumn(ACol: integer);
begin
  // do nothing yet
  // update empty cells in descendants
end;

function TfpgCustomGrid.DoCreateColumnClass: TfpgGridColumn;
begin
  Result := TfpgGridColumn.Create;
end;

function TfpgCustomGrid.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

procedure TfpgCustomGrid.SetColumnCount(const AValue: Integer);
var
  n: Integer;
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
    // removing columns
    while n > AValue do
    begin
      DoDeleteColumn(n-1);
      dec(n);
    end;
  end;
  
  // graemeg 2008-07-18: I believe after all the repaint and event fixes
  //   this check is not required anymore.
  //if csUpdating in ComponentState then
    //Exit;
  Update;
  //UpdateScrollBars;
  //RePaint;

end;

procedure TfpgCustomGrid.SetRowCount(const AValue: Integer);
begin
  if FRowCount = AValue then
    Exit; //==>
  BeginUpdate;
  try
    FRowCount := AValue;
    if FocusRow > FRowCount-1 then
      FocusRow := FRowCount-1;
    DoSetRowCount(AValue);  // could be implemented by descendants
  finally
    EndUpdate;
  end;
end;

function TfpgCustomGrid.GetColumnWidth(ACol: Integer): integer;
begin
  if (ACol >= 0) and (ACol < ColumnCount) then
    Result := TfpgGridColumn(FColumns[ACol]).Width
  else
    result := DefaultColWidth;
end;

procedure TfpgCustomGrid.SetColumnWidth(ACol: Integer; const AValue: integer);
var
  lCol: TfpgGridColumn;
begin
  lCol := TfpgGridColumn(FColumns[ACol]);
  
  if lCol.Width <> AValue then
  begin
    if AValue < 1 then
      lCol.Width := 1
    else
      lCol.Width := AValue;
    Update;
    //UpdateScrollBars;
    //Repaint;
  end;
end;

function TfpgCustomGrid.GetColumnBackgroundColor(ACol: Integer): TfpgColor;
begin
  if (ACol >= 0) and (ACol < ColumnCount) then
    Result := TfpgGridColumn(FColumns[ACol]).FBackgroundColor
  else
    Result := BackgroundColor;
end;

procedure TfpgCustomGrid.SetColumnBackgroundColor(ACol: Integer; const AValue: TfpgColor);
var
  lCol: TfpgGridColumn;
begin
  lCol := TfpgGridColumn(FColumns[ACol]);

  if lCol.FBackgroundColor <> AValue then
  begin
    lCol.FBackgroundColor := AValue;
    Update;
    //Repaint;
  end;
end;

function TfpgCustomGrid.GetColumnTextColor(ACol: Integer): TfpgColor;
begin
  if (ACol >= 0) and (ACol < ColumnCount) then
    Result := TfpgGridColumn(FColumns[ACol]).FTextColor
  else
    result := TextColor;
end;

procedure TfpgCustomGrid.SetColumnTextColor(ACol: Integer; const AValue: TfpgColor);
var
  lCol: TfpgGridColumn;
begin
  lCol := TfpgGridColumn(FColumns[ACol]);

  if lCol.FTextColor <> AValue then
  begin
    lCol.FTextColor := AValue;
//    UpdateScrollBars;
    Repaint;
  end;
end;

function TfpgCustomGrid.GetHeaderText(ACol: Integer): string;
begin
  Result := TfpgGridColumn(FColumns[ACol]).Title;
end;

constructor TfpgCustomGrid.Create(AOwner: TComponent);
begin
  FColumns := TFPList.Create;
  inherited Create(AOwner);
  ColumnCount := 0;
  RowCount    := 0;
end;

destructor TfpgCustomGrid.Destroy;
var
  i: integer;
begin
  for i := FColumns.Count-1 downto 0 do
  begin
    TfpgGridColumn(FColumns.Items[i]).Free;
  end;
  FColumns.Free;
  inherited Destroy;
end;

function TfpgCustomGrid.AddColumn(ATitle: string; AWidth: integer): TfpgGridColumn;
var
  i: integer;
begin
  Result := DoCreateColumnClass;
  Result.Title := ATitle;
  Result.Width := AWidth;
  Result.Backgroundcolor := clBoxcolor;
  Result.TextColor := TextColor;
  i := FColumns.Add(Result);
  DoAfterAddColumn(i);  // update empty cells in descendants
  
  //if csUpdating in ComponentState then
    //Exit; //==>
  Update;
  //UpdateScrollBars;
  //RePaint;
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

procedure TfpgCustomGrid.DeleteRow(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex > FRowCount-1) then
    Exit;
  { This is just some sanity checking. Actual row deletion must occur in
    descendant classed. See TfpgStringGrid for an example. }
end;

procedure TfpgCustomGrid.MoveColumn(oldindex, newindex: integer);
begin
  FColumns.Move(oldindex, newindex);
  if HasHandle then
    Update;
end;

end.

