unit gui_customgrid; 

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
  gfxbase,
  fpgfx,
  gui_basegrid;
  
type

  // data object for grid columns
  TGridColumn = class(TObject)
  private
    FAlignment: TAlignment;
    FTitle: string;
    FWidth: integer;
  public
    constructor Create;
    property    Width: integer read FWidth write FWidth;
    property    Title: string read FTitle write FTitle;
    property    Alignment: TAlignment read FAlignment write FAlignment;
  end;
  
  
  TfpgCustomGrid = class(TfpgBaseGrid)
  private
    FRowCount: integer;
    function    GetColumns(AIndex: integer): TGridColumn;
  protected
    FColumns: TList;
    function    GetColumnCount: integer; override;
    procedure   SetColumnCount(const AValue: integer);
    function    GetRowCount: integer; override;
    procedure   SetRowCount(const AValue: integer); virtual;
    function    GetColumnWidth(ACol: integer): integer; override;
    procedure   SetColumnWidth(ACol: integer; const AValue: integer); override;
    function    GetHeaderText(ACol: integer): string; override;
    property    RowCount: integer read GetRowCount write SetRowCount;
    property    ColumnCount: integer read GetColumnCount write SetColumnCount;
    property    Columns[AIndex: integer]: TGridColumn read GetColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddColumn(ATitle: string; AWidth: integer): TGridColumn;
  end;
  
  
  // Just for testing!!!!!
  TfpgGrid = class(TfpgCustomGrid)
  published
    property    Columns;
    property    DefaultColWidth;
    property    DefaultRowHeight;
    property    Font;
    property    HeaderFont;
    property    BackgroundColor;
    property    FocusCol;
    property    FocusRow;
    property    RowSelect;
    property    ColumnCount;
    property    RowCount;
    property    ShowHeader;
    property    ShowGrid;
    property    HeaderHeight;
    property    ColResizing;
    property    ColumnWidth;
    property    OnFocusChange;
    property    OnRowChange;

  end;
  

implementation

{ TGridColumn }

constructor TGridColumn.Create;
begin
  Width     := 64;
  Title     := '';
  Alignment := taCenter;
end;

{ TfpgCustomGrid }

function TfpgCustomGrid.GetRowCount: integer;
begin
  Result := FRowCount;
end;

function TfpgCustomGrid.GetColumns(AIndex: integer): TGridColumn;
begin
  if (AIndex < 0) or (AIndex > FColumns.Count-1) then
    Result := nil
  else
    Result := TGridColumn(FColumns[AIndex]);
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
      TGridColumn(FColumns.Items[n-1]).Free;
      dec(n);
      FColumns.Count := n;
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
  RePaint;
end;

function TfpgCustomGrid.GetColumnWidth(ACol: integer): integer;
begin
  if (ACol > 0) and (ACol <= ColumnCount) then
    Result := TGridColumn(FColumns[ACol-1]).Width
  else
    result := DefaultColWidth;
end;

procedure TfpgCustomGrid.SetColumnWidth(ACol: integer; const AValue: integer);
var
  lCol: TGridColumn;
begin
  lCol := TGridColumn(FColumns[ACol-1]);
  
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

function TfpgCustomGrid.GetHeaderText(ACol: integer): string;
begin
  Result := TGridColumn(FColumns[ACol-1]).Title;
end;

constructor TfpgCustomGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TList.Create;
  ColumnCount := 5;
  RowCount    := 5;
end;

destructor TfpgCustomGrid.Destroy;
begin
  SetColumnCount(0);
  FColumns.Free;
  inherited Destroy;
end;

function TfpgCustomGrid.AddColumn(ATitle: string; AWidth: integer): TGridColumn;
begin
  Result := TGridColumn.Create;
  Result.Title := ATitle;
  Result.Width := AWidth;
  FColumns.Add(Result);
  
  UpdateScrollBars;
  RePaint;
end;

end.

