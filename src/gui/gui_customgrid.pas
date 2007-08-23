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
  public
    constructor Create; virtual;
    property    Width: integer read FWidth write FWidth;
    property    Title: string read FTitle write FTitle;
    property    Alignment: TAlignment read FAlignment write FAlignment;
  end;
  
  
  TfpgCustomGrid = class(TfpgBaseGrid)
  protected
    FRowCount: integer;
    FColumns: TList;
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
    function    GetHeaderText(ACol: integer): string; override;
    property    RowCount: integer read GetRowCount write SetRowCount;
    property    ColumnCount: integer read GetColumnCount write SetColumnCount;
    property    Columns[AIndex: integer]: TfpgGridColumn read GetColumns;
//    property AlternateColor: TColor read FAlternateColor write SetAlternateColor stored IsAltColorStored;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddColumn(ATitle: string; AWidth: integer): TfpgGridColumn; virtual;
  end;
  
  
implementation

{ TfpgGridColumn }

constructor TfpgGridColumn.Create;
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

function TfpgCustomGrid.GetColumns(AIndex: integer): TfpgGridColumn;
begin
  if (AIndex < 0) or (AIndex > FColumns.Count-1) then
    Result := nil
  else
    Result := TfpgGridColumn(FColumns[AIndex]);
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

function TfpgCustomGrid.GetHeaderText(ACol: integer): string;
begin
  Result := TfpgGridColumn(FColumns[ACol-1]).Title;
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
  FColumns.Add(Result);
  
  UpdateScrollBars;
  RePaint;
end;

end.

