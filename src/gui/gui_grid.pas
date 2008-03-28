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
      Defines a File Grid and String Grid. Both are decendants of Custom Grid.
}

unit gui_grid;

{$mode objfpc}{$H+}

{
  TODO:
    * TCustomStringGrid: Col[] and Row[] properties need to be implemented,
      returning a TStrings with all related text inserted.
    * File Grid: Introduce support for images based on file types. User must
      be able to override the default images with their own.
    * Remove the usage of libc unit. libc is linux/x86 specific.
}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gui_customgrid;
  
type
{
  TfpgGrid = class(TfpgCustomGrid)
  public
    property    Font;
    property    HeaderFont;
  published
    property    Columns;
    property    DefaultColWidth;
    property    DefaultRowHeight;
    property    FontDesc;
    property    HeaderFontDesc;
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
    property    OnDoubleClick;
  end;
}
  
  { TfpgFileGrid }

  TfpgFileGrid = class(TfpgCustomGrid)
  private
    FFileList: TfpgFileList;
    FFixedFont: TfpgFont;
  protected
    function    GetRowCount: Longword; override;
    procedure   DrawCell(ARow, ACol: Longword; ARect: TfpgRect; AFlags: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CurrentEntry: TFileEntry;
    property    FixedFont: TfpgFont read FFixedFont;
    property    FileList: TfpgFileList read FFileList;
    property    DefaultRowHeight;
    property    Font;
    property    HeaderFont;
  published
    property    FontDesc;
    property    HeaderFontDesc;
    property    RowCount;
    property    ColumnCount;
    property    Columns;
    property    FocusRow;
    property    ScrollBarStyle;
    property    TabOrder;
    property    OnRowChange;
    property    OnDoubleClick;
  end;


  TfpgStringColumn = class(TfpgGridColumn)
  private
    FCells: TStringList;
  public
	  constructor Create; override;
	  destructor  Destroy; override;
    property    Cells: TStringList read FCells write FCells;
  end;


  TfpgCustomStringGrid = class(TfpgCustomGrid)
  private
    function    GetCell(ACol, ARow: Longword): string;
    function    GetColumnTitle(ACol: Longword): string;
    function    GetObjects(ACol, ARow: Longword): TObject;
    procedure   SetCell(ACol, ARow: Longword; const AValue: string);
    procedure   SetColumnTitle(ACol: Longword; const AValue: string);
    procedure   SetObjects(ACol, ARow: Longword; const AValue: TObject);
  protected
    function    GetColumnWidth(ACol: Longword): integer; override;
    procedure   SetColumnWidth(ACol: Longword; const AValue: integer); override;
    function    GetColumns(AIndex: Longword): TfpgStringColumn; reintroduce;
    procedure   DoDeleteColumn(ACol: integer); override;
    procedure   DoSetRowCount(AValue: integer); override;
    function    DoCreateColumnClass: TfpgStringColumn; reintroduce; override;
    procedure   DrawCell(ARow, ACol: Longword; ARect: TfpgRect; AFlags: integer); override;
    { AIndex is 1-based. }
    property    Columns[AIndex: Longword]: TfpgStringColumn read GetColumns;
  public
    constructor Create(AOwner: TComponent); override;
    function    AddColumn(ATitle: string; AWidth: integer; AAlignment: TAlignment = taLeftJustify;
        AbackgroundColor: TfpgColor = clDefault; ATextColor: TfpgColor = clDefault): TfpgStringColumn; overload;
    { ACol and ARow is 1-based. }
    property    Cells[ACol, ARow: Longword]: string read GetCell write SetCell;
    property    Objects[ACol, ARow: Longword]: TObject read GetObjects write SetObjects;
    property    ColumnTitle[ACol: Longword]: string read GetColumnTitle write SetColumnTitle;
    property    ColumnWidth[ACol: Longword]: integer read GetColumnWidth write SetColumnWidth;
    property    ColumnBackgroundColor[ACol: Longword]: TfpgColor read GetColumnBackgroundColor write SetColumnBackgroundColor;
    property    ColumnTextColor[ACol: Longword]: TfpgColor read GetColumnTextColor write SetColumnTextColor;
//    property    Cols[index: Integer]: TStrings read GetCols write SetCols;
//    property    Rows[index: Integer]: TStrings read GetRows write SetRows;
  end;


  TfpgStringGrid = class(TfpgCustomStringGrid)
  published
    property    BackgroundColor;
//    property    ColResizing;
    property    ColumnCount;
    property    Columns;
    property    ColumnWidth;
    property    DefaultColWidth;
    property    DefaultRowHeight;
    property    FocusCol;
    property    FocusRow;
    property    FontDesc;
    property    HeaderFontDesc;
    property    HeaderHeight;
    property    RowCount;
    property    RowSelect;
    property    ScrollBarStyle;
    property    ShowGrid;
    property    ShowHeader;
    property    TabOrder;
    property    TopRow;
    property    OnCanSelectCell;
    property    OnDrawCell;
    property    OnDoubleClick;
    property    OnFocusChange;
    property    OnKeyPress;
    property    OnRowChange;
  end;

function CreateStringGrid(AOwner: TComponent; x, y, w, h: TfpgCoord; AColumnCount: integer = 0): TfpgStringGrid;


implementation

uses
  gfx_constants
  {$IFDEF MSWINDOWS}
  ,Windows   // Graeme: temporary, just to see how the grid looks under Windows.
  {$ENDIF}
  {$IFDEF UNIX}
    // Graeme: temporary. libc is not available for FreeBSD.
    {$if defined(linux) and defined(cpu386)},libc{$endif}
//  ,baseunix
  {$ENDIF}
  ;

function CreateStringGrid(AOwner: TComponent; x, y, w, h: TfpgCoord; AColumnCount: integer = 0): TfpgStringGrid;
begin
  Result  := TfpgStringGrid.Create(AOwner);
  Result.Left         := x;
  Result.Top          := y;
  Result.Width        := w;
  Result.Height       := h;
  Result.ColumnCount  := AColumnCount;
end;

{$IFDEF UNIX}
{$if defined(linux) and defined(cpu386)}
function GetGroupName(gid: integer): string;
var
  p: PGroup;
begin
  p := getgrgid(gid);
  if p <> nil then
    result := p^.gr_name;
end;
{$else}
// Still need to find an alternative for FreeBSD as we can't use the libc unit.
function GetGroupName(gid: integer): string;
begin
  result := IntToStr(gid);
end;
{$endif}

{$if defined(linux) and defined(cpu386)}
function GetUserName(uid: integer): string;
var
  p: PPasswd;
begin
  p := getpwuid(uid);
  if p <> nil then
    result := p^.pw_name
  else
    result := '';
end;
{$else}
// Still need to find an alternative for FreeBSD as we can't use the libc unit.
function GetUserName(uid: integer): string;
begin
  result := IntToStr(uid);
end;
{$endif}
{$ENDIF UNIX}

{ TfpgFileGrid }

function TfpgFileGrid.GetRowCount: Longword;
begin
  Result := FFileList.Count;
end;

procedure TfpgFileGrid.DrawCell(ARow, ACol: Longword; ARect: TfpgRect; AFlags: integer);
const
  modestring: string[9] = 'xwrxwrxwr';  // must be in reverse order
var
  e: TFileEntry;
  x: integer;
  y: integer;
  s: string;
  img: TfpgImage;
  b: integer;
  n: integer;
begin
  e := FFileList.Entry[ARow];
  if e = nil then
    Exit; //==>

  x := ARect.Left + 2;
  y := ARect.Top;// + 1;
  s := '';

  if (e.EntryType = etDir) and (ACol = 1) then
    Canvas.SetFont(HeaderFont)
  else
    Canvas.SetFont(Font);

  case ACol of
    1:  begin
          if e.EntryType = etDir then
            img := fpgImages.GetImage('stdimg.folder')            // Do NOT localize
          else
          begin
            img := fpgImages.GetImage('stdimg.document');         // Do NOT localize
            {$IFDEF UNIX}
           if (e.Mode and $40) <> 0 then
              img := fpgImages.GetImage('stdimg.executable');     // Do NOT localize
            {$ENDIF}
           {$IFDEF MSWINDOWS}
           if lowercase(e.Extention) = 'exe' then
              img := fpgImages.GetImage('stdimg.executable');     // Do NOT localize
           {$ENDIF}
          end;

          if img <> nil then
            Canvas.DrawImage(ARect.Left+1, y, img);
          if e.IsLink then
            Canvas.DrawImage(ARect.Left+1, y, fpgImages.GetImage('stdimg.link'));
          x := ARect.Left + 20;
          s := e.Name;
        end;
        
    2:  begin
          s := FormatFloat('###,###,###,##0', e.size);
          x := ARect.Right - Font.TextWidth(s) - 1;
          if x < (ARect.Left + 2) then
            x := ARect.Left + 2;
        end;

    3:  s := FormatDateTime('yyyy-mm-dd hh:nn', e.ModTime);

    4:  begin
          {$IFDEF MSWINDOWS}
          // File attributes
          s := '';
          //if (e.attributes and FILE_ATTRIBUTE_ARCHIVE) <> 0    then s := s + 'a' else s := s + ' ';
          if (e.attributes and FILE_ATTRIBUTE_HIDDEN) <> 0     then s := s + 'h';
          if (e.attributes and FILE_ATTRIBUTE_READONLY) <> 0   then s := s + 'r';
          if (e.attributes and FILE_ATTRIBUTE_SYSTEM) <> 0     then s := s + 's';
          if (e.attributes and FILE_ATTRIBUTE_TEMPORARY) <> 0  then s := s + 't';
          if (e.attributes and FILE_ATTRIBUTE_COMPRESSED) <> 0 then s := s + 'c';
          {$ENDIF}
          {$IFDEF UNIX}
          // rights
          //rwx rwx rwx
          b := 1;
          n := 1;
          s := '';
          while n <= 9 do
          begin
            if (e.Mode and b) = 0 then
              s := '-' + s
            else
              s := modestring[n] + s;
            inc(n);
            b := b shl 1;
          end;
          {$ENDIF}

          Canvas.SetFont(FixedFont);
        end;

    {$IFDEF UNIX}
    5:  s := GetUserName(e.ownerid);  // use getpwuid(); for the name of this user
    {$ENDIF}

    {$IFDEF UNIX}
    6:  s := GetGroupName(e.groupid);  // use getgrgid(); for the name of this group
    {$ENDIF}
  end;
  // centre text in row height
  y := y + ((DefaultRowHeight - Canvas.Font.Height) div 2);
  Canvas.DrawString(x, y, s);
end;

constructor TfpgFileGrid.Create(AOwner: TComponent);
begin
  FFileList := TfpgFileList.Create;
  inherited Create(AOwner);
  ColumnCount := 0;
  RowCount := 0;
  FFixedFont := fpgGetFont('Courier New-9');

  {$Note No IFDEF's allowed!!! But how the hell to we get around this? }
{$ifdef MSWINDOWS}
  AddColumn(rsName, 320);
{$else}
  AddColumn(rsName, 220);
{$endif}

  AddColumn(rsSize, 80);
  AddColumn(rsFileModifiedTime, 108);
{$ifdef MSWINDOWS}
  AddColumn(rsFileAttributes, 78);
{$else}
  AddColumn(rsFileRights, 78);
  AddColumn(rsFileOwner, 54);
  AddColumn(rsFileGroup, 54);
{$endif}

  RowSelect := True;
  DefaultRowHeight := fpgImages.GetImage('stdimg.document').Height + 2;
end;

destructor TfpgFileGrid.Destroy;
begin
  OnRowChange := nil;
  FFixedFont.Free;
  FFileList.Free;
  inherited Destroy;
end;

function TfpgFileGrid.CurrentEntry: TFileEntry;
begin
  Result := FFileList.Entry[FocusRow];
end;

{ TfpgStringColumn }

constructor TfpgStringColumn.Create;
begin
  inherited Create;
  FCells := TStringList.Create;
end;

destructor TfpgStringColumn.Destroy;
begin
  FCells.Free;
  inherited Destroy;
end;

{ TfpgCustomStringGrid }

function TfpgCustomStringGrid.GetCell(ACol, ARow: Longword): string;
begin
  if ACol > ColumnCount then
    Exit; //==>
  if ARow > RowCount then
    Exit; //==>
  Result := TfpgStringColumn(FColumns.Items[ACol-1]).Cells[ARow-1];
end;

function TfpgCustomStringGrid.GetColumnTitle(ACol: Longword): string;
begin
  if ACol > ColumnCount then
    Exit; //==>
  Result := TfpgStringColumn(FColumns.Items[ACol-1]).Title;
end;

function TfpgCustomStringGrid.GetObjects(ACol, ARow: Longword): TObject;
begin
  if ACol > ColumnCount then
    Exit; //==>
  if ARow > RowCount then
    Exit; //==>
  Result := TfpgStringColumn(FColumns.Items[ACol-1]).Cells.Objects[ARow-1];
end;

function TfpgCustomStringGrid.GetColumnWidth(ACol: Longword): integer;
begin
  if ACol > ColumnCount then
    Exit; //==>
  Result := TfpgStringColumn(FColumns.Items[ACol-1]).Width;
end;

procedure TfpgCustomStringGrid.SetCell(ACol, ARow: Longword;
  const AValue: string);
begin
  if ACol > ColumnCount then
    Exit; //==>
  if ARow > RowCount then
    Exit; //==>
  if TfpgStringColumn(FColumns.Items[ACol-1]).Cells[ARow-1] <> AValue then
  begin
    BeginUpdate;
    TfpgStringColumn(FColumns.Items[ACol-1]).Cells[ARow-1] := AValue;
    EndUpdate;
  end;
end;

procedure TfpgCustomStringGrid.SetColumnTitle(ACol: Longword; const AValue: string);
begin
  if ACol > ColumnCount then
    Exit; //==>
  BeginUpdate;
  TfpgStringColumn(FColumns.Items[ACol-1]).Title := AValue;
  EndUpdate;
end;

procedure TfpgCustomStringGrid.SetObjects(ACol, ARow: Longword;
  const AValue: TObject);
begin
  if ACol > ColumnCount then
    Exit; //==>
  if ARow > RowCount then
    Exit; //==>
  TfpgStringColumn(FColumns.Items[ACol-1]).Cells.Objects[ARow-1] := AValue;
end;

procedure TfpgCustomStringGrid.SetColumnWidth(ACol: Longword; const AValue: integer);
begin
  if ACol > ColumnCount then
    Exit; //==>
  BeginUpdate;
  TfpgStringColumn(FColumns.Items[ACol-1]).Width := AValue;
  EndUpdate;
end;

function TfpgCustomStringGrid.GetColumns(AIndex: Longword): TfpgStringColumn;
begin
  if (AIndex < 1) or (AIndex > ColumnCount) then
    Result := nil
  else
    Result := TfpgStringColumn(FColumns.Items[AIndex-1]);
end;

procedure TfpgCustomStringGrid.DoDeleteColumn(ACol: integer);
begin
  TfpgStringColumn(FColumns.Items[ACol-1]).Free;
  FColumns.Delete(ACol-1);
end;

procedure TfpgCustomStringGrid.DoSetRowCount(AValue: integer);
var
  diff: integer;
  c: integer;
begin
  inherited DoSetRowCount(AValue);
  if FColumns.Count = 0 then
    Exit; //==>

  diff := AValue - TfpgStringColumn(FColumns.Items[0]).Cells.Count;
  if diff > 0 then  // We need to add rows
  begin
    for c := 0 to FColumns.Count - 1 do
    begin
      while TfpgStringColumn(FColumns[c]).Cells.Count <> AValue do
        TfpgStringColumn(FColumns[c]).Cells.Append('');
    end;
  end;
end;

function TfpgCustomStringGrid.DoCreateColumnClass: TfpgStringColumn;
begin
  Result := TfpgStringColumn.Create;
end;

procedure TfpgCustomStringGrid.DrawCell(ARow, ACol: Longword; ARect: TfpgRect;
    AFlags: integer);
var
  x: TfpgCoord;
begin
  if Cells[ACol, ARow] <> '' then
  begin
    if not Enabled then
      Canvas.SetTextColor(clShadow1);

    case Columns[ACol].Alignment of
      taLeftJustify:
          begin
            x := ARect.Left + 1;
          end;
      taCenter:
          begin
            x := (ARect.Width - Font.TextWidth(Cells[ACol, ARow])) div 2;
            Inc(x, ARect.Left);
          end;
      taRightJustify:
          begin
            x := ARect.Right - Font.TextWidth(Cells[ACol, ARow]) - 1;
            if x < (ARect.Left + 1) then
              x := ARect.Left + 1;
          end;
    end;  { case }

    Canvas.DrawString(x, ARect.Top+1, Cells[ACol, ARow]);
  end;
end;

constructor TfpgCustomStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ColumnCount := 0;
  RowCount := 0;
end;

function TfpgCustomStringGrid.AddColumn(ATitle: string; AWidth: integer;
    AAlignment: TAlignment; ABackgroundColor: TfpgColor; ATextColor: TfpgColor): TfpgStringColumn;
var
  r: integer;
begin
  Updating;
  Result := TfpgStringColumn(inherited AddColumn(ATitle, AWidth));
  Result.Alignment := AAlignment;

  if ABackgroundColor = clDefault then
    Result.BackgroundColor := clBoxColor
  else
    Result.BackgroundColor:= ABackgroundColor;

  if ATextColor = clDefault then
    Result.TextColor := TextColor
  else
    Result.TextColor:= ATextColor;
    
  for r := 1 to RowCount do
    Result.Cells.Append('');
  Updated;
end;

end.

