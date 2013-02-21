unit elastictabstops;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_memo;
  
type

  TMutableInteger = class(TObject)
  public
    Value: integer;
  end;
  

  TETLine = class(TObject)
  public
    StartPos: integer;
    EndPos: integer;
    NumTabs: integer;
    constructor Create;
  end;


  TETTabstop = class(TObject)
  public
    TextWidthPix: integer;
    WidestWidthPix: TMutableInteger;
    StartPos: integer;
    EndPos: integer;
    EndsInTab: boolean;
    constructor Create;
  end;
  

  
  TElasticTabstopsDocFilter = class(TObject)
  private
    FMemo: TfpgMemo;
    FTabMultiples: integer;
    FTabMinimum: integer;
    FTabPadding: integer;
    FMaxTabstops: integer;
  protected
    property    Memo: TfpgMemo read FMemo;
    function    CalcTabWidth(TextWidthInTab: integer): integer;
  public
    constructor Create(AMemo: TfpgMemo);
    procedure   StretchTabstops;
    property    MaxTabstops: integer read FMaxTabstops write FMaxTabstops;
  end;


implementation

{ TETLine }

constructor TETLine.Create;
begin
  StartPos        := 0;
  EndPos          := 0;
  NumTabs         := 0;
end;

{ TETTabstop }

constructor TETTabstop.Create;
begin
  TextWidthPix    := 0;
//  WidestWidthPix  := 0;
  StartPos        := 0;
  EndPos          := 0;
  EndsInTab       := False;
end;


{ TElasticTabstopsDocFilter }

function TElasticTabstopsDocFilter.CalcTabWidth(TextWidthInTab: integer): integer;
var
  w: integer;
begin
  w := ((TextWidthInTab div FTabMultiples) + 1) * FTabMultiples;
  if w < FTabMinimum then
    w := FTabMinimum;
  Inc(w, FTabPadding);
  Result := w;
end;

constructor TElasticTabstopsDocFilter.Create(AMemo: TfpgMemo);
begin
  FMemo := AMemo;
  // tabstops are at least 32 pixels plus 8 pixels of padding
  FTabMultiples   := 1; // must be greater than 0
  FTabMinimum     := 32;
  FTabPadding     := 8;

  FMaxTabstops    := 32;  // tabs per line
end;

procedure TElasticTabstopsDocFilter.StretchTabstops;
var
  linecount: integer;
  lines: array of TETLine;
  grid: array of array of TETTabstop;
  l: integer;
  t: integer;
  c: integer;
  lineStart: integer;
  lineEnd: integer;
  lineText: string;
  tabs_on_line: integer;
  textWidthInTab: integer;
  theWidestWidthPix: TMutableInteger;
  maxWidth: integer;
  accTabStop: integer;
begin
  linecount := FMemo.Lines.Count;
  SetLength(lines, linecount);
  SetLength(grid, linecount, FMaxTabstops);
  
  // initialise array
  for l := 0 to linecount - 1 do  // for each line
  begin
    lines[l] := TETLine.Create;
    for t := 0 to MaxTabstops - 1 do  // for each column
      grid[l, t] := TETTabstop.Create;
  end;
  
  // get width of text in cells
  for l := 0 to linecount - 1 do  // for each line
  begin
    {$Note What must these be??? }
    lineStart := 0;
    lineEnd := 0;
    lines[l].StartPos := lineStart;
    lines[l].EndPos := lineEnd;
    
    lineText := FMemo.Lines[l];
    tabs_on_line := 0;
    textWidthInTab := 0;
    
    for c := 1 to Length(lineText) do // for each char in current line
    begin
      if c = Length(lineText) then
      begin
        grid[l, tabs_on_line].EndsInTab := False;
        grid[l, tabs_on_line].EndPos := lineStart + c;
        textWidthInTab := 0;
      end
      else if lineText[c] = #9 then
      begin
        grid[l, tabs_on_line].EndsInTab := True;
        grid[l, tabs_on_line].EndPos := lineStart + c;
        grid[l, tabs_on_line].TextWidthPix := CalcTabWidth(textWidthInTab);
        Inc(tabs_on_line, 1);
        grid[l, tabs_on_line].StartPos := lineStart + c + 1;
        Inc(lines[l].NumTabs, 1);
        textWidthInTab := 0;
      end
      else
        Inc(textWidthInTab, FMemo.Canvas.Font.TextWidth(lineText[c]));
    end;  { for c }
  end;  { for l }
  
  // find columns blocks and stretch to fit the widest cell
  for t := 0 to MaxTabstops - 1 do // for each column
  begin
    // All tabstops in column block point to same number. You change one, and
    // they all change
    theWidestWidthPix := TMutableInteger.Create; // reference
    theWidestWidthPix.Value := 0;
    maxWidth := 0;
    for l := 0 to linecount - 1 do  // for each line
    begin
      if grid[l, t].EndsInTab then
      begin
        grid[l, t].WidestWidthPix := theWidestWidthPix; // copy reference
        if grid[l, t].TextWidthPix < maxWidth then
          grid[l, t].TextWidthPix := maxWidth
        else
        begin
          maxWidth := grid[l, t].TextWidthPix;
          theWidestWidthPix.Value := maxWidth;
        end;
      end
      else  // end column block
      begin
        theWidestWidthPix := TMutableInteger.Create;  // new reference
        theWidestWidthPix.Value := 0;
        maxWidth := 0;
      end;  { if/else }
    end; { for l }
  end;
  
  // apply tabstop sizes to the text
  for l := 0 to linecount - 1 do  // for each line
  begin
    // accumulate tabstop widths
    accTabStop := 0;
    for t := 0 to lines[l].NumTabs - 1 do
    begin
      Inc(accTabStop, grid[l, t].WidestWidthPix.Value);
      grid[l, t].TextWidthPix := accTabStop;
    end;
    
    // SetBlocksTabStops(grid[l], lines[l].NumTabs);
//    Delete();

//    FMemo.Lines[l];
//      FMemo.Invalidate;
  end;
  
end;

end.

