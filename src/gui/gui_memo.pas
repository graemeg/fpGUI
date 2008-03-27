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
      Defines a Memo control. Also known as a multi-line text edit control.
}

unit gui_memo;

{$mode objfpc}{$H+}

  { TODO : Started a implementation for Tab support. It is still very experimental and should not be used yet. }

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget,
  gui_scrollbar;

type

  { TfpgMemo }

  TfpgMemo = class(TfpgWidget)
  private
    FLines: TStrings;
    FMaxLength: integer;
    FCursorPos: integer;
    FCursorLine: integer;
    FOnChange: TNotifyEvent;
    FSideMargin: integer;
    FSelStartLine: integer;
    FSelEndLine: integer;
    FSelStartPos: integer;
    FSelEndPos: integer;
    FSelecting: boolean;
    FMouseDragging: boolean;
    FMouseDragPos: integer;
    FFont: TfpgFont;
    FDrawOffset: integer;
    FLineHeight: integer;
    FFirstLine: integer;
    FTabWidth: integer;
    FUseTabs: boolean;
    FVScrollBar: TfpgScrollBar;
    FHScrollBar: TfpgScrollBar;
    FWrapping: boolean;
    FLongestLineWidth: TfpgCoord;
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
    procedure   RecalcLongestLine(AStartLine, AEndLine: Integer; OnlyCheckifBigger: Boolean);
    procedure   DeleteSelection;
    procedure   DoCopy;
    procedure   DoPaste;
    procedure   AdjustCursor;
    function    LineCount: integer;
    function    GetLineText(linenum: integer): string;
    procedure   SetLineText(linenum: integer; Value: string);
    function    GetCursorX: integer;
    procedure   SetCPByX(x: integer);
    function    CurrentLine: string;
    function    VisibleLines: integer;
    function    VisibleWidth: integer;
    procedure   VScrollBarMove(Sender: TObject; position: integer);
    procedure   HScrollBarMove(Sender: TObject; position: integer);
    procedure   SetText(const AValue: string);
    function    GetText: string;
    procedure   SetCursorLine(aValue: integer);
    procedure   UpdateScrollBarCoords;
  protected
    procedure   MsgTextChanged(var msg: TfpgMessageRec); message FPGM_TEXT_CHANGE;
    procedure   MsgTextInsert(var msg: TfpgMessageRec); message FPGM_TEXT_INSERT;
    procedure   MsgTextDelete(var msg: TfpgMessageRec); message FPGM_TEXT_DELETE;
  protected
    procedure   HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleResize(dwidth, dheight: integer); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleTextChanged; virtual;
    procedure   HandleTextInsert(AText: PChar; ALength: Integer; BeforeEvent: Boolean; AStartLine, AEndLine: Integer); virtual;
    procedure   HandleTextDelete(AText: PChar; ALength: Integer; BeforeEvent: Boolean; AStartLine, AEndLine: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   UpdateScrollBars;
    procedure   Refresh;
    function    SelectionText: string;
    property    CursorLine: integer read FCursorLine write SetCursorLine;
    property    Font: TfpgFont read FFont;
    property    LineHeight: integer read FLineHeight;
    property    MaxLength: integer read FMaxLength write FMaxLength;
    property    TabWidth: integer read FTabWidth write FTabWidth;
    property    Text: string read GetText write SetText;
    property    UseTabs: boolean read FUseTabs write FUseTabs default False;
  published
    property    BackgroundColor default clBoxColor;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Lines: TStrings read FLines;
    property    TabOrder;
    property    TextColor;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


function CreateMemo(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgMemo;


implementation

uses
  gfx_UTF8utils;
  
  
type
  // custom stringlist that will notify the memo of item changes


  { TfpgMemoIndex }

  TfpgMemoIndex = object
    FLineIndices  : array of Integer;
    FLineLength   : array of Integer;
    FLineCount    : Integer;
    FLastLine     : Integer;
    FChangeAmount : Integer;
    
    procedure   ApplyChanges(AToLine: Integer = -1);

    constructor Init;
    destructor  Destroy;
    
    function    LineCount: Integer;
    function    LineFromOffset(const AOffset: Integer): Integer;
    function    LineLength(const AIndex: Integer): Integer; // UTF8 chars
    function    OffsetFromLine(AIndex: Integer): Integer;
    procedure   AdjustLinesAfterIndex(const AIndex: Integer; const ADelta: Integer);
    procedure   InsertLines(AIndex: Integer; const ACount: Integer);
    procedure   DeleteLines(const AStartIndex: Integer; const ACount: Integer);
    procedure   SetLineCount(const AValue: Integer);
    procedure   SetLineLength(const AIndex: Integer; const AValue: Integer); // UTF8 chars
    procedure   SetLineOffset(const AIndex: Integer; const AOffset: Integer);
  end;

  { TfpgMemoStrings }

  TfpgMemoStrings = class(TStrings)
  private
    FTextStream: TMemoryStream;
    FTextStreamSize: Integer;
    FIndex     : TfpgMemoIndex;
    Memo       : TfpgMemo;
    FCursorPos : Integer;
    FUpdating  : Boolean;
    function    LineHasLineEnding(AIndex: Integer): Boolean;
  protected
    function    Get(Index: Integer): string; override;
    function    GetCount: Integer; override;
    function    GetTextStr: string; override;
    procedure   Put(Index: Integer; const S: string); override;
    procedure   SetTextStr(const Value: string); override;
    procedure   SetUpdateState(Updating: Boolean); override;
    function    StreamSize(SetToSize: Integer = -1): Integer;
  public
    constructor Create(AMemo: TfpgMemo); reintroduce;
    destructor  Destroy; override;

    function    Add(const s: String): Integer; override;
    procedure   Assign(Source: TPersistent); override;
    procedure   Clear; override;
    procedure   Delete(Index: Integer); override;
    procedure   Insert(Index: Integer; const S: string); override;
    procedure   SaveToStream(Stream: TStream); override;
    
    procedure   InsertText(AText: String; AOffset: Integer);
    procedure   DeleteText(AOffset, ALength: Integer);
    
    // Offsets are in UTF chars
    procedure   DeleteCharsByLines(AStartLine, AEndLine, AStartLineOffset, AEndLineOffset: Integer);
    procedure   InsertCharsByLine (AChars: String; AStartLine, ALineOffset: Integer);
  end;

{ TfpgMemoIndex }

function TfpgMemoIndex.LineCount: Integer;
begin
  Result := FLineCount;
end;

function TfpgMemoIndex.LineFromOffset(const AOffset: Integer): Integer;
var
  i: Integer;
  maxLine,
  minLine: Integer;
  factor: Integer;
  tmpOffset: Integer;
  tmpIndex: Integer;
begin
  if OffsetFromLine(FLastLine) = AOffset then
    Exit(FLastLine); //==>

  if AOffset = 0 then
    Exit(0); //==>


  // divide linst in half and move in the right direction
  factor := FLineCount div 2;
  tmpIndex := 0;
  maxLine := LineCount-1;
  minLine := 0;
  while factor > 10 do
  begin
    ApplyChanges(tmpIndex+factor);
    tmpOffset := FLineIndices[tmpIndex+factor];
    if tmpOffset = AOffset then
      Exit(tmpIndex+factor) //==>
    else if  tmpOffset > AOffset then
    begin
      maxLine  := tmpIndex+factor;
    end
    else if  tmpOffset < AOffset then
    begin
      tmpIndex := minLine+factor;
      minLine  := tmpIndex;
    end;
    factor := (maxLine - minLine) div 2;
    if factor mod 2 <> 0 then
      Inc(factor);
  end;
  if minline < 15 then
    minLine := 0;
  //writeln('minline = ', minline,' maxline = ', maxline);
  for i := minLine to maxLine do
  begin
    if FLineIndices[i] > AOffset then
      Exit(i-1); //==>
  end;
  
  Result := FLineCount-1;
end;

function TfpgMemoIndex.LineLength(const AIndex: Integer): Integer;
begin
   Result := FLineLength[AIndex];
end;

function TfpgMemoIndex.OffsetFromLine(AIndex: Integer): Integer;
begin
  ApplyChanges(AIndex);

  if AIndex < LineCount then
    Result := FLineIndices[AIndex]
  else
    Result := -1;
  WriteLn('LineCount = ', FLineCount,' Want LineIndex=', AIndex, ' Result Offset = ',Result);
end;

procedure TfpgMemoIndex.ApplyChanges(AToLine: Integer = -1);
var
  i: Integer;
begin
  if FChangeAmount = 0 then
    Exit; //==>
  if (AToLine <= FLastLine) and (AToLine > -1) then
    Exit; //==>

  if (AToLine = -1) or (AToLine > FLastLine-1) then
    AToLine := FLineCount-1;
  //WriteLn('Applying Changes from ', FLastLine+1, ' to ', AToLine, ' by ',FChangeAmount);
  for i := FLastLine+1 to AToLine do
    FLineIndices[i] := FLineIndices[i] + FChangeAmount;
  FChangeAmount := 0;
  //WriteLn('Line 0 offset = ',  FLineIndices[0], ' Line 1 offset = ', FLineIndices[1]);
end;

procedure TfpgMemoIndex.AdjustLinesAfterIndex(const AIndex: Integer;
  const ADelta: Integer);
var
  FOldDelta: Integer;
begin
  WriteLN('Adjusting lines after ', AIndex, ' by ', ADelta);
  if AIndex < 0 then
  begin
    //WriteLn('AHHHH!');
  end;
  if AIndex > FLastLine then
  begin
    FOldDelta := FChangeAmount;
    ApplyChanges(AIndex);
    FChangeAmount := FOldDelta;
  end
  else if AIndex < FLastLine then
  begin
    ApplyChanges;
  end;
  
  FChangeAmount := FChangeAmount + ADelta;
  FLastLine := AIndex;
end;

procedure TfpgMemoIndex.InsertLines(AIndex: Integer; const ACount: Integer);
var
  NeedMove: Boolean;
begin
  //WriteLn('Inserting Lines, Count = ', Acount, ' At index: ', AIndex);
  if ACount = 0 then
    Exit; //==>

  NeedMove := AIndex < FLineCount;
  SetLineCount(LineCount+ACount);
  
  if NeedMove then
  begin
    //WriteLn('Moving ahead by: ', LineCount-AIndex-ACount);
    Move(FLineIndices[AIndex], FLineIndices[AIndex+ACount], (LineCount-AIndex-ACount)*SizeOf(Integer));
    Move(FLineLength[AIndex], FLineLength[AIndex+ACount], (LineCount-AIndex-ACount)*SizeOf(Integer));
  end;
end;

procedure TfpgMemoIndex.DeleteLines(const AStartIndex: Integer; const ACount: Integer
  );
begin
  //WriteLn('Deleting Indices starting at ', AStartIndex,' Count = ', ACount);
  if ACount <= 0 then
    Exit; //==>
  if AStartIndex+ACount <> LineCount then
  begin
    Move(FLineIndices[AStartIndex+ACount], FLineIndices[AStartIndex], (LineCount-AStartIndex-ACount)*SizeOf(Integer));
    Move(FLineLength[AStartIndex+ACount], FLineLength[AStartIndex], (LineCount-AStartIndex-ACount)*SizeOf(Integer));
  end;
  SetLineCount(LineCount-ACount);
end;

procedure TfpgMemoIndex.SetLineCount(const AValue: Integer);
var
  ArraySize: Integer;
begin
  if AValue = 0 then
  begin
    SetLength(FLineIndices, 0);
    SetLength(FLineLength,  0);
    FLineCount := 0;
    Exit; //==>
  end;
  ArraySize := Length(FLineIndices);

  if (AValue < ArraySize-99) or (AValue > ArraySize) then
  begin
    SetLength(FLineIndices,AValue+50);
    SetLength(FLineLength, AValue+50);
  end;
  FLineCount := AValue;
end;

procedure TfpgMemoIndex.SetLineLength(const AIndex: Integer;
  const AValue: Integer);
begin
  FLineLength[AIndex] := AValue;
end;

procedure TfpgMemoIndex.SetLineOffset(const AIndex: Integer;  const AOffset: Integer);
begin
  FLineIndices[AIndex] := AOffset;
end;

constructor TfpgMemoIndex.Init;
begin
  FLineCount    := 0;
  FLastLine     := 0;
  FChangeAmount := 0;
end;

destructor TfpgMemoIndex.Destroy;
begin
  SetLineCount(0);
end;

{ TfpgMemoStrings }

procedure TfpgMemoStrings.InsertText(AText: String; AOffset: Integer);
type
  PLineData = ^TLineData;
  TLineData = record
    LineSize  : Integer;
    LineOffset: Integer;
    LineLength: Integer; // UTF8 chars
  end;
var
  NewLines: TList;
  NewLineCount: Integer;
  NewLineStart: Integer;
  LastLineHasEnd: Boolean;
  LastLineOffset: Integer;
  LineSize: Integer;
  TextSize: Integer;
  Line: PLineData;
  Buf: String;
  TmpStr: String;
  i: Integer;
  StartIndex: Integer;
  CurentOffset: Integer;
  msgParm: TfpgMessageParams;
begin
  //if AOffset < 0 then //WriteLn('ERROR! AOffset < 0 !');
  TextSize := Length(AText);

  if TextSize = 0 then
    Exit; //==>

  NewLines := TList.Create;
  NewLineStart := 1;
  
  // check for lines in text to be added
  SetLength(Buf, Length(LineEnding));
  for i := 1 to TextSize do
  begin
    if i < NewLineStart then
      Continue;
    Buf := Copy(AText, i, Length(LineEnding));
    if (i = Length(AText)) or (Buf = LineEnding) then
    begin
      Line := New(PLineData);
      Line^.LineOffset := i;
      Line^.LineSize := i-NewLineStart+1;
      TmpStr := Copy(AText, NewLineStart, Line^.LineSize);
      Line^.LineLength := UTF8Length(TmpStr);
      //WriteLn('TmpStr = "', tmpstr,'"');
      Inc(NewLineStart, Line^.LineSize);
      NewLines.Add(Line);
      WriteLn('Added Line');
    end;
    //WriteLn('In loop i = ',i,' Length = ', Length(Atext));
  end;
  LastLineHasEnd := Buf = LineEnding;
  WriteLn('NewLines.Count = ', NewLines.Count);
  // update line indices
  StartIndex := FIndex.LineFromOffset(AOffset);
  WriteLn('Got StartIndex = ', StartIndex);
  if (StartIndex = Count-1) and (AOffset = StreamSize) then
    if LineHasLineEnding(StartIndex) then
      StartIndex := Count;

  WriteLn('after check StartIndex = ', StartIndex);
  
  if LastLineHasEnd or (FIndex.LineCount = 0) then
    NewLineCount := NewLines.Count
  else
    NewLineCount := NewLines.Count-1;
    
  msgParm.text.StartLine := StartIndex;
  msgParm.text.EndLine   := StartIndex + NewLineCount;
  msgParm.text.Text := PChar(AText);
  msgParm.text.Length := TextSize;
  msgParm.text.Before := True;
  fpgPostMessage(Self, Memo, FPGM_TEXT_INSERT, msgParm);

  FIndex.InsertLines(StartIndex+Ord(FIndex.OffsetFromLine(StartIndex)>AOffset), NewLineCount);

  // write new text to the stream
  StreamSize(StreamSize + TextSize);
  if AOffset < StreamSize then
  begin
     System.Move((FTextStream.Memory+AOffset)^, (FTextStream.Memory+AOffset+TextSize)^, StreamSize - AOffset - TextSize)
  end;
  FTextStream.Position := AOffset;
  FTextStream.Write(AText[1], TextSize);

  //Dec(StartIndex);
  CurentOffset := AOffset;
  
  for i := 0 to NewLines.Count-1 do
  begin
    Line := PLineData(NewLines[i]);
    //WriteLn('AOffset = ', Aoffset);
    if  (i = 0)
    and (((FIndex.OffsetFromLine(StartIndex) <> 0) and (StartIndex > 0)) or (StartIndex = 0))
    and (FIndex.OffsetFromLine(StartIndex) < AOffset) then
    // we need to update the prev line UTF8 length since we inserted in the middle of the line
    begin
      WriteLn('Inserting In the middle');
      if i = NewLines.Count-1 then
      begin
        if LastLineHasEnd then
        begin
          LineSize := (CurentOffset - FIndex.FLineIndices[StartIndex]) + Line^.LineSize;
          FIndex.SetLineOffset(StartIndex+1, FIndex.FLineIndices[StartIndex]+LineSize-TextSize);
          SetLength(TmpStr, LineSize);
          FTextStream.Position := FIndex.OffsetFromLine(StartIndex+1);
          FTextStream.Read(TmpStr[1], LineSize);
          FIndex.SetLineLength(StartIndex+1, UTF8Length(TmpStr));
        end
        else if StartIndex < FIndex.LineCount-1 then
          LineSize := FIndex.FLineIndices[StartIndex+1+NewLineCount] - FIndex.FLineIndices[StartIndex]
        else
          LineSize := StreamSize - Findex.OffsetFromLine(StartIndex) + Line^.LineSize;
        WriteLn('Adjusted Line Size = ',LineSize);
        
      end
      else begin
        LineSize := AOffset - FIndex.OffsetFromLine(StartIndex) + Line^.LineSize;
        FIndex.SetLineOffset(StartIndex+1, FIndex.OffsetFromLine(StartIndex)+ LineSize);
      end;

      SetLength(TmpStr, LineSize);
      FTextStream.Position := FIndex.OffsetFromLine(StartIndex);
      FTextStream.Read(TmpStr[1], LineSize);
      FIndex.SetLineLength(StartIndex-1, UTF8Length(TmpStr));
    end
    else if (LastLineHasEnd = False) and (i = NewLines.Count-1) then
    begin
      WriteLn ('Inserting with no lineending at ', StartIndex + i);
      //WriteLn('Line Offset = ',Findex.OffsetFromLine(StartIndex));
      //WriteLn('NewLineCount = ',NewLineCount);
      if i+StartIndex < FIndex.LineCount-1 then
        LineSize := FIndex.FLineIndices[StartIndex+1+i+NewLineCount] - FIndex.FLineIndices[StartIndex+i]
      else
        LineSize := StreamSize - Findex.OffsetFromLine(StartIndex+i);
      //WriteLn('OldLineSize = ', LineSize);
      Inc(LineSize, Line^.LineSize);
      //WriteLn('NewLineSize = ', LineSize);
      
      SetLength(TmpStr, LineSize);
      FTextStream.Position := FIndex.OffsetFromLine(StartIndex+i);
      FTextStream.Read(TmpStr[1], LineSize);
      FIndex.SetLineLength(StartIndex+i, UTF8Length(TmpStr));
      FIndex.SetLineOffset(StartIndex+i, CurentOffset);
      //if StartIndex+i < FIndex.LineCount-1 then
        //FIndex.SetLineOffset(StartIndex+i+1, CurentOffset + Line^.LineSize);
    end
    else begin
      WriteLn('Inserting normal line');
      FIndex.SetLineLength(StartIndex+i, Line^.LineLength);
      //if StartIndex+i > 0 then
      WriteLn('CurentOffset = ', CurentOffset);
        FIndex.SetLineOffset(StartIndex+i, CurentOffset);
    end;
    Inc(CurentOffset, Line^.LineSize);
  end;
  //WriteLn('LineSize = ', Line^.LineSize);
  //WriteLn('StartIndex = ', StartIndex,' i = ',i);
  FIndex.AdjustLinesAfterIndex(StartIndex+i, TextSize);
   WriteLn('Line Count = ', Count);
  // free our temp index
  for i := 0 to NewLines.Count-1 do
    Dispose(PLineData(NewLines[i]));
  NewLines.Free;

  msgParm.text.Before := False;
  fpgPostMessage(Self, Memo, FPGM_TEXT_INSERT, msgParm);
  //WriteLn;
  
end;

procedure TfpgMemoStrings.DeleteText(AOffset, ALength: Integer);
var
  FirstIndex,
  LastIndex : Integer;
  NextLineStart: Integer;
  TmpStr: String;
  IgnoreCount: Integer;
  LineStart: Integer;
  msgPArm: TfpgMessageParams;
  KeepStartLine: Boolean;
begin
  if ALength = 0 then
    Exit;//==>
  //WriteLn('Before');
  for LineStart := 0 to Count-1 do
  begin
    //WriteLN('Line ', LineStart,' offset = ', FIndex.OffsetFromLine(LineStart));
    //WriteLN('Line ', LineStart,' Length = ', FIndex.LineLength(LineStart));
  end;
    
  //WriteLn('DeleteOffset = ', AOffset,' Length = ', ALength);
    
  FirstIndex := FIndex.LineFromOffset(AOffset);
  LastIndex  := FIndex.LineFromOffset(AOffset+ALength);
  
  //WriteLn('FirstIndex = ', FirstIndex,' LastIndex = ', LastIndex);
  
  msgParm.text.StartLine := FirstIndex;
  msgParm.text.EndLine   := LastIndex;
  msgParm.text.Text := PChar(FTextStream.Memory+AOffset);
  msgParm.text.Length := ALength;
  msgParm.text.Before := True;
  fpgPostMessage(Self, Memo, FPGM_TEXT_DELETE, msgParm);

  if LastIndex < FIndex.LineCount-1 then
    NextLineStart := Findex.OffsetFromLine(LastIndex+1) - ALength
  else
    NextLineStart := StreamSize-ALength;

  KeepStartLine := FirstIndex = 0;{(Findex.OffsetFromLine(FirstIndex) < AOffset)
                or ((Findex.OffsetFromLine(FirstIndex) = AOffset) and (AOffset + ALength < NextLineStart));}


  FIndex.DeleteLines(FirstIndex+1{Ord(KeepStartLine)}, LastIndex - (FirstIndex{+Ord(KeepStartLine)}));

  FTextStream.Position := AOffset;
  FTextStream.Write((FTextStream.Memory+AOffset+ALength)^, StreamSize-(AOffset+ALength));
  StreamSize(StreamSize-ALength);
  
  LineStart := FIndex.OffsetFromLine(FirstIndex-Ord(not KeepStartLine));

  SetLength(TmpStr, NextLineStart-LineStart);
  
  FTextStream.Position := LineStart;
  FTextStream.Read(TmpStr[1], NextLineStart-LineStart);
  
  FIndex.SetLineLength(FirstIndex-Ord(not KeepStartLine), UTF8Length(TmpStr));
  
  FIndex.AdjustLinesAfterIndex(FirstIndex{-Ord(not KeepStartLine)}, -ALength);
  //WriteLn('After');
  for LineStart := 0 to Count-1 do
  begin
    //WriteLN('Line ', LineStart,' offset = ', FIndex.OffsetFromLine(LineStart));
    //WriteLN('Line ', LineStart,' Length = ', FIndex.LineLength(LineStart));
  end;
  //WriteLN;

  msgParm.text.Text := nil;
  msgParm.text.Length := 0;
  msgParm.text.Before := False;
  fpgPostMessage(Self, Memo, FPGM_TEXT_DELETE, msgParm);
end;

procedure TfpgMemoStrings.DeleteCharsByLines(AStartLine, AEndLine,
  AStartLineOffset, AEndLineOffset: Integer);
var
  LineString: string;
  Offset: Integer;
  LineStart: PChar;
  StartPos,
  EndPos: Integer;
begin
  inherited;

  LineString := Get(AStartLine);
  LineStart := PChar(LineString);
  //WriteLn('StartLine = ', AStartLine);
  //WriteLn('EndLine   = ', AEndLine);

  OffSet := FIndex.OffsetFromLine(AStartLine);
  if Offset < 0 then
    Offset := 0;
  StartPos := Offset + (UTF8CharStart(LineStart, Length(LineString), AStartLineOffset) - LineStart);
  //WriteLn('Inserting char at offset ',LineBytePos-Offset,' on line ', AStartLine);
  if AEndLine <> AStartLine then
  begin
    if AEndLineOffset > 0 then
    begin
      LineString := Get(AEndLine);
      LineStart := PChar(LineString);
    end;
    OffSet := FIndex.OffsetFromLine(AEndLine);
    if Offset < 0 then
      Offset := 0;
  end;

  if AEndLineOffset > 0 then
    EndPos := Offset + (UTF8CharStart(LineStart, Length(LineString), AEndLineOffset) - LineStart)
  else
    EndPos := Offset;

  DeleteText(StartPos, EndPos- StartPos);
end;

procedure TfpgMemoStrings.InsertCharsByLine(AChars: String; AStartLine,
  ALineOffset: Integer);
var
  LineString: string;
  Offset: Integer;
  LineStart: PChar;
  LineBytePos: Integer;
begin
  inherited;

  LineString := Get(AStartLine);
  LineStart := PChar(LineString);
  //WriteLn('LineIndex = ', AStartLine);

  OffSet := FIndex.OffsetFromLine(AStartLine);
  if Offset < 0 then
      Offset := 0;
  LineBytePos := Offset + (UTF8CharStart(LineStart, Length(LineString), ALineOffset) - LineStart);
  //WriteLn('Inserting char at offset ',LineBytePos-Offset,' on line ', AStartLine);
  InsertText(AChars, LineBytePos);
end;

function TfpgMemoStrings.LineHasLineEnding(AIndex: Integer): Boolean;
var
  Str: String;
  StrEnd: Integer;
begin
  SetLength(Str, Length(LineEnding));
  if AIndex < Count-1 then
    StrEnd := FIndex.OffsetFromLine(AIndex+1)
  else
    StrEnd := StreamSize;
    
  FTextStream.Position:= StrEnd-Length(LineEnding);
  FTextStream.Read(Str[1], Length(LineEnding));
  
  Result := Str = LineEnding;
end;

function TfpgMemoStrings.Get(Index: Integer): string;
var
  LineStart,
  LineEnd: Integer;
begin
  LineStart := FIndex.OffsetFromLine(Index);
  if Index < FIndex.LineCount-1 then
    LineEnd := FIndex.OffsetFromLine(Index+1)
  else
    LineEnd := StreamSize;
  SetLength(Result, LineEnd-LineStart);
  FTextStream.Position := LineStart;
  FTextStream.Read(Result[1], LineEnd-LineStart);
  if Copy(Result, Length(Result)+1-Length(LineEnding), Length(LineEnding)) = LineEnding then
    SetLength(Result, Length(Result) - Length(LineEnding));
  
end;

function TfpgMemoStrings.GetCount: Integer;
begin
  Result := FIndex.LineCount;
end;

function TfpgMemoStrings.GetTextStr: string;
begin
  SetLength(Result, StreamSize);
  FTextStream.Position := 0;
  FTextStream.Read(Result[1], StreamSize);
end;

procedure TfpgMemoStrings.Put(Index: Integer; const S: string);
begin
  BeginUpdate;
  Inherited;
  EndUpdate;
end;

procedure TfpgMemoStrings.SetTextStr(const Value: string);
begin
  Clear;
  //WriteLn('InsertingText: ', Value);
  InsertText(Value, 0);
end;

procedure TfpgMemoStrings.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
end;

function TfpgMemoStrings.StreamSize(SetToSize: Integer): Integer;
begin
  if SetToSize > -1 then
  begin
    FTextStreamSize := SetToSize;
    if (FTextStreamSize > FTextStream.Size)
    or (FTextStream.Size - 256 > FTextStreamSize)
    then
      FTextStream.Size := FTextStreamSize + 128
  end;
  Result := FTextStreamSize;
end;

constructor TfpgMemoStrings.Create(AMemo: TfpgMemo);
begin
  inherited Create;
  Memo := AMemo;
  FTextStream := TMemoryStream.Create;
  FIndex.Init;
end;

destructor TfpgMemoStrings.Destroy;
begin
  if Memo.FLines = Self then
    Memo.FLines := nil;
  Memo := nil;
  FTextStream.Free;
  FIndex.Destroy;
  inherited Destroy;
end;

function TfpgMemoStrings.Add(const s: String): Integer;
begin
  Insert(Count, S);
end;

procedure TfpgMemoStrings.Assign(Source: TPersistent);
begin

end;

procedure TfpgMemoStrings.Delete(Index: Integer);
var
  Offset: Integer;
  DelCount: Integer;
begin
  Offset := FIndex.OffsetFromLine(Index);
  if Offset = -1 then
    Offset := StreamSize;
    
  if Index < Count-1 then
    DelCount := FIndex.OffsetFromLine(Index+1) - Offset
  else
    DelCount := StreamSize-Offset;
    
  DeleteText(Offset, DelCount);

  if (FUpdating = False) and Assigned(Memo) and (Memo.HasHandle) then
      Memo.Refresh;
end;

procedure TfpgMemoStrings.Insert(Index: Integer; const S: string);
var
  Offset: Integer;
begin
  WriteLn('Begin Insert');
  Offset := FIndex.OffsetFromLine(Index);
  if Offset = -1 then
    Offset := StreamSize;
  //WriteLn('FTextStream Size = ', StreamSize);
  InsertText(S+LineEnding, Offset);
  WriteLn('End Insert');
  WriteLn();
  
  if (FUpdating = False) and Assigned(Memo) and (Memo.HasHandle) then
      Memo.Refresh;

end;

procedure TfpgMemoStrings.SaveToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FTextStream.Memory^,StreamSize);
end;

procedure TfpgMemoStrings.Clear;
begin
  FIndex.SetLineCount(0);
  FTextStream.SetSize(0);
  if Assigned(Memo) and (Memo.HasHandle) then
    Memo.Refresh;
end;


{ TfpgMemo }


function CreateMemo(AOwner: TComponent; x, y, w, h: TfpgCoord): TfpgMemo;
begin
  Result       := TfpgMemo.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Width := w;
  if h > 0 then
    Result.Height := h;
end;


procedure TfpgMemo.SetCursorLine(aValue: integer);
var
  i: integer;
  MaxLine: integer;
  yp: integer;
begin
  if (aValue < 1) or (aValue = FCursorLine) then
    Exit; // wrong value
  if aValue < FFirstLine then
  begin
    FFirstLine  := aValue; // moves the selected line to the top of the displayed rectangle
    FCursorLine := aValue;
    FCursorPos  := 0;
    RePaint;
    Exit;
  end;
  yp      := 2;
  MaxLine := 0;
  for i := FFirstLine to LineCount do
  begin
    yp := yp + LineHeight;
    if yp > Height then
    begin
      MaxLine := i - 1;
      break;
    end;
  end;
  if MaxLine < aValue then
  begin
    FFirstLine  := aValue;
    FCursorLine := aValue;
    FCursorPos  := 0;
    RePaint;
    Exit;
  end
  else
  begin
    FCursorLine := aValue;
    FCursorPos  := 0;
    RePaint;
    Exit;
  end;
end;

procedure TfpgMemo.UpdateScrollBarCoords;
var
  HWidth: integer;
  VHeight: integer;
begin
  VHeight := Height - 4;
  HWidth  := Width - 4;
  
  if FVScrollBar.Visible then
    Dec(HWidth, FVScrollBar.Width);
  if FHScrollBar.Visible then
    Dec(VHeight, FHScrollBar.Height);
  
  FHScrollBar.Top     := Height -FHScrollBar.Height - 2;
  FHScrollBar.Left    := 2;
  FHScrollBar.Width   := HWidth;

  FVScrollBar.Top     := 2;
  FVScrollBar.Left    := Width - FVScrollBar.Width - 2;
  FVScrollBar.Height  := VHeight;

  FVScrollBar.UpdateWindowPosition;
  FHScrollBar.UpdateWindowPosition;
end;

procedure TfpgMemo.MsgTextChanged(var msg: TfpgMessageRec);
begin
  if Assigned(FFormDesigner) then
    FFormDesigner.Dispatch(msg);
  if msg.Stop then
    Exit; //==>
  HandleTextChanged;
end;

procedure TfpgMemo.MsgTextInsert(var msg: TfpgMessageRec);
begin
  if Assigned(FFormDesigner) then
    FFormDesigner.Dispatch(msg);
  if msg.Stop then
    Exit; //==>
  with msg.Params do
    HandleTextInsert(text.Text, text.Length, text.Before, text.StartLine,text.EndLine);
end;

procedure TfpgMemo.MsgTextDelete(var msg: TfpgMessageRec);
begin
  if Assigned(FFormDesigner) then
    FFormDesigner.Dispatch(msg);
  if msg.Stop then
    Exit; //==>
  with msg.Params do
    HandleTextDelete(text.Text, text.Length, text.Before, text.StartLine, text.EndLine);
end;

constructor TfpgMemo.Create(AOwner: TComponent);
begin
  inherited;
  Focusable   := True;
  FFont       := fpgGetFont('#Edit1');
  FHeight     := FFont.Height * 3 + 4;
  FWidth      := 120;
  FLineHeight := FFont.Height + 2;
  FSelecting  := False;
  FSideMargin := 3;
  FMaxLength  := 0;
  FWrapping   := False;
  FOnChange   := nil;
  FTextColor  := Parent.TextColor;
  FBackgroundColor := clBoxColor;
  FUseTabs    := False;
  FTabWidth   := 4;

  FLines      := TfpgMemoStrings.Create(self);
  FFirstLine  := 1;
  FCursorLine := 1;

  FCursorPos    := 0;
  FSelStartPos  := FCursorPos;
  FSelEndPos    := 0;
  FSelStartLine := 0;
  FSelEndLine   := 0;

  FDrawOffset    := 0;
  FMouseDragging := False;

  FVScrollBar          := TfpgScrollBar.Create(self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := @VScrollBarMove;
  FVScrollBar.Visible  := False;

  FHScrollBar          := TfpgScrollBar.Create(self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := @HScrollBarMove;
  FHScrollBar.ScrollStep := 5;
  FHScrollBar.Visible  := False;
end;

destructor TfpgMemo.Destroy;
begin
  if Assigned(FLines) then
    TfpgMemoStrings(FLines).Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgMemo.RecalcLongestLine(AStartLine, AEndLine: Integer; OnlyCheckifBigger: Boolean);
var
  n: integer;
  lw: TfpgCoord;
begin
  if OnlyCheckIfBigger = False then
    FLongestLineWidth := 0;
  for n := AStartLine to AEndLine do
  begin
    lw := FFont.TextWidth(getlinetext(n));
    if lw > FlongestLineWidth then
      FlongestLineWidth := lw;
  end;
end;

function TfpgMemo.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgMemo.DeleteSelection;
var
  n: integer;
  selsl: integer;
  selsp: integer;
  selel: integer;
  selep: integer;
  ls: string;
  len: integer;
  st: integer;
begin
  if FSelEndLine < 1 then
    Exit;

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine;
    selsp := FSelStartPos;
    selel := FSelEndLine;
    selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine;
    selep := FSelStartPos;
    selsl := FSelEndLine;
    selsp := FSelEndPos;
  end;
  
  TfpgMemoStrings(Lines).DeleteCharsByLines(selsl-1, selel-1, selsp, selep);
  
  {//WriteLn('Start=',FSelStartPos,' End=', FSelEndPos);

  for n := selsl to selel do
  begin
    ls := GetLineText(n);

    if selsl < n then
      st  := 0
    else
      st  := selsp;
    if selel > n then
      len := UTF8Length(ls)
    else
      len := selep - st;

    UTF8Delete(ls, st + 1, len);
    
    SetLineText(n, ls);
  end;

  if selsl < selel then
  begin
    ls := GetlineText(selsl);
    ls := ls + GetLineText(selel);
    SetLineText(selsl, ls);
  end;

  for n := selsl + 1 to selel do
    FLines.Delete(selsl);}

  FCursorPos  := selsp;
  FCursorLine := selsl;
  FSelEndLine := 0;
end;

procedure TfpgMemo.DoCopy;
var
  n: integer;
  selsl: integer;
  selsp: integer;
  selel: integer;
  selep: integer;
  ls: string;
  len: integer;
  st: integer;
  s: string;
begin
  if FSelEndLine < 1 then
    Exit;

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine;
    selsp := FSelStartPos;
    selel := FSelEndLine;
    selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine;
    selep := FSelStartPos;
    selsl := FSelEndLine;
    selsp := FSelEndPos;
  end;

  s := '';

  for n := selsl to selel do
  begin
    if n > selsl then
      s := s + LineEnding;//#13#10;

    ls := GetLineText(n);

    if selsl < n then
      st := 0
    else
      st := selsp;

    if selel > n then
      len := UTF8Length(ls)
    else
      len := selep - st;

    s := s + UTF8Copy(ls, st + 1, len);
  end;

  //SetClipboardText(s);
end;

procedure TfpgMemo.DoPaste;
var
  s: string;
  si: string;
  si8: string;
  lineend: string;
  n: integer;
  l: integer;
  lcnt: integer;
begin
  Exit;
  (*
  DeleteSelection;
  s := GetClipboardText;

  si := UTF8Copy(CurrentLine,1,FCursorPos);
  lineend := UTF8Copy(CurrentLine,FCursorPos+1, UTF8Length(CurrentLine));
  l := FCursorLine;
  n := 1;
  lcnt := 0;
  si8 := '';
  while n <= length(s) do
  begin
    if (s[n] = #13) or (s[n] = #10) then
    begin
      if lcnt = 0 then SetLineText(l, si + si8)
                  else FLines.Insert(l-1, si + si8);

      si := '';
      si8 := '';
      inc(lcnt);
      inc(l);

      // skip multibyte line end:
      if (s[n]=#13) and (n < length(s)) and (s[n+1]=#10) then inc(n);
    end
    else
    begin
      si8 := si8 + s[n];
    end;
    inc(n);
  end;

  si := si + si8;

  FCursorPos := UTF8Length(si);
  si := si + lineend;

  if lcnt = 0 then
  begin
    SetLineText(l, si)
  end
  else
  begin
    FLines.Insert(l-1, si);
    FCursorLine := l;
  end;

  AdjustCursor;
  Repaint;
*)
end;

procedure TfpgMemo.AdjustCursor;
var
  tw: integer;
begin
  // horizontal adjust
  //RecalcLongestLine(1, LineCount, True);
  tw := FFont.TextWidth(UTF8Copy(CurrentLine, 1, FCursorPos));

  if tw - FDrawOffset > VisibleWidth - 2 then
    FDrawOffset := tw - VisibleWidth + 2
  else if tw - FDrawOffset < 0 then
  begin
    FDrawOffset := tw;
    if tw <> 0 then
      Dec(FDrawOffset, 2);
  end;

  // vertical adjust
  if FCursorLine < FFirstLine then
    FFirstLine := FCursorLine;
  if FCursorline - FFirstLine + 1 > VisibleLines then
    FFirstLine := FCursorline - VisibleLines + 1;

  if FFirstLine + VisibleLines > LineCount then
  begin
    FFirstLine := LineCount - VisibleLines + 1;
    if FFirstline < 1 then
      FFirstLine := 1;
  end;

  UpdateScrollbars;
end;

procedure TfpgMemo.UpdateScrollBars;
var
  vlines: integer;
  vsbw: integer;
  hsbwas: boolean;
  vsbwas: boolean;
  vsbvis: boolean;
begin
  hsbwas := FHScrollBar.Visible;
  vsbwas := FVScrollBar.Visible;
  vlines := (Height - (FSideMargin shl 1)) div Lineheight;
  vsbvis := (LineCount > vlines);

  if vsbvis then
    vsbw := FVScrollBar.Width
  else
    vsbw := 0;

  FHScrollBar.Visible := FLongestLineWidth > (Width - vsbw - FSideMargin * 2) - 1;

  if FHScrollBar.Visible and not vsbvis then
  begin
    // recheck vertical scrollbar
    vlines := (Height - (FSideMargin shl 1) - FHScrollBar.Height) div Lineheight;
    vsbvis := (LineCount > vlines);
  end;

  FVScrollBar.Visible := vsbvis;

  UpdateScrollBarCoords;

  if FHScrollBar.Visible then
  begin
    FHScrollBar.Min := 0;
    FHScrollBar.Max := FLongestLineWidth - VisibleWidth - 1;
    if (FLongestLineWidth <= 0) or (FLongestLineWidth <= VisibleWidth) then
      FHScrollBar.SliderSize := 1
    else
      FHScrollBar.SliderSize := VisibleWidth / FLongestLineWidth;
    FHScrollBar.Position := FDrawOffset;
    FHScrollBar.RepaintSlider;
  end;

  if FVScrollBar.Visible then
  begin
    FVScrollBar.Min        := 1;
    // TODO: Look at calculation of vlines value to improve this!
    if LineCount > 0 then
    begin
      FVScrollBar.SliderSize := VisibleLines / LineCount;
      FVScrollBar.Max        := LineCount - VisibleLines + 1;
    end
    else
    begin
      FVScrollBar.SliderSize := 0.5;
      FVScrollBar.Max        := 10;
    end;
    FVScrollBar.Position   := FFirstLine;
    FVScrollBar.RepaintSlider;
  end;

  if (hsbwas <> FHScrollBar.Visible) or (vsbwas <> FVScrollBar.Visible) then
    AdjustCursor;
end;

procedure TfpgMemo.Refresh;
begin
  UpdateScrollBars;
  Invalidate;
end;

function TfpgMemo.LineCount: integer;
begin
  Result := FLines.Count;
end;

function TfpgMemo.GetLineText(linenum: integer): string;
begin
  if LineCount < 1 then
    FLines.Add('');
  if (linenum >= 1) and (linenum <= LineCount) then
    Result := FLines.Strings[linenum - 1]
  else
    Result := '';
end;

procedure TfpgMemo.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TfpgMemo.SetLineText(linenum: integer; Value: string);
begin
  FLines.Strings[linenum - 1] := Value;
end;

function TfpgMemo.GetCursorX: integer;
begin
  Result := FFont.TextWidth(copy(CurrentLine, 1, FCursorPos));
end;

// Set cursor position by X
procedure TfpgMemo.SetCPByX(x: integer);
var
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  ls: string;
begin
  // searching the appropriate character position
  ls  := CurrentLine;
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)); // + FDrawOffset + FSideMargin;
  cp  := FCursorPos;
  if cp > UTF8Length(ls) then
    cp := UTF8Length(ls);

  for n := 0 to UTF8Length(ls) do
  begin
    cx := FFont.TextWidth(UTF8Copy(ls, 1, n)); // + FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

  FCursorPos := cp;
end;

function TfpgMemo.CurrentLine: string;
begin
  Result := GetLineText(FCursorLine);
end;

function TfpgMemo.VisibleLines: integer;
var
  sh: integer;
begin
  if FHScrollBar.Visible then
    sh := 18
  else
    sh := 0;
  Result := (Height - (FSideMargin shl 1) - sh) div Lineheight;
end;

function TfpgMemo.VisibleWidth: integer;
var
  sw: integer;
begin
  if FVScrollBar.Visible then
    sw := FVScrollBar.Width
  else
    sw := 0;
  Result := (Width - (FSideMargin shl 1) - sw);
end;

procedure TfpgMemo.HandleShow;
begin
  inherited HandleShow;
  if (csLoading in ComponentState) then
    Exit;
  RecalcLongestLine(1, LineCount, True);
  UpdateScrollBars;
  UpdateScrollBarCoords;
end;

procedure TfpgMemo.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  MouseCursor := mcIBeam;
end;

procedure TfpgMemo.HandleMouseExit;
begin
  inherited HandleMouseExit;
  MouseCursor := mcDefault;
end;

procedure TfpgMemo.VScrollBarMove(Sender: TObject; position: integer);
begin
  if FFirstLine <> position then
  begin
    FFirstLine := position;
    repaint;
  end;
end;

procedure TfpgMemo.HScrollBarMove(Sender: TObject; position: integer);
begin
  if position <> FDrawOffset then
  begin
    FDrawOffset := position;
    Repaint;
  end;
end;

procedure TfpgMemo.HandlePaint;
var
  n: integer;
  tw, tw2, st, len: integer;
  yp, xp: integer;
  ls: string;
  r: TfpgRect;
  selsl, selsp, selel, selep: integer;
  c: integer;
  s: string;
begin
  Canvas.BeginDraw;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);
  Canvas.FillRectAngle(r);

  Canvas.SetTextColor(FTextColor);
  Canvas.SetFont(FFont);

  if (FSelStartLine shl 16) + FSelStartPos <= (FSelEndLine shl 16) + FSelEndPos then
  begin
    selsl := FSelStartLine;
    selsp := FSelStartPos;
    selel := FSelEndLine;
    selep := FSelEndPos;
  end
  else
  begin
    selel := FSelStartLine;
    selep := FSelStartPos;
    selsl := FSelEndLine;
    selsp := FSelEndPos;
  end;

  yp := 3;
  for n := FFirstline to LineCount do
  begin
    ls := GetLineText(n);
    if FUseTabs then
    begin
      xp := 0;
      s := '';
      for c := 1 to Length(ls) do
      begin
        if ls[c] = #9 then
        begin
          if s <> '' then
            Canvas.DrawString(-FDrawOffset + FSideMargin + xp, yp, s);
          xp := xp + Canvas.Font.TextWidth(' ') * FTabWidth;
          s := '';
        end
        else
          s := s + ls[c];
      end;
      if s <> '' then
        Canvas.DrawString(-FDrawOffset + FSideMargin + xp, yp, s);
    end
    else
      Canvas.DrawString(-FDrawOffset + FSideMargin, yp, ls);

    if Focused then
    begin
      // drawing selection
      if (FSelEndLine > 0) and (selsl <= n) and (selel >= n) then
      begin
        if selsl < n then
          st  := 0
        else
          st  := selsp;
        if selel > n then
          len := UTF8Length(ls)
        else
          len := selep - st;

        tw  := FFont.TextWidth(UTF8Copy(ls, 1, st));
        tw2 := FFont.TextWidth(UTF8Copy(ls, 1, st + len));
        Canvas.XORFillRectangle(fpgColorToRGB(clSelection) xor $FFFFFF, -FDrawOffset +
          FSideMargin + tw, yp, tw2 - tw, LineHeight);
      end;

      //drawing cursor
      if FCursorLine = n then
      begin
        // drawing cursor
        tw := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos));
        fpgCaret.SetCaret(Canvas, -FDrawOffset + FSideMargin + tw, yp, fpgCaret.Width, FFont.Height);
      end;
    end;  { if }

    yp := yp + LineHeight;
    if yp > Height then
      Break;
  end;  { for }

  if not Focused then
    fpgCaret.UnSetCaret(Canvas);
    
  // The little square in the bottom right corner
  if FHScrollBar.Visible and FVScrollBar.Visible then
  begin
    Canvas.SetColor(clButtonFace);
    Canvas.FillRectangle(FHScrollBar.Left+FHScrollBar.Width,
                         FVScrollBar.Top+FVScrollBar.Height,
                         FVScrollBar.Width,
                         FHScrollBar.Height);
  end;

  Canvas.EndDraw;
end;

procedure TfpgMemo.HandleKeyChar(var AText: TfpgChar; var shiftstate: TShiftState; var consumed: boolean);
var
  s: string;
  Strings: TfpgMemoStrings;
begin
  inherited;
  s        := AText;
  
  Strings := TfpgMemoStrings(FLines);

  // Printable characters only
  // Note: This is now UTF-8 compliant!
  if (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) or (Length(AText) > 1) then
  begin
    DeleteSelection;
    Strings.InsertCharsByLine(s, FCursorLine-1, FCursorPos);

    Inc(FCursorPos);
    FSelStartPos  := FCursorPos;
    FSelStartLine := FCursorLine;
    FSelEndLine   := 0;
    AdjustCursor;

    consumed := True;
  end;

  if consumed then
    RePaint;
end;

procedure TfpgMemo.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  cx: integer;
  ls: string;
  ls2: string;
  hasChanged: boolean;
  
  procedure StopSelection;
  begin
    FSelStartLine := FCursorLine;
    FSelStartPos  := FCursorPos;
    FSelEndLine   := 0;
  end;
  
begin
  Lines.BeginUpdate;
  Consumed := True;
  hasChanged := False;
  case CheckClipBoardKey(keycode, shiftstate) of
    ckCopy:
        begin
          DoCopy;
        end;
    ckPaste:
        begin
          DoPaste;
          hasChanged := True;
        end;
    ckCut:
        begin
          DoCopy;
          DeleteSelection;
          hasChanged := True;
        end;
  else
    Consumed := False;
  end;

  if not Consumed then
  begin
    // checking for movement keys:
    consumed   := True;
    FSelecting := (ssShift in shiftstate);

    case keycode of
      keyLeft:
        begin
          if FCursorPos > 0 then
            Dec(FCursorPos)
          else if FCursorLine > 1 then
          begin
            Dec(FCursorLine);
            FCursorPos := UTF8Length(CurrentLine);
          end;

          if (ssCtrl in shiftstate) then
            // word search...
            (*
                    while (FCursorPos > 0) and not pgfIsAlphaNum(copy(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);

                    while (FCursorPos > 0) and pgfIsAlphaNum(copy(CurrentLine,FCursorPos,1))
                      do Dec(FCursorPos);
            *);

        end;// left

      keyRight:
        begin
          if FCursorPos < UTF8Length(CurrentLine) then
            Inc(FCursorPos)
          else if FCursorLine < Lines.Count then
          begin
            Inc(FCursorLine);
            FCursorPos := 0;
          end;

          if (ssCtrl in shiftstate) then
            // word search...
            (*
                    while (FCursorPos < length(CurrentLine)) and pgfIsAlphaNum(copy(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);

                    while (FCursorPos < length(CurrentLine)) and not pgfIsAlphaNum(copy(CurrentLine,FCursorPos+1,1))
                      do Inc(FCursorPos);
              *);

        end;// right

      keyUp:
      begin  // up
        cx := GetCursorX;
        if FCursorLine > 1 then
        begin
          Dec(FCursorline);
          SetCPByX(cx);
        end;
      end;

      keyDown:
      begin
        cx := GetCursorX;
        if FCursorLine < LineCount then
        begin
          Inc(FCursorline);
          SetCPByX(cx);
        end;
      end;

      keyHome:
      begin
        if (ssCtrl in shiftstate) then
          FCursorLine := 1;
        FCursorPos := 0;
      end;

      keyEnd:
      begin
        if (ssCtrl in shiftstate) then
          FCursorLine := LineCount;
        FCursorPos := UTF8Length(CurrentLine);
      end;

      keyPageUp:
        if FCursorLine > 1 then
        begin
          cx := GetCursorX;
          Dec(FCursorLine, VisibleLines);
          if FCursorLine < 1 then
            FCursorLine := 1;
          SetCPByX(cx);
        end;

      keyPageDown:
      begin
        cx := GetCursorX;
        if FCursorLine < LineCount then
        begin
          Inc(FCursorline, VisibleLines);
          if FCursorLine > LineCount then
            FCursorLine := LineCount;
          SetCPByX(cx);
        end;
      end;

      else
        Consumed := False;
    end;

    if Consumed then
    begin
      AdjustCursor;

      if FSelecting then
      begin
        FSelEndPos  := FCursorPos;
        FSelEndLine := FCursorLine;
      end
      else
        StopSelection;
    end;
  end;

  if not Consumed then
  begin
    consumed := True;

    case keycode of
      keyReturn:
          begin
            TfpgMemoStrings(Lines).InsertCharsByLine(LineEnding, FCursorLine-1, FCursorPos);
            Inc(FCursorLine);
            FCursorPos := 0;
            hasChanged := True;
          end;
      keyBackSpace:
          begin
            if FSelEndLine > 0 then
              DeleteSelection
            else if FCursorPos > 0 then
            begin
              TfpgMemoStrings(Lines).DeleteCharsByLines
                (FCursorLine-1, FCursorLine-1, FCursorPos-1 , FCursorPos);
              Dec(FCursorPos);
            end
            else if FCursorLine > 1 then
            begin
              Dec(FCursorLine);
              FCursorPos := UTF8Length(FLines.Strings[FCursorLine - 1]);
              TfpgMemoStrings(Lines).DeleteCharsByLines
                (FCursorLine-1, FCursorLine, FCursorPos , 0);

            end;
            hasChanged := True;
          end;

      keyDelete:
          begin
            ls := GetLineText(FCursorLine);
            if FSelEndLine > 0 then
              DeleteSelection
            else if FCursorPos < UTF8Length(ls) then
            begin
              //UTF8Delete(ls, FCursorPos + 1, 1);
              //SetLineText(FCursorLine, ls);
              TfpgMemoStrings(Lines).DeleteCharsByLines
                (FCursorLine-1, FCursorLine-1, FCursorPos , FCursorPos+1);
            end
            else if FCursorLine < LineCount then
            begin
              TfpgMemoStrings(Lines).DeleteCharsByLines
                (FCursorLine-1, FCursorLine, FCursorPos, 0);
              {ls2 := FLines.Strings[FCursorLine];
              FLines.Delete(FCursorLine);
              FLines.Strings[FCursorLine - 1] := ls + ls2;}
            end;
            hasChanged := True;
          end;
      keyTab:
          begin
            if FUseTabs then
            begin
              ls := GetLineText(FCursorLine);
{              if FSelEndLine > 0 then
                DeleteSelection
              else} if FCursorPos < UTF8Length(ls) then
              begin
                UTF8Insert(#9, ls, FCursorPos);
                SetLineText(FCursorLine, ls);
              end;
{
              else if FCursorLine < LineCount then
              begin
                ls2 := FLines.Strings[FCursorLine];
                FLines.Delete(FCursorLine);
                FLines.Strings[FCursorLine - 1] := ls + ls2;
              end;
}
              hasChanged := True;
            end
            else
              Consumed := False;
          end;
      else
        Consumed := False;
    end;

    if Consumed then
    begin
      StopSelection;
      AdjustCursor;
    end;
  end;

  if Consumed then
    RePaint
  else
    inherited;
    
  if hasChanged then
    if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TfpgMemo.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  inherited HandleKeyRelease(keycode, shiftstate, consumed);
  Lines.EndUpdate;
end;

procedure TfpgMemo.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  lnum: integer;
  ls: string;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  // searching the appropriate character position
  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > LineCount then
    lnum := LineCount;

  ls  := GetLineText(lnum);
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp  := FCursorPos;

  for n := 0 to UTF8Length(ls) do
  begin
    cx := FFont.TextWidth(UTF8Copy(ls, 1, n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

  FMouseDragging := True;
  FMouseDragPos  := cp;
  FCursorPos     := cp;
  FCursorLine    := lnum;

  if (ssShift in shiftstate) then
  begin
    FSelEndLine := lnum;
    FSelEndpos  := cp;
  end
  else
  begin
    FSelStartLine := lnum;
    FSelStartPos  := cp;
    FSelEndLine   := 0;
  end;
  Repaint;
end;

procedure TfpgMemo.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  n: integer;
  cpx: integer;
  cp: integer;
  cx: integer;
  lnum: integer;
  ls: string;
begin
  if not FMouseDragging or ((btnstate and 1) = 0) then
  begin
    FMouseDragging := False;
    Exit;
  end;

  // searching the appropriate character position
  lnum := FFirstLine + (y - FSideMargin) div LineHeight;
  if lnum > LineCount then
    lnum := LineCount;

  ls  := GetLineText(lnum);
  cpx := FFont.TextWidth(UTF8Copy(ls, 1, FCursorPos)) - FDrawOffset + FSideMargin;
  cp  := FCursorPos;

  for n := 0 to UTF8Length(ls) do
  begin
    cx := FFont.TextWidth(UTF8Copy(ls, 1, n)) - FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp  := n;
    end;
  end;

  if (cp <> FCursorPos) or (lnum <> FCursorLine) then
  begin
    FCursorLine := lnum;
    FSelEndLine := lnum;
    FSelEndPos  := cp;
    FCursorPos  := cp;
    Repaint;
  end;


  // searching the appropriate character position
  {
  cpx := FFont.TextWidth16(copy16(FText,1,FCursorPos)) + FDrawOffset + FSideMargin;
  cp := FCursorPos;

  s := '';

  for n := 0 to Length16(Text) do
  begin
    cx := FFont.TextWidth16(copy16(Text,1,n)) + FDrawOffset + FSideMargin;
    if abs(cx - x) < abs(cpx - x) then
    begin
      cpx := cx;
      cp := n;
    end;
  end;

  //FMouseDragPos := cp;
  FSelOffset := cp-FSelStart;
  if FCursorPos <> cp then
  begin
    FCursorPos := cp;
    Repaint;
  end;
}
end;

(*
procedure TfpgMemo.HandleWindowScroll(direction, amount: integer);
var
  pfl, pdo : integer;
begin
  inherited HandleWindowScroll(direction, amount);

  pfl := FFirstLine;
  pdo := FDrawOffset;

  if direction = 0 then
  begin
    dec(FFirstLine, amount);
  end;
  if direction = 1 then
  begin
    inc(FFirstLine, amount);
  end;
  if FFirstLine > LineCount - VisibleLines + 1 then FFirstLine := LineCount - VisibleLines + 1;
  if FFirstLine < 1 then FFirstLine := 1;

  if FHScrollBar.Visible then
  begin
    if Direction = 2 then
    begin
      dec(FDrawOffset, amount*16);
    end;
    if Direction = 3 then
    begin
      inc(FDrawOffset, amount*16);
    end;

    if FDrawOffset > FHScrollBar.Max then FDrawOffset := FHScrollBar.Max;
    if FDrawOffset < 0 then FDrawOffset := 0;
  end;

  if (pfl <> FFirstLine) or (pdo <> FDrawOffset) then
  begin
    UpdateScrollBars;
    Repaint;
  end;

end;
*)

procedure TfpgMemo.HandleResize(dwidth, dheight: integer);
begin
  inherited HandleResize(dwidth, dheight);
  if (csLoading in ComponentState) then
    Exit;
  UpdateScrollBarCoords;
  UpdateScrollBars;
end;

procedure TfpgMemo.HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
  delta: smallint);
var
  pfl, pdo : integer;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);

  pfl := FFirstLine;
  pdo := FDrawOffset;

  if delta < 0 then
    dec(FFirstLine, abs(delta))   // scroll up
  else
    inc(FFirstLine, abs(delta));  // scroll down

  if FFirstLine > LineCount - VisibleLines + 1 then
    FFirstLine := LineCount - VisibleLines + 1;
  if FFirstLine < 1 then
    FFirstLine := 1;

  if FHScrollBar.Visible then
  begin
    if FDrawOffset > FHScrollBar.Max then
      FDrawOffset := FHScrollBar.Max;
    if FDrawOffset < 0 then
      FDrawOffset := 0;
  end;

  if (pfl <> FFirstLine) or (pdo <> FDrawOffset) then
  begin
    UpdateScrollBars;
    Repaint;
  end;
end;

procedure TfpgMemo.HandleTextChanged;
begin
  if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TfpgMemo.HandleTextInsert(AText: PChar; ALength: Integer; BeforeEvent: Boolean; AStartLine, AEndLine: Integer);
begin
  if Not BeforeEvent then
    RecalcLongestLine(AStartLine, AEndLine, False);
end;

procedure TfpgMemo.HandleTextDelete(AText: PChar; ALength: Integer; BeforeEvent: Boolean; AStartLine, AEndLine: Integer);
begin
  if Not BeforeEvent then
    RecalcLongestLine(AStartLine, AEndLine, False);
end;

function TfpgMemo.SelectionText: string;
begin
  {
  if FSelOffset <> 0 then
  begin
    if FSelOffset < 0 then
    begin
      Result := Copy(FText,1+FSelStart + FSelOffset,-FSelOffset);
    end
    else
    begin
      result := Copy(FText,1+FSelStart,FSelOffset);
    end;
  end
  else
    Result := '';
}
end;

function TfpgMemo.GetText: string;
var
  n: integer;
  s: string;
begin
  {s := '';
  for n := 1 to LineCount do
  begin
    if n > 1 then
      s := s + LineEnding;//#13#10;
    s := s + GetLineText(n);
  end;
  Result := s;}
  Result := Lines.Text;
end;

procedure TfpgMemo.SetText(const AValue: string);
var
  n: integer;
  c: string[2];
  s: string;
begin
  FLines.Clear;
  {s := '';
  n := 1;
  while n <= UTF8Length(AValue) do
  begin
    c := UTF8Copy(AValue, n, 1);
    if (c[1] = #13) or (c[1] = #10) then
    begin
      FLines.Add(s);
      s := '';
      c := UTF8Copy(AValue, n + 1, 1);
      if c[1] = #10 then
        Inc(n);
    end
    else
      s := s + c;
    Inc(n);
  end;

  if s <> '' then
    FLines.Add(s);   }
  TfpgMemoStrings(FLines).SetTextStr(AValue);

  FDrawOffset   := 0;
  FCursorPos    := 0;
  FCursorLine   := 1;
  FSelStartLine := FCursorLine;
  FSelStartPos  := FCursorPos;
  FSelEndLine   := 0;

  AdjustCursor;
  Repaint;
end;

end.

