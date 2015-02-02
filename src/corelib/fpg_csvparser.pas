{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2014 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Uses a Finite State Machine to parse CSV files.
      Graeme Geldenhuys <graemeg@gmail.com>

      This unit shows how one could use the State Design Pattern to implement a
      FSM (Finite State Machine) to create a CSV Parser.  It handles invalid
      CSV as well and will raise an appropriate exception. In the State pattern,
      each of the states becomes a subclass of the base class. Each subclass must
      implement the abstract method which will handle the input character and
      decide on the next state.
}

unit fpg_CSVParser;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  { forward declarations }
  TCSVParser        = class;
  TParserStateClass = class of TCSVParserState;


  { Abstract State object }
  TCSVParserState = class(TObject)
  private
    FParser: TCSVParser;
    procedure ChangeState(NewState: TParserStateClass);
    procedure AddCharToCurrField(Ch: char);
    procedure AddCurrFieldToList;
  public
    constructor Create(AParser: TCSVParser);
    { Must be implemented in the concrete classes to handle the input character
      and decide on the next state. }
    procedure ProcessChar(Ch: AnsiChar; Pos: integer); virtual; abstract;
  end;


  { A concrete state object - used when starting a new field }
  TCSVParserFieldStartState = class(TCSVParserState)
  public
    procedure ProcessChar(Ch: AnsiChar; Pos: integer); override;
  end;


  { A concrete state object - used while scanning a field }
  TCSVParserScanFieldState = class(TCSVParserState)
  public
    procedure ProcessChar(Ch: AnsiChar; Pos: integer); override;
  end;


  { A concrete state object - used while scanning double quoted fields }
  TCSVParserScanQuotedState = class(TCSVParserState)
  public
    procedure ProcessChar(Ch: AnsiChar; Pos: integer); override;
  end;


  { A concrete state object - used when found the ending double quote }
  TCSVParserEndQuotedState = class(TCSVParserState)
  public
    procedure ProcessChar(Ch: AnsiChar; Pos: integer); override;
  end;


  { A concrete state object - some error occured / invalid CSV structure }
  TCSVParserGotErrorState = class(TCSVParserState)
  public
    procedure ProcessChar(Ch: AnsiChar; Pos: integer); override;
  end;


  { The actual state machine - CSV parser }
  TCSVParser = class(TObject)
  private
    FCurrentLine: string;
    FState: TCSVParserState;
    { Cache state objects for greater performance. This comes in handy when
      parsing a large CSV file. For smaller files you might want to create them
      on the fly. }
    FFieldStartState: TCSVParserFieldStartState;
    FScanFieldState: TCSVParserScanFieldState;
    FScanQuotedState: TCSVParserScanQuotedState;
    FEndQuotedState: TCSVParserEndQuotedState;
    FGotErrorState: TCSVParserGotErrorState;
    { Fields used during parsing }
    FCurrField: string;
    FFieldList: TStrings;
    function GetState: TParserStateClass;
    procedure SetState(const Value: TParserStateClass);
  protected
    procedure AddCharToCurrField(Ch: char);
    procedure AddCurrFieldToList;
    { An example of Self Encapsulating Field refactoring }
    property State: TParserStateClass read GetState write SetState;
  public
    constructor Create;
    destructor Destroy; override;
    { prodecure to call, to start the parsing process }
    procedure ExtractFields(const S: string; const pFieldList: TStrings);
    property CurrentLine: string read FCurrentLine;
  end;


// global singleton function
function gCSVParser: TCSVParser;


implementation

uses
  SysUtils;

var
  uCSVParser: TCSVParser;


// Lazy mans singleton
function gCSVParser: TCSVParser;
begin
  if uCSVParser = nil then
    uCSVParser := TCSVParser.Create;
  Result := uCSVParser;
end;

{ TCSVParser }

constructor TCSVParser.Create;
begin
  inherited Create;
  FCurrentLine     := '';
  FFieldStartState := TCSVParserFieldStartState.Create(Self);
  FScanFieldState  := TCSVParserScanFieldState.Create(Self);
  FScanQuotedState := TCSVParserScanQuotedState.Create(Self);
  FEndQuotedState  := TCSVParserEndQuotedState.Create(Self);
  FGotErrorState   := TCSVParserGotErrorState.Create(Self);
end;

destructor TCSVParser.Destroy;
begin
  FFieldStartState.Free;
  FScanFieldState.Free;
  FScanQuotedState.Free;
  FEndQuotedState.Free;
  FGotErrorState.Free;
  inherited;
end;

function TCSVParser.GetState: TParserStateClass;
begin
  Result := TParserStateClass(FState.ClassType);
end;

procedure TCSVParser.SetState(const Value: TParserStateClass);
begin
  if Value = TCSVParserFieldStartState then
    FState := FFieldStartState
  else if Value = TCSVParserScanFieldState then
    FState := FScanFieldState
  else if Value = TCSVParserScanQuotedState then
    FState := FScanQuotedState
  else if Value = TCSVParserEndQuotedState then
    FState := FEndQuotedState
  else if Value = TCSVParserGotErrorState then
    FState := FGotErrorState;
end;

procedure TCSVParser.ExtractFields(const S: string; const pFieldList: TStrings);
var
  i: integer;
  Ch: AnsiChar;
begin
  FCurrentLine := S;
  FFieldList   := pFieldList;
  Assert(Assigned(FFieldList), 'FieldList not assigned');
  { Initialize by clearing the string list, and starting in FieldStart state }
  FFieldList.Clear;
  State        := TCSVParserFieldStartState;
  FCurrField   := '';

  { Read through all the characters in the string }
  for i := 1 to Length(s) do
  begin
    { Get the next character }
    Ch := s[i];
    FState.ProcessChar(Ch, i);
  end;

  { If we are in the ScanQuoted or GotError state at the end of the string, 
    there was a problem with a closing quote. You can add the second if test
    for an extra failsafe!  }
  if (State = TCSVParserScanQuotedState) then
    //      or (State = TCSVParserGotErrorState) then
    raise Exception.Create('Missing closing quote');

  { If the current field is not empty, add it to the list }
  if (FCurrField <> '') then
    AddCurrFieldToList;
end;

procedure TCSVParser.AddCharToCurrField(Ch: char);
begin
  FCurrField := FCurrField + Ch;
end;

procedure TCSVParser.AddCurrFieldToList;
begin
  FFieldList.Add(FCurrField);
  // Clear the field in preparation for collecting the next one
  FCurrField := '';
end;

{ TCSVParserState }

constructor TCSVParserState.Create(AParser: TCSVParser);
begin
  inherited Create;
  FParser := AParser;
end;

procedure TCSVParserState.ChangeState(NewState: TParserStateClass);
begin
  FParser.State := NewState;
end;

procedure TCSVParserState.AddCharToCurrField(Ch: char);
begin
  FParser.AddCharToCurrField(Ch);
end;

procedure TCSVParserState.AddCurrFieldToList;
begin
  FParser.AddCurrFieldToList;
end;

{ TCSVParserFieldStartState }

procedure TCSVParserFieldStartState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  case Ch of
    '"': ChangeState(TCSVParserScanQuotedState);
    ',': AddCurrFieldToList;
    else
      AddCharToCurrField(Ch);
      ChangeState(TCSVParserScanFieldState);
  end;
end;

{ TCSVParserScanFieldState }

procedure TCSVParserScanFieldState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  if (Ch = ',') then
  begin
    AddCurrFieldToList;
    ChangeState(TCSVParserFieldStartState);
  end
  else
    AddCharToCurrField(Ch);
end;

{ TCSVParserScanQuotedState }

procedure TCSVParserScanQuotedState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  if (Ch = '"') then
    ChangeState(TCSVParserEndQuotedState)
  else
    AddCharToCurrField(Ch);
end;

{ TCSVParserEndQuotedState }

procedure TCSVParserEndQuotedState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  if (Ch = ',') then
  begin
    AddCurrFieldToList;
    ChangeState(TCSVParserFieldStartState);
  end
  else
    ChangeState(TCSVParserGotErrorState);
end;

{ TCSVParserGotErrorState }

procedure TCSVParserGotErrorState.ProcessChar(Ch: AnsiChar; Pos: integer);
begin
  raise Exception.Create(Format('Error in line at position %d: ' + #10 +
    '<%s>', [Pos, FParser.CurrentLine]));
end;


initialization
  uCSVParser := nil;

finalization
  if uCSVParser <> nil then
    uCSVParser.Free;

end.

