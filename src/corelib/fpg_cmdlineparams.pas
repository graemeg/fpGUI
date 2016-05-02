{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Unit to handle command line processing
}

{ TODO : Make sure Unicode parameter handling is supported. }

unit fpg_cmdlineparams;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes;

const
  ctiCommandLineParamPrefix = '-';

type

  TfpgCommandLineParams = class(TObject)
  private
    FsParams: string;
    FslParams: TStringList;
    procedure   ReadParams;
    function    WordExtract(const AInput: string; const APos: integer; const ADelims: string): string;
    function    WordCount(const AStrToProcess: string; ADelims: string): integer;
    function    WordPosition(const AN: integer; const AStr: string; ADelims: string): integer;
    function    ExtractChar(const AValue: string; const APos: integer): char;
    function    CharInStr(const AChr: char; const AStr: string): Boolean;
    function    StripLeadingDelims(const AStrToProcess: string; ADelims: string): string;
    function    StripTrailingDelims(const AStrToProcess: string; ADelims: string): string;
    function    NumToken(const AValue, AToken: string): integer;
    function    Token(const AValue, AToken: string; const APos: integer): string;
    function    StrTran(AValue, ADel, AIns: string): string;
  public
    constructor Create;
    destructor  Destroy; override;
    function    IsParam(const AParam: string): Boolean; overload;
    function    IsParam(const AParams: array of string): Boolean; overload;
    function    GetParam(const AParam: string): string;
    property    Params: TStringList read FslParams;
    property    AsString: string read FsParams;
  end deprecated 'Use fpgApplication''s ICmdLineParams interface instead';


  ICmdLineParams = interface
    ['{EDF51E67-1119-11E6-B131-C86000E37EB0}']
    { main interaction functions }
    function    FindOptionIndex(const S: string; var Longopt: Boolean): integer;
    function    GetOptionValue(const S: string): string;
    function    GetOptionValue(const C: char; const S: string): string;
    function    HasOption(const S: string): Boolean;
    function    HasOption(const C: char; const S: string): Boolean;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: array of string; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: array of string; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: string; AllErrors: Boolean = False): string;
    { property getters and setters }
    function    GetCaseSensitiveOptions: Boolean;
    function    GetOptionChar: char;
    procedure   SetCaseSensitiveOptions(AValue: Boolean);
    procedure   SetOptionChar(AValue: char);
    function    GetParams(AIndex: integer): string;
    function    GetParamCount: integer;
    { our properties }
    property    OptionChar: char read GetOptionChar write SetOptionChar;
    property    CaseSensitiveOptions: Boolean read GetCaseSensitiveOptions write SetCaseSensitiveOptions;
    property    Params[Index: integer]: string read GetParams;
    property    ParamCount: integer read GetParamCount;
  end;


  TfpgCmdLineParams = class(TObject, ICmdLineParams)
  private
    FOptionChar: char;
    FCaseSensitiveOptions: Boolean;
    { main interaction functions }
    function    FindOptionIndex(const S: string; var Longopt: Boolean): integer;
    function    GetOptionValue(const S: string): string;
    function    GetOptionValue(const C: char; const S: string): string;
    function    HasOption(const S: string): Boolean;
    function    HasOption(const C: char; const S: string): Boolean;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: array of string; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: array of string; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: string; AllErrors: Boolean = False): string;
    { property getters and setters }
    function    GetCaseSensitiveOptions: Boolean;
    function    GetOptionChar: char;
    procedure   SetCaseSensitiveOptions(AValue: Boolean);
    procedure   SetOptionChar(AValue: char);
    function    GetParams(AIndex: integer): string;
    function    GetParamCount: integer;
    { our properties }
    property    OptionChar: char read GetOptionChar write SetOptionChar;
    property    CaseSensitiveOptions: Boolean read GetCaseSensitiveOptions write SetCaseSensitiveOptions;
    property    Params[Index: integer]: string read GetParams;
    property    ParamCount: integer read GetParamCount;
  public
    constructor Create;
  end;


// Singleton
function gCommandLineParams: TfpgCommandLineParams; deprecated 'Use fpgApplication''s ICmdLineParams interface instead';


implementation

uses
  SysUtils,
  fpg_constants;

var
  uCommandLineParams: TfpgCommandLineParams;

// Singleton
function gCommandLineParams: TfpgCommandLineParams;
begin
  if uCommandLineParams = nil then
    uCommandLineParams := TfpgCommandLineParams.Create;
  Result := uCommandLineParams;
end;

{ TfpgCommandLineParams }

constructor TfpgCommandLineParams.Create;
begin
  inherited;
  FslParams := TStringList.Create;
  ReadParams;
end;

destructor TfpgCommandLineParams.Destroy;
begin
  FslParams.Free;
  inherited;
end;

function TfpgCommandLineParams.GetParam(const AParam: string): string;
begin
  Result := FslParams.Values[upperCase(AParam)];
end;

function TfpgCommandLineParams.IsParam(const AParam: string): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FslParams.Count - 1 do
    if FslParams.Names[i] = upperCase(AParam) then
    begin
      Result := True;
      break; //==>
    end;
end;

function TfpgCommandLineParams.IsParam(const AParams: array of string): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(AParams) to High(AParams) do
    if IsParam(AParams[i]) then
    begin
      Result := True;
      Exit; //==>
    end;
end;

procedure TfpgCommandLineParams.ReadParams;
var
  i: integer;
  j: integer;
  lsNameValue: string;
  lsValue: string;
  lsName: string;
const
  cDelim = ' ';
begin
  lsValue  := '';
  FsParams := '';
  j        := ParamCount;
  for i := 1 to j do
  begin
    if FsParams <> '' then
      FsParams := FsParams + cDelim;
    FsParams := FsParams + ParamStr(i);
  end;

  j := WordCount(FsParams, ctiCommandLineParamPrefix);
  for i := 1 to j do
  begin
    lsNameValue := WordExtract(FsParams, i, ctiCommandLineParamPrefix);
    lsName      := Token(lsNameValue, cDelim, 1);
    lsValue     := copy(lsNameValue, Length(lsName) + 1,
      Length(FsParams) - Length(lsValue));

    lsValue := Trim(lsValue);
    lsName  := StrTran(lsName, ctiCommandLineParamPrefix, '');
    lsName  := upperCase(lsName);

    FslParams.Add(lsName + '=' + lsValue);
  end;
end;

function TfpgCommandLineParams.StrTran(AValue, ADel, AIns: string): string;
var
  i: integer;
  sToChange: string;
begin
  Result    := '';
  sToChange := AValue;
  i         := pos(ADel, sToChange);
  while i <> 0 do
  begin
    Result := Result + copy(sToChange, 1, i - 1) + AIns;
    Delete(sToChange, 1, i + Length(ADel) - 1);
    i      := pos(ADel, sToChange);
  end;
  Result := Result + sToChange;
end;

function TfpgCommandLineParams.NumToken(const AValue, AToken: string): integer;
var
  i, iCount: integer;
  lsValue: string;
begin
  Result := 0;
  if AValue = '' then
    Exit; //==>

  iCount  := 0;
  lsValue := AValue;
  i       := pos(AToken, lsValue);
  while i <> 0 do
  begin
    Delete(lsValue, i, Length(AToken));
    Inc(iCount);
    i := pos(AToken, lsValue);
  end;
  Result := iCount + 1;
end;

function TfpgCommandLineParams.Token(const AValue, AToken: string; const APos: integer): string;
var
  i, iCount, iNumToken: integer;
  lsValue: string;
begin
  Result := '';

  iNumToken := NumToken(AValue, AToken);
  if APos = 1 then
  begin
    if pos(AToken, AValue) = 0 then
      Result := AValue
    else
      Result := copy(AValue, 1, pos(AToken, AValue) - 1);
  end
  else if (iNumToken < APos - 1) or (APos < 1) then
    Result  := ''
  else
  begin
    { Remove leading blocks }
    iCount  := 1;
    lsValue := AValue;
    i       := pos(AToken, lsValue);
    while (i <> 0) and (iCount < APos) do
    begin
      Delete(lsValue, 1, i + Length(AToken) - 1);
      Inc(iCount);
      i := pos(AToken, lsValue);
    end;

    if (i = 0) and (iCount = APos) then
      Result := lsValue
    else if (i = 0) and (iCount <> APos) then
      Result := ''
    else
      Result := copy(lsValue, 1, i - 1);
  end;
end;

function TfpgCommandLineParams.WordExtract(const AInput: string; const APos: integer; const ADelims: string): string;
var
  iStart: integer;
  i: integer;
  iLen: integer;
begin
  Result := '';

  // Find the starting pos of the Nth word
  iStart := WordPosition(APos, AInput, ADelims);

  if iStart <> 0 then
  begin
    i    := iStart;
    iLen := Length(AInput);
    // Build up result until we come to our next wordDelim
    // while (i <= iLen) and not(S[i] in ADelims) do begin
    while (i <= iLen) and not (CharInStr(ExtractChar(AInput, i), ADelims)) do
    begin
      Result := Result + ExtractChar(AInput, i);
      Inc(i);
    end;
  end;
end;

function TfpgCommandLineParams.WordPosition(const AN: integer; const AStr: string; ADelims: string): integer;
var
  lCount: integer;
  lI: word;
  lSLen: integer;
begin
  lCount := 0;
  lI     := 1;
  Result := 0;
  lSLen  := Length(AStr);

  while (lI <= lSLen) and (lCount <> AN) do
  begin
    while (lI <= lSLen) and (CharInStr(ExtractChar(AStr, lI), ADelims)) do
      Inc(lI);

    // if we're not beyond end of S, we're at the start of a word
    if lI <= lSLen then
      Inc(lCount);

    // if not finished, find the end of the current word
    if lCount <> AN then
      while (lI <= lSLen) and not (CharInStr(ExtractChar(AStr, lI), ADelims)) do
        Inc(lI)
    else
      Result := lI;
  end;
end;

function TfpgCommandLineParams.ExtractChar(const AValue: string; const APos: integer): char;
var
  lResult: string;
begin
  if APos > Length(AValue) then
  begin
    Result := ' ';
    Exit;
  end;
  lResult := copy(AValue, APos, 1);
  Result  := lResult[1];
end;

function TfpgCommandLineParams.StripLeadingDelims(const AStrToProcess: string; ADelims: string): string;
var
  i: integer;
  lCharCurrent: char;
begin
  Result := AStrToProcess;
  // Loop through each char in the string
  for i := 1 to Length(AStrToProcess) do
  begin
    // Extract the current character
    lCharCurrent := ExtractChar(AStrToProcess, i);

    // Is this character a NON word delim?, then we have found the body of the string.
    if not CharInStr(lCharCurrent, ADelims) then
    begin
      Result := copy(AStrToProcess, i,
        Length(AStrToProcess) - i + 1);
      Exit; //==>
      // The current char is a word delim, but we are at the end of the string -
      // so no words
    end
    else if i = Length(AStrToProcess) then
      Result := '';
  end;
end;

// Strip any trailing ADelims
function TfpgCommandLineParams.StripTrailingDelims(const AStrToProcess: string; ADelims: string): string;
var
  i: integer;
  lCharCurrent: char;
begin
  Result := AStrToProcess;
  // Loop through each char in the string
  for i := Length(AStrToProcess) downto 1 do
  begin
    // Extract the current character
    lCharCurrent := ExtractChar(AStrToProcess, i);

    // Is this character a NON word delim?, then we have found the body of the string.
    if not CharInStr(lCharCurrent, ADelims) then
    begin
      Result := copy(AStrToProcess, 1, i);
      Exit; //==>
      // The current char is a word delim, but we are at the beginning of the string -
      // so no words
    end
    else if i = Length(AStrToProcess) then
      Result := '';
  end;
end;

// Given a set of word delimiters, return number of words in S
function TfpgCommandLineParams.WordCount(const AStrToProcess: string; ADelims: string): integer;
var
  i: integer;
  lCharLast: char;
  lCharCurrent: char;
  lStrToProcess: string;
begin
  // Strip any leading ADelims
  lStrToProcess := StripLeadingDelims(AStrToProcess, ADelims);
  lStrToProcess := StripTrailingDelims(lStrToProcess, ADelims);

  // If lStrToProcess is empty, then there are no words
  if lStrToProcess = '' then
  begin
    Result := 0;
    Exit; //==>
  end;

  // lStrToProcess is not empty, therefore there must be at least one word
  // Every wordDelim we find equals another word:
  // 0 word delim := 1 word
  // 1 word delim := 2 words...
  Result := 1;

  // lCharLast is used to check for more than 1 wordDelim together
  lCharLast := #0;

  for i := 1 to Length(lStrToProcess) do
  begin
    lCharCurrent := ExtractChar(lStrToProcess, i);
    if CharInStr(lCharCurrent, ADelims) and not (CharInStr(lCharLast, ADelims)) then
      Inc(Result);
    lCharLast := lCharCurrent;
  end;
end;

// Is AChr in the string AStr ?
function TfpgCommandLineParams.CharInStr(const AChr: char; const AStr: string): Boolean;
begin
  Result := pos(AChr, AStr) <> 0;
end;

{ TfpgCmdLineParams }

function TfpgCmdLineParams.FindOptionIndex(const S: string; var Longopt: Boolean): integer;
var
  SO, O: string;
  I, P: integer;
begin
  if not CaseSensitiveOptions then
    SO := UpperCase(S)
  else
    SO := S;
  Result := -1;
  I := ParamCount;
  while (Result = -1) and (I > 0) do
  begin
    O := Params[i];
    if (Length(O) > 0) and (O[1] = FOptionChar) then
    begin
      Delete(O, 1, 1);
      LongOpt := (Length(O) > 0) and (O[1] = FOptionChar);
      if LongOpt then
      begin
        Delete(O, 1, 1);
        P := Pos('=', O);
        if (P <> 0) then
          O := Copy(O, 1, P - 1);
      end;
      if not CaseSensitiveOptions then
        O      := UpperCase(O);
      if (O = SO) then
        Result := i;
    end;
    Dec(i);
  end;
end;

function TfpgCmdLineParams.GetOptionValue(const S: string): string;
begin
  Result := GetoptionValue(#255, S);
end;

function TfpgCmdLineParams.GetOptionValue(const C: char; const S: string): string;
var
  B: Boolean;
  I, P: integer;
  O: string;
begin
  Result := '';
  B := False;
  I      := FindOptionIndex(C, B);
  if (I = -1) then
    I := FindoptionIndex(S, B);
  if (I <> -1) then
    if B then
    begin
      // Long options have form --option=value
      O := Params[I];
      P := Pos('=', O);
      if (P = 0) then
        P := Length(O);
      Delete(O, 1, P);
      Result := O;
    end
    else if (I < ParamCount) then
    begin
      if (Copy(Params[I + 1], 1, 1) <> '-') then
        Result := Params[I + 1]; // short options have form '-o value'
    end;
end;

function TfpgCmdLineParams.HasOption(const S: string): Boolean;
var
  B: Boolean;
begin
  B := False;
  Result := FindOptionIndex(S, B) <> -1;
end;

function TfpgCmdLineParams.HasOption(const C: char; const S: string): Boolean;
var
  B: Boolean;
begin
  B := False;
  Result := (FindOptionIndex(C, B) <> -1) or (FindOptionIndex(S, B) <> -1);
end;

function TfpgCmdLineParams.CheckOptions(const ShortOptions: string; const Longopts: TStrings; Opts, NonOpts: TStrings; AllErrors: Boolean): string;
var
  I, J, L, P: integer;
  O, OV, SO: string;
  UsedArg, HaveArg: Boolean;

  function FindLongOpt(S: string): Boolean;
  var
    I: integer;
  begin
    Result := Assigned(LongOpts);
    if not Result then
      Exit;
    if CaseSensitiveOptions then
    begin
      I := LongOpts.Count - 1;
      while (I >= 0) and (LongOpts[i] <> S) do
        Dec(i);
    end
    else
    begin
      S := UpperCase(S);
      I := LongOpts.Count - 1;
      while (I >= 0) and (UpperCase(LongOpts[i]) <> S) do
        Dec(i);
    end;
    Result := (I <> -1);
  end;

  procedure AddToResult(const Msg: string);
  begin
    if (Result <> '') then
      Result := Result + LineEnding;
    Result := Result + Msg;
  end;

begin
  if CaseSensitiveOptions then
    SO := ShortOptions
  else
    SO := LowerCase(ShortOptions);
  Result := '';
  I := 1;
  while (I <= ParamCount) and ((Result = '') or AllErrors) do
  begin
    O := ParamStr(I);
    if (Length(O) = 0) or (O[1] <> FOptionChar) then
    begin
      if Assigned(NonOpts) then
        NonOpts.Add(O);
    end
    else if (Length(O) < 2) then
      AddToResult(Format(SErrInvalidOption, [i, O]))
    else
    begin
      HaveArg := False;
      OV      := '';
      // Long option ?
      if (O[2] = FOptionChar) then
      begin
        Delete(O, 1, 2);
        J := Pos('=', O);
        if J <> 0 then
        begin
          HaveArg := True;
          OV      := O;
          Delete(OV, 1, J);
          O       := Copy(O, 1, J - 1);
        end;
        // Switch Option
        if FindLongopt(O) then
        begin
          if HaveArg then
            AddToResult(Format(SErrNoOptionAllowed, [I, O]));
        end
        else
        begin // Required argument
          if FindLongOpt(O + ':') then
          begin
            if not HaveArg then
              AddToResult(Format(SErrOptionNeeded, [I, O]));
          end
          else if not FindLongOpt(O + '::') then
            AddToResult(Format(SErrInvalidOption, [I, O]))// Optional Argument.
          ;
        end;
      end
      else // Short Option.
      begin
        HaveArg := (I < ParamCount) and (Length(ParamStr(I + 1)) > 0) and (ParamStr(I + 1)[1] <> FOptionChar);
        UsedArg := False;
        if HaveArg then
          OV := ParamStr(I + 1);
        if not CaseSensitiveOptions then
          O := LowerCase(O);
        L := Length(O);
        J := 2;
        while ((Result = '') or AllErrors) and (J <= L) do
        begin
          P := Pos(O[J], ShortOptions);
          if (P = 0) or (O[j] = ':') then
            AddToResult(Format(SErrInvalidOption, [I, O[J]]))
          else if (P < Length(ShortOptions)) and (Shortoptions[P + 1] = ':') then
          begin
            // Required argument
            if ((P + 1) = Length(ShortOptions)) or (Shortoptions[P + 2] <> ':') then
              if (J < L) or not haveArg then // Must be last in multi-opt !!
                AddToResult(Format(SErrOptionNeeded, [I, O[J]]));
            O       := O[j]; // O is added to arguments.
            UsedArg := True;
          end;
          Inc(J);
        end;
        if HaveArg and UsedArg then
        begin
          Inc(I); // Skip argument.
          O := O[Length(O)]; // O is added to arguments !
        end;
      end;
      if HaveArg and ((Result = '') or AllErrors) then
        if Assigned(Opts) then
          Opts.Add(O + '=' + OV);
    end;
    Inc(I);
  end;
end;

function TfpgCmdLineParams.CheckOptions(const ShortOptions: string; const Longopts: array of string; Opts, NonOpts: TStrings; AllErrors: Boolean): string;
var
  L: TStringList;
  I: integer;
begin
  L := TStringList.Create;
  try
    for I := 0 to High(LongOpts) do
      L.Add(LongOpts[i]);
    Result := CheckOptions(ShortOptions, L, Opts, NonOpts, AllErrors);
  finally
    L.Free;
  end;
end;

function TfpgCmdLineParams.CheckOptions(const ShortOptions: string; const Longopts: TStrings; AllErrors: Boolean): string;
begin
  Result := CheckOptions(ShortOptions, LongOpts, nil, nil, AllErrors);
end;

function TfpgCmdLineParams.CheckOptions(const ShortOptions: string; const LongOpts: array of string; AllErrors: Boolean): string;
var
  L: TStringList;
  I: integer;
begin
  L := TStringList.Create;
  try
    for I := 0 to High(LongOpts) do
      L.Add(LongOpts[i]);
    Result := CheckOptions(ShortOptions, L, AllErrors);
  finally
    L.Free;
  end;
end;

function TfpgCmdLineParams.CheckOptions(const ShortOptions: string; const LongOpts: string; AllErrors: Boolean): string;
const
  cSepChars = ' '#10#13#9;
var
  L: TStringList;
  Len, I, J: integer;
begin
  L := TStringList.Create;
  try
    I := 1;
    Len := Length(LongOpts);
    while I <= Len do
    begin
      while Isdelimiter(cSepChars, LongOpts, I) do
        Inc(I);
      J := I;
      while (J <= Len) and not IsDelimiter(cSepChars, LongOpts, J) do
        Inc(J);
      if (I <= J) then
        L.Add(Copy(LongOpts, I, (J - I)));
      I := J + 1;
    end;
    Result := CheckOptions(Shortoptions, L, AllErrors);
  finally
    L.Free;
  end;
end;

function TfpgCmdLineParams.GetCaseSensitiveOptions: Boolean;
begin
  Result := FCaseSensitiveOptions;
end;

function TfpgCmdLineParams.GetOptionChar: char;
begin
  Result := FOptionChar;
end;

procedure TfpgCmdLineParams.SetCaseSensitiveOptions(AValue: Boolean);
begin
  FCaseSensitiveOptions := AValue;
end;

procedure TfpgCmdLineParams.SetOptionChar(AValue: char);
begin
  FOptionChar := AValue;
end;

function TfpgCmdLineParams.GetParams(AIndex: integer): string;
begin
  Result := ParamStr(AIndex);
end;

function TfpgCmdLineParams.GetParamCount: integer;
begin
  Result := System.ParamCount;
end;

constructor TfpgCmdLineParams.Create;
begin
  FOptionChar           := '-';
  FCaseSensitiveOptions := True;
end;


initialization

finalization
  uCommandLineParams.Free;

end.

