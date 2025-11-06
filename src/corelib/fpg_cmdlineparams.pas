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

{$I fpg_defines.inc}

interface

uses
  Classes;

const
  ctiCommandLineParamPrefix = '-';

type
  TStringArray = Array of string;


  ICmdLineParams = interface
    ['{EDF51E67-1119-11E6-B131-C86000E37EB0}']
    { main interaction functions }
    function    GetOptionAtIndex(AIndex: Integer; IsLong: Boolean): string;
    function    FindOptionIndex(const S: string; var LongOpt: Boolean; StartAt: Integer = -1): Integer;
    function    GetOptionValue(const S: string): string;
    function    GetOptionValue(const C: char; const S: string): string;
    function    GetOptionValues(const C: Char; const S: string): TStringArray;
    function    HasOption(const S: string): Boolean;
    function    HasOption(const C: char; const S: string): Boolean;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: array of string; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: array of string; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: string; AllErrors: Boolean = False): string;
    function    GetNonOptions(const ShortOptions: string; const LongOpts: Array of string): TStringArray;
    procedure   GetNonOptions(const ShortOptions: string; const LongOpts: Array of string; NonOptions: TStrings);
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


  TfpgCmdLineParams = class(TInterfacedObject, ICmdLineParams)
  private
    FOptionChar: char;
    FCaseSensitiveOptions: Boolean;
    { main interaction functions }
    function    GetOptionAtIndex(AIndex: Integer; IsLong: Boolean): string;
    function    FindOptionIndex(const S: string; var LongOpt: Boolean; StartAt: Integer = -1): Integer;
    function    GetOptionValue(const S: string): string;
    function    GetOptionValue(const C: char; const S: string): string;
    function    GetOptionValues(const C: Char; const S: string): TStringArray;
    function    HasOption(const S: string): Boolean;
    function    HasOption(const C: char; const S: string): Boolean;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: array of string; Opts, NonOpts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const Longopts: TStrings; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: array of string; AllErrors: Boolean = False): string;
    function    CheckOptions(const ShortOptions: string; const LongOpts: string; AllErrors: Boolean = False): string;
    function    GetNonOptions(const ShortOptions: string; const LongOpts: Array of string): TStringArray;
    procedure   GetNonOptions(const ShortOptions: string; const LongOpts: Array of string; NonOptions: TStrings);
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



implementation

uses
  SysUtils,
  fpg_constants;

{ TfpgCmdLineParams }

function TfpgCmdLineParams.GetOptionAtIndex(AIndex: Integer; IsLong: Boolean): string;
var
  P: Integer;
  O: String;
begin
  Result := '';
  If (AIndex = -1) then
    Exit;
  If IsLong then
  begin // Long options have form --option=value
    O := Params[AIndex];
    P := Pos('=', O);
    if (P = 0) then
      P := Length(O);
    Delete(O, 1, P);
    Result := O;
  end
  else
  begin // short options have form '-o value'
    if (AIndex<ParamCount) then
      if (Copy(Params[AIndex+1], 1, 1) <> '-') then
        Result := Params[AIndex+1];
  end;
end;

function TfpgCmdLineParams.FindOptionIndex(const S: string; var LongOpt: Boolean; StartAt: Integer): Integer;
var
  SO, O: string;
  I, P: integer;
begin
  if not CaseSensitiveOptions then
    SO := UpperCase(S)
  else
    SO := S;
  Result := -1;
  I := StartAt;
  if (I = -1) then
    I := ParamCount;
  while (Result = -1) and (I > 0) do
  begin
    O := Params[i];
    // - must be seen as an option value
    if (Length(O) > 1) and (O[1] = FOptionChar) then
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

function TfpgCmdLineParams.GetOptionValues(const C: Char; const S: string): TStringArray;
var
  I, Cnt: Integer;
  B: Boolean;
begin
  SetLength(Result, ParamCount);
  Cnt := 0;
  Repeat
    I := FindOptionIndex(C, B, I);
    If I<>-1 then
    begin
      Inc(Cnt);
      Dec(I);
    end;
  Until I = -1;
  Repeat
    I := FindOptionIndex(S, B,I );
    If I <> -1 then
    begin
      Inc(Cnt);
      Dec(I);
    end;
  Until I = -1;
  SetLength(Result, Cnt);
  Cnt := 0;
  I := -1;
  Repeat
    I := FindOptionIndex(C, B, I);
    If (I <> -1) then
    begin
      Result[Cnt] := GetOptionAtIndex(I, False);
      Inc(Cnt);
      Dec(i);
    end;
  Until (I = -1);
  I := -1;
  Repeat
    I := FindOptionIndex(S, B, I);
    If I <> -1 then
    begin
      Result[Cnt] := GetOptionAtIndex(I, True);
      Inc(Cnt);
      Dec(i);
    end;
  Until (I = -1);
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

function TfpgCmdLineParams.CheckOptions(const ShortOptions: string; const Longopts: TStrings; AllErrors: Boolean): string;
begin
  Result := CheckOptions(ShortOptions, LongOpts, nil, nil, AllErrors);
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
    else
	  begin
      if (Length(O) < 2) then
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
            else
            begin // Optional Argument.
              if not FindLongOpt(O + '::') then
                AddToResult(Format(SErrInvalidOption, [I, O]));
            end;
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
            else
            begin
		          if (P < Length(ShortOptions)) and (Shortoptions[P + 1] = ':') then
			        begin
                // Required argument
                if ((P + 1) = Length(ShortOptions)) or (Shortoptions[P + 2] <> ':') then
                  if (J < L) or not haveArg then // Must be last in multi-opt !!
                    AddToResult(Format(SErrOptionNeeded, [I, O[J]]));
                O := O[j]; // O is added to arguments.
                UsedArg := True;
              end;
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

function TfpgCmdLineParams.GetNonOptions(const ShortOptions: string; const LongOpts: array of string): TStringArray;
var
  NO : TStrings;
  I : Integer;
begin
  NO := TStringList.Create;
  try
    GetNonOptions(ShortOptions, LongOpts, No);
    SetLength(Result, NO.Count);
    For I := 0 to NO.Count-1 do
      Result[I] := NO[i];
  finally
    NO.Free;
  end;
end;

procedure TfpgCmdLineParams.GetNonOptions(const ShortOptions: string; const LongOpts: array of string; NonOptions: TStrings);
var
  S: String;
begin
  S := CheckOptions(ShortOptions, LongOpts, Nil, NonOptions, true);
  if (S <> '') then
    Raise EListError.Create(S);
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


end.

