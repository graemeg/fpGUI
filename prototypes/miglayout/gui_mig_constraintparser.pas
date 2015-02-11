unit gui_mig_constraintparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_mig_lc, gui_mig_unitvalue, gui_mig_boundsize;
  
type

  { TConstraintParser }

  TConstraintParser = class(TObject)
  private
    function  StartsWithLenient(s: String; Matches: array of string; MinChars: array of integer; AcceptTrailing: Boolean): Integer;
    function  StartsWithLenient(s: String; Match: string; MinChars: integer; AcceptTrailing: Boolean): Integer;
    function  ParseUnitValue(const s: string; EmptyReplacement: TUnitValue; IsHorizontal: Boolean): TUnitValue;
    function  ToTrimmedTokens(s: string; seperator: char): TStrings;
  public
    function  ParseBoundSize(s: string; IsGap, IsHorizontal: Boolean): TBoundSize;
    function  ParseInsets(s: string; AcceptPanel: Boolean): TUnitValueArray;
    function  ParseLayoutConstraint(const s: string): TLC;
    function  ParseUnitValue(const s: string; IsHorizontal: Boolean): TUnitValue;
  end;

implementation

uses
  gfx_tokenlibrary, gui_mig_exceptions, gui_mig_platformdefaults;

{ TConstraintParser }

function TConstraintParser.ParseLayoutConstraint(const s: string): TLC;
var
  tokenizer: TTokens;
  parts: TStringList;
  part: string;
  i: integer;
  len: integer;
  ix: integer = -1;
  c: char;
  num: string;
  gaps: TStrings;
  millis: String;
  insStr: String;
begin
  result := TLC.Create;
  
  if s = '' then
    Exit;
    
  tokenizer := TTokens.Create(s, ',', '"', '"', '\', tsSingleSeparatorBetweenTokens);
  parts := TStringList.Create;
  for i := 1 to tokenizer.TokenCount do
    parts.Add(trim(tokenizer.Token(i)));
  tokenizer.Free;
  
  // First check for "ltr" or "rtl" since that will affect the interpretation
  // of the other constraints.
  for i := 0 to parts.Count-1 do
  begin
    part := parts[i];
    if part = '' then
      continue;
      
    len := Length(part);
    if (len = 3) or (len = 11) then // optimization
    begin
      if (part = 'ltr') or (part = 'rtl') or (part = 'lefttoright') or (part = 'righttoleft') then
      begin
        result.LeftToRight_prop := part[1] = 'l';
        parts[i] := ''; // so we don't process it again
      end;
      if (part = 'ttb') or (part = 'btt') or (part = 'toptobottom') or (part = 'bottomtotop') then
      begin
        result.TopToBottom_prop := part[1] = 't';
        parts[i] := ''; // so we don't process it again
      end;
    end;
  end;
  
  for i := 0 to parts.Count-1 do
  begin
    part := parts[i];
    if part = '' then
      continue;

    try
      c := part[1];
      case c of
        'w':
          begin
            ix := StartsWithLenient(part, 'wrap', -1, True);
            if ix > 0 then
            begin
              num := Trim(Copy(part, ix, Length(part)));
              if Length(num) <> 0 then
                Result.WrapAfter_prop := StrToInt(num)
              else
                Result.WrapAfter_prop := 0;
            end;
          end;
        'g':
          begin
            if Pos('gapx ', part) = 1 then
              Result.GridGapX := ParseBoundSize(Trim(Copy(part, 6, Length(Part))), True, True);
            if Pos('gapy ', part) = 1 then
              Result.GridGapY := ParseBoundSize(Trim(Copy(part, 6, Length(Part))), True, False);
            if Pos('gap ', part) = 1 then
            begin
              gaps := ToTrimmedTokens(Copy(part, 5, Length(Part)),' ');
              Result.GridGapX := ParseBoundSize(gaps[0], True, True);
              if gaps.Count > 1 then
                Result.GridGapY := ParseBoundSize(gaps[1], True, False);
            end;
          end;
        'd':
          begin
            ix := StartsWithLenient(part, 'debug', 5,true);
            if ix > 0 then
            begin
              millis := Trim(Copy(part, ix, Length(part)));
              if Length(millis) > 0 then
                Result.DebugMillis:=StrToInt(millis)
              else
                Result.DebugMillis := 1000;
            end;
          end;
        'n':
          begin
            if part = 'nogrid' then
              Result.NoGrid_prop:=True;
            if part = 'nocache' then
              Result.NoCache_prop:=True;
            if part = 'novisualpadding' then
              Result.VisualPadding_prop := False;
          end;
        'f':
          begin
            if (part = 'fill') or (part = 'fillx') or (part = 'filly') then
            begin
              Result.FillX_prop:= (Length(part) = 4) or ((Length(part) > 4) and (part[5] = 'x'));
              Result.FillY_prop:= (Length(part) = 4) or ((Length(part) > 4) and (part[5] = 'y'));
            end;
            if part = 'flowy' then
              Result.FlowY_prop:=True;
            if part = 'flowx' then
              Result.FlowX_prop:=True;
          end;
        'i':
          begin
            ix := StartsWithLenient(part, 'insets', 3, true);
            if ix > 0 then
            begin
              insStr := Copy(part,ix,Length(part));
              //ins: parseinsets
              {$NOTE resume translation here}
            end;
          end;
      end;

    finally
    end;

  end;
  
  parts.Free;
end;

function TConstraintParser.ParseUnitValue(const s: string; IsHorizontal: Boolean): TUnitValue;
begin
  ParseUnitValue(s, nil, IsHorizontal);
end;

function TConstraintParser.StartsWithLenient(s: String;  Matches: array of string;
  MinChars: array of integer; AcceptTrailing: Boolean): Integer;
var
  MinChar: Integer;
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Matches) do
  begin
    if i <= High(MinChars) then
      MinChar := MinChars[i]
    else
      MinChar := -1;
    Result := StartsWithLenient(s, Matches[i], MinChar, AcceptTrailing);
    if Result > -1 then
      Break;
  end;
end;

function TConstraintParser.StartsWithLenient(s: String; Match: string;
  MinChars: integer; AcceptTrailing: Boolean): Integer;
var
  sSz: Integer;
  mSz: Integer;
  sIx: Integer;
  mIx: Integer;
begin
  // changed for 1 based string index
  Result := -1;
  if s[1] <> Match[1] then  // Fast sanity check.
    Exit;

  if MinChars = -1 then
    MinChars := Length(Match);

  sSz := Length(s);
  if sSz < MinChars then
    Exit;

  mSz := Length(Match);
  sIx := 1;

  mIx := 1;
  while mIx <= mSz do
  begin
    while (sIx < sSz) and ((s[sIx] in [' ', '_'])) do // Disregard spaces and _
      Inc(sIx);

    if (sIx > sSz) or (s[sIx] <> Match[sIx]) then
    begin
      if (mIx > MinChars) and (AcceptTrailing or (sIx > sSz)) and ((sIx > sSz) or (s[sIx-1] = ' ')) then
        Result := sIx;
      Exit;
    end;
    Inc(sIx);
    Inc(mIx);
  end;

  if (sIx > sSz) or AcceptTrailing or (s[sIx] = ' ') then
    Result := sIx;
end;

function TConstraintParser.ParseUnitValue(const s: string;
  EmptyReplacement: TUnitValue; IsHorizontal: Boolean): TUnitValue;
begin

end;

function TConstraintParser.ToTrimmedTokens(s: string; seperator: char): TStrings;
var
  p: Integer = 0;
  i: Integer;
  st: Integer = 1;
begin
  Result := TStringList.Create;
  for i := 1 to Length(s) do
  begin
    case s[i] of
      '(' :  Inc(p);
      ')' :  Dec(p);
    else
      if (p = 0) and (s[i] = seperator) then
      begin
        Result.Add(Trim(Copy(s,st,i-st)));
        st := i+1;
      end;
    end;
    if P < 0 then
      raise EArgumentException.CreateFmt('Unbalanced parentheses: ''%s''', [s]);
  end;
  if P < 0 then
    raise EArgumentException.CreateFmt('Unbalanced parentheses: ''%s''', [s]);

  if Result.Count = 0 then
    Result.Add(Trim(s));

end;

function TConstraintParser.ParseBoundSize(s: string; IsGap,
  IsHorizontal: Boolean): TBoundSize;
var
  push: Boolean = False;
  l: Integer;
  sizes: TStrings;
  s0: String;
  HasEM: Boolean;
  uv: TUnitValue;
  AMin, AMax: TUnitValue;
begin
  Result := nil;
  if (s = '') or (s = 'n') then
    Exit;

  if Copy(s, Length(s) - 4, 4) = 'push' then
  begin
    if not IsGap then
      Raise EIllegalArgument.Create('Only gaps can have ''push'' in size. Use as separate keyword for components.');
    push := True;
    l := Length(s);
    if s[Length(s)-5] = ':' then
      s := Copy(s, 1, l-5)
    else
      s := Copy(s, 1, l-4);

    if s = '' then
      Exit(TBoundSize.Create(nil,nil,nil,True,s))
  end;

  sizes := ToTrimmedTokens(s, ':');
  s0 := sizes[0];

  AMin := nil;
  AMax := nil;

  case sizes.Count of
    1 :
      begin
        HasEM := (s0 <> '') and (s0[Length(s0)] = '!');
        if HasEm then
          s0 := Copy(s0, 1, Length(s0)-1);

        uv := ParseUnitValue(s0, nil, IsHorizontal);
        if (IsGap or HasEM) then
          AMin := uv;
        if HasEM then
          AMax := uv;
        Result := TBoundSize.Create(AMin,uv,AMax,push,s);
      end;
    2 :
      begin
        Result := TBoundSize.Create(ParseUnitValue(s0, nil, IsHorizontal), ParseUnitValue(sizes[1], nil, IsHorizontal), nil, push, s);
      end;
    3 :
      begin
        Result := TBoundSize.Create(ParseUnitValue(s0, nil, IsHorizontal), ParseUnitValue(sizes[1], nil, IsHorizontal), ParseUnitValue(sizes[2], nil, IsHorizontal), push, s);
      end;
  else
    raise EIllegalArgument.CreateFmt('Min:Preferred:Max size section must contain 0, 1 or 2 colons. ''%s''', [s]);
  end; // case

  sizes.Free;


end;

function TConstraintParser.ParseInsets(s: string; AcceptPanel: Boolean): TUnitValueArray;
var
  isPanel: Boolean;
  i: Integer;
begin
  FillChar(Result[0], SizeOf(TUnitValueArray), 0);
  if (Length(s) = 0) or (s = 'dialog') or (s = 'panel') then
  begin
    if AcceptPanel = False then
      raise EIllegalAccess.CreateFmt('Insets now allowed: %s', [s]); // *not* allowed?

    isPanel  :=  s[1] = 'p';
    for i := 0 to 3 do
    begin
      if isPanel then
        //Result[i] := PlatformDefaults.
        {$NOTE Resume translation here}
    end;

  end;

end;

end.

