unit fpg_mig_constraintparser;

{
  MigLayout v11 ConstraintParser for fpGUI

  Ported from: net.miginfocom.layout.ConstraintParser.java (v11.4.2)

  Parses constraint strings into constraint objects.
  Supports:
  - UnitValue parsing: "10px", "50%", "100mm", "pref+10px"
  - BoundSize parsing: "10:20:30", "100px:pref:200px", "10px:push"
  - Insets parsing: "10 20 30 40", "10", "dialog", "panel"
  - Alignment keywords: "left", "right", "center", "top", "bottom", "baseline"
  - Operations: min(), max(), mid(), +, -, *, /
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_mig_boundsize, fpg_mig_unitvalue, fpg_mig_platformdefaults;

type
  { Array type for insets parsing }
  TfpgMigUnitValueArray = array of TfpgMigUnitValue;

  { String array type }
  TStringArray = array of string;

{ Parse a BoundSize from a string like "100px" or "50:100:200" }
function ParseBoundSize(const AStr: string; AIsGap, AIsHor: Boolean): TfpgMigBoundSize;

{ Parse alignment keywords like "left", "right", "top", "bottom", "center", etc. }
function ParseAlignKeywords(const AStr: string; AIsHorizontal: Boolean): TfpgMigUnitValue;

{ Parse a single UnitValue from a string }
function ParseUnitValue(const AStr: string; AIsHorizontal: Boolean): TfpgMigUnitValue; overload;
function ParseUnitValue(const AStr: string; AEmptyReplacement: TfpgMigUnitValue;
                        AIsHorizontal: Boolean): TfpgMigUnitValue; overload;

{ Parse UnitValue or alignment keyword }
function ParseUnitValueOrAlign(const AStr: string; AIsHorizontal: Boolean;
                               AEmptyReplacement: TfpgMigUnitValue = nil): TfpgMigUnitValue;

{ Parse insets from a string like "10" or "10 20" or "10 20 30 40" }
function ParseInsets(const AStr: string; AAcceptPanel: Boolean): TfpgMigUnitValueArray;

implementation

uses
  Math, StrUtils;

{ Helper Functions }

{ Splits a string by separator, respecting parentheses.
  For example: "min(10px,20px),30px" with sep=',' returns ["min(10px,20px)", "30px"] }
function ToTrimmedTokens(const AStr: string; ASep: Char): TStringArray;
var
  toks, sSize, p, i, st, pNr: Integer;
  c: Char;
  disregardDoubles: Boolean;
begin
  sSize := Length(AStr);
  toks := 0;
  disregardDoubles := (ASep = ' ');

  // Count the separators
  p := 0;
  i := 1;
  while i <= sSize do
  begin
    c := AStr[i];
    if c = '(' then
      Inc(p)
    else if c = ')' then
      Dec(p)
    else if (p = 0) and (c = ASep) then
    begin
      Inc(toks);
      while disregardDoubles and (i < sSize) and (AStr[i + 1] = ' ') do
        Inc(i);
    end;

    if p < 0 then
      raise Exception.Create('Unbalanced parentheses: ''' + AStr + '''');
    Inc(i);
  end;

  if p <> 0 then
    raise Exception.Create('Unbalanced parentheses: ''' + AStr + '''');

  // If no separators, return single trimmed string
  if toks = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Trim(AStr);
    Exit;
  end;

  // Split the string
  SetLength(Result, toks + 1);
  st := 1;
  pNr := 0;
  p := 0;
  i := 1;

  while i <= sSize do
  begin
    c := AStr[i];
    if c = '(' then
      Inc(p)
    else if c = ')' then
      Dec(p)
    else if (p = 0) and (c = ASep) then
    begin
      Result[pNr] := Trim(Copy(AStr, st, i - st));
      Inc(pNr);
      st := i + 1;
      while disregardDoubles and (i < sSize) and (AStr[i + 1] = ' ') do
        Inc(i);
    end;
    Inc(i);
  end;

  Result[pNr] := Trim(Copy(AStr, st, sSize - st + 1));
end;

{ Splits a text-number combination such as "hello10.0" into ["hello", "10.0"].
  Or "10px" into ["10", "px"]. }
function GetNumTextParts(const AStr: string): TStringArray;
var
  i, sSize: Integer;
  c: Char;
begin
  sSize := Length(AStr);
  for i := 1 to sSize do
  begin
    c := AStr[i];
    if c = ' ' then
      raise Exception.Create('Space in UnitValue: ''' + AStr + '''');

    // If character is not a digit, dot, or minus, split here
    if not (c in ['0'..'9', '.', '-']) then
    begin
      SetLength(Result, 2);
      Result[0] := Trim(Copy(AStr, 1, i - 1));
      Result[1] := Trim(Copy(AStr, i, sSize - i + 1));
      Exit;
    end;
  end;

  // All digits - return number with empty unit
  SetLength(Result, 2);
  Result[0] := AStr;
  Result[1] := '';
end;

{ Returns the operation type based on the string format.
  Detects: min(...), max(...), mid(...), or inline +, -, *, / }
function GetOper(const AStr: string): TfpgMigOperation;
var
  len, i, j, p: Integer;
  c: Char;
begin
  len := Length(AStr);
  if len < 3 then
    Exit(opStatic);

  // Check for min(...), max(...), mid(...)
  if (len > 5) and (AStr[4] = '(') and (AStr[len] = ')') then
  begin
    if Copy(AStr, 1, 4) = 'min(' then
      Exit(opMin);
    if Copy(AStr, 1, 4) = 'max(' then
      Exit(opMax);
    if Copy(AStr, 1, 4) = 'mid(' then
      Exit(opMid);
  end;

  // Try inline add/sub, then mul/div (precedence order)
  for j := 0 to 1 do
  begin
    p := 0;
    for i := len downto 2 do  // Start from end, skip first char
    begin
      c := AStr[i];
      if c = ')' then
        Inc(p)
      else if c = '(' then
        Dec(p)
      else if p = 0 then
      begin
        if j = 0 then
        begin
          if c = '+' then Exit(opAdd);
          if c = '-' then Exit(opSub);
        end
        else
        begin
          if c = '*' then Exit(opMul);
          if c = '/' then Exit(opDiv);
        end;
      end;
    end;
  end;

  Result := opStatic;
end;

{ Checks if string s starts with match string, allowing lenient matching.
  Returns index of first unmatched character or -1 if no match.
  minChars = minimum characters to match (-1 means full length) }
function StartsWithLenient(const AStr, AMatch: string; AMinChars: Integer;
                          AAcceptTrailing: Boolean): Integer;
var
  i, sLen, mLen: Integer;
begin
  sLen := Length(AStr);
  mLen := Length(AMatch);

  // Fast sanity check
  if (sLen = 0) or (mLen = 0) or (AStr[1] <> AMatch[1]) then
    Exit(-1);

  if AMinChars < 0 then
    AMinChars := mLen;

  // Match characters
  i := 1;
  while (i <= sLen) and (i <= mLen) do
  begin
    if AStr[i] <> AMatch[i] then
    begin
      // If we've matched enough characters, return success
      if (i - 1) >= AMinChars then
        Exit(i - 1);
      Exit(-1);
    end;
    Inc(i);
  end;

  // Reached end of one or both strings
  if (i - 1) >= AMinChars then
  begin
    if AAcceptTrailing or (i > sLen) then
      Exit(i - 1);
  end;

  Result := -1;
end;

{ Core Parsing Functions }

function ParseAlignKeywords(const AStr: string; AIsHorizontal: Boolean): TfpgMigUnitValue;
begin
  // Check for center (works for both horizontal and vertical)
  if StartsWithLenient(AStr, 'center', 1, False) <> -1 then
    Exit(UnitValueCenter);

  if AIsHorizontal then
  begin
    if StartsWithLenient(AStr, 'left', 1, False) <> -1 then
      Exit(UnitValueLeft);
    if StartsWithLenient(AStr, 'right', 1, False) <> -1 then
      Exit(UnitValueRight);
    if StartsWithLenient(AStr, 'leading', 4, False) <> -1 then
      Exit(UnitValueLeading);
    if StartsWithLenient(AStr, 'trailing', 5, False) <> -1 then
      Exit(UnitValueTrailing);
    if StartsWithLenient(AStr, 'label', 5, False) <> -1 then
      Exit(UnitValueLabel);
  end
  else
  begin
    if StartsWithLenient(AStr, 'baseline', 4, False) <> -1 then
      Exit(UnitValueBaselineIdentity);
    if StartsWithLenient(AStr, 'top', 1, False) <> -1 then
      Exit(UnitValueTop);
    if StartsWithLenient(AStr, 'bottom', 1, False) <> -1 then
      Exit(UnitValueBottom);
  end;

  Result := nil;
end;

function ParseUnitValue(const AStr: string; AIsHorizontal: Boolean): TfpgMigUnitValue;
begin
  Result := ParseUnitValue(AStr, nil, AIsHorizontal);
end;

function ParseUnitValue(const AStr: string; AEmptyReplacement: TfpgMigUnitValue;
                        AIsHorizontal: Boolean): TfpgMigUnitValue;
var
  s, cs: string;
  c0: Char;
  oper: TfpgMigOperation;
  inline: Boolean;
  uvs: TStringArray;
  sub: string;
  delim: Char;
  last, first: string;
  sub1, sub2: TfpgMigUnitValue;
  numParts: TStringArray;
  value: Single;
  unitStr: string;
begin
  // Handle empty/nil
  if (AStr = '') or (AStr = 'null') or (AStr = 'n') then
    Exit(AEmptyReplacement);

  s := AStr;
  cs := AStr;  // Save creation string
  c0 := s[1];

  // Remove parentheses if present
  if (c0 = '(') and (s[Length(s)] = ')') then
    s := Copy(s, 2, Length(s) - 2);

  // Check for null
  if (c0 = 'n') and ((s = 'null') or (s = 'n')) then
    Exit(nil);

  // Check for infinity
  if (c0 = 'i') and (s = 'inf') then
    Exit(UnitValueInf);

  // Detect operation type
  oper := GetOper(s);
  inline := (oper = opAdd) or (oper = opSub) or (oper = opMul) or (oper = opDiv);

  // If multi-value expression
  if oper <> opStatic then
  begin
    if not inline then
    begin
      // Format: min(xxx,yyy)
      sub := Trim(Copy(s, 5, Length(s) - 5));  // Remove "min(" and ")"
      uvs := ToTrimmedTokens(sub, ',');
      if Length(uvs) = 1 then
        Exit(ParseUnitValue(sub, nil, AIsHorizontal));
    end
    else
    begin
      // Inline format: 10px+5mm
      case oper of
        opAdd: delim := '+';
        opSub: delim := '-';
        opMul: delim := '*';
        else delim := '/';
      end;

      uvs := ToTrimmedTokens(s, delim);

      // If more than one operator, group all but last
      if Length(uvs) > 2 then
      begin
        last := uvs[High(uvs)];
        first := Copy(s, 1, Length(s) - Length(last) - 1);
        SetLength(uvs, 2);
        uvs[0] := first;
        uvs[1] := last;
      end;
    end;

    if Length(uvs) <> 2 then
      raise Exception.Create('Malformed UnitValue: ''' + AStr + '''');

    sub1 := ParseUnitValue(uvs[0], nil, AIsHorizontal);
    sub2 := ParseUnitValue(uvs[1], nil, AIsHorizontal);

    if (sub1 = nil) or (sub2 = nil) then
      raise Exception.Create('Malformed UnitValue. Must be two sub-values: ''' + AStr + '''');

    // Create UnitValue with operation and sub-units
    // Note: Need to add this constructor to TfpgMigUnitValue
    Result := TfpgMigUnitValue.CreateOper(AIsHorizontal, oper, sub1, sub2, cs);
  end
  else
  begin
    // Simple value: "10px", "50%", "related"
    try
      numParts := GetNumTextParts(s);

      // If no number part (e.g. "related"), use 1.0
      if Length(numParts[0]) > 0 then
        value := StrToFloat(numParts[0])
      else
        value := 1.0;

      unitStr := numParts[1];

      Result := TfpgMigUnitValue.Create(value, unitStr, AIsHorizontal, oper, cs);
    except
      on E: Exception do
        raise Exception.Create('Malformed UnitValue: ''' + AStr + '''. Error: ' + E.Message);
    end;
  end;
end;

function ParseUnitValueOrAlign(const AStr: string; AIsHorizontal: Boolean;
                               AEmptyReplacement: TfpgMigUnitValue = nil): TfpgMigUnitValue;
var
  align: TfpgMigUnitValue;
begin
  if AStr = '' then
    Exit(AEmptyReplacement);

  // Try alignment keywords first
  align := ParseAlignKeywords(AStr, AIsHorizontal);
  if align <> nil then
    Exit(align);

  // Parse as unit value
  Result := ParseUnitValue(AStr, AEmptyReplacement, AIsHorizontal);
end;

function ParseBoundSize(const AStr: string; AIsGap, AIsHor: Boolean): TfpgMigBoundSize;
var
  s, s0: string;
  push, hasEM: Boolean;
  sizes: TStringArray;
  uv, uvMax, uv1, uv2, uv3: TfpgMigUnitValue;
  len: Integer;
begin
  // Handle empty/null
  if (AStr = '') or (AStr = 'null') or (AStr = 'n') then
    Exit(nil);

  s := AStr;
  push := False;

  // Check for "push" suffix
  if EndsStr('push', s) then
  begin
    push := True;
    len := Length(s);
    if EndsStr(':push', s) then
      s := Copy(s, 1, len - 5)
    else
      s := Copy(s, 1, len - 4);

    if s = '' then
      Exit(TfpgMigBoundSize.Create(nil, nil, nil, push));
  end;

  // Split by ':' to get min:pref:max
  sizes := ToTrimmedTokens(s, ':');
  s0 := sizes[0];

  case Length(sizes) of
    1:
      begin
        // Single value: "100px" or "100px!"
        hasEM := EndsStr('!', s0);
        if hasEM then
          s0 := Copy(s0, 1, Length(s0) - 1);

        uv := ParseUnitValue(s0, nil, AIsHor);
        try
          // If gap or has !, use value for min
          // Always use value for pref
          // If has !, use value for max too
          if AIsGap or hasEM then
          begin
            if hasEM then
              uvMax := uv
            else
              uvMax := nil;
            Result := TfpgMigBoundSize.Create(uv, uv, uvMax, push);
          end
          else
            Result := TfpgMigBoundSize.Create(nil, uv, nil, push);
        finally
          // BoundSize clones the UnitValues, so free our temporary
          uv.Free;
        end;
      end;

    2:
      begin
        // min:pref
        uv1 := ParseUnitValue(s0, nil, AIsHor);
        uv2 := ParseUnitValue(sizes[1], nil, AIsHor);
        try
          Result := TfpgMigBoundSize.Create(uv1, uv2, nil, push);
        finally
          // BoundSize clones the UnitValues, so free our temporaries
          uv1.Free;
          uv2.Free;
        end;
      end;

    3:
      begin
        // min:pref:max
        uv1 := ParseUnitValue(s0, nil, AIsHor);
        uv2 := ParseUnitValue(sizes[1], nil, AIsHor);
        uv3 := ParseUnitValue(sizes[2], nil, AIsHor);
        try
          Result := TfpgMigBoundSize.Create(uv1, uv2, uv3, push);
        finally
          // BoundSize clones the UnitValues, so free our temporaries
          uv1.Free;
          uv2.Free;
          uv3.Free;
        end;
      end;
  else
    raise Exception.Create('Min:Preferred:Max size section must contain 0, 1 or 2 colons. ''' + AStr + '''');
  end;
end;

function ParseInsets(const AStr: string; AAcceptPanel: Boolean): TfpgMigUnitValueArray;
var
  isPanel: Boolean;
  j: Integer;
  insS: TStringArray;
  insSz: TfpgMigUnitValue;
  defaultInset: TfpgMigUnitValue;
begin
  SetLength(Result, 4);

  // Handle dialog/panel defaults
  // Return singleton references directly (caller must clone if it needs ownership)
  if (AStr = '') or (AStr = 'dialog') or (AStr = 'panel') then
  begin
    if not AAcceptPanel then
      raise Exception.Create('Insets not allowed: ' + AStr);

    isPanel := (AStr <> '') and (AStr[1] = 'p');
    for j := 0 to 3 do
    begin
      if isPanel then
        Result[j] := TfpgMigPlatformDefaults.GetPanelInsets(j)
      else
        Result[j] := TfpgMigPlatformDefaults.GetDialogInsets(j);
    end;
    Exit;
  end;

  // Parse space-separated values: "10 20 30 40"
  insS := ToTrimmedTokens(AStr, ' ');

  for j := 0 to 3 do
  begin
    // Use last value if not enough values provided
    // j % 2 == 1 means left/right (horizontal)
    if j < Length(insS) then
      insSz := ParseUnitValue(insS[j], UnitValueZero, (j mod 2) = 1)
    else
      insSz := ParseUnitValue(insS[High(insS)], UnitValueZero, (j mod 2) = 1);

    if insSz <> nil then
      Result[j] := insSz
    else
      Result[j] := TfpgMigPlatformDefaults.GetPanelInsets(j);
  end;
end;

end.
