{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt,
    member of the Free Pascal development team

    FPReport exra functions for FPC expression parser.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpExtFuncs;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  DateUtils,
  fpexprpars;


procedure RegisterExtraFunctions;

implementation


procedure BuiltInCurrentDate(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := FormatDateTime(Args[0].ResString, Date);
end;

procedure BuiltInCurrentTime(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := FormatDateTime(Args[0].ResString, Time);
end;

procedure BuiltInCurrentDateTime(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := FormatDateTime(Args[0].ResString, Now);
end;

procedure BuiltInScanDateTime(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResDateTime := ScanDateTime(Args[0].ResString, Args[1].ResString, Args[2].ResInteger);
end;

procedure BuiltInMonthOf(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := MonthOf(Args[0].ResDateTime);
end;

procedure BuiltInDayOf(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := DayOf(Args[0].ResDateTime);
end;

procedure BuiltInYearOf(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := YearOf(Args[0].ResDateTime);
end;

procedure BuiltInHourOf(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := HourOf(Args[0].ResDateTime);
end;

procedure BuiltInMinuteOf(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := MinuteOf(Args[0].ResDateTime);
end;

procedure BuiltInSecondOf(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := SecondOf(Args[0].ResDateTime);
end;

procedure BuiltInFormatFloat(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := FormatFloat(Args[0].ResString, Args[1].ResFloat);
end;

{ For example:  Memo.Text := '[IntToStrEx(1234,''%4.4x (%0:d)'')]'; }
procedure BuiltInIntToStrEx(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := Format(Args[1].ResString, [Args[0].ResInteger]);
end;

{ First letter uppercased, rest untouched. }
procedure BuiltInPrettyCase(var Result : TFPExpressionResult; const Args : TExprParameterArray);
var
  sBuffer: string;
begin
  sBuffer := Args[0].ResString;
  Result.ResString := UpperCase(Copy(sBuffer, 1, 1)) + Copy(sBuffer, 2, Length(sBuffer)-1);
end;

procedure BuiltInInsert(var Result : TFPExpressionResult; const Args : TExprParameterArray);
var
  s: string;
begin
  s := Args[1].ResString;
  Insert(Args[0].ResString, s, Args[2].ResInteger);
  Result.ResString := s;
end;

procedure BuiltInRPos(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := RPos(Args[0].ResString, Args[1].ResString);
end;

procedure BuiltInPosEx(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResInteger := PosEx(Args[0].ResString, Args[1].ResString, Args[2].ResInteger);
end;

procedure BuiltInPadLeft(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := PadLeft(Args[0].ResString, Args[1].ResInteger);
end;

procedure BuiltInPadRight(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := PadRight(Args[0].ResString, Args[1].ResInteger);
end;

procedure BuiltInPadCenter(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := PadCenter(Args[0].ResString, Args[1].ResInteger);
end;

procedure BuiltInExtractWord(var Result : TFPExpressionResult; const Args : TExprParameterArray);
begin
  Result.ResString := ExtractWord(Args[0].ResInteger, Args[1].ResString, StdWordDelims);
end;

{ Extract Nth word. Use chars from string parameter as word separators. }
procedure BuiltInExtractWordEx(var Result : TFPExpressionResult; const Args : TExprParameterArray);
var
  a: TSysCharSet;
  i: integer;
  s: string;
begin
  a := [];
  s := Args[2].ResString;
  for i := 1 to Length(s) do
    a := a + [s[i]];
  Result.ResString := ExtractWord(Args[0].ResInteger, Args[1].ResString, a);
end;


procedure RegisterExtraFunctions;
begin
  with BuiltinIdentifiers do
  begin
    AddFunction(bcDateTime, 'CurrentDate', 'S', 'S', @BuiltInCurrentDate);
    AddFunction(bcDateTime, 'CurrentTime', 'S', 'S', @BuiltInCurrentTime);
    AddFunction(bcDateTime, 'CurrentDateTime', 'S', 'S', @BuiltInCurrentDateTime);
    AddFunction(bcDateTime, 'ScanDateTime', 'D', 'SSI', @BuiltInScanDateTime);
    AddFunction(bcDateTime, 'MonthOf', 'I', 'D', @BuiltInMonthOf);
    AddFunction(bcDateTime, 'DayOf', 'I', 'D', @BuiltInDayOf);
    AddFunction(bcDateTime, 'YearOf', 'I', 'D', @BuiltInYearOf);
    AddFunction(bcDateTime, 'HourOf', 'I', 'D', @BuiltInHourOf);
    AddFunction(bcDateTime, 'MinuteOf', 'I', 'D', @BuiltInMinuteOf);
    AddFunction(bcDateTime, 'SecondOf', 'I', 'D', @BuiltInSecondOf);
    {$IFNDEF VER3}
    // AddFunction(bcConversion, 'FormatFloat', 'S', 'SF', @BuiltInFormatFloat);
    {$ENDIF VER3}
    AddFunction(bcConversion, 'IntToStrEx', 'S', 'IS', @BuiltInIntToStrEx);
    AddFunction(bcStrings, 'PrettyCase', 'S', 'S', @BuiltInPrettyCase);
    AddFunction(bcStrings, 'Insert', 'S', 'SSI', @BuiltInInsert);
    AddFunction(bcStrings, 'RPos', 'I', 'SS', @BuiltInRPos);
    AddFunction(bcStrings, 'PosEx', 'I', 'SSI', @BuiltInPosEx);
    AddFunction(bcStrings, 'PadLeft', 'S', 'SI', @BuiltInPadLeft);
    AddFunction(bcStrings, 'PadRight', 'S', 'SI', @BuiltInPadRight);
    AddFunction(bcStrings, 'PadCenter', 'S', 'SI', @BuiltInPadCenter);
    AddFunction(bcStrings, 'ExtractWord', 'S', 'IS', @BuiltInExtractWord);
    AddFunction(bcStrings, 'ExtractWordEx', 'S', 'ISS', @BuiltInExtractWordEx);
  end;
end;

end.

