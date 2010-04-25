unit ideutils;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,fpg_base
  ;


function tiNumToken(const AValue, AToken: string): integer;
function tiToken(const AValue, AToken: string; const APos: integer): string;
procedure ShowString(const AString: TfpgString; const AHeading: TfpgString);


implementation

uses
  fpg_form
  ,fpg_memo
  ,fpg_main
  ;


function tiNumToken(const AValue, AToken : string): integer;
var
  i, iCount : integer;
  lsValue : string;
begin
  Result := 0;
  if AValue = '' then
    Exit; //==>

  iCount := 0;
  lsValue := AValue;
  i := pos(AToken, lsValue);
  while i <> 0 do begin
    delete(lsValue, i, length(AToken));
    inc(iCount);
    i := pos(AToken, lsValue);
  end;
  Result := iCount + 1;
end;


function tiToken(const AValue, AToken : string; const APos : integer): string;
var
  i, iCount, iNumToken : integer;
  lsValue : string;
begin
  result := '';

  iNumToken := tiNumToken(AValue, AToken);
  if APos = 1 then
  begin
    if pos(AToken, AValue) = 0 then
      result := AValue
    else
      result := copy(AValue, 1, pos(AToken, AValue)-1);
  end
  else if (iNumToken < APos-1) or (APos<1) then
  begin
    result := '';
  end
  else
  begin
    { Remove leading blocks }
    iCount := 1;
    lsValue := AValue;
    i := pos(AToken, lsValue);
    while (i<>0) and (iCount<APos) do
    begin
      delete(lsValue, 1, i + length(AToken) - 1);
      inc(iCount);
      i := pos(AToken, lsValue);
    end;

    if (i=0) and (iCount=APos) then
      result := lsValue
    else if (i=0) and (iCount<>APos) then
      result := ''
    else
      result := copy(lsValue, 1, i-1);
  end;
end;

procedure ShowString(const AString: TfpgString; const AHeading: TfpgString);
var
  lForm: TfpgForm;
  lMemo: TfpgMemo;
begin
  lForm := TfpgForm.Create(nil);
  lMemo := TfpgMemo.Create(lForm);
  try
    lForm.WindowTitle := AHeading;
    lForm.Width       := 450;
    lForm.Height      := 250;
    lForm.WindowPosition := wpOneThirdDown;
    lForm.Name        := 'FormShowStrings';
    lMemo.Lines.Text  := AString;
    lMemo.FontDesc    := '#Edit2';
    lMemo.SetPosition(0, 0, lForm.Width, lForm.Height);
    lMemo.Align       := alClient;
    lForm.ShowModal;
  finally
    lForm.free;
  end;
end;


end.

