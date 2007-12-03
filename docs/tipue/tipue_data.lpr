program tipue_data;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils;
  
type
  TContentParser = class(TObject)
  private
    FList: TStringList;
    index: integer;
    procedure   ParseCntFile(AFilename: string);
    procedure   WriteDataFile;
    function    ExtractDescription(AFilename: string): string;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Run;
  end;
  
  
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

function tiToken(const AValue, AToken: string; const APos: integer): string;
var
  i, iCount, iNumToken: integer;
  lsValue: string;
begin
  result := '';

  iNumToken := tiNumToken(AValue, AToken);
  if APos = 1 then begin
    if pos(AToken, AValue) = 0 then result := AValue
    else result := copy(AValue, 1, pos(AToken, AValue)-1);
    end
  else if (iNumToken < APos-1) or (APos<1) then begin
    result := '';
    end
  else begin

    { Remove leading blocks }
    iCount := 1;
    lsValue := AValue;
    i := pos(AToken, lsValue);
    while (i<>0) and (iCount<APos) do begin
      delete(lsValue, 1, i + length(AToken) - 1);
      inc(iCount);
      i := pos(AToken, lsValue);
    end;

    if (i=0) and (iCount=APos) then result := lsValue
    else if (i=0) and (iCount<>APos) then
      result := ''
    else
      result := copy(lsValue, 1, i-1);
  end;
end;

function tiGetTickCount: Cardinal;
begin
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;


{ TContentParser }

procedure TContentParser.ParseCntFile(AFilename: string);
const
  cLine = 's[%d] = "%s^%s^%s^%s^0"';
var
  sl: TStringList;
  l: integer;
  s: string;
  prefix: string;
  desc: string;
begin
  s := '#';
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    l := sl.IndexOf(':link tree');
    while (s <> '') or (s <> ':classes') do
    begin
//      writeln(' prosessing.... ', s);
      if (s = '') or (s =':classes') then
        break
      else if s[1] = '#' then
      begin
        delete(s, 1, 1);
        prefix := lowercase(tiToken(s, ' ', 1)) + PathDelim;
      end
      else
      begin
        desc := ExtractDescription(prefix + tiToken(trim(s), ' ', 2));
        FList.Add(Format(cLine, [
            index,                                // Array element number
            tiToken(trim(s), ' ', 1),             // Title
            prefix + tiToken(trim(s), ' ', 2),    // URL
            desc,                                 // Display Description
            desc                                  // Searchable Description
            ]));
        inc(index);
      end;

      if l+1 < sl.Count then
      begin
        inc(l);
        s := trim(sl[l]);
      end
      else
        break;
    end;  { while }
  finally
    sl.Free;
  end;
end;

procedure TContentParser.WriteDataFile;
begin
  FList.Insert(0, 'var s = new Array()');
  FList.Insert(1, '');
  FList.SaveToFile('tip_data.js');
end;

function TContentParser.ExtractDescription(AFilename: string): string;
var
  sl: TStringList;
  idx: integer;
  s: string;
begin
  sl := TStringList.Create;
  try
  sl.LoadFromFile('./' + AFilename);
  idx := sl.IndexOf('<h2>Declaration</h2>') -1;
  if idx >= 0 then
  begin
    s := Copy(sl[idx], 4, Length(sl[idx]) -8);
    Result := s;
  end
  else
    Result := '';
  except
      Result := '';
  end;
  sl.Free;
end;

constructor TContentParser.Create;
begin
  FList := TStringList.Create;
  index := 0;
end;

destructor TContentParser.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TContentParser.Run;
begin
  ParseCntFile('./corelib.cnt');
  ParseCntFile('./gui.cnt');
  WriteDataFile;
end;


var
  p: TContentParser;
  start: TDateTime;
begin
  p := TContentParser.Create;
  try
    start := Now;
    p.Run;
    writeln('Data processed in ', FormatDateTime('ss.zzz', Now - start), ' seconds.');
  finally
    p.Free;
  end;
end.

