unit idemacros;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,fpg_base
  ,fpg_main
  ;

type
  TIDEMacro = class(TObject)
  public
    Name: string;
    Value: string;
    Description: string;
    constructor Create(AName, AValue, ADescription: TfpgString);
  end;


  TIDEMacroList = class(TObject)
  private
    FItems: TList;
    function    GetItems(AIndex: integer): TIDEMacro;
    procedure   SetItems(AIndex: integer; const AValue: TIDEMacro);
  public
    constructor Create;
    destructor  Destroy; override;
    function    Count: integer;
    function    FindByName(const MacroName: TfpgString): TIDEMacro;
    function    StrHasMacros(const s: TfpgString): boolean;
    function    SubstituteStr(var s: TfpgString): boolean;
    procedure   Add(NewMacro: TIDEMacro);
    procedure   Clear;
    procedure   Delete(AIndex: integer);
    procedure   SetValue(const MacroName, NewValue: TfpgString);
    property    Items[AIndex: integer]: TIDEMacro read GetItems write SetItems; default;
  end;


// lazy-man singleton of IDE Macros
function GMacroList: TIDEMacroList;


implementation

var
  uIDEMacroList: TIDEMacroList;


function GMacroList: TIDEMacroList;
begin
  if not Assigned(uIDEMacroList) then
    uIDEMacroList := TIDEMacroList.Create;
  Result := uIDEMacroList;
end;


{ TIDEMacro }

constructor TIDEMacro.Create(AName, AValue, ADescription: TfpgString);
begin
  Name := AName;
  Value := AValue;
  Description := ADescription;
end;


{ TIDEMacroList }

function TIDEMacroList.GetItems(AIndex: integer): TIDEMacro;
begin
  Result := TIDEMacro(FItems[AIndex]);
end;

procedure TIDEMacroList.SetItems(AIndex: integer; const AValue: TIDEMacro);
begin
  FItems[AIndex] := AValue;
end;

constructor TIDEMacroList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TIDEMacroList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TIDEMacroList.Count: integer;
begin
  Result := FItems.Count;
end;

function TIDEMacroList.FindByName(const MacroName: TfpgString): TIDEMacro;
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l := 0;
  r := FItems.Count-1;
  m := 0;
  while l <= r do
  begin
    m := (l+r) shr 1;
    Result := Items[m];
    cmp := AnsiCompareText(MacroName,Result.Name);
    if cmp < 0 then
      r := m-1
    else if cmp > 0 then
      l := m+1
    else
      exit;
  end;
  Result := nil;
end;

function TIDEMacroList.StrHasMacros(const s: TfpgString): boolean;
// search for ${
var
  p: Integer;
  Len: Integer;
begin
  Result := false;
  p := 1;
  Len := length(s);
  while (p < Len) do
  begin
    if s[p] = '$' then
    begin
      inc(p);
      if (p<Len) and (s[p]<>'$') then
      begin
        // skip macro function name
        while (p<Len) and (s[p]<>'{') do inc(p);
        if (p<Len) then
        begin
          Result:=true;
          exit;
        end;
      end
      else
      begin
        // $$ is not a macro
        inc(p);
      end;
    end else
      inc(p);
  end;
end;

function TIDEMacroList.SubstituteStr(var s: TfpgString): boolean;
begin

end;

procedure TIDEMacroList.Add(NewMacro: TIDEMacro);
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l := 0;
  r := FItems.Count-1;
  m := 0;
  while l <= r do
  begin
    m := (l+r) shr 1;
    cmp := AnsiCompareText(NewMacro.Name, Items[m].Name);
    if cmp < 0 then
      r := m-1
    else if cmp > 0 then
      l := m + 1
    else
      break;
  end;
  if (m < FItems.Count) and (AnsiCompareText(NewMacro.Name, Items[m].Name) > 0) then
    inc(m);
  FItems.Insert(m, NewMacro);
end;

procedure TIDEMacroList.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count-1 do Items[i].Free;
  FItems.Clear;
end;

procedure TIDEMacroList.Delete(AIndex: integer);
begin
  Items[AIndex].Free;
  FItems.Delete(AIndex);
end;

procedure TIDEMacroList.SetValue(const MacroName, NewValue: TfpgString);
var
  lMacro: TIDEMacro;
begin
  lMacro := FindByName(MacroName);
  if lMacro <> nil then
    lMacro.Value := NewValue;
end;


initialization
  uIDEMacroList := nil;

finalization
  uIDEMacroList.Free;

end.

