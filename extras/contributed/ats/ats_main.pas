unit ats_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TatsFormat = (atsPureText, atsPascalSource, atsCSV);

  TatsTextItem = record
    LangId : string;
    Text   : string;
  end;

  { TatsTextRow }

  TatsTextRow = class
  protected
    FTextId : string;
    FTexts : array of TatsTextItem;
  public
    constructor Create(const atextid : string);
    destructor Destroy; override;

    procedure SetText(const alangid, atext : string);
    procedure DeleteText(const alangid : string);

    function GetText(const alangid : string; var afound : boolean) : string;
    
    property TextId : string read FTextId;
  end;
  
  { TatsTextTable }

  TatsTextTable = class
  private
    FLangIds : TStringList;

    FTable : TStringList;

    FCurrentLangId : string;

  protected
    function FindRow(const atextid : string) : TatsTextRow;

    function AddRow(const atextid : string) : TatsTextRow;

  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Clear;

    procedure AddLang(const alangid : string);

    procedure SetText(const atextid, alangid, atext : string);
    function GetText(const atextid, alangid : string; var found : boolean) : string; overload;
    function GetText(const atextid : string) : string; overload;

    // later this can prepare a fast search table too:
    procedure SelectLang(const alangid : string);

    //procedure SetFallbackOrder(const alangids : string);

    property CurrentLang : string read FCurrentLangId;
    
  public

    function LoadFromFile(const afilename : string) : boolean;
    
    function LoadFromPascalFile(const afilename : string) : boolean;
    
    function LoadFromArray(const aarr : array of string) : boolean;
    
    procedure LoadPureText(const fdata : string);
    
    procedure SaveToFile(const afilename : string; aformat : TatsFormat);
    
    // utility functions
    function RowCount : integer;
    function GetRow(arow : integer) : TatsTextRow;

    property LangList : TStringList read FLangIds;
  end;

var
  atsTexts : TatsTextTable;

function atsGetText(const atextid : string) : string;
function atsText(const atextid : string) : string;

implementation

uses
  ptrparsefunc;

function atsGetText(const atextid : string) : string;
begin
  result := atsTexts.GetText(atextid);
end;

function atsText(const atextid : string) : string;
begin
  result := atsTexts.GetText(atextid);
end;

{ TatsTextRow }

constructor TatsTextRow.Create(const atextid : string);
begin
  FTextId := atextid;
  SetLength(FTexts,0);
end;

destructor TatsTextRow.Destroy;
var
  n : integer;
begin
  // good to be sure about freeing ansi string
  for n := 0 to length(FTexts)-1 do
  begin
    FTexts[n].LangId := '';
    FTexts[n].Text   := '';
  end;
  SetLength(FTexts,0);
end;

procedure TatsTextRow.SetText(const alangid, atext: string);
var
  n : integer;
begin
  // search for an existing
  for n := 0 to length(FTexts)-1 do
  begin
    if FTexts[n].LangId = alangid then
    begin
      FTexts[n].Text := atext;
      Exit; //==>
    end;
  end;
  // add as new
  n := length(FTexts);
  SetLength(FTexts,n+1);
  FTexts[n].LangId := alangid;
  FTexts[n].Text := atext;
end;

procedure TatsTextRow.DeleteText(const alangid: string);
var
  n : integer;
begin
  // search for an existing
  n := 0;
  while (n < length(FTexts)) and (FTexts[n].LangId <> alangid) do
  begin
    inc(n);
  end;

  // for ansi string safety
  if n < length(FTexts) then
  begin
    FTexts[n].LangId := '';
    FTexts[n].Text := '';
    inc(n);
  end;

  while (n < length(FTexts))  do
  begin
    FTexts[n-1] := FTexts[n];
    inc(n);
  end;

  SetLength(FTexts,length(FTexts)-1);
end;

function TatsTextRow.GetText(const alangid: string; var afound : boolean) : string;
var
  n : integer;
begin
  // search for an existing
  for n := 0 to length(FTexts)-1 do
  begin
    if FTexts[n].LangId = alangid then
    begin
      result := FTexts[n].Text;
      afound := true;
      Exit; //==>
    end;
  end;
  result := '';
  afound := false;
end;

{ TatsTextTable }

function TatsTextTable.FindRow(const atextid: string) : TatsTextRow;
var
  i : integer;
begin
  i := FTable.IndexOf(atextid);

  if i >= 0 then result := TatsTextRow(FTable.Objects[i])
            else result := nil;
end;

function TatsTextTable.AddRow(const atextid: string) : TatsTextRow;
begin
  result := FindRow(atextid);
  if result = nil then
  begin
    result := TatsTextRow.Create(atextid);
    FTable.AddObject(atextid, result);
  end;
end;

constructor TatsTextTable.Create;
begin
  FTable := TStringList.Create;
  FLangIds := TStringList.Create;
  FCurrentLangId := '';
end;

destructor TatsTextTable.Destroy;
begin
  Clear;
  FTable.Free;
  FLangIds.Free;
end;

procedure TatsTextTable.Clear;
var
  n : integer;
begin
  for n := 0 to FTable.Count - 1 do
  begin
    TatsTextRow(FTable.Objects[n]).Free;
  end;
  FTable.Clear;
  FLangIds.Clear;
end;

procedure TatsTextTable.AddLang(const alangid: string);
var
  i : integer;
begin
  i := FLangIds.IndexOf(alangid);
  if i < 0 then FLangIds.Add(alangid);
  if FCurrentLangId = '' then FCurrentLangId := alangid;
end;

procedure TatsTextTable.SetText(const atextid, alangid, atext: string);
var
  tr : TatsTextRow;
begin
  AddLang(alangid);
  tr := AddRow(atextid);
  tr.SetText(alangid, atext);
end;

function TatsTextTable.GetText(const atextid, alangid: string; var found: boolean): string;
var
  tr : TatsTextRow;
begin
  tr := FindRow(atextid);
  if tr <> nil then result := tr.GetText(alangid, found)
  else
  begin
    result := '';
    found := false;
  end;
end;

function TatsTextTable.GetText(const atextid: string): string;
var
  found : boolean;
begin
  result := GetText(atextid, FCurrentLangId, found);
  if not found then
  begin
    // some fallback mechanism
    result := '#'+atextid+'@'+FCurrentLangId;
  end;
end;

procedure TatsTextTable.SelectLang(const alangid: string);
var
  i : integer;
begin
  i := FLangIds.indexof(alangid);
  if i >= 0 then
  begin
    FCurrentLangId := alangid;
  end;
end;

function GetFileContent(const afilename : string) : string;
var
  f : file;
  toread, rcnt, brres : integer;
begin
  try
    AssignFile(f,afilename);
    Reset(f,1);
    
    toread := FileSize(f);
    rcnt := 0;
    
    SetLength(result, toread);
    
    repeat
      BlockRead(f, result[1+rcnt], toread, brres);

      if brres > 0 then
      begin
        inc(rcnt,brres);
        dec(toread,brres);
      end;
    until toread <= 0;

  finally
    CloseFile(f);
  end;
end;

procedure PutFileContent(const afilename : string; const adata : string);
var
  f : file;
  towrite, wcnt, wrres : integer;
begin
  try
    AssignFile(f,afilename);
    Rewrite(f,1);

    towrite := length(adata);
    wcnt := 0;

    repeat
      BlockWrite(f, adata[1+wcnt], towrite, wrres);

      if wrres > 0 then
      begin
        inc(wcnt,wrres);
        dec(towrite,wrres);
      end;
    until towrite <= 0;

  finally
    CloseFile(f);
  end;
end;


function TatsTextTable.LoadFromFile(const afilename: string): boolean;
var
  fdata : string;
begin
  fdata := GetFileContent(afilename);
  //writeln('File content:');
  //writeln(fdata);
  LoadPureText(fdata);
  
  result := true;
end;

function TatsTextTable.LoadFromPascalFile(const afilename : string) : boolean;
var
  fdata, data : string;
  rp,sp,ep : PChar; // read ptr, start ptr, end ptr
  len : integer;
  tid, lid, txt : string;
  inquote : boolean;
begin
  result := false;
  
  fdata := GetFileContent(afilename);
  
  if fdata = '' then Exit;

  sp := @fdata[1];
  ep := sp + length(fdata);

  rp := sp;

  // skipping UTF8 marker first
  ppCheckSymbol(rp, ep, #$EF#$BB#$BF);
  
  ppSkipSpaces(rp, ep);
  
  if not ppCheckSymbolCI(rp, ep, 'array') then EXIT;
  
  ppSkipSpaces(rp, ep);
  if not ppCheckSymbol(rp, ep, '[') then EXIT;

  if not ppSearchPattern(rp, ep, ']', len) then EXIT;
  
  ppSkipSpaces(rp, ep);
  if not ppCheckSymbolCI(rp, ep, 'of') then EXIT;
  ppSkipSpaces(rp, ep);
  if not ppCheckSymbolCI(rp, ep, 'string') then EXIT;
  ppSkipSpaces(rp, ep);
  if not ppCheckSymbol(rp, ep, '=') then EXIT;
  ppSkipSpaces(rp, ep);
  if not ppCheckSymbol(rp, ep, '(') then EXIT;

  ppSkipSpaces(rp, ep);
  
  data := '';
  
  inquote := false;

  while rp < ep do
  begin
    if not inquote then
    begin
      ppSkipSpaces(rp, ep);
      if ppCheckSymbol(rp, ep, '''') then
      begin
        inquote := true;
        sp := rp;
      end
      else if ppCheckSymbol(rp, ep, ',') then
      begin
        // just skip ip
        ppSkipSpaces(rp, ep);
      end
      else if ppCheckSymbol(rp, ep, ')') then
      begin
        // closing char
        break;
      end
      else
      begin
        // invalid char
        break;
      end;
    end;
    
    if inquote then
    begin
      if ppReadTo(rp, ep, '''', len) then
      begin
        // closing quote or double quote
        if len > 0 then data := data + ppMakeString(sp, len);
        inc(rp);
        if (rp < ep) and (rp^ = '''') then
        begin
          data := data + '''';
          inc(rp);
          sp := rp;
        end
        else
        begin
          data := data + #10;
          inquote := false;
        end;
      end;
    end;
  end; // while
  
  //writeln('Pure content:');
  //writeln(data);
  
  LoadPureText(data);

  result := true;
end;

function TatsTextTable.LoadFromArray(const aarr: array of string): boolean;
var
  fdata : string;
  n : integer;
begin
  fdata := '';
  for n := low(aarr) to high(aarr) do
  begin
    fdata := fdata + aarr[n] + #10;
  end;
  LoadPureText(fdata);
end;

procedure TatsTextTable.LoadPureText(const fdata: string);
var
  rp,sp,ep : PChar; // read ptr, start ptr, end ptr
  len : integer;
  tid, lid, txt : string;
begin
  if fdata = '' then Exit;

  sp := @fdata[1];
  ep := sp + length(fdata);

  rp := sp;

  // skipping UTF8 marker first
  ppCheckSymbol(rp, ep, #$EF#$BB#$BF);

  ppSkipSpaces(rp, ep);

  tid := '???';

  while rp < ep do
  begin
    sp := rp;
    // reading identifier (txt or lang)
    if ppReadTo(rp, ep, '=:', len) then
    begin
      if rp^ = ':' then
      begin
        // text id is this
        tid := trim(ppMakeString(sp, len));
        inc(rp); // skip ':'
      end
      else if rp^ = '=' then
      begin
        // lang id is this
        lid := trim(ppMakeString(sp,len));
        inc(rp); // skip '='

        ppSkipSpaces(rp, ep);

        if not ppCheckSymbol(rp,ep,'"') then
        begin
          // starting quote is missing
        end;

        txt := '';
        sp := rp;
        if ppReadTo(rp, ep, '"', len) then
        begin
          txt := ppMakeString(sp,len);
          inc(rp);
        end
        else
        begin
          // end quote is missing
        end;

        if txt <> '' then
        begin
          atsTexts.SetText(tid, lid, txt);
        end;
      end;

      ppSkipSpaces(rp, ep);
    end; // reading identifier
  end;
end;

procedure TatsTextTable.SaveToFile(const afilename: string; aformat: TatsFormat);
var
  sl : TStringList;
  tr : TatsTextRow;
  n,i : integer;
  s : string;
  b : boolean;
  fdata : string;
begin
  sl := TStringList.Create;
  
  for n:=0 to FTable.Count-1 do
  begin
    tr := TatsTextRow(FTable.Objects[n]);
    if aformat = atsCSV then
    begin
      s := '"'+tr.TextId+'"';
      for i := 0 to FLangIds.Count-1 do
      begin
        s := s + ',"' + tr.GetText(FLangIds[i],b) + '"';
      end;
      sl.Add(s);
    end
    else
    begin
      sl.Add(tr.TextID+':');
      for i := 0 to FLangIds.Count-1 do
      begin
        s := tr.GetText(FLangIds[i],b);
        if b then sl.Add('  '+FLangIds[i]+'="'+s+'"');
      end;
    end;
  end;
  
  // assembly the final buffer
  
  if aformat = atsCSV then
  begin
    fdata := '"ATSF1"';
    for i := 0 to FLangIds.Count-1 do
    begin
      fdata := fdata + ',"' + FLangIds[i] + '"';
    end;
    for n := 0 to sl.Count-1 do
    begin
      fdata := fdata + #13#10 + sl[n];
    end;
  end
  else if aformat = atsPascalSource then
  begin
    fdata := 'array[1..'+IntToStr(sl.Count)+'] of string = ('+#13#10;
    s := ' ';
    for n := 0 to sl.Count-1 do
    begin
      fdata := fdata + '  ' + s + QuotedStr(sl[n]) + #13#10;
      s := ',';
    end;
    fdata := fdata + #13#10 + ');' + #13#10;
  end
  else
  begin
    // pure text...
    fdata := ''; // no header
    for n := 0 to sl.Count-1 do
    begin
      fdata := fdata + sl[n] + #13#10;
    end;
  end;
  
  sl.Free;
  
  PutFileContent(afilename, fdata);
end;

function TatsTextTable.RowCount: integer;
begin
  result := FTable.Count;
end;

function TatsTextTable.GetRow(arow: integer): TatsTextRow;
begin
  result := TatsTextRow(FTable.Objects[arow]);
end;


initialization
begin
  atsTexts := TatsTextTable.Create;
end;

end.

