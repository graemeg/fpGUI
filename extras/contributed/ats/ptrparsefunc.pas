unit ptrparsefunc;

interface

procedure ppSkipSpaces(var ReadPtr : PChar; bufend : PChar);

function ppReadLine(var ReadPtr : PChar; bufend : PChar; var LineLength : integer) : boolean;

function ppReadTo(var ReadPtr : PChar; bufend : PChar; const stopchars : shortstring; var CharCount : integer) : boolean;

function ppCheckSymbol(var ReadPtr : PChar; bufend : PChar; const checkstring : shortstring) : boolean;
function ppCheckSymbolCI(var ReadPtr : PChar; bufend : PChar; const checkstring : shortstring) : boolean; // case insensitive

function ppSearchPattern(var ReadPtr : PChar; bufend : PChar; const checkstring : shortstring;
  var distance : integer) : boolean;

function ppMakeString(buf : pointer; len : integer) : string;

implementation

function ppMakeString(buf : pointer; len : integer) : string;
begin
  SetLength(result, len);
  if len > 0 then move(buf^, result[1], len);
end;

procedure ppSkipSpaces(var ReadPtr : PChar; bufend : PChar);
begin
  while (ReadPtr < bufend) and (ReadPtr^ in [#13,#10,#9,#32]) do
  begin
    inc(ReadPtr);
  end;
end;

function ppReadLine(var ReadPtr : PChar; bufend : PChar; var LineLength : integer) : boolean;
begin
  LineLength := 0;
  result := true;
  while (ReadPtr < bufend) do
  begin
    if ReadPtr^ = #10 then
    begin
      // unix line end
      inc(ReadPtr);
      exit;
    end
    else if ReadPtr^ = #13 then
    begin
      // DOS or Mac line end
      inc(ReadPtr);
      if (ReadPtr < bufend) and (ReadPtr^ = #10) then inc(ReadPtr);  // DOS line ending
      Exit;
    end;
    inc(LineLength);
    inc(ReadPtr);
  end;
  result := false;
end;

function ppReadTo(var ReadPtr : PChar; bufend : PChar; const stopchars : shortstring; var CharCount : integer) : boolean;
begin
  CharCount := 0;
  while (ReadPtr < bufend) do
  begin
    if pos(ReadPtr^,stopchars) > 0 then
    begin
      result := true;
      exit;
    end;
    inc(CharCount);
    inc(ReadPtr);
  end;
  Result := false;
end;


function ppCheckSymbol(var ReadPtr : PChar; bufend : PChar; const checkstring : shortstring) : boolean;
var
  rp : PChar;
  cc : integer;
begin
  result := false;
  cc := 1;
  rp := ReadPtr;
  while rp < bufend do
  begin
    if checkstring[cc] <> rp^ then
    begin
      EXIT;
    end
    else if cc >= length(checkstring) then
    begin
      ReadPtr := rp;
      inc(ReadPtr);
      result := true;
      EXIT;
    end;
    inc(cc);
    inc(rp);
  end;
end;

function ppCheckSymbolCI(var ReadPtr : PChar; bufend : PChar; const checkstring : shortstring) : boolean;
var
  rp : PChar;
  cc : integer;
begin
  result := false;
  cc := 1;
  rp := ReadPtr;
  while rp < bufend do
  begin
    if UpCase(checkstring[cc]) <> UpCase(rp^) then
    begin
      EXIT;
    end
    else if cc >= length(checkstring) then
    begin
      ReadPtr := rp;
      inc(ReadPtr);
      result := true;
      EXIT;
    end;
    inc(cc);
    inc(rp);
  end;
end;

function ppSearchPattern(var ReadPtr : PChar; bufend : PChar; const checkstring : shortstring;
  var distance : integer) : boolean;
var
  rp, cstartp : PChar;
  cc : integer;
begin
  result := false;
  cc := 1;
  cstartp := ReadPtr;
  rp := ReadPtr;
  while rp < bufend do
  begin
    if checkstring[cc] <> rp^ then
    begin
      // try the next position
      inc(cstartp);
      rp := cstartp;
      cc := 1;
    end
    else if cc >= length(checkstring) then
    begin
      inc(rp);
      distance := rp - ReadPtr;
      ReadPtr := rp;
      result := true;
      EXIT;
    end
    else
    begin
      inc(cc);
      inc(rp);
    end;
  end;
end;

end.

