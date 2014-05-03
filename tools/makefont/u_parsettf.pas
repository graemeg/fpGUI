unit u_parsettf;

{$mode objfpc}

interface

uses
  Classes, SysUtils, StrUtils,
  fpg_dialogs;

type
  T_Parser= class(TObject)
    private
      OriginalSize: Longint;
      FEncoding: string;
      UnitsPerEm: Integer;
      Coef: Extended;
      BBox: array[0..3] of Smallint;
      NumHMetrix: Integer;
      NumGlyphs: Integer;
      Widths: array of Smallint;
      Chars: array of Word;
      PostScriptName: string;
      Embeddable: Boolean;
      Bold: Boolean;
      StemV: SmallInt;
      Ascender: SmallInt;
      Descender: SmallInt;
      CapHeight: SmallInt;
      ItalicAngle: Smallint;
      Flags: Integer;
      MissingWidth: SmallInt;
      UnderlinePos: Smallint;
      UnderlineThick: Smallint;
      IsFixedPitch: Boolean;
      CharWidth: array[0..255] of SmallInt;
      Differences: widestring;
      procedure ParseHead;
      procedure ParseHhea;
      procedure ParseMaxp;
      procedure ParseHmtx;
      procedure ParseCmap;
      procedure ParseName;
      procedure ParseOS2;
      procedure ParsePost;
      procedure ParseTtfFile(const FontFile: string);
      procedure PrepareEncoding;
      procedure MakedefinitionFile(FontFile: string);
      function MakeDifferences: widestring;
    public
      procedure MakeFont(const FontFile: string; const Encoding: string; Embed: Boolean);
      constructor Create(AOwner: TComponent);
    end;

var
  Parser: T_Parser;
  RepCourant: string;

implementation

uses
  u_data;

var
  Flux: TFileStream;
  CharNames: array[0..255] of string;
  CharCodes: array[0..255] of Word;
  CharBase: array[0..255] of string;

function Puissance(Base,Exposant: Integer): Integer;
begin
if Exposant> 1
then
  Puissance:= Base*Puissance(Base,Pred(Exposant))
else
  Puissance:= Exposant;
end;

function ReadULong(AFlux: TFileStream): Longword;
var
  ALong: Longword;
  Chaine: string;
  Cpt,Coef: Integer;
  Value: array of Longword;
begin
AFlux.Read(ALong,SizeOf(ALong));
Chaine:= IntToHex(ALong,8);
Chaine:= Copy(Chaine,7,2)+Copy(Chaine,5,2)+Copy(Chaine,3,2)+Copy(Chaine,1,2);
SetLength(Value,8);
for Cpt:= 1 to 8 do
  begin
  Coef:= 9-Cpt;
  case Chaine[Cpt] of
    '0'..'9':
      Value[Cpt]:= Puissance(16,Coef)*StrToInt(Chaine[Cpt]);
    'A':
      Value[Cpt]:= Puissance(16,Coef)*10;
    'B':
      Value[Cpt]:= Puissance(16,Coef)*11;
    'C':
      Value[Cpt]:= Puissance(16,Coef)*12;
    'D':
      Value[Cpt]:= Puissance(16,Coef)*13;
    'E':
      Value[Cpt]:= Puissance(16,Coef)*14;
    'F':
      Value[Cpt]:= Puissance(16,Coef)*15;
    end;
  end;
Result:= 0;
for Cpt:= 1 to 8 do
  Result:= Result+Value[Cpt];
end;

function ReadUShort(AFlux: TFileStream): Word;
var
  AWord: Word;
  Chaine: string;
  Cpt,Coef: Integer;
  Value: array of Word;
begin
AFlux.Read(AWord,SizeOf(AWord));
Chaine:= IntToHex(AWord,4);
Chaine:= Copy(Chaine,3,2)+Copy(Chaine,1,2);
SetLength(Value,4);
for Cpt:= 1 to 4 do
  begin
  Coef:= 5-Cpt;
  case Chaine[Cpt] of
    '0'..'9':
      Value[Cpt]:= Puissance(16,Coef)*StrToInt(Chaine[Cpt]);
    'A':
      Value[Cpt]:= Puissance(16,Coef)*10;
    'B':
      Value[Cpt]:= Puissance(16,Coef)*11;
    'C':
      Value[Cpt]:= Puissance(16,Coef)*12;
    'D':
      Value[Cpt]:= Puissance(16,Coef)*13;
    'E':
      Value[Cpt]:= Puissance(16,Coef)*14;
    'F':
      Value[Cpt]:= Puissance(16,Coef)*15;
    end;
  end;
Result:= 0;
for Cpt:= 1 to 4 do
  Result:= Result+Value[Cpt];
end;

function ReadShort(AFlux: TFileStream): Smallint;
var
  AWord: Word;
  Chaine: string;
  Cpt,Coef: Integer;
  Value: array of Word;
begin
AFlux.Read(AWord,SizeOf(AWord));
Chaine:= IntToHex(AWord,4);
Chaine:= Copy(Chaine,3,2)+Copy(Chaine,1,2);
SetLength(Value,4);
for Cpt:= 1 to 4 do
  begin
  Coef:= 5-Cpt;
  case Chaine[Cpt] of
    '0'..'9':
      Value[Cpt]:= Puissance(16,Coef)*StrToInt(Chaine[Cpt]);
    'A':
      Value[Cpt]:= Puissance(16,Coef)*10;
    'B':
      Value[Cpt]:= Puissance(16,Coef)*11;
    'C':
      Value[Cpt]:= Puissance(16,Coef)*12;
    'D':
      Value[Cpt]:= Puissance(16,Coef)*13;
    'E':
      Value[Cpt]:= Puissance(16,Coef)*14;
    'F':
      Value[Cpt]:= Puissance(16,Coef)*15;
    end;
  end;
Result:= 0;
for Cpt:= 1 to 4 do
  Result:= Result+Value[Cpt];
end;

procedure T_Parser.ParseHead;
var
  AWord: Word;
  ALong: Longword;
  MagicNumber: Longword;
  Cpt: Integer;
begin
for Cpt:= 1 to 3 do
  Flux.Read(ALong,SizeOf(ALong));                // skip 12 bytes - Version, FontRevision, ChecksumAdjustment
MagicNumber:= ReadULong(Flux);                   // 4 bytes - MagicNumber
if IntToHex(MagicNumber,4)<> '5F0F3CF5'
then
  begin
  ShowMessage('Incorrect magic number',True);
  Exit;
  end;
Flux.Read(AWord,SizeOf(AWord));                  // skip 2 bytes - Flags
UnitsPerEm:= ReadUShort(Flux);                   // 2 bytes - UnitsPerEm
Coef:= 1000/UnitsPerEm;
for Cpt:= 1 to 4 do
  Flux.Read(ALong,SizeOf(ALong));                // skip 16 bytes - Created, Modified
BBox[0]:= Round(Coef*ReadShort(Flux));                          // 2 bytes
BBox[1]:= Round(Coef*ReadShort(Flux));                          // 2 bytes
BBox[2]:= Round(Coef*ReadShort(Flux));                          // 2 bytes
BBox[3]:= Round(Coef*ReadShort(Flux));                          // 2 bytes
end;

procedure T_Parser.ParseHhea;
var
  AWord: Word;
  ALong: Longword;
  Cpt: Integer;
begin
Flux.Read(ALong,SizeOf(ALong));                  // skip 4 bytes
for Cpt:= 1 to 15 do
  Flux.Read(AWord,SizeOf(AWord));                // skip 30 bytes
NumHMetrix:= ReadUShort(Flux);                   // 2 bytes
end;

procedure T_Parser.ParseMaxp;
var
  ALong: Longword;
begin
Flux.Read(ALong,SizeOf(ALong));                  // skip 4 bytes
NumGlyphs:= ReadUShort(Flux);                    // 2 bytes
end;

procedure T_Parser.ParseHmtx;
var
  AWord: Word;
  Cpt: Integer;
begin
SetLength(Widths,NumGlyphs);
for Cpt:= 0 to Pred(NumHMetrix) do
  begin
  Widths[Cpt]:= ReadUShort(Flux);                // 2 bytes
  Flux.Read(AWord,SizeOf(AWord));                // skip 2 bytes - Lsb
  end;
if NumHMetrix< NumGlyphs
then
  for Cpt:= NumHMetrix to Pred(NumGlyphs) do
    Widths[Cpt]:= 0;
//MissingWidth:= Round(Coef*Widths[0]);
//for Cpt:= 0 to 255 do
//  CharWidth[Cpt]:= MissingWidth;
end;

procedure T_Parser.ParseCmap;
var
  AWord: Word;
  ALong: Longword;
  NumTables: Word;
  SubTableFormat: Word;
  SegCount: Word;
  Gid: Word;
  EndCount: array of Word;
  StartCount: array of Word;
  IDDelta: array of Word;
  IDRangeOffset: array of Word;
  Cpt,Cpt2: Integer;
  PlatformID,EncodingID: Word;
  Offset,Offset31,TableStartPos,TablePos: LongWord;
begin
TableStartPos:= Flux.Position;                   // memorize Table start position
Flux.Read(AWord,SizeOf(AWord));                  // skip 2 bytes - version
NumTables:= ReadUShort(Flux);                    // 2 bytes
Offset31:= 0;
for Cpt:= 1 to NumTables do
  begin
  PlatformID:= ReadUShort(Flux);                 // 2 bytes
  EncodingID:= ReadUShort(Flux);                 // 2 bytes
  Offset:= ReadULong(Flux);                      // 4 bytes - Offset of subtable
  if (PlatformID= 3) and (EncodingID= 1)
  then
    Offset31:= Offset;
  end;
if Offset31= 0
then
  begin
  ShowMessage('No unicode encoding found',True);
  Exit;
  end;
Flux.Position:= TableStartPos+Offset31;
SubTableFormat:= ReadUShort(Flux);               // 2 bytes - Format of subtable
if SubTableFormat<> 4
then
  begin
  ShowMessage('Unexpected subtable format',True);
  Exit;
  end;
Flux.Read(ALong,SizeOf(ALong));                  // skip 4 bytes - Length, language
SegCount:= Round(ReadUShort(Flux)/2);            // 2 bytes - Segments count
for Cpt:= 1 to 3 do
  Flux.Read(AWord,SizeOf(AWord));                // skip 6 bytes - SearchRange, EntrySelector, RangeShift
SetLength(EndCount,SegCount);
for Cpt:= 0 to Pred(SegCount) do
  EndCount[Cpt]:= ReadUShort(Flux);              // 2 bytes * SegCount
Flux.Read(AWord,SizeOf(AWord));                  // skip 2 bytes - ReservedPad
SetLength(StartCount,SegCount);
for Cpt:= 0 to Pred(SegCount) do
  StartCount[Cpt]:= ReadUShort(Flux);            // 2 bytes * SegCount
SetLength(IDDelta,SegCount);
for Cpt:= 0 to Pred(SegCount) do
  IDDelta[Cpt]:= ReadUShort(Flux);               // 2 bytes * SegCount
TablePos:= Flux.Position;                        // set position to Table offset
SetLength(IDRangeOffset,SegCount);
for Cpt:= 0 to Pred(SegCount) do
  IDRangeOffset[Cpt]:= ReadUShort(Flux);         // 2 bytes * SegCount
for Cpt:= 0 to Pred(SegCount) do
  begin
  if IDRangeOffset[Cpt]> 0
  then
    Flux.Position:= TablePos+2*Cpt+IDRangeOffset[Cpt];  // set position
  SetLength(Chars,Length(Chars)+EndCount[Cpt]);
  for Cpt2:= StartCount[Cpt] to EndCount[Cpt] do
    begin
    if Cpt2= 65535
    then
      Break;
    if IDRangeOffset[Cpt]> 0
    then
      begin
      Gid:= ReadUShort(Flux);
      if Gid> 0
      then
        Gid:= Gid+IDDelta[Cpt];
      end
    else
      Gid:= Cpt2+IDDelta[Cpt];
    if Gid>= 65536
    then
      Gid:= Gid-65536;
    if Gid> 0
    then
      Chars[Cpt2]:= Gid;
    end;
  end;
end;

procedure T_Parser.ParseName;
var
  AWord: Word;
  NameID: Word;
  Count: Word;
  Long: Word;
  StringOffset: Word;
  Offset: Word;
  TableStartPos: LongWord;
  Cpt,Cpt2: Integer;
  Chaine: string;
  CharIdent: Char;
begin
TableStartPos:= Flux.Position;                   // memorize Table start position
PostScriptName:= '';
Flux.Read(AWord,SizeOf(AWord));                  // skip 2 bytes - Format
Count:= ReadUShort(Flux);                        // 2 bytes
StringOffset:= ReadUShort(Flux);                 // 2 bytes
for Cpt:= 0 to Pred(Count) do
  begin
  for Cpt2:= 1 to 3 do
    Flux.Read(AWord,SizeOf(AWord));              // skip 6 bytes - PlatformID, encodingID, languageID
  NameID:= ReadUShort(Flux);                     // 2 bytes
  Long:= ReadUShort(Flux);                       // 2 bytes
  Offset:= ReadUShort(Flux);                     // 2 bytes
  if NameID= 6
  then
    begin
    Flux.Position:= TableStartPos+StringOffset+Offset;   // set position
    Chaine:= '';
    for Cpt2:= 1 to Long do
      begin
      Flux.Read(CharIdent,SizeOf(CharIdent));    // 1 byte
      Chaine:= Chaine+CharIdent;
      end;
    PostScriptName:= Chaine;
    Break;
    end;
  end;
end;

procedure T_Parser.ParseOS2;
var
  AWord: Word;
  ALong: Longword;
  Version: Word;
  FsType: Word;
  Cpt: Integer;
begin
Version:= ReadUShort(Flux);                     // 2 bytes
for Cpt:= 1 to 3 do
  Flux.Read(AWord,SizeOf(AWord));               // skip 6 bytes - xAvgCharWidth, usWeightClass, usWidthClass
FsType:= ReadUShort(Flux);                      // 2 bytes
Embeddable:= (FsType<> 2) and ((FsType and 512)= 0);
if not Embeddable
then
  begin
  ShowMessage('Font licence does not allow embedding',True);
  Exit;
  end;
for Cpt:= 1 to 13 do
  Flux.Read(ALong,SizeOf(ALong));               // skip 52 bytes
Bold:= (ReadUShort(Flux) and 32)<> 0 ;          // 2 bytes
if Bold
then
  StemV:= 120
else
  StemV:= 70;
Flux.Read(ALong,SizeOf(ALong));                 // skip 4 bytes - usFirstCharIndex, usLastCharIndex
Ascender:= Round(Coef*ReadShort(Flux));         // 2 bytes
Descender:= Round(Coef*ReadShort(Flux));        // 2 bytes
if Version>= 2
then
  begin
  for Cpt:= 1 to 4 do
    Flux.Read(ALong,SizeOf(ALong));             // skip 16 bytes
  CapHeight:= Round(Coef*ReadShort(Flux));      // 2 bytes
  end
else
  CapHeight:= Ascender;
end;

procedure T_Parser.ParsePost;
var
  AWord: Word;
  ALong: Longint;
begin
Flux.Read(ALong,SizeOf(ALong));                 // skip 4 bytes - Version
ItalicAngle:= ReadShort(Flux);                  // 2 bytes
Flux.Read(AWord,SizeOf(AWord));                 // skip 2 bytes - decimal part
UnderlinePos:= Round(Coef*ReadShort(Flux));     // 2 bytes
UnderlineThick:= Round(Coef*ReadShort(Flux));   // 2 bytes
IsFixedPitch:= ReadULong(Flux)<> 0;             // 4 bytes
Flags:= 32;                                     // non symbolic
if IsFixedPitch
then
  Flags:= Flags+1;
if ItalicAngle<> 0
then
  Flags:= Flags+64;
end;

procedure T_Parser.ParseTtfFile(const FontFile: string);
var
  Version: Longint;
  NumTables: Word;
  AWord: Word;
  ALong: Longword;
  TableIdent: array of string;
  TableOffset: array of Longword;
  Cpt,Cpt2: Integer;
  CharIdent: Char;
begin
Flux:= TFileStream.Create(FontFile,fmOpenRead);
try
  OriginalSize:= Flux.Size;
  Flux.Position:= 0;                              // Affset Table (starts at byte 0
  Flux.Read(Version,SizeOf(Version));             // 4 bytes
  NumTables:= ReadUShort(Flux);                   // 2 bytes - Number of Tables
  Flux.Read(ALong,SizeOf(ALong));                 // skip 4 bytes
  Flux.Read(AWord,SizeOf(AWord));                 // skip 2 bytes
  SetLength(TableIdent,NumTables);
  SetLength(TableOffset,NumTables);
  for Cpt:= 0 to Pred(NumTables) do               // Table Directory (start at byte 12)
    begin
    for Cpt2:= 1 to 4 do
      begin
      Flux.Read(CharIdent,SizeOf(CharIdent));     // 1 byte
      TableIdent[Cpt]:= TableIdent[Cpt]+CharIdent;
      end;
    Flux.Read(ALong,SizeOf(ALong));               // skip 4 bytes - Checksum
    TableOffset[Cpt]:= ReadULong(Flux);           // 4 bytes - Offset
    Flux.Read(ALong,SizeOf(ALong));               // skip 4 bytes - Length
    end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'head'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParseHead;                                  // lecture table "Head"
      Break;
      end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'hhea'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParseHhea;                                  // lecture table "Hhea"
      Break;
      end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'maxp'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParseMaxp;                                  // lecture table "Maxp"
      Break;
      end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'hmtx'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParseHmtx;                                  // lecture table "Hmtx"
      Break;
      end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'cmap'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParseCmap;                                  // lecture table "Cmap"
      Break;
      end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'name'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParseName;                                  // lecture table "Name"
      Break;
      end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'OS/2'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParseOS2;                                 // lecture table "OS/2"
      Break;
      end;
  for Cpt:= 0 to Pred(NumTables) do
    if TableIdent[Cpt]= 'post'
    then
      begin
      Flux.Position:= TableOffset[Cpt];
      ParsePost;                                  // lecture table "Post"
      Break;
      end;
finally
  Flux.Free;
  end;
end;

procedure T_Parser.PrepareEncoding;
var
  Cpt: Integer;
begin
if FEncoding= 'cp874'
then
  for Cpt:= 0 to 255 do
    begin
    CharNames[Cpt]:= cp874_n[Cpt];
    CharCodes[Cpt]:= cp874_v[Cpt];
    end;
if FEncoding= 'cp1250'
then
  for Cpt:= 0 to 255 do
    begin
    CharNames[Cpt]:= cp1250_n[Cpt];
    CharCodes[Cpt]:= cp1250_v[Cpt];
    end;
if FEncoding= 'cp1251'
then
  for Cpt:= 0 to 255 do
    begin
    CharNames[Cpt]:= cp1251_n[Cpt];
    CharCodes[Cpt]:= cp1251_v[Cpt];
    end;
if FEncoding= 'cp1252'
then
  for Cpt:= 0 to 255 do
    begin
    CharNames[Cpt]:= cp1252_n[Cpt];
    CharCodes[Cpt]:= cp1252_v[Cpt];
    end
else
  for Cpt:= 0 to 255 do
    CharBase[Cpt]:= cp1252_n[Cpt];
if FEncoding= 'cp1253'
then
  for Cpt:= 0 to 255 do
    begin
    CharNames[Cpt]:= cp1253_n[Cpt];
    CharCodes[Cpt]:= cp1253_v[Cpt];
    end;
end;

procedure T_Parser.MakedefinitionFile(FontFile: string);
var
  FileDlg: TfpgFileDialog;
  DestFile: TStringList;
  Chaine,Fichier: widestring;
  Cpt: Integer;
begin
DestFile:= TStringList.Create;
Chaine:= 'FontType=TrueType';
DestFile.Add(Chaine);
Chaine:= 'FontName='+PostScriptName;
DestFile.Add(Chaine);
Chaine:= 'Ascent='+IntToStr(Ascender);
DestFile.Add(Chaine);
Chaine:= 'Descent='+IntToStr(Descender);
DestFile.Add(Chaine);
Chaine:= 'CapHeight='+IntToStr(CapHeight);
DestFile.Add(Chaine);
Chaine:= 'Flags='+IntToStr(Flags);
DestFile.Add(Chaine);
Chaine:= 'FontBBox=';
for Cpt:= 0 to 3 do
  Chaine:= Chaine+IntToStr(BBox[Cpt])+' ';
Chaine:= Chaine+']';
DestFile.Add(Chaine);
Chaine:= 'ItalicAngle='+IntToStr(ItalicAngle);
DestFile.Add(Chaine);
Chaine:= 'StemV='+IntToStr(StemV);
DestFile.Add(Chaine);
Chaine:= 'MissingWidth='+IntToStr(MissingWidth);
DestFile.Add(Chaine);
Chaine:= 'FontUp='+IntToStr(UnderlinePos);
DestFile.Add(Chaine);
Chaine:= 'FontUt='+IntToStr(UnderlineThick);
DestFile.Add(Chaine);
Chaine:= 'Encoding='+FEncoding;
DestFile.Add(Chaine);
Chaine:= 'FontFile='+Copy(FontFile,1,Length(FontFile)-4)+'.z';
//Chaine:= 'FontFile='+FontFile;
DestFile.Add(Chaine);
Chaine:= 'OriginalSize='+IntToStr(OriginalSize);
DestFile.Add(Chaine);
if Differences> ''
then
  begin
  Chaine:= 'Diffs='+Differences;
  DestFile.Add(Chaine);
 end;
Chaine:= 'CharWidth=';
for Cpt:= 32 to 255 do
  Chaine:= Chaine+IntToStr(CharWidth[Cpt])+' ';
Chaine:= Chaine+']';
DestFile.Add(Chaine);
FileDlg:= TfpgFileDialog.Create(nil);
FileDlg.InitialDir:= RepCourant;
FileDlg.Filter:= 'Fichiers fnt |*.fnt';
FontFile:= StringReplace(FontFile,'-Regular','',[rfIgnoreCase]);
Fichier:= Copy(FontFile,1,Length(FontFile)-3)+'fnt';
FileDlg.FileName:= Fichier;
try
  if FileDlg.RunSaveFile
  then
    DestFile.SaveToFile(Fichier);
finally
  FileDlg.Free;
  DestFile.Free;
  end;
end;

function T_Parser.MakeDifferences: widestring;
var
  Cpt,Last: Integer;
begin
Result:= '';
Last:= 0;
for Cpt:=32 to 255 do
  if CharNames[Cpt]<> CharBase[Cpt]
  then
    begin
    if Cpt<> Succ(Cpt)
    then
      Result:= Result+IntToStr(Cpt)+' ';
    Last:= Cpt;
    Result:= Result+'/'+CharNames[Cpt]+' ';
    end;
end;

procedure T_Parser.MakeFont(const FontFile: string; const Encoding:string; Embed: Boolean);
var
  Cpt: Integer;
begin
FEncoding:= Encoding;
PrepareEncoding;
ParseTtfFile(FontFile);
MissingWidth:= Round(Coef*Widths[Chars[CharCodes[32]]]);
for Cpt:= 0 to 255 do
  begin
  if (Widths[Chars[CharCodes[Cpt]]]> 0) and (CharNames[Cpt]<> '.notdef')
  then
    CharWidth[Cpt]:= Round(Coef*Widths[Chars[CharCodes[Cpt]]])
  else
    CharWidth[Cpt]:= MissingWidth;
  end;
if Encoding<> 'cp1252'
then
  Differences:= MakeDifferences;
MakeDefinitionFile(FontFile);
end;

constructor T_Parser.Create(AOwner: TComponent);
begin
inherited Create;
end;

end.

