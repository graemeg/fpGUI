{$mode objfpc}{$H+}
unit bin2pas;
interface
uses SysUtils, classes;

function ConvertImage(const AFileName: string): string;



implementation
uses fpg_utils;

function ConvertImage(const AFileName: string): string;
const
  Prefix = '     ';
  MaxLineLength = 72;
var
  InStream: TFileStream;
  I, Count: longint;
  b: byte;
  Line, ToAdd: String;
  ConstName: string;

  procedure WriteStr(const St: string);
  begin
    Result := Result + St;
  end;

  procedure WriteStrLn(const St: string);
  begin
    Result := Result + St + LineEnding;
  end;

  function sanitize(const s: string): string;
  var
    x: integer;
  begin
    result:=s;
    for x:=1 to length(result) do
      if not (result[x] in ['0'..'9', 'A'..'Z', 'a'..'z', '_']) then
        result[x]:='_';
  end;

begin
  b := Byte(0);
  result:='';
  InStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    ConstName := 'newimg_' + sanitize(ChangeFileExt(fpgExtractFileName(AFileName), ''));
    WriteStrLn('');
    WriteStrLn('const');

    InStream.Seek(0, soFromBeginning);
    Count := InStream.Size;
    WriteStrLn(Format('  %s: array[0..%d] of byte = (',[ConstName, Count-1]));
    Line := Prefix;
    for I := 1 to Count do
    begin
      InStream.Read(B, 1);
      ToAdd := Format('%3d',[b]);
      if I < Count then
        ToAdd := ToAdd + ',';
      Line := Line + ToAdd;
      if Length(Line) >= MaxLineLength then
      begin
        WriteStrLn(Line);
        Line := PreFix;
      end;
    end; { for }
    WriteStrln(Line+');');
    WriteStrLn('');
  finally
    InStream.Free;
  end;
end;


end.
