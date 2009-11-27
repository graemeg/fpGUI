unit filestreamhelper; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFileTextStream = class(TFileStream)
  public
    procedure   WriteLn(const fmt: String; const args: array of const);
    procedure   WriteLn(const s: String);
  end;

implementation

{ TFileTextStream }

procedure TFileTextStream.WriteLn(const fmt: String; const args: array of const);
var
  temp: String;
begin
  temp := Format(fmt, args) + LineEnding;
  Write(temp[1], Length(temp));
end;

procedure TFileTextStream.WriteLn(const s: String);
begin
  self.WriteLn('%s', [s]);
end;

end.

