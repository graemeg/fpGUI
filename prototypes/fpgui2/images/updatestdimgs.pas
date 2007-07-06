program updatestdimgs;

{$IFDEF FPC}
    {$mode delphi}
    {$H+}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses SysUtils, {$ifdef Win32}Windows{$else}linux{$endif};

var
  sr : TSearchRec;
  i,p : integer;
  s : string;
  cmdline : string;
begin
  i := FindFirst('*.bmp',faAnyFile,sr);
  while i=0 do
  begin
    s := sr.Name;
    p := pos('.bmp',s);
    if p > 0 then s := copy(s,1,p-1);
    cmdline := 'bin2obj -c stdimg_'+s+' '+sr.Name;
    WinExec(PChar(cmdline),0);
    i := FindNext(sr);
  end;
end.
