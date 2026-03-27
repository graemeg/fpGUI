unit RecursiveSearchThread;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_utils;

type
  TRecursiveSearchThread = class(TThread)
  private
    FStartDir: TfpgString;
    procedure ProcessDirectory(dir: TfpgString);
  protected
    procedure Execute; override;
  public
    property StartDir: TfpgString read FStartDir write FStartDir;
  end;


implementation

uses
  frm_main; // to get access to the QueueForm instance

{ TRecursiveSearchThread }

procedure TRecursiveSearchThread.ProcessDirectory(dir: TfpgString);
var
  sr: TSearchRec;
begin
  if not Terminated then
  begin
    if fpgFindFirst(dir + AllFilesMask, faReadOnly + faDirectory, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory) = faDirectory then
        begin
          if (sr.name <> '.') and (sr.name <> '..') then
            ProcessDirectory(IncludeTrailingPathDelimiter(dir + sr.Name));
        end
        else
        begin
          { QueueForm.Queue is thread-safe — call it directly without
            Synchronize so the search thread is never blocked. }
          QueueForm.Queue(dir + sr.name);
        end;
      until (fpgFindNext(sr) <> 0) or terminated;
      FindClose(sr);
    end;
  end;
end;

procedure TRecursiveSearchThread.Execute;
begin
  writeln(' - TRecursiveSearchThread.Execute');
  ProcessDirectory(IncludeTrailingPathDelimiter(StartDir));
end;

end.

