program docview;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$apptype gui}{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_main, frm_main;

{$IFDEF WINDOWS}
  {$R docview.rc}
{$ENDIF}

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;

  // always load custom style for help viewer
  //if Assigned(fpgStyle) then
  //  fpgStyle.Free;
  //fpgStyle := TMyStyle.Create;

  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

