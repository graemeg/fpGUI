{ Demo program for PDF Reporting engine }

program pdf_demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_main,
  U_Demo;

procedure MainProc;
begin
  fpgApplication.Initialize;
  F_Demo:= TF_Demo.Create(nil);
  try
    F_Demo.Show;
    fpgApplication.Run;
  finally
    F_Demo.Free;
  end;
end;

begin
  MainProc;
end.
