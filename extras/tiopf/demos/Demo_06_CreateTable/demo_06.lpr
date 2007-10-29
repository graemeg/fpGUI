program demo_06;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpgfx, frm_main, tiOPFManager;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  
  // Change the connection string to suite your database location
  // ** Remote connection
//  gTIOPFManager.ConnectDatabase('192.168.0.54|/home/graemeg/programming/data/tiopf.fdb', 'sysdba', 'masterkey');
  // ** Local connection
  gTIOPFManager.ConnectDatabase('/home/graemeg/programming/data/tiopf.fdb', 'sysdba', 'masterkey');

  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
    gTIOPFManager.DisconnectDatabase;
    gTIOPFManager.Terminate;
  end;
end;

begin
  MainProc;
end.


