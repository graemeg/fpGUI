program demo_06;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  fpg_main, frm_main, tiOPFManager, tiConstants;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;

  if GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(cTIPersistFBL) = nil then
    raise Exception.Create('The system failed to find the <' + cTIPersistFBL + '> persistence layer')
  else
    GTIOPFManager.DefaultPersistenceLayerName := cTIPersistFBL;

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


