program demo_08;

{$mode objfpc}{$H+}

// Which persistence layer to activate. (Un)Comment the one you want to use
{.$define UseFBL}
{$define UseSqldbIB}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpgfx, frm_main,
  Client_DBIndependentVisitors_Svr, Client_BOM,
  Client_AutoMap_Svr, Client_HardCodedVisitors_Svr, tiOPFManager,
  tiConstants{, tiLog, tiLogToConsole};

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;

  {$IFDEF UseFBL}
  if GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(cTIPersistFBL) = nil then
    raise Exception.Create('The system failed to find the <' + cTIPersistFBL + '> persistence layer')
  else
    GTIOPFManager.DefaultPersistenceLayerName := cTIPersistFBL;
  {$ENDIF}
  {$IFDEF UseSqldbIB}
  if GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(cTIPersistSqldbIB) = nil then
    raise Exception.Create('The system failed to find the <' + cTIPersistSqldbIB + '> persistence layer')
  else
    GTIOPFManager.DefaultPersistenceLayerName := cTIPersistSqldbIB;
  {$ENDIF}

  { Change the connection string to suite your database location }
  // ** Remote connection
  //gTIOPFManager.ConnectDatabase('192.168.0.54|/home/graemeg/programming/data/tiopf.fdb', 'sysdba', 'masterkey');
  // ** Local connection
  gTIOPFManager.ConnectDatabase('/home/graemeg/programming/data/tiopf.fdb', 'sysdba', 'masterkey');


  { Which persistence mechanism do you want to use? Uncomment one. }
  Client_AutoMap_Svr.RegisterMappings;
  //Client_HardCodedVisitors_Svr.RegisterVisitors;
  //Client_DBIndependentVisitors_Svr.RegisterVisitors;


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


