program webdemo;

{$mode objfpc}{$H+}

uses
  fpmimetypes,sysutils, httproute, fphttpapp, regreports, wmreports, fpwebfile;

Procedure RegisterModules(Const StartFile : String);

begin
  TPageReportModule.RegisterModule('Page',True);
  TGenerateReportModule.RegisterModule('Generate',True);
  TViewReportModule.RegisterModule('View',True);
  TReportListModule.RegisterModule('ReportList',True);
  if (StartFile<>'') then
    RegisterFileLocation('Start',ExtractFilePath(StartFile));
  HTTPRouter.RegisterRoute('/*',@ShowPage,true);
end;

begin
{$ifndef windows}
  MimeTypesFile:='/etc/mime.types';
{$endif}
  With Application do
    begin
    Port:=8080;
    AllowDefaultModule:=True;
    RegisterModules(GetOptionValue('s','start'));
    DefaultModuleName:='Page';
    if IsConsole then
      begin
      Writeln('Point your browser to http://localhost:',Port,'/Page  or http://localhost:',Port);
      if HasOption('s','start') then
        Writeln('An alternate start location is available at http://localhost:',Port,'/Start/',ExtractFileName(GetOptionValue('s','start')));
      end;
    PreferModuleName:=True;
   Initialize;
   Run;
   end;
end.

