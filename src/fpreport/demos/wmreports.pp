unit wmreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fphttp, fpweb, fpreport;

Type

  { TGenerateReportModule }

  TGenerateReportModule = class(TCustomHTTPModule)
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TPageReportModule }

  TPageReportModule = class(TCustomHTTPModule)
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

  { TViewReportModule }

  TViewReportModule = class(TCustomHTTPModule)
  Public
    Procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;

implementation

uses udapp;

{ TViewReportModule }

procedure TViewReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

begin
end;

{ TGenerateReportModule }

procedure TGenerateReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
Var
  F,D : String;
  FRPT : TReportDemoApp;
  Fmt : TRenderFormat;
  FRunner : TReportRunner;

begin
  D:=ARequest.ContentFields.Values['demo'];
  if (D='') or (TReportDemoApplication.GetReportClass(D)=Nil) then
    Raise Exception.CreateFmt('Invalid or empty demo name : "%s"',[D]);
  F:=ARequest.ContentFields.Values['format'];
  Fmt:=High(TRenderFormat);
  While (fmt>rfDefault) and (CompareText(TReportDemoApplication.FormatName(fmt),F)<>0) do
    fmt:=Pred(fmt);
  if (fmt=rfDefault) then
    Raise Exception.CreateFmt('Invalid or empty format name : "%s"',[F]);
  FRunner:=TReportRunner.Create(Self);
  FRunner.ReportApp:=TReportDemoApplication.GetReportClass(D).Create(Self);
  FRunner.ReportApp.rpt:=TFPReport.Create(FRunner.ReportApp);
  FRunner.Format:=Fmt;
  FRunner.Execute;
end;

{ TPageReportModule }

procedure TPageReportModule.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);

Var
  L,RL : TStrings;
  I : Integer;
  F : TRenderFormat;

begin
  RL:=Nil;
  L:=TStringList.Create;
  try
    RL:=TStringList.Create;
    L.Add('<HTML><HEAD><TITLE>FPReport web demo</TITLE></HEAD>');
    L.Add('<BODY>');
    L.Add('<H1>Select report and output type</H1>');
    L.Add('<FORM ACTION="../Generate" METHOD=POST>');
    L.Add('Report: ');
    L.Add('<SELECT NAME="demo">');
    //
    TReportDemoApplication.GetRegisteredReports(RL);
    For I:=0 to RL.Count-1 do
      L.Add('<OPTION>'+RL[i]+'</option>');
    L.Add('</SELECT>');
    L.Add('</p>');
    L.Add('Format: ');
    L.Add('<SELECT NAME="format">');
    for F in TRenderFormat do
      if TReportDemoApplication.GetRenderClass(F)<>Nil then
      L.Add('<OPTION>'+TReportDemoApplication.FormatName(F)+'</option>');
    L.Add('</SELECT>');
    L.Add('</p>');
    L.Add('<INPUT TYPE="Submit" Value="Generate"/>');
    L.Add('</FORM>');
    L.Add('</BODY>');
    L.Add('</HTML>');
    AResponse.Content:=L.Text;
  finally
    L.Free;
  end;
end;

initialization
  TPageReportModule.RegisterModule('Page',True);
  TGenerateReportModule.RegisterModule('Generate',True);
  TViewReportModule.RegisterModule('View',True);
end.

