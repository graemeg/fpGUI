unit U_Demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifdef win32}
  ShellApi,
  {$endif}
  fpg_main, fpg_base,
  fpg_form, fpg_button, fpg_label, fpg_dialogs, fpg_utils, U_Report;

type
  TF_Demo = class(TfpgForm)
    private
      FReport: T_Report;
      L_Pdf: TfpgLabel;
      Bt_PdfEmptyPage: TfpgButton;
      Bt_PdfSimpleText: TfpgButton;
      Bt_PdfMultiPages: TfpgButton;
      Bt_PdfMultiSections: TfpgButton;
      Bt_PdfOutlines: TfpgButton;
      Bt_PdfCadres: TfpgButton;
      Bt_PdfColor: TfpgButton;
      Bt_PdfLines: TfpgButton;
      Bt_PdfGrid: TfpgButton;
      Bt_PdfGraph: TfpgButton;
      Bt_PdfSurf: TfpgButton;
      Bt_PdfImages: TfpgButton;
      L_Visu: TfpgLabel;
      Bt_VisuEmptyPage: TfpgButton;
      Bt_VisuSimpleText: TfpgButton;
      Bt_VisuMultiPages: TfpgButton;
      Bt_VisuMultiSections: TfpgButton;
      Bt_VisuOutlines: TfpgButton;
      Bt_VisuCadres: TfpgButton;
      Bt_VisuColor: TfpgButton;
      Bt_VisuLines: TfpgButton;
      Bt_VisuGrid: TfpgButton;
      Bt_VisuGraph: TfpgButton;
      Bt_VisuSurf: TfpgButton;
      Bt_VisuImages: TfpgButton;
      L_Print: TfpgLabel;
      Bt_PrintEmptyPage: TfpgButton;
      Bt_PrintSimpleText: TfpgButton;
      Bt_PrintMultiPages: TfpgButton;
      Bt_PrintMultiSections: TfpgButton;
      Bt_PrintOutlines: TfpgButton;
      Bt_PrintCadres: TfpgButton;
      Bt_PrintColor: TfpgButton;
      Bt_PrintLines: TfpgButton;
      Bt_PrintGrid: TfpgButton;
      Bt_PrintGraph: TfpgButton;
      Bt_PrintSurf: TfpgButton;
      Bt_PrintImages: TfpgButton;
      Bt_Exit: TfpgButton;
      procedure Bt_PdfEmptyPageClick(Sender: TObject);
      procedure Bt_PdfSimpleTextClick(Sender: TObject);
      procedure Bt_PdfMultiPagesClick(Sender: TObject);
      procedure Bt_PdfMultiSectionsClick(Sender: TObject);
      procedure Bt_PdfOutlinesClick(Sender: TObject);
      procedure Bt_PdfCadresClick(Sender: TObject);
      procedure Bt_PdfColorClick(Sender: TObject);
      procedure Bt_PdfLinesClick(Sender: TObject);
      procedure Bt_PdfGridClick(Sender: TObject);
      procedure Bt_PdfGraphClick(Sender: TObject);
      procedure Bt_PdfSurfClick(Sender: TObject);
      procedure Bt_PdfImagClick(Sender: TObject);
      procedure Bt_VisuEmptyPageClick(Sender: TObject);
      procedure Bt_VisuSimpleTextClick(Sender: TObject);
      procedure Bt_VisuMultiPagesClick(Sender: TObject);
      procedure Bt_VisuMultiSectionsClick(Sender: TObject);
      procedure Bt_VisuOutlinesClick(Sender: TObject);
      procedure Bt_VisuCadresClick(Sender: TObject);
      procedure Bt_VisuColorClick(Sender: TObject);
      procedure Bt_VisuLinesClick(Sender: TObject);
      procedure Bt_VisuGridClick(Sender: TObject);
      procedure Bt_VisuGraphClick(Sender: TObject);
      procedure Bt_VisuSurfClick(Sender: TObject);
      procedure Bt_VisuImagClick(Sender: TObject);
      procedure Bt_PrintEmptyPageClick(Sender: TObject);
      procedure Bt_PrintSimpleTextClick(Sender: TObject);
      procedure Bt_PrintMultiPagesClick(Sender: TObject);
      procedure Bt_PrintMultiSectionsClick(Sender: TObject);
      procedure Bt_PrintOutlinesClick(Sender: TObject);
      procedure Bt_PrintCadresClick(Sender: TObject);
      procedure Bt_PrintColorClick(Sender: TObject);
      procedure Bt_PrintLinesClick(Sender: TObject);
      procedure Bt_PrintGridClick(Sender: TObject);
      procedure Bt_PrintGraphClick(Sender: TObject);
      procedure Bt_PrintSurfClick(Sender: TObject);
      procedure Bt_PrintImagClick(Sender: TObject);
      procedure Bt_ExitClick(Sender: TObject);
      procedure PrintEmptyPage(Preview: Boolean);
      procedure PrintSimpleText(Preview: Boolean);
      procedure PrintMultiPages(Preview: Boolean);
      procedure PrintMultiSections(Preview: Boolean);
      procedure PrintOutlines(Preview: Boolean);
      procedure PrintCadres(Preview: Boolean);
      procedure PrintColor(Preview: Boolean);
      procedure PrintLines(Preview: Boolean);
      procedure PrintGrid(Preview: Boolean);
      procedure PrintGraph(Preview: Boolean);
      procedure PrintSurf(Preview: Boolean);
      procedure PrintImage(Preview: Boolean);
    public
      constructor Create(AOwner: TComponent); override;
    end;

var
  F_Demo: TF_Demo;

implementation

uses
  U_Command, U_Pdf, U_ReportImages;

var
  ChartValues: array[0..18] of Integer;

const
  Langue= 'F';

procedure TF_Demo.Bt_PdfEmptyPageClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintEmptyPage(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'EmptyPage.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfSimpleTextClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintSimpleText(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'SimpleText.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfMultiPagesClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintMultiPages(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'MultiPages.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfMultiSectionsClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintMultiSections(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'MultiSections.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfOutlinesClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintOutlines(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Outlines.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfCadresClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintCadres(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Cadres.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfColorClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintColor(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Color.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfLinesClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintLines(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Lines.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfGridClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintGrid(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Grid.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfGraphClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintGraph(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Graph.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfSurfClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintSurf(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Surface.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfImagClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
FReport:= T_Report.Create;
with FReport do
  begin
//  Language:= Version;
  PrintImage(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SavePdf:= TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SavePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SavePdf.Filter:= 'PDF files (*.pdf) |*.pdf';
  Fd_SavePdf.FileName:= 'Images.pdf';
  try
    if Fd_SavePdf.RunSaveFile
    then
      begin
      PdfFile:= Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile,Length(PdfFile)-3,4))<> '.pdf'
      then
         PdfFile:= PdfFile+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        PdfFileStream:= TFileStream.Create(PdfFile,fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(PdfFile);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(PdfFile),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SavePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuEmptyPageClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'EmptyPage.pdf';
  PrintEmptyPage(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuSimpleTextClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'SimpleText.pdf';
  PrintSimpleText(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuMultiPagesClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'MultiPages.pdf';
  PrintMultiPages(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuMultiSectionsClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'MultiSections.pdf';
  PrintMultiSections(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuOutlinesClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Outlines.pdf';
  PrintOutlines(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuCadresClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Cadres.pdf';
  PrintCadres(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuColorClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Color.pdf';
  PrintColor(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuLinesClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Lines.pdf';
  PrintLines(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuGridClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Grid.pdf';
  PrintGrid(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuGraphClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Graph.pdf';
  PrintGraph(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuSurfClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Surface.pdf';
  PrintSurf(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuImagClick(Sender: TObject);
begin
FReport:= T_Report.Create;
with FReport do
  begin
  //Language:= Version;
  DefaultFile:= 'Images.pdf';
  PrintImage(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_PrintEmptyPageClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintSimpleTextClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintMultiPagesClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintMultiSectionsClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintOutlinesClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintCadresClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintColorClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintLinesClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintGridClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintGraphClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintSurfClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_PrintImagClick(Sender: TObject);
begin

end;

procedure TF_Demo.Bt_ExitClick(Sender: TObject);
begin
Close;
end;

procedure TF_Demo.PrintEmptyPage(Preview: Boolean);
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins
  Section(0,0,0,0);
  // create an empty page
  Page;
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintSimpleText(Preview: Boolean);
var
  FtText1,FtText2,FtText3: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins: 10 mm each side
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtText1:= Font('helvetica-15:bold',clBlack);
  FtText2:= Font('helvetica-8',clBlack);
  FtText3:= Font('helvetica-8:italic',clBlack);
  // write the text at position 100 mm from left and 120 mm from top
  WritePage(100,120,'Big text at absolute position',-1,FtText1);
  // write the text aligned to left
  WritePage(cnLeft,50,'Text aligned to left',ColDefaut,FtText2);
  // write the text aligned to right
  WritePage(cnRight,75,'Text aligned to right',ColDefaut,FtText3);
  // write the text aligned to center
  WritePage(cnCenter,100,'Text aligned to center',ColDefaut,FtText2);
  // write a long text in the default column
  WritePage(cnLeft,150,'This long text is supposed to be written on two lines in ColDefaut (and does include parenthesis). If it does not, there is abviously a remaining bug in the way wrapping is performed.',ColDefaut,FtText2);
  // write a long text without columns
  WritePage(50,200,'This long text is supposed to be written on two lines whithout column (and does include parenthesis). If it does not, there is abviously a remaining bug in the way wrapping is performed.',-1,FtText2);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintMultiPages(Preview: Boolean);
var
  FtTitle,FtText: Integer;
  Cpt: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtText:= Font('helvetica-8',clBlack);
  // write title on each page
  WriteHeader(cnCenter,lnEnd,'MULTIPAGE DOCUMENT',ColDefaut,FtTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtText);
  // create five new empty pages
  for Cpt:= 1 to 5 do
    Page;
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintMultiSections(Preview: Boolean);
var
  FtTitleS1,FtTitleS2,FtTitleS3,FtText,FtNum,FtNumS: Integer;
  Cpt: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitleS1:= Font('helvetica-15:bold',clBlack);
  FtTitleS2:= Font('helvetica-14:italic',clBlack);
  FtTitleS3:= Font('helvetica-12:bold:italic',clBlack);
  FtText:= Font('helvetica-8',clBlack);
  FtNum:= Font('helvetica-7:italic',clBlack);
  FtNumS:= Font('helvetica-7:italic',clGray);
  // create a new section and define the margins
  Section(20,10,10,10);
  // write title on each page of the section
  WriteHeader(cnCenter,lnEnd,'MULTI SECTION DOCUMENT',ColDefaut,FtTitleS1);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnEnd,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionFooter(cnCenter,lnEnd,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnEnd,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 3 do
    Page;

  // create a new section and define the margins
  Section(10,10,10,10,0,oLandscape);
  // create a default column for section2 which is landscape oriented
  // write title on each page of the section
  WriteHeader(cnCenter,lnEnd,'MULTI SECTION DOCUMENT',ColDefaut,FtTitleS2);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnEnd,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnEnd,'Section page','of',True,True,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnEnd,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 2 do
    Page;

  // create a new section and define the margins
  Section(20,20,20,20);
  // write title on each page of the section
  WriteHeader(cnCenter,lnEnd,'MULTI SECTION DOCUMENT',ColDefaut,FtTitleS3);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnEnd,'Section','of',True,True,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnEnd,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnEnd,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 4 do
    Page;

  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintOutlines(Preview: Boolean);
var
  FtTitleS1,FtTitleS2,FtTitleS3,FtText,FtNum,FtNumS: Integer;
  Cpt: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitleS1:= Font('helvetica-15:bold',clBlack);
  FtTitleS2:= Font('helvetica-14:italic',clBlack);
  FtTitleS3:= Font('helvetica-12:bold:italic',clBlack);
  FtText:= Font('helvetica-8',clBlack);
  FtNum:= Font('helvetica-7:italic',clBlack);
  FtNumS:= Font('helvetica-7:italic',clGray);
  // create a new section and define the margins
  Section(20,10,10,10);
  // write title on each page of the section
  WriteHeader(cnCenter,lnEnd,'MULTI SECTION DOCUMENT',ColDefaut,FtTitleS1);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnEnd,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionFooter(cnCenter,lnEnd,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnEnd,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 3 do
    Page;

  // create a new section and define the margins
  Section(10,10,10,10,0,oLandscape);
  SectionTitle:= 'Landscape oriented';
  // write title on each page of the section
  WriteHeader(cnCenter,lnEnd,'MULTI SECTION DOCUMENT',ColDefaut,FtTitleS2);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnEnd,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnEnd,'Section page','of',True,True,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnEnd,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 2 do
    Page;

  // create a new section and define the margins
  Section(20,20,20,20);
  // write title on each page of the section
  WriteHeader(cnCenter,lnEnd,'MULTI SECTION DOCUMENT',ColDefaut,FtTitleS3);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnEnd,'Section','of',True,True,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnEnd,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnEnd,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 4 do
    Page;

  // preparation is finished, so create PDF objects
  Outline:= True;
  EndWrite;
  end;
end;

procedure TF_Demo.PrintCadres(Preview: Boolean);
var
  FtTitle,FtText: Integer;
  TsThin,TsNorm,TsThick: Integer;
  IlTitle,IlText: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10,5);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtText:= Font('helvetica-8',clBlack);
  // create the style of lines to be used
  TsThin:= LineStyle(0.2,clBlack,lsSolid);
  TsNorm:= LineStyle(1,clBlack,lsSolid);
  TsThick:= LineStyle(2,clBlack,lsSolid);
  // create line spacings to be used
  IlTitle:= LineSpace(3,0,3);
  IlText:= LineSpace(1,0,1);
  // write title on each page
  WriteHeader(cnCenter,lnEnd,'SHOWING FRAMES',ColDefaut,FtTitle,IlTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtText,IlText);
  // draw thin frame rectangle at margins
//  FrameMargins(TsThin);
  // draw thick frame rectangle at header
  FrameHeader(TsThick);
  // draw thick frame rectangle at footer
  FramePage(TsNorm);
  // draw normal frame rectangle at page
  FrameFooter(TsThick);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintColor(Preview: Boolean);
var
  FtTitle,FtNormBlack,FtNormRed,FtNormGreen,FtBoldBlue,FtItalGray,FtBoldItalFuchsia: Integer;
  BcBeige,BcAqua,BcPaleGreen: Integer;
  IlTitle,IlText: Integer;
  Col1,Col2,Col3: Integer;

begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the filling colors to be used
  BcBeige:= BackColor(clBeige);
  BcAqua:= BackColor(clAqua);
  BcPaleGreen:= BackColor(clPaleGreen);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtNormBlack:= Font('helvetica-8',clBlack);
  FtNormRed:= Font('helvetica-8',clRed);
  FtNormGreen:= Font('helvetica-8',clGreen);
  FtBoldBlue:= Font('helvetica-8:bold',clBlue);
  FtItalGray:= Font('helvetica-8:italic',clGray);
  FtBoldItalFuchsia:= Font('helvetica-8:bold:italic',clFuchsia);
  // create columns to be used
  Col1:= Column(20,100,2);
  Col2:= Column(120,80,1);
  Col3:= Column(70,100,5);
  // create line spacings to be used
  IlTitle:= LineSpace(5,0,5);
  IlText:= LineSpace(0,0,0);
  // write title on each page
  WriteHeader(cnCenter,lnEnd,'SHOWING COLORS',ColDefaut,FtTitle,IlTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtNormRed,IlText);
  // write some example texts
  WritePage(cnLeft,lnEnd,'Bold blue text aligned to left',ColDefaut,FtBoldBlue,IlText);
  SpacePage(10,ColDefaut,BcPaleGreen);
  WritePage(cnCenter,lnEnd,'followed by centered normal black text after a 1 cm colored space',ColDefaut,FtNormBlack,IlText);
  SpacePage(15);
  WritePage(cnLeft,lnEnd,'text written on colored background after a 1.5 cm colored space',ColDefaut,FtItalGray,IlText,BcAqua);
  SpacePage(10);
  WritePage(cnLeft,lnCurrent,'This text starts in column 1',Col1,FtNormGreen,IlText,BcBeige);
  WritePage(cnLeft,lnEnd,'and ends in column 2',Col2,FtBoldItalFuchsia,IlText);
  WritePage(cnCenter,lnEnd,'And this one is centered in column 3',Col3,FtNormRed,IlText,BcBeige);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintLines(Preview: Boolean);
var
  FtTitle,FtText: Integer;
  TsThinBlack,TsThinBlue,TsThinRed,TsThick: Integer;
  IlTitle,IlText: Integer;
  Col1,Col2,Col3: Integer;
  BdRect,BdColn,BdEndColn: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtText:= Font('helvetica-8',clBlack);
  // create the style of lines to be used
  TsThinBlack:= LineStyle(0.2,clBlack,lsSolid);
  TsThinBlue:= LineStyle(0.1,clBlue,lsDash);
  TsThick:= LineStyle(2,clBlack,lsSolid);
  TsThinRed:= LineStyle(1,clRed,lsDashDot);
  // create line spacings to be used
  IlTitle:= LineSpace(3,0,3);
  IlText:= LineSpace(0,0,0);
  // define column borders
  BdRect:= Border([bfLeft,bfRight,bfTop,bfBottom],TsThick);
  BdColn:= Border([bfLeft,bfRight,bfTop],TsThinBlue);
  BdEndColn:= Border([bfLeft,bfRight,bfTop,bfBottom],TsThinBlack);
  // create columns to be used
  Col1:= Column(20,60,2);
  Col2:= Column(80,60,2);
  Col3:= Column(140,60,2);
  // write title on each page
  WriteHeader(cnCenter,lnEnd,'SHOWING LINES',ColDefaut,FtTitle,IlTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtText,IlText);
  // write some example texts with column borders
  WritePage(cnLeft,lnCurrent,'Example of lines',Col1,FtText,IlText,-1,BdColn);
  WritePage(cnLeft,lnCurrent,'with column borders',Col2,FtText,IlText,-1,BdEndColn);
  WritePage(cnLeft,lnEnd,'',Col3,FtText);
  SpacePage(5);
  WritePage(cnLeft,lnEnd,'A thick border',Col3,FtText,IlText,-1,BdRect);
  HorizLinePage(2,2,Col2,TsThick);
  LinePage(30,100,150,150,TsThinBlack);
  LinePage(50,70,180,100,TsThinBlue);
  LinePage(40,140,160,80,TsThinRed);
  LinePage(60,50,60,120,TsThick);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintGrid(Preview: Boolean);
var
  FtTitle,FtText,FtTextBlue,FtTextRed,FtSTitle: Integer;
  TsThinBlack,TsThickBlue: Integer;
  IlTitle,IlText: Integer;
  BcBeige: Integer;
  Col: array[1..5] of Integer;
  BdColn,BdColnL,BdColnR: Integer;
  CptLin,CptCol: Integer;
  PosHoriz,PredPosHoriz: Single;
const
  Col1Pos= 20;
  Col1Wid= 40;
  Col2Pos= 60;
  Col2Wid= 35;
  Col3Pos= 95;
  Col3Wid= 35;
  Col4Pos= 130;
  Col4Wid= 35;
  Col5Pos= 165;
  Col5Wid= 35;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtText:= Font('helvetica-7',clBlack);
  FtTextBlue:= Font('helvetica-7',clBlue);
  FtTextRed:= Font('helvetica-7',clRed);
  FtSTitle:= Font('helvetica-9:bold:italic',clBlue);
  // create the style of lines to be used
  TsThinBlack:= LineStyle(0.5,clBlack,lsSolid);
  TsThickBlue:= LineStyle(1.5,clBlue,lsSolid);
  // create line spacings to be used
  IlTitle:= LineSpace(3,0,3);
  IlText:= LineSpace(1,0,0);
  // define column background color
  BcBeige:= BackColor(clBeige);
  // define column borders
  BdColn:= Border([bfLeft,bfRight],TsThinBlack);
  BdColnL:= Border([bfLeft],TsThickBlue);
  BdColnR:= Border([bfRight],TsThickBlue);
  // create columns to be used
  Col[1]:= Column(Col1Pos,Col1Wid,2);
  Col[2]:= Column(Col2Pos,Col2Wid,2);
  Col[3]:= Column(Col3Pos,Col3Wid,2);
  Col[4]:= Column(Col4Pos,Col4Wid,2);
  Col[5]:= Column(Col5Pos,Col5Wid,2);
  // write title on each page
  WriteHeader(cnCenter,lnEnd,'SHOWING GRIDS',ColDefaut,FtTitle,IlTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtText,IlText);
  // write a grid without borders
  WritePage(cnCenter,lnEnd,'Grid without borders',ColDefaut,FtSTitle,IlTitle);
  for CptLin:= 1 to 10 do
    for CptCol:= 1 to 5 do
      if CptCol= 5
      then
        WritePage(cnLeft,lnEnd,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtText,IlText)
      else
        WritePage(cnLeft,lnCurrent,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtText,IlText);
  SpacePage(5);
  // write a grid with borders
  PosHoriz:= WritePage(cnCenter,lnEnd,'Grid with borders and colors',ColDefaut,FtSTitle,IlTitle);
  LinePage(Col1Pos,PosHoriz,Col5Pos+Col5Wid,PosHoriz,TsThickBlue);
  for CptLin:= 1 to 10 do
    for CptCol:= 1 to 5 do
      if CptCol= 1
      then
        if CptLin mod 2= 0
        then
          WritePage(cnLeft,lnCurrent,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtText,IlText,BcBeige,BdColnL)
        else
          WritePage(cnLeft,lnCurrent,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTextBlue,IlText,-1,BdColnL)
      else
        if CptCol= 5
        then
          if CptLin= 10
          then
            begin
            PredPosHoriz:= PosHoriz;
            PosHoriz:= WritePage(cnLeft,lnEnd,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTextRed,IlText,BcBeige,BdColnR);
            LinePage(Col1Pos,PredPosHoriz,Col5Pos+Col5Wid,PredPosHoriz,TsThinBlack);
            LinePage(Col1Pos,PosHoriz,Col5Pos+Col5Wid,PosHoriz,TsThickBlue);
            LinePage(Col5Pos,PredPosHoriz,Col5Pos,PosHoriz,TsThinBlack);
            end
          else
            begin
            if CptLin= 1
            then
              PosHoriz:= WritePage(cnLeft,lnEnd,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtText,IlText,-1,BdColnR)
            else
              if CptLin mod 2= 0
              then
                begin
                PredPosHoriz:= PosHoriz;
                PosHoriz:= WritePage(cnLeft,lnEnd,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTextRed,IlText,BcBeige,BdColnR);
                LinePage(Col1Pos,PredPosHoriz,Col5Pos+Col5Wid,PredPosHoriz,TsThinBlack);
                LinePage(Col5Pos,PredPosHoriz,Col5Pos,PosHoriz,TsThinBlack);
                end
              else
                begin
                PredPosHoriz:= PosHoriz;
                PosHoriz:= WritePage(cnLeft,lnEnd,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtText,IlText,-1,BdColnR);
                LinePage(Col1Pos,PredPosHoriz,Col5Pos+Col5Wid,PredPosHoriz,TsThinBlack);
                end;
            end
        else
          if CptLin mod 2= 0
          then
            WritePage(cnLeft,lnCurrent,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtText,IlText,BcBeige,BdColn)
          else
            WritePage(cnLeft,lnCurrent,'line '+IntToStr(CptLin)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTextBlue,IlText,-1,BdColn);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintGraph(Preview: Boolean);
var
  FtTitle,FtText,FtMax: Integer;
  TsBlack,TsGray,TsBlue,TsFuchsia: Integer;
  IlTitle,IlText: Integer;
  Cpt,Max: Integer;
const
  Base= 150;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtText:= Font('helvetica-7',clBlack);
  FtMax:= Font('helvetica-7',clFuchsia);
  // create line spacings to be used
  IlTitle:= LineSpace(3,0,3);
  IlText:= LineSpace(1,0,0);
  // create the style of lines to be used
  TsBlack:= LineStyle(1,clBlack,lsSolid);
  TsGray:= LineStyle(1,clGray,lsDot);
  TsBlue:= LineStyle(1,clBlue,lsSolid);
  TsFuchsia:= LineStyle(1,clFuchsia,lsDot);
  WriteHeader(cnCenter,lnEnd,'SHOWING GRAPH',ColDefaut,FtTitle,IlTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtText,IlText);
  // draw a graph
  Max:= 0;
  WritePage(10,Base,'0',-1,FtText);
  LinePage(20,Base,200,Base,TsBlack);
  for Cpt:= 1 to 5 do
    begin
    WritePage(10,Base-Cpt*20,IntToStr(Cpt),-1,FtText);
    LinePage(20,Base-Cpt*20,200,Base-Cpt*20,TsGray);
    end;
  for Cpt:= 0 to 18 do
    begin
    if ChartValues[Cpt]> Max
    then
      Max:= ChartValues[Cpt];
    WritePage(18+Cpt*10,Base+5,IntToStr(Cpt),-1,FtText);
    LinePage(20+Cpt*10,Base,20+Cpt*10,Base-ChartValues[Cpt],TsGray);
    if Cpt>0 then
      LinePage(20+Pred(Cpt)*10,Base-ChartValues[Pred(Cpt)],20+Cpt*10,Base-ChartValues[Cpt],TsBlue);
    end;
  WritePage(16,Base-Max,IntToStr(Max),-1,FtMax);
  LinePage(20,Base-Max,200,Base-Max,TsFuchsia);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintSurf(Preview: Boolean);
var
  FtTitle,FtText: Integer;
  IlTitle,IlText: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtText:= Font('helvetica-7',clBlack);
  // create line spacings to be used
  IlTitle:= LineSpace(3,0,3);
  IlText:= LineSpace(1,0,0);
  WriteHeader(cnCenter,lnEnd,'SHOWING SURFACE',ColDefaut,FtTitle,IlTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtText,IlText);
  // paint some surfaces
  SurfPage([40,40,100],[50,110,80],clGreen);
  SurfPage([30,50,150,80,120,130],[120,180,180,160,140,120],clFuchsia);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.PrintImage(Preview: Boolean);
var
  FtTitle,FtText: Integer;
  IlTitle,IlText: Integer;
  Col1,Col2,Col3: Integer;
begin
with FReport do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitle:= Font('helvetica-15:bold',clBlack);
  FtText:= Font('helvetica-7',clBlack);
  // create line spacings to be used
  IlTitle:= LineSpace(0,0,3);
  IlText:= LineSpace(1,0,0);
  Col1:= Column(20,60,2);
  Col2:= Column(80,60,2);
  Col3:= Column(140,60,2);
  WriteHeader(cnCenter,lnEnd,'SHOWING IMAGES',ColDefaut,FtTitle,IlTitle);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnEnd,'Page','of',True,ColDefaut,FtText,IlText);
  // paint some images
  ImageHeader(0,0,'poppy.jpg',Col1,4);
  ImagePage(30,40,'poppy.jpg',ColDefaut,3);
  ImagePage(40,70,'poppy.jpg',ColDefaut,2);
  ImagePage(50,130,'poppy.jpg');
  ImagePage(0,20,'sys.radiobuttons',Col3);
  Page;
  ImagePage(0,0,'poppy-nb.jpg',Col2);
  ImagePage(20,100,'poppy.jpg',ColDefaut,2);
  Page;
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

constructor TF_Demo.Create(AOwner: TComponent);
var
  Cpt: Integer;
begin
inherited Create(AOwner);
Name := 'F_Demo';
WindowTitle:= 'PDF demo';
SetPosition(0, 0, 650, 550);
WindowPosition:= wpScreenCenter;
Sizeable:= False;
fpgSetNamedColor(clWindowBackground,clPaleGreen);
fpgSetNamedColor(clButtonFace,clCyan);
fpgSetNamedColor(clText1,clBlue);
fpgSetNamedColor(clSelection,clSkyBlue);
fpgSetNamedColor(clSelectionText,clDarkBlue);
fpgSetNamedFont('Label1','bitstream vera sans-10');
fpgSetNamedFont('Edit1','bitstream vera sans-10');
L_Pdf:= CreateLabel(Self,50,5,'Print to PDF',150,20,taCenter);
Bt_PdfEmptyPage:= CreateButton(Self,50,30,150,'Empty page',@Bt_PdfEmptyPageClick,'stdimg.Adobe_pdf');
Bt_PdfSimpleText:= CreateButton(Self,50,70,150,'Simple text',@Bt_PdfSimpleTextClick,'stdimg.Adobe_pdf');
Bt_PdfMultiPages:= CreateButton(Self,50,110,150,'Multiple pages',@Bt_PdfMultiPagesClick,'stdimg.Adobe_pdf');
Bt_PdfMultiSections:= CreateButton(Self,50,150,150,'Multiple sections',@Bt_PdfMultiSectionsClick,'stdimg.Adobe_pdf');
Bt_PdfOutlines:= CreateButton(Self,50,190,150,'Outlines',@Bt_PdfOutlinesClick,'stdimg.Adobe_pdf');
Bt_PdfCadres:= CreateButton(Self,50,230,150,'Draw frames',@Bt_PdfCadresClick,'stdimg.Adobe_pdf');
Bt_PdfColor:= CreateButton(Self,50,270,150,'Show colors',@Bt_PdfColorClick,'stdimg.Adobe_pdf');
Bt_PdfLines:= CreateButton(Self,50,310,150,'Draw lines',@Bt_PdfLinesClick,'stdimg.Adobe_pdf');
Bt_PdfGrid:= CreateButton(Self,50,350,150,'Show grid',@Bt_PdfGridClick,'stdimg.Adobe_pdf');
Bt_PdfGraph:= CreateButton(Self,50,390,150,'Show graph',@Bt_PdfGraphClick,'stdimg.Adobe_pdf');
Bt_PdfSurf:= CreateButton(Self,50,430,150,'Show surface',@Bt_PdfSurfClick,'stdimg.Adobe_pdf');
Bt_PdfImages:= CreateButton(Self,50,470,150,'Show images',@Bt_PdfImagClick,'stdimg.Adobe_pdf');
L_Visu:= CreateLabel(Self,250,5,'Preview',150,20,taCenter);
Bt_VisuEmptyPage:= CreateButton(Self,250,30,150,'Empty page',@Bt_VisuEmptyPageClick,'stdimg.preview');
Bt_VisuSimpleText:= CreateButton(Self,250,70,150,'Simple text',@Bt_VisuSimpleTextClick,'stdimg.preview');
Bt_VisuMultiPages:= CreateButton(Self,250,110,150,'Multiple pages',@Bt_VisuMultiPagesClick,'stdimg.preview');
Bt_VisuMultiSections:= CreateButton(Self,250,150,150,'Multiple sections',@Bt_VisuMultiSectionsClick,'stdimg.preview');
Bt_VisuOutlines:= CreateButton(Self,250,190,150,'Outlines',@Bt_VisuOutlinesClick,'stdimg.preview');
Bt_VisuCadres:= CreateButton(Self,250,230,150,'Draw frames',@Bt_VisuCadresClick,'stdimg.preview');
Bt_VisuColor:= CreateButton(Self,250,270,150,'Show colors',@Bt_VisuColorClick,'stdimg.preview');
Bt_VisuLines:= CreateButton(Self,250,310,150,'Draw lines',@Bt_VisuLinesClick,'stdimg.preview');
Bt_VisuGrid:= CreateButton(Self,250,350,150,'Show grid',@Bt_VisuGridClick,'stdimg.preview');
Bt_VisuGraph:= CreateButton(Self,250,390,150,'Show graph',@Bt_VisuGraphClick,'stdimg.preview');
Bt_VisuSurf:= CreateButton(Self,250,430,150,'Show surface',@Bt_VisuSurfClick,'stdimg.preview');
Bt_VisuImages:= CreateButton(Self,250,470,150,'Show images',@Bt_VisuImagClick,'stdimg.preview');
L_Print:= CreateLabel(Self,450,5,'Print to printer',150,20,taCenter);
Bt_PrintEmptyPage:= CreateButton(Self,450,30,150,'Empty page',@Bt_PrintEmptyPageClick,'stdimg.print');
Bt_PrintEmptyPage.Enabled:= False;
Bt_PrintSimpleText:= CreateButton(Self,450,70,150,'Simple text',@Bt_PrintSimpleTextClick,'stdimg.print');
Bt_PrintSimpleText.Enabled:= False;
Bt_PrintMultiPages:= CreateButton(Self,450,110,150,'Multiple pages',@Bt_PrintMultiPagesClick,'stdimg.print');
Bt_PrintMultiPages.Enabled:= False;
Bt_PrintMultiSections:= CreateButton(Self,450,150,150,'Multiple sections',@Bt_PrintMultiSectionsClick,'stdimg.print');
Bt_PrintMultiSections.Enabled:= False;
Bt_PrintOutlines:= CreateButton(Self,450,190,150,'Outlines',@Bt_PrintOutlinesClick,'stdimg.print');
Bt_PrintOutlines.Enabled:= False;
Bt_PrintCadres:= CreateButton(Self,450,230,150,'Draw frames',@Bt_PrintCadresClick,'stdimg.print');
Bt_PrintCadres.Enabled:= False;
Bt_PrintColor:= CreateButton(Self,450,270,150,'Show colors',@Bt_PrintColorClick,'stdimg.print');
Bt_PrintColor.Enabled:= False;
Bt_PrintLines:= CreateButton(Self,450,310,150,'Draw lines',@Bt_PrintLinesClick,'stdimg.print');
Bt_PrintLines.Enabled:= False;
Bt_PrintGrid:= CreateButton(Self,450,350,150,'Show grid',@Bt_PrintGridClick,'stdimg.print');
Bt_PrintGrid.Enabled:= False;
Bt_PrintGraph:= CreateButton(Self,450,390,150,'Show graph',@Bt_PrintGraphClick,'stdimg.print');
Bt_PrintGraph.Enabled:= False;
Bt_PrintSurf:= CreateButton(Self,450,430,150,'Show surface',@Bt_PrintSurfClick,'stdimg.print');
Bt_PrintSurf.Enabled:= False;
Bt_PrintImages:= CreateButton(Self,450,470,150,'Show images',@Bt_PrintImagClick,'stdimg.print');
Bt_PrintImages.Enabled:= False;
Bt_Exit:= CreateButton(Self,450,510,150,'Exit',@Bt_ExitClick,'stdimg.exit');
Bt_Exit.BackgroundColor:= clTomato;
Randomize;
for Cpt:= 0 to 18 do
  ChartValues[Cpt]:= Round(Random*100);
end;

end.

