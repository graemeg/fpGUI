unit U_Demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifdef win32}
  ShellApi,
  {$endif}
  fpg_main, fpg_base,
  fpg_form, fpg_button, fpg_label, fpg_dialogs, fpg_utils, U_Imprime;

type
  TF_Demo = class(TfpgForm)
    private
      FImprime: T_Imprime;
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
      Bt_Fermer: TfpgButton;
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
      procedure Bt_FermerClick(Sender: TObject);
      procedure ImprimeEmptyPage(Preview: Boolean);
      procedure ImprimeSimpleText(Preview: Boolean);
      procedure ImprimeMultiPages(Preview: Boolean);
      procedure ImprimeMultiSections(Preview: Boolean);
      procedure ImprimeOutlines(Preview: Boolean);
      procedure ImprimeCadres(Preview: Boolean);
      procedure ImprimeColor(Preview: Boolean);
      procedure ImprimeLines(Preview: Boolean);
      procedure ImprimeGrid(Preview: Boolean);
      procedure ImprimeGraph(Preview: Boolean);
      procedure ImprimeSurf(Preview: Boolean);
    public
      constructor Create(AOwner: TComponent); override;
    end;

var
  F_Demo: TF_Demo;

implementation

uses
  U_Commande, U_Pdf, U_ReportImages{, U_DemoImages};

var
  ChartValues: array[0..18] of Integer;

const
  Langue= 'F';

procedure TF_Demo.Bt_PdfEmptyPageClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeEmptyPage(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'EmptyPage.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfSimpleTextClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeSimpleText(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'SimpleText.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfMultiPagesClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeMultiPages(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'MultiPages.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfMultiSectionsClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeMultiSections(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'MultiSections.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfOutlinesClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeOutlines(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'Outlines.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfCadresClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeCadres(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'Cadres.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfColorClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeColor(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'Color.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfLinesClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeLines(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'Lines.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfGridClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeGrid(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'Grid.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfGraphClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeGraph(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'Graph.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_PdfSurfClick(Sender: TObject);
var
  Fd_SauvePdf: TfpgFileDialog;
  FichierPdf: string;
  FluxFichier: TFileStream;
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
//  Language:= Version;
  ImprimeSurf(False);
  if T_Section(Sections[Pred(Sections.Count)]).TotPages= 0
  then
    begin
    ShowMessage('There is no file to print');
    Exit;
    end;
  Fd_SauvePdf:= TfpgFileDialog.Create(nil);
  Fd_SauvePdf.InitialDir:= ExtractFilePath(Paramstr(0));
  Fd_SauvePdf.FontDesc:= 'bitstream vera sans-9';
  Fd_SauvePdf.Filter:= 'Fichiers pdf |*.pdf';
  Fd_SauvePdf.FileName:= 'Surface.pdf';
  try
    if Fd_SauvePdf.RunSaveFile
    then
      begin
      FichierPdf:= Fd_SauvePdf.FileName;
      if Lowercase(Copy(FichierPdf,Length(FichierPdf)-3,4))<> '.pdf'
      then
         FichierPdf:= FichierPdf+'.pdf';
      Document:= TPdfDocument.CreateDocument;
      with Document do
        begin
        FluxFichier:= TFileStream.Create(FichierPdf,fmCreate);
        WriteDocument(FluxFichier);
        FluxFichier.Free;
        Free;
        end;
  {$ifdef linux}
      fpgOpenURL(FichierPdf);
  {$endif}
  {$ifdef win32}
      ShellExecute(0,PChar('OPEN'),PChar(FichierPdf),PChar(''),PChar(''),1);
  {$endif}
      end;
  finally
    Fd_SauvePdf.Free;
    end;
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuEmptyPageClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'EmptyPage.pdf';
  ImprimeEmptyPage(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuSimpleTextClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'SimpleText.pdf';
  ImprimeSimpleText(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuMultiPagesClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'MultiPages.pdf';
  ImprimeMultiPages(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuMultiSectionsClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'MultiSections.pdf';
  ImprimeMultiSections(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuOutlinesClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'Outlines.pdf';
  ImprimeOutlines(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuCadresClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'Cadres.pdf';
  ImprimeCadres(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuColorClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'Color.pdf';
  ImprimeColor(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuLinesClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'Lines.pdf';
  ImprimeLines(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuGridClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'Grid.pdf';
  ImprimeGrid(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuGraphClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'Graph.pdf';
  ImprimeGraph(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuSurfClick(Sender: TObject);
begin
FImprime:= T_Imprime.Create;
with FImprime do
  begin
  //Language:= Version;
  DefaultFile:= 'Surface.pdf';
  ImprimeSurf(True);
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

procedure TF_Demo.Bt_FermerClick(Sender: TObject);
begin
Close;
end;

procedure TF_Demo.ImprimeEmptyPage(Preview: Boolean);
begin
with FImprime do
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

procedure TF_Demo.ImprimeSimpleText(Preview: Boolean);
var
  FtTexte1,FtTexte2,FtTexte3: Integer;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins: 10 mm each side
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTexte1:= Font('helvetica-15:bold',clBlack);
  FtTexte2:= Font('helvetica-8',clBlack);
  FtTexte3:= Font('helvetica-8:italic',clBlack);
  // write the text at position 100 mm from left and 120 mm from top
  WritePage(100,120,'Big text at absolute position',-1,FtTexte1);
  // write the text aligned to left
  WritePage(cnLeft,50,'Text aligned to left',ColDefaut,FtTexte2);
  // write the text aligned to right
  WritePage(cnRight,75,'Text aligned to right',ColDefaut,FtTexte3);
  // write the text aligned to center
  WritePage(cnCenter,100,'Text aligned to center',ColDefaut,FtTexte2);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeMultiPages(Preview: Boolean);
var
  FtTitre,FtTexte: Integer;
  Cpt: Integer;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Font('helvetica-15:bold',clBlack);
  FtTexte:= Font('helvetica-8',clBlack);
  // write title on each page
  WriteHeader(cnCenter,lnFin,'MULTIPAGE DOCUMENT',ColDefaut,FtTitre);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte);
  // create five new empty pages
  for Cpt:= 1 to 5 do
    Page;
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeMultiSections(Preview: Boolean);
var
  FtTitreS1,FtTitreS2,FtTitreS3,FtTexte,FtNum,FtNumS: Integer;
  ColDefSect2: Integer;
  Cpt: Integer;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitreS1:= Font('helvetica-15:bold',clBlack);
  FtTitreS2:= Font('helvetica-14:italic',clBlack);
  FtTitreS3:= Font('helvetica-12:bold:italic',clBlack);
  FtTexte:= Font('helvetica-8',clBlack);
  FtNum:= Font('helvetica-7:italic',clBlack);
  FtNumS:= Font('helvetica-7:italic',clGray);
  // create a new section and define the margins
  Section(20,10,10,10);
  // write title on each page of the section
  WriteHeader(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS1);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnFin,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionFooter(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 3 do
    Page;

  // create a new section and define the margins
  Section(10,10,10,10,0,oLandscape);
  // create a default column for section2 which is landscape oriented
  ColDefSect2:= Column(20,257);
  // write title on each page of the section
  WriteHeader(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefSect2,FtTitreS2);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnFin,'Section','of',True,False,ColDefSect2,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnFin,'Section page','of',True,True,ColDefSect2,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnFin,'Page','of',True,ColDefSect2,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 2 do
    Page;

  // create a new section and define the margins
  Section(20,20,20,20);
  // write title on each page of the section
  WriteHeader(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS3);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnFin,'Section','of',True,True,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 4 do
    Page;

  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeOutlines(Preview: Boolean);
var
  FtTitreS1,FtTitreS2,FtTitreS3,FtTexte,FtNum,FtNumS: Integer;
  ColDefSect2: Integer;
  Cpt: Integer;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitreS1:= Font('helvetica-15:bold',clBlack);
  FtTitreS2:= Font('helvetica-14:italic',clBlack);
  FtTitreS3:= Font('helvetica-12:bold:italic',clBlack);
  FtTexte:= Font('helvetica-8',clBlack);
  FtNum:= Font('helvetica-7:italic',clBlack);
  FtNumS:= Font('helvetica-7:italic',clGray);
  // create a new section and define the margins
  Section(20,10,10,10);
  // write title on each page of the section
  WriteHeader(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS1);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnFin,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionFooter(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 3 do
    Page;

  // create a new section and define the margins
  Section(10,10,10,10,0,oLandscape);
  SectionTitle:= 'Landscape oriented';
  // write title on each page of the section
  WriteHeader(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS2);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnFin,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnFin,'Section page','of',True,True,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 2 do
    Page;

  // create a new section and define the margins
  Section(20,20,20,20);
  // write title on each page of the section
  WriteHeader(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS3);
  // write section number and total of sections on each page
  NumSectionHeader(cnRight,lnFin,'Section','of',True,True,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionHeader(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPageFooter(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 4 do
    Page;

  // preparation is finished, so create PDF objects
  Outline:= True;
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeCadres(Preview: Boolean);
var
  FtTitre,FtTexte: Integer;
  TsFin,TsNorm,TsEpais: Integer;
  IlTitre,IlTexte: Integer;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10,5);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Font('helvetica-15:bold',clBlack);
  FtTexte:= Font('helvetica-8',clBlack);
  // create the style of lines to be used
  TsFin:= LineStyle(0.2,clBlack,lsSolid);
  TsNorm:= LineStyle(1,clBlack,lsSolid);
  TsEpais:= LineStyle(2,clBlack,lsSolid);
  // create line spacings to be used
  IlTitre:= LineSpace(3,0,3);
  IlTexte:= LineSpace(1,0,1);
  // write title on each page
  WriteHeader(cnCenter,lnFin,'SHOWING FRAMES',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // draw thin frame rectangle at margins
//  CadreMarges(TsFin);
  // draw thick frame rectangle at header
  FrameHeader(TsEpais);
  // draw thick frame rectangle at footer
  FramePage(TsNorm);
  // draw normal frame rectangle at page
  FrameFooter(TsEpais);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeColor(Preview: Boolean);
var
  FtTitre,FtNormBlack,FtNormRed,FtNormGreen,FtBoldBlue,FtItalGray,FtBoldItalFuchsia: Integer;
  TsFinNoir,TsFinBleu,TsFinRouge,TsEpaisNoir,TsEpaisGris,TsEpaisGreen: Integer;
  FdBlanc,FdBeige,FdEau,FdVertPale: Integer;
  IlTitre,IlTexte: Integer;
  Col1,Col2,Col3: Integer;

begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the filling colors to be used
  FdBlanc:= BackColor(clWhite);
  FdBeige:= BackColor(clBeige);
  FdEau:= BackColor(clAqua);
  FdVertPale:= BackColor(clPaleGreen);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Font('helvetica-15:bold',clBlack);
  FtNormBlack:= Font('helvetica-8',clBlack);
  FtNormRed:= Font('helvetica-8',clRed);
  FtNormGreen:= Font('helvetica-8',clGreen);
  FtBoldBlue:= Font('helvetica-8:bold',clBlue);
  FtItalGray:= Font('helvetica-8:italic',clGray);
  FtBoldItalFuchsia:= Font('helvetica-8:bold:italic',clFuchsia);
  // create the style of lines to be used
  TsFinNoir:= LineStyle(1,clBlack,lsSolid);
  TsFinBleu:= LineStyle(1,clBlue,lsSolid);
  TsFinRouge:= LineStyle(1,clRed,lsSolid);
  TsEpaisNoir:= LineStyle(3,clBlack,lsSolid);
  TsEpaisGris:= LineStyle(3,clGray,lsdot);
  TsEpaisGreen:= LineStyle(3,clGreen,lsSolid);
  // create columns to be used
  Col1:= Column(20,100,2);
  Col2:= Column(120,80,1);
  Col3:= Column(70,100,5);
  // create line spacings to be used
  IlTitre:= LineSpace(5,0,5);
  IlTexte:= LineSpace(0,0,0);
  // write title on each page
  WriteHeader(cnCenter,lnFin,'SHOWING COLORS',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnFin,'Page','of',True,ColDefaut,FtNormRed,IlTexte);
  // write some example texts
  WritePage(cnLeft,lnFin,'Bold blue text aligned to left',ColDefaut,FtBoldBlue,IlTexte);
  SpacePage(10,ColDefaut,FdVertPale);
  WritePage(cnCenter,lnFin,'followed by centered normal black text after a 1 cm colored space',ColDefaut,FtNormBlack,IlTexte);
  SpacePage(15);
  WritePage(cnLeft,lnFin,'text written on colored background after a 1.5 cm colored space',ColDefaut,FtItalGray,IlTexte,FdEau);
  SpacePage(10);
  WritePage(cnLeft,lnCourante,'This text starts in column 1',Col1,FtNormGreen,IlTexte,FdBeige);
  WritePage(cnLeft,lnFin,'and ends in column 2',Col2,FtBoldItalFuchsia,IlTexte);
  WritePage(cnCenter,lnFin,'And this one is centered in column 3',Col3,FtNormRed,IlTexte,FdBeige);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeLines(Preview: Boolean);
var
  FtTitre,FtTexte: Integer;
  TsFinNoir,TsFinBleu,TsEpais,TsFinRouge: Integer;
  IlTitre,IlTexte: Integer;
  Col1,Col2,Col3: Integer;
  BdRect,BdColn,BdFinCol: Integer;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Font('helvetica-15:bold',clBlack);
  FtTexte:= Font('helvetica-8',clBlack);
  // create the style of lines to be used
  TsFinNoir:= LineStyle(0.2,clBlack,lsSolid);
  TsFinBleu:= LineStyle(0.1,clBlue,lsDash);
  TsEpais:= LineStyle(2,clBlack,lsSolid);
  TsFinRouge:= LineStyle(1,clRed,lsDashDot);
  // create line spacings to be used
  IlTitre:= LineSpace(3,0,3);
  IlTexte:= LineSpace(0,0,0);
  // define column borders
  BdRect:= Border([bcGauche,bcDroite,bcHaut,bcBas],TsEpais);
  BdColn:= Border([bcGauche,bcDroite,bcHaut],TsFinBleu);
  BdFinCol:= Border([bcGauche,bcDroite,bcHaut,bcBas],TsFinNoir);
  // create columns to be used
  Col1:= Column(20,60,2);
  Col2:= Column(80,60,2);
  Col3:= Column(140,60,2);
  // write title on each page
  WriteHeader(cnCenter,lnFin,'SHOWING LINES',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // write some example texts with column borders
  WritePage(cnLeft,lnCourante,'Example of lines',Col1,FtTexte,IlTexte,-1,BdColn);
  WritePage(cnLeft,lnCourante,'with column borders',Col2,FtTexte,IlTexte,-1,BdFinCol);
  WritePage(cnLeft,lnFin,'',Col3,FtTexte);
  SpacePage(5);
  WritePage(cnLeft,lnFin,'A thick border',Col3,FtTexte,IlTexte,-1,BdRect);
  HorizLinePage(2,2,Col2,TsEpais);
  LinePage(30,100,150,150,tsFinNoir);
  LinePage(50,70,180,100,tsFinBleu);
  LinePage(40,140,160,80,tsFinRouge);
  LinePage(60,50,60,120,tsEpais);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeGrid(Preview: Boolean);
var
  FtTitre,FtTexte,FtTexteBlue,FtTexteRed,FtSTitre: Integer;
  TsFinNoir,TsEpaisBleu: Integer;
  IlTitre,IlTexte: Integer;
  FdBeige: Integer;
  Col: array[1..5] of Integer;
  BdColn,BdColnG,BdColnD: Integer;
  CptLig,CptCol: Integer;
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
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Font('helvetica-15:bold',clBlack);
  FtTexte:= Font('helvetica-7',clBlack);
  FtTexteBlue:= Font('helvetica-7',clBlue);
  FtTexteRed:= Font('helvetica-7',clRed);
  FtSTitre:= Font('helvetica-9:bold:italic',clBlue);
  // create the style of lines to be used
  TsFinNoir:= LineStyle(0.5,clBlack,lsSolid);
  TsEpaisBleu:= LineStyle(1.5,clBlue,lsSolid);
  // create line spacings to be used
  IlTitre:= LineSpace(3,0,3);
  IlTexte:= LineSpace(1,0,0);
  // define column background color
  FdBeige:= BackColor(clBeige);
  // define column borders
  BdColn:= Border([bcGauche,bcDroite],TsFinNoir);
  BdColnG:= Border([bcGauche],TsEpaisBleu);
  BdColnD:= Border([bcDroite],TsEpaisBleu);
  // create columns to be used
  Col[1]:= Column(Col1Pos,Col1Wid,2);
  Col[2]:= Column(Col2Pos,Col2Wid,2);
  Col[3]:= Column(Col3Pos,Col3Wid,2);
  Col[4]:= Column(Col4Pos,Col4Wid,2);
  Col[5]:= Column(Col5Pos,Col5Wid,2);
  // write title on each page
  WriteHeader(cnCenter,lnFin,'SHOWING GRIDS',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // write a grid without borders
  WritePage(cnCenter,lnFin,'Grid without borders',ColDefaut,FtSTitre,IlTitre);
  for CptLig:= 1 to 10 do
    for CptCol:= 1 to 5 do
      if CptCol= 5
      then
        WritePage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte)
      else
        WritePage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte);
  SpacePage(5);
  // write a grid with borders
  PosHoriz:= WritePage(cnCenter,lnFin,'Grid with borders and colors',ColDefaut,FtSTitre,IlTitre);
  LinePage(Col1Pos,PosHoriz,Col5Pos+Col5Wid,PosHoriz,TsEpaisBleu);
  for CptLig:= 1 to 10 do
    for CptCol:= 1 to 5 do
      if CptCol= 1
      then
        if CptLig mod 2= 0
        then
          WritePage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,FdBeige,BdColnG)
        else
          WritePage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexteBlue,IlTexte,-1,BdColnG)
      else
        if CptCol= 5
        then
          if CptLig= 10
          then
            begin
            PredPosHoriz:= PosHoriz;
            PosHoriz:= WritePage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexteRed,IlTexte,FdBeige,BdColnD);
            LinePage(Col1Pos,PredPosHoriz,Col5Pos+Col5Wid,PredPosHoriz,TsFinNoir);
            LinePage(Col1Pos,PosHoriz,Col5Pos+Col5Wid,PosHoriz,TsEpaisBleu);
            LinePage(Col5Pos,PredPosHoriz,Col5Pos,PosHoriz,TsFinNoir);
            end
          else
            begin
            if CptLig= 1
            then
              PosHoriz:= WritePage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,-1,BdColnD)
            else
              if CptLig mod 2= 0
              then
                begin
                PredPosHoriz:= PosHoriz;
                PosHoriz:= WritePage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexteRed,IlTexte,FdBeige,BdColnD);
                LinePage(Col1Pos,PredPosHoriz,Col5Pos+Col5Wid,PredPosHoriz,TsFinNoir);
                LinePage(Col5Pos,PredPosHoriz,Col5Pos,PosHoriz,TsFinNoir);
                end
              else
                begin
                PredPosHoriz:= PosHoriz;
                PosHoriz:= WritePage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,-1,BdColnD);
                LinePage(Col1Pos,PredPosHoriz,Col5Pos+Col5Wid,PredPosHoriz,TsFinNoir);
                end;
            end
        else
          if CptLig mod 2= 0
          then
            WritePage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,FdBeige,BdColn)
          else
            WritePage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexteBlue,IlTexte,-1,BdColn);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeGraph(Preview: Boolean);
var
  FtTitre,FtTexte,FtMax: Integer;
  TsNoir,TsGris,TsBleu,TsFuchsia: Integer;
  IlTitre,IlTexte: Integer;
  Cpt,Max: Integer;
const
  Base= 150;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Font('helvetica-15:bold',clBlack);
  FtTexte:= Font('helvetica-7',clBlack);
  FtMax:= Font('helvetica-7',clFuchsia);
  // create line spacings to be used
  IlTitre:= LineSpace(3,0,3);
  IlTexte:= LineSpace(1,0,0);
  // create the style of lines to be used
  TsNoir:= LineStyle(1,clBlack,lsSolid);
  TsGris:= LineStyle(1,clGray,lsDot);
  TsBleu:= LineStyle(1,clBlue,lsSolid);
  TsFuchsia:= LineStyle(1,clFuchsia,lsDot);
  WriteHeader(cnCenter,lnFin,'SHOWING GRAPH',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // draw a graph
  Max:= 0;
  WritePage(10,Base,'0',-1,FtTexte);
  LinePage(20,Base,200,Base,TsNoir);
  for Cpt:= 1 to 5 do
    begin
    WritePage(10,Base-Cpt*20,IntToStr(Cpt),-1,FtTexte);
    LinePage(20,Base-Cpt*20,200,Base-Cpt*20,TsGris);
    end;
  for Cpt:= 0 to 18 do
    begin
    if ChartValues[Cpt]> Max
    then
      Max:= ChartValues[Cpt];
    WritePage(18+Cpt*10,Base+5,IntToStr(Cpt),-1,FtTexte);
    LinePage(20+Cpt*10,Base,20+Cpt*10,Base-ChartValues[Cpt],TsGris);
    if Cpt>0 then
      LinePage(20+Pred(Cpt)*10,Base-ChartValues[Pred(Cpt)],20+Cpt*10,Base-ChartValues[Cpt],TsBleu);
    end;
  WritePage(16,Base-Max,IntToStr(Max),-1,FtMax);
  LinePage(20,Base-Max,200,Base-Max,TsFuchsia);
  // preparation is finished, so create PDF objects
  EndWrite;
  end;
end;

procedure TF_Demo.ImprimeSurf(Preview: Boolean);
var
  FtTitre,FtTexte: Integer;
  IlTitre,IlTexte: Integer;
begin
with FImprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  BeginWrite(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Font('helvetica-15:bold',clBlack);
  FtTexte:= Font('helvetica-7',clBlack);
  // create line spacings to be used
  IlTitre:= LineSpace(3,0,3);
  IlTexte:= LineSpace(1,0,0);
  WriteHeader(cnCenter,lnFin,'SHOWING SURFACE',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPageFooter(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // paint some surfaces
  SurfPage([40,40,100],[50,110,80],clGreen);
  SurfPage([30,50,150,80,120,130],[120,180,180,160,140,120],clFuchsia);
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
L_Visu:= CreateLabel(Self,50,5,'Print to PDF',150,20,taCenter);
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
L_Pdf:= CreateLabel(Self,250,5,'Preview',150,20,taCenter);
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
Bt_Fermer:= CreateButton(Self,450,500,150,'Fermer',@Bt_FermerClick,'stdimg.exit');
Bt_Fermer.BackgroundColor:= clTomato;
Randomize;
for Cpt:= 0 to 18 do
  ChartValues[Cpt]:= Round(Random*100);
end;

end.

