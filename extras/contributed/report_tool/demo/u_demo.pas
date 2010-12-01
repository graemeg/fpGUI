unit U_Demo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifdef win32}
  ShellApi,
  {$endif}
  fpg_main, fpg_base,
  fpg_form, fpg_button, fpg_label, fpg_dialogs, fpg_utils;

type
  TF_Demo = class(TfpgForm)
    private
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
    public
      constructor Create(AOwner: TComponent); override;
    end;

var
  F_Demo: TF_Demo;

implementation

uses
  U_Imprime, U_Commande, U_Pdf, U_ReportImages;

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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
//  Langue:= Version;
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
        EcritDocument(FluxFichier);
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
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'EmptyPage.pdf';
  ImprimeEmptyPage(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuSimpleTextClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'SimpleText.pdf';
  ImprimeSimpleText(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuMultiPagesClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'MultiPages.pdf';
  ImprimeMultiPages(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuMultiSectionsClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'MultiSections.pdf';
  ImprimeMultiSections(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuOutlinesClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'Outlines.pdf';
  ImprimeOutlines(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuCadresClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'Cadres.pdf';
  ImprimeCadres(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuColorClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'Color.pdf';
  ImprimeColor(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuLinesClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'Lines.pdf';
  ImprimeLines(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuGridClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'Grid.pdf';
  ImprimeGrid(True);
  Free;
  end;
end;

procedure TF_Demo.Bt_VisuGraphClick(Sender: TObject);
begin
Imprime:= T_Imprime.Create;
with Imprime do
  begin
  //Langue:= Version;
  DefaultFile:= 'Graph.pdf';
  ImprimeGraph(True);
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

procedure TF_Demo.Bt_FermerClick(Sender: TObject);
begin
Close;
end;

procedure TF_Demo.ImprimeEmptyPage(Preview: Boolean);
begin
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins
  Section(0,0,0,0);
  // create an empty page
  Page;
  // preparation is finished, so create PDF objects
  Fin;
  end;
end;

procedure TF_Demo.ImprimeSimpleText(Preview: Boolean);
var
  FtTexte1,FtTexte2,FtTexte3: Integer;
begin
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins: 10 mm each side
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTexte1:= Fonte('helvetica-15:bold',clBlack);
  FtTexte2:= Fonte('helvetica-8',clBlack);
  FtTexte3:= Fonte('helvetica-8:italic',clBlack);
  // write the text at position 100 mm from left and 120 mm from top
  EcritPage(100,120,'Big text at absolute position',-1,FtTexte1);
  // write the text aligned to left
  EcritPage(cnLeft,50,'Text aligned to left',ColDefaut,FtTexte2);
  // write the text aligned to right
  EcritPage(cnRight,75,'Text aligned to right',ColDefaut,FtTexte3);
  // write the text aligned to center
  EcritPage(cnCenter,100,'Text aligned to center',ColDefaut,FtTexte2);
  // preparation is finished, so create PDF objects
  Fin;
  end;
end;

procedure TF_Demo.ImprimeMultiPages(Preview: Boolean);
var
  FtTitre,FtTexte: Integer;
  Cpt: Integer;
begin
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Fonte('helvetica-15:bold',clBlack);
  FtTexte:= Fonte('helvetica-8',clBlack);
  // write title on each page
  EcritEnTete(cnCenter,lnFin,'MULTIPAGE DOCUMENT',ColDefaut,FtTitre);
  // write page number and total of pages on each page
  NumPagePied(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte);
  // create five new empty pages
  for Cpt:= 1 to 5 do
    Page;
  // preparation is finished, so create PDF objects
  Fin;
  end;
end;

procedure TF_Demo.ImprimeMultiSections(Preview: Boolean);
var
  FtTitreS1,FtTitreS2,FtTitreS3,FtTexte,FtNum,FtNumS: Integer;
  ColDefSect2: Integer;
  Cpt: Integer;
begin
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitreS1:= Fonte('helvetica-15:bold',clBlack);
  FtTitreS2:= Fonte('helvetica-14:italic',clBlack);
  FtTitreS3:= Fonte('helvetica-12:bold:italic',clBlack);
  FtTexte:= Fonte('helvetica-8',clBlack);
  FtNum:= Fonte('helvetica-7:italic',clBlack);
  FtNumS:= Fonte('helvetica-7:italic',clGray);
  // create a new section and define the margins
  Section(20,10,10,10);
  // write title on each page of the section
  EcritEnTete(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS1);
  // write section number and total of sections on each page
  NumSectionEnTete(cnRight,lnFin,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionPied(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPagePied(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 3 do
    Page;

  // create a new section and define the margins
  Section(10,10,10,10,0,oLandscape);
  // create a default column for section2 which is landscape oriented
  ColDefSect2:= Colonne(20,257);
  // write title on each page of the section
  EcritEnTete(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefSect2,FtTitreS2);
  // write section number and total of sections on each page
  NumSectionEnTete(cnRight,lnFin,'Section','of',True,False,ColDefSect2,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionEnTete(cnCenter,lnFin,'Section page','of',True,True,ColDefSect2,FtNumS);
  // write page number and total of pages on each page
  NumPagePied(cnCenter,lnFin,'Page','of',True,ColDefSect2,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 2 do
    Page;

  // create a new section and define the margins
  Section(20,20,20,20);
  // write title on each page of the section
  EcritEnTete(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS3);
  // write section number and total of sections on each page
  NumSectionEnTete(cnRight,lnFin,'Section','of',True,True,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionEnTete(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPagePied(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 4 do
    Page;

  // preparation is finished, so create PDF objects
  Fin;
  end;
end;

procedure TF_Demo.ImprimeOutlines(Preview: Boolean);
var
  FtTitreS1,FtTitreS2,FtTitreS3,FtTexte,FtNum,FtNumS: Integer;
  ColDefSect2: Integer;
  Cpt: Integer;
begin
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitreS1:= Fonte('helvetica-15:bold',clBlack);
  FtTitreS2:= Fonte('helvetica-14:italic',clBlack);
  FtTitreS3:= Fonte('helvetica-12:bold:italic',clBlack);
  FtTexte:= Fonte('helvetica-8',clBlack);
  FtNum:= Fonte('helvetica-7:italic',clBlack);
  FtNumS:= Fonte('helvetica-7:italic',clGray);
  // create a new section and define the margins
  Section(20,10,10,10);
  // write title on each page of the section
  EcritEnTete(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS1);
  // write section number and total of sections on each page
  NumSectionEnTete(cnRight,lnFin,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionPied(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPagePied(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 3 do
    Page;

  // create a new section and define the margins
  Section(10,10,10,10,0,oLandscape);
  TitreSection:= 'Landscape oriented';
  // write title on each page of the section
  EcritEnTete(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS2);
  // write section number and total of sections on each page
  NumSectionEnTete(cnRight,lnFin,'Section','of',True,False,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionEnTete(cnCenter,lnFin,'Section page','of',True,True,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPagePied(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 2 do
    Page;

  // create a new section and define the margins
  Section(20,20,20,20);
  // write title on each page of the section
  EcritEnTete(cnCenter,lnFin,'MULTI SECTION DOCUMENT',ColDefaut,FtTitreS3);
  // write section number and total of sections on each page
  NumSectionEnTete(cnRight,lnFin,'Section','of',True,True,ColDefaut,FtNum);
  // write page number for the section and total pages of the section on each page
  NumPageSectionEnTete(cnCenter,lnFin,'Section page','of',True,False,ColDefaut,FtNumS);
  // write page number and total of pages on each page
  NumPagePied(cnCenter,lnFin,'Page','of',True,ColDefaut,FtNum);
  // create some new empty pages in the section
  for Cpt:= 1 to 4 do
    Page;

  // preparation is finished, so create PDF objects
  Outline:= True;
  Fin;
  end;
end;

procedure TF_Demo.ImprimeCadres(Preview: Boolean);
var
  FtTitre,FtTexte: Integer;
  TsFin,TsEpais: Integer;
  IlTitre,IlTexte: Integer;
begin
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10,5);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Fonte('helvetica-15:bold',clBlack);
  FtTexte:= Fonte('helvetica-8',clBlack);
  // create the style of lines to be used
  TsFin:= StyleTrait(1,clBlack,lsSolid);
  TsEpais:= StyleTrait(2,clBlack,lsSolid);
  // create line spacings to be used
  IlTitre:= Interligne(3,0,3);
  IlTexte:= Interligne(1,0,1);
  // write title on each page
  EcritEnTete(cnCenter,lnFin,'SHOWING FRAMES',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPagePied(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // draw thin frame rectangle at margins
  CadreMarges(TsFin);
  // draw thick frame rectangle at header
  CadreEnTete(TsEpais);
  // draw thick frame rectangle at footer
  CadrePied(TsEpais);
  // preparation is finished, so create PDF objects
  Fin;
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
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the filling colors to be used
  FdBlanc:= Fond(clWhite);
  FdBeige:= Fond(clBeige);
  FdEau:= Fond(clAqua);
  FdVertPale:= Fond(clPaleGreen);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Fonte('helvetica-15:bold',clBlack);
  FtNormBlack:= Fonte('helvetica-8',clBlack);
  FtNormRed:= Fonte('helvetica-8',clRed);
  FtNormGreen:= Fonte('helvetica-8',clGreen);
  FtBoldBlue:= Fonte('helvetica-8:bold',clBlue);
  FtItalGray:= Fonte('helvetica-8:italic',clGray);
  FtBoldItalFuchsia:= Fonte('helvetica-8:bold:italic',clFuchsia);
  // create the style of lines to be used
  TsFinNoir:= StyleTrait(1,clBlack,lsSolid);
  TsFinBleu:= StyleTrait(1,clBlue,lsSolid);
  TsFinRouge:= StyleTrait(1,clRed,lsSolid);
  TsEpaisNoir:= StyleTrait(3,clBlack,lsSolid);
  TsEpaisGris:= StyleTrait(3,clGray,lsdot);
  TsEpaisGreen:= StyleTrait(3,clGreen,lsSolid);
  // create columns to be used
  Col1:= Colonne(20,100,2);
  Col2:= Colonne(120,80,1);
  Col3:= Colonne(70,100,5);
  // create line spacings to be used
  IlTitre:= Interligne(5,0,5);
  IlTexte:= Interligne(0,0,0);
  // write title on each page
  EcritEnTete(cnCenter,lnFin,'SHOWING COLORS',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPagePied(cnRight,lnFin,'Page','of',True,ColDefaut,FtNormRed,IlTexte);
  // write some example texts
  EcritPage(cnLeft,lnFin,'Bold blue text aligned to left',ColDefaut,FtBoldBlue,IlTexte);
  EspacePage(10,ColDefaut,FdVertPale);
  EcritPage(cnCenter,lnFin,'followed by centered normal black text after a 1 cm colored space',ColDefaut,FtNormBlack,IlTexte);
  EspacePage(15);
  EcritPage(cnLeft,lnFin,'text written on colored background after a 1.5 cm colored space',ColDefaut,FtItalGray,IlTexte,FdEau);
  EspacePage(10);
  EcritPage(cnLeft,lnCourante,'This text starts in column 1',Col1,FtNormGreen,IlTexte,FdBeige);
  EcritPage(cnLeft,lnFin,'and ends in column 2',Col2,FtBoldItalFuchsia,IlTexte);
  EcritPage(cnCenter,lnFin,'And this one is centered in column 3',Col3,FtNormRed,IlTexte,FdBeige);
  // preparation is finished, so create PDF objects
  Fin;
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
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Fonte('helvetica-15:bold',clBlack);
  FtTexte:= Fonte('helvetica-8',clBlack);
  // create the style of lines to be used
  TsFinNoir:= StyleTrait(1,clBlack,lsSolid);
  TsFinBleu:= StyleTrait(1,clBlue,lsDash);
  TsEpais:= StyleTrait(2,clBlack,lsSolid);
  TsFinRouge:= StyleTrait(1,clRed,lsDashDot);
  // create line spacings to be used
  IlTitre:= Interligne(3,0,3);
  IlTexte:= Interligne(0,0,0);
  // define column borders
  BdRect:= Bordure([bcGauche,bcDroite,bcHaut,bcBas],TsEpais);
  BdColn:= Bordure([bcGauche,bcDroite,bcHaut],TsFinBleu);
  BdFinCol:= Bordure([bcGauche,bcDroite,bcHaut,bcBas],TsFinNoir);
  // create columns to be used
  Col1:= Colonne(20,60,2);
  Col2:= Colonne(80,60,2);
  Col3:= Colonne(140,60,2);
  // write title on each page
  EcritEnTete(cnCenter,lnFin,'SHOWING LINES',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPagePied(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // write some example texts with column borders
  EcritPage(cnLeft,lnCourante,'Example of lines',Col1,FtTexte,IlTexte,-1,BdColn);
  EcritPage(cnLeft,lnCourante,'with column borders',Col2,FtTexte,IlTexte,-1,BdFinCol);
  EcritPage(cnLeft,lnFin,'',Col3,FtTexte);
  EspacePage(5);
  EcritPage(cnLeft,lnFin,'A thick border',Col3,FtTexte,IlTexte,-1,BdRect);
  TraitHorizPage(2,2,Col2,TsEpais);
  TraitPage(30,100,150,150,tsFinNoir);
  TraitPage(50,70,180,100,tsFinBleu);
  TraitPage(40,140,160,80,tsFinRouge);
  TraitPage(60,50,60,120,tsEpais);
  // preparation is finished, so create PDF objects
  Fin;
  end;
end;

procedure TF_Demo.ImprimeGrid(Preview: Boolean);
var
  FtTitre,FtTexte,FtSTitre: Integer;
  TsFinNoir: Integer;
  IlTitre,IlTexte: Integer;
  Col: array[1..5] of Integer;
  BdColn,BdFinCol: Integer;
  CptLig,CptCol: Integer;
begin
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(20,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Fonte('helvetica-15:bold',clBlack);
  FtTexte:= Fonte('helvetica-7',clBlack);
  FtSTitre:= Fonte('helvetica-9:bold:italic',clBlue);
  // create the style of lines to be used
  TsFinNoir:= StyleTrait(1,clBlack,lsSolid);
  // create line spacings to be used
  IlTitre:= Interligne(3,0,3);
  IlTexte:= Interligne(1,0,0);
  // define column borders
  BdColn:= Bordure([bcGauche,bcDroite,bcHaut],TsFinNoir);
  BdFinCol:= Bordure([bcGauche,bcDroite,bcHaut,bcBas],TsFinNoir);
  // create columns to be used
  Col[1]:= Colonne(20,40,2);
  Col[2]:= Colonne(60,35,2);
  Col[3]:= Colonne(95,35,2);
  Col[4]:= Colonne(130,35,2);
  Col[5]:= Colonne(165,35,2);
  // write title on each page
  EcritEnTete(cnCenter,lnFin,'SHOWING GRIDS',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPagePied(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // write a grid without borders
  EcritPage(cnCenter,lnFin,'Grid without borders',ColDefaut,FtSTitre,IlTitre);
  for CptLig:= 1 to 10 do
    for CptCol:= 1 to 5 do
      if CptCol= 5
      then
        EcritPage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte)
      else
        EcritPage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte);
  EspacePage(5);
  // write a grid with borders
  EcritPage(cnCenter,lnFin,'Grid with borders',ColDefaut,FtSTitre,IlTitre);
  for CptLig:= 1 to 10 do
    for CptCol:= 1 to 5 do
      if CptCol= 5
      then
        if CptLig= 10
        then
          EcritPage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,-1,BdFinCol)
        else
          EcritPage(cnLeft,lnFin,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,-1,BdColn)
      else
        if CptLig= 10
        then
          EcritPage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,-1,BdFinCol)
        else
          EcritPage(cnLeft,lnCourante,'line '+IntToStr(CptLig)+' ; column '+IntToStr(CptCol),Col[CptCol],FtTexte,IlTexte,-1,BdColn);
  // preparation is finished, so create PDF objects
  Fin;
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
with Imprime do
  begin
  // define orientation, page format, measurement unit, language, preview (true) or print (false)
  Debut(oPortrait,A4,msMM,Langue,Preview);
  // create a new section and define the margins with an additional one due to frames drawing
  Section(10,10,10,10);
  // create the fonts to be used (use one of the 14 Adobe PDF standard fonts)
  FtTitre:= Fonte('helvetica-15:bold',clBlack);
  FtTexte:= Fonte('helvetica-7',clBlack);
  FtMax:= Fonte('helvetica-7',clFuchsia);
  // create line spacings to be used
  IlTitre:= Interligne(3,0,3);
  IlTexte:= Interligne(1,0,0);
  // create the style of lines to be used
  TsNoir:= StyleTrait(1,clBlack,lsSolid);
  TsGris:= StyleTrait(1,clGray,lsDot);
  TsBleu:= StyleTrait(1,clBlue,lsSolid);
  TsFuchsia:= StyleTrait(1,clFuchsia,lsDot);
  EcritEnTete(cnCenter,lnFin,'SHOWING GRAPH',ColDefaut,FtTitre,IlTitre);
  // write page number and total of pages on each page
  NumPagePied(cnRight,lnFin,'Page','of',True,ColDefaut,FtTexte,IlTexte);
  // draw a graph
  Max:= 0;
  EcritPage(10,Base,'0',-1,FtTexte);
  TraitPage(20,Base,200,Base,TsNoir);
  for Cpt:= 1 to 5 do
    begin
    EcritPage(10,Base-Cpt*20,IntToStr(Cpt),-1,FtTexte);
    TraitPage(20,Base-Cpt*20,200,Base-Cpt*20,TsGris);
    end;
  for Cpt:= 0 to 18 do
    begin
    if ChartValues[Cpt]> Max
    then
      Max:= ChartValues[Cpt];
    EcritPage(18+Cpt*10,Base+5,IntToStr(Cpt),-1,FtTexte);
    TraitPage(20+Cpt*10,Base,20+Cpt*10,Base-ChartValues[Cpt],TsGris);
    if Cpt>0 then
      TraitPage(20+Pred(Cpt)*10,Base-ChartValues[Pred(Cpt)],20+Cpt*10,Base-ChartValues[Cpt],TsBleu);
    end;
  EcritPage(16,Base-Max,IntToStr(Max),-1,FtMax);
  TraitPage(20,Base-Max,200,Base-Max,TsFuchsia);
  // preparation is finished, so create PDF objects
  Fin;
  end;
end;

constructor TF_Demo.Create(AOwner: TComponent);
var
  Cpt: Integer;
begin
inherited Create(AOwner);
Name := 'F_Demo';
WindowTitle:= 'PDF demo';
SetPosition(0, 0, 650, 500);
WindowPosition:= wpScreenCenter;
Sizeable:= False;
CreateReportImages;
fpgSetNamedColor(clWindowBackground,clPaleGreen);
fpgSetNamedColor(clButtonFace,clCyan);
fpgSetNamedColor(clText1,clBlue);
fpgSetNamedColor(clSelection,clSkyBlue);
fpgSetNamedColor(clSelectionText,clDarkBlue);
fpgSetNamedFont('Label1','bitstream vera sans-10');
fpgSetNamedFont('Edit1','bitstream vera sans-10');
L_Visu:= CreateLabel(Self,50,5,'Print to PDF',150,20,taCenter);
Bt_PdfEmptyPage:= CreateButton(Self,50,30,150,'Empty page',@Bt_PdfEmptyPageClick,'repimg.Adobe_pdf');
Bt_PdfSimpleText:= CreateButton(Self,50,70,150,'Simple text',@Bt_PdfSimpleTextClick,'repimg.Adobe_pdf');
Bt_PdfMultiPages:= CreateButton(Self,50,110,150,'Multiple pages',@Bt_PdfMultiPagesClick,'repimg.Adobe_pdf');
Bt_PdfMultiSections:= CreateButton(Self,50,150,150,'Multiple sections',@Bt_PdfMultiSectionsClick,'repimg.Adobe_pdf');
Bt_PdfOutlines:= CreateButton(Self,50,190,150,'Outlines',@Bt_PdfOutlinesClick,'repimg.Adobe_pdf');
Bt_PdfCadres:= CreateButton(Self,50,230,150,'Draw frames',@Bt_PdfCadresClick,'repimg.Adobe_pdf');
Bt_PdfColor:= CreateButton(Self,50,270,150,'Show colors',@Bt_PdfColorClick,'repimg.Adobe_pdf');
Bt_PdfLines:= CreateButton(Self,50,310,150,'Draw lines',@Bt_PdfLinesClick,'repimg.Adobe_pdf');
Bt_PdfGrid:= CreateButton(Self,50,350,150,'Show grid',@Bt_PdfGridClick,'repimg.Adobe_pdf');
Bt_PdfGraph:= CreateButton(Self,50,390,150,'Show graph',@Bt_PdfGraphClick,'repimg.Adobe_pdf');
L_Pdf:= CreateLabel(Self,250,5,'Preview',150,20,taCenter);
Bt_VisuEmptyPage:= CreateButton(Self,250,30,150,'Empty page',@Bt_VisuEmptyPageClick,'repimg.Preview');
Bt_VisuSimpleText:= CreateButton(Self,250,70,150,'Simple text',@Bt_VisuSimpleTextClick,'repimg.Preview');
Bt_VisuMultiPages:= CreateButton(Self,250,110,150,'Multiple pages',@Bt_VisuMultiPagesClick,'repimg.Preview');
Bt_VisuMultiSections:= CreateButton(Self,250,150,150,'Multiple sections',@Bt_VisuMultiSectionsClick,'repimg.Preview');
Bt_VisuOutlines:= CreateButton(Self,250,190,150,'Outlines',@Bt_VisuOutlinesClick,'repimg.Preview');
Bt_VisuCadres:= CreateButton(Self,250,230,150,'Draw frames',@Bt_VisuCadresClick,'repimg.Preview');
Bt_VisuColor:= CreateButton(Self,250,270,150,'Show colors',@Bt_VisuColorClick,'repimg.Preview');
Bt_VisuLines:= CreateButton(Self,250,310,150,'Draw lines',@Bt_VisuLinesClick,'repimg.Preview');
Bt_VisuGrid:= CreateButton(Self,250,350,150,'Show grid',@Bt_VisuGridClick,'repimg.Preview');
Bt_VisuGraph:= CreateButton(Self,250,390,150,'Show graph',@Bt_VisuGraphClick,'repimg.Preview');
L_Print:= CreateLabel(Self,450,5,'Print to printer',150,20,taCenter);
Bt_PrintEmptyPage:= CreateButton(Self,450,30,150,'Empty page',@Bt_PrintEmptyPageClick,'repimg.Imprimer');
Bt_PrintEmptyPage.Enabled:= False;
Bt_PrintSimpleText:= CreateButton(Self,450,70,150,'Simple text',@Bt_PrintSimpleTextClick,'repimg.Imprimer');
Bt_PrintSimpleText.Enabled:= False;
Bt_PrintMultiPages:= CreateButton(Self,450,110,150,'Multiple pages',@Bt_PrintMultiPagesClick,'repimg.Imprimer');
Bt_PrintMultiPages.Enabled:= False;
Bt_PrintMultiSections:= CreateButton(Self,450,150,150,'Multiple sections',@Bt_PrintMultiSectionsClick,'repimg.Imprimer');
Bt_PrintMultiSections.Enabled:= False;
Bt_PrintOutlines:= CreateButton(Self,450,190,150,'Outlines',@Bt_PrintOutlinesClick,'repimg.Imprimer');
Bt_PrintOutlines.Enabled:= False;
Bt_PrintCadres:= CreateButton(Self,450,230,150,'Draw frames',@Bt_PrintCadresClick,'repimg.Imprimer');
Bt_PrintCadres.Enabled:= False;
Bt_PrintColor:= CreateButton(Self,450,270,150,'Show colors',@Bt_PrintColorClick,'repimg.Imprimer');
Bt_PrintColor.Enabled:= False;
Bt_PrintLines:= CreateButton(Self,450,310,150,'Draw lines',@Bt_PrintLinesClick,'repimg.Imprimer');
Bt_PrintLines.Enabled:= False;
Bt_PrintGrid:= CreateButton(Self,450,350,150,'Show grid',@Bt_PrintGridClick,'repimg.Imprimer');
Bt_PrintGrid.Enabled:= False;
Bt_PrintGraph:= CreateButton(Self,450,390,150,'Show graph',@Bt_PrintGraphClick,'repimg.Imprimer');
Bt_PrintGraph.Enabled:= False;
Bt_Fermer:= CreateButton(Self,450,450,150,'Fermer',@Bt_FermerClick,'repimg.Fermer');
Bt_Fermer.BackgroundColor:= clTomato;
Randomize;
for Cpt:= 0 to 18 do
  ChartValues[Cpt]:= Round(Random*100);
end;

end.

