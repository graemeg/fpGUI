{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
  distribution, for details of the copyright.

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    This unit forms part of the PDF Reporting Engine. This unit
    implements the report preview form.

    The PDF Reporting Engine was originally written by
    Jean-Marc Levecque <jean-marc.levecque@jmlesite.fr>
}

unit U_Visu;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {$ifdef win32}
  shellapi,
  {$endif}
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_panel,
  fpg_label,
  fpg_button,
  fpg_edit,
  fpg_dialogs,
  fpg_utils,
  U_Report;

type
  TF_Visu = class(TfpgForm)
  private
    FReport: T_Report;
    Bv_Command: TfpgBevel;
    Bt_Close: TfpgButton;
    Bt_Print: TfpgButton;
    Bt_Printer: TfpgButton;
    Bt_Stop: TfpgButton;
    Bt_Pdf: TfpgButton;
    Bv_Pages: TfpgBevel;
    L_Pages: TfpgLabel;
    Bt_FirstPage: TfpgButton;
    Bt_PrecPage: TfpgButton;
    E_NumPage: TfpgEditInteger;
    Bt_NextPage: TfpgButton;
    Bt_LastPage: TfpgButton;
    L_FromPage: TfpgLabel;
    L_NbrPages: TfpgLabel;
    Bv_Sections: TfpgBevel;
    L_Sections: TfpgLabel;
    Bt_PrecSect: TfpgButton;
    E_NumSect: TfpgEditInteger;
    Bt_NextSect: TfpgButton;
    L_FromSect: TfpgLabel;
    L_NbrSect: TfpgLabel;
    L_PageSect: TfpgLabel;
    L_NumPageSect: TfpgLabel;
    L_FromPageSect: TfpgLabel;
    L_NbrPageSect: TfpgLabel;
    procedure FormShow(Sender: TObject);
    procedure Bt_CloseClick(Sender: TObject);
    procedure Bt_PrintClick(Sender: TObject);
    procedure Bt_PrinterClick(Sender: TObject);
    procedure Bt_StopClick(Sender: TObject);
    procedure Bt_PdfClick(Sender: TObject);
    procedure Bt_FirstPageClick(Sender: TObject);
    procedure Bt_PrecPageClick(Sender: TObject);
    procedure Bt_NextPageClick(Sender: TObject);
    procedure Bt_LastPageClick(Sender: TObject);
    procedure Bt_PrecSectClick(Sender: TObject);
    procedure Bt_NextSectClick(Sender: TObject);
    procedure E_NumPageKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: Boolean);
    procedure E_NumSectKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: Boolean);
    procedure ChangeButtons;
  public
    constructor Create(AOwner: TComponent; AImprime: T_Report); reintroduce;
    destructor Destroy; override;
  end;

var
  F_Visu: TF_Visu;
  Bv_Visu: TfpgBevel;

implementation

uses
  U_Command,
  U_Pdf,
  U_ReportImages;

procedure TF_Visu.FormShow(Sender: TObject);
begin
  L_Pages.Text    := 'Page';
  L_Sections.Text := 'Section';
  L_PageSect.Text := 'Page';
  L_FromPage.Text := 'of';
  with FReport do
  begin
    if Sections.Count = 1 then
      E_NumSect.Focusable := False;
    if T_Section(Sections[Pred(Sections.Count)]).TotPages = 1 then
      E_NumPage.Focusable := False;
    E_NumPage.Text := IntToStr(NumPage);
    L_NbrPages.Text := IntToStr(T_Section(Sections[Pred(Sections.Count)]).TotPages);
    E_NumSect.Text := IntToStr(NumSection);
    L_NbrSect.Text     := IntToStr(Sections.Count);
    L_NumPageSect.Text := IntToStr(NumPageSection);
    L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
    ChangeButtons;
  end;
end;

procedure TF_Visu.Bt_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TF_Visu.Bt_PrintClick(Sender: TObject);
begin
end;

procedure TF_Visu.Bt_PrinterClick(Sender: TObject);
begin
end;

procedure TF_Visu.Bt_StopClick(Sender: TObject);
begin
end;

procedure TF_Visu.Bt_PdfClick(Sender: TObject);
var
  Fd_SavePdf: TfpgFileDialog;
  PdfFile: string;
  PdfFileStream: TFileStream;
begin
  Fd_SavePdf          := TfpgFileDialog.Create(nil);
  Fd_SavePdf.InitialDir := ExtractFilePath(ParamStr(0));
  Fd_SavePdf.FontDesc := 'bitstream vera sans-9';
  Fd_SavePdf.Filter   := 'PDF Documents |*.pdf';
  Fd_SavePdf.FileName := FReport.DefaultFile;
  try
    if Fd_SavePdf.RunSaveFile then
    begin
      PdfFile := Fd_SavePdf.FileName;
      if Lowercase(Copy(PdfFile, Length(PdfFile) - 3, 4)) <> '.pdf' then
        PdfFile := PdfFile + '.pdf';
      Document := TPdfDocument.CreateDocument;
      with Document do
      begin
        PdfFileStream := TFileStream.Create(PdfFile, fmCreate);
        WriteDocument(PdfFileStream);
        PdfFileStream.Free;
        Free;
      end;
      {$ifdef linux}
      fpgOpenURL(PdfFile);
      {$endif}
      {$ifdef win32}
      ShellExecute(0, PChar('OPEN'), PChar(PdfFile), PChar(''), PChar(''), 1);
      {$endif}
    end;
  finally
    Fd_SavePdf.Free;
  end;
end;

procedure TF_Visu.Bt_FirstPageClick(Sender: TObject);
begin
  with FReport do
  begin
    NumPage         := 1;
    NumSection      := 1;
    NumPageSection  := 1;
    E_NumPage.Text  := IntToStr(NumPage);
    Bv_Visu.Visible := False;
    with T_Section(Sections[Pred(NumSection)]), F_Visu do
    begin
      Bv_Visu.Height := Paper.H;
      Bv_Visu.Width  := Paper.W;
      Bv_Visu.Top    := 50 + (F_Visu.Height - 50 - Paper.H) div 2;
      Bv_Visu.Left   := (F_Visu.Width - Paper.W) div 2;
    end;
    Bv_Visu.Visible := True;
    ChangeButtons;
    E_NumSect.Text     := IntToStr(NumSection);
    L_NumPageSect.Text := IntToStr(NumPageSection);
    L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
  end;
end;

procedure TF_Visu.Bt_PrecPageClick(Sender: TObject);
begin
  with FReport do
  begin
    NumPage := NumPage - 1;
    if NumPageSection = 1 then
    begin
      NumSection      := NumSection - 1;
      NumPageSection  := T_Section(Sections[Pred(NumSection)]).NbPages;
      Bv_Visu.Visible := False;
      with T_Section(Sections[Pred(NumSection)]), F_Visu do
      begin
        Bv_Visu.Height := Paper.H;
        Bv_Visu.Width  := Paper.W;
        Bv_Visu.Top    := 50 + (F_Visu.Height - 50 - Paper.H) div 2;
        Bv_Visu.Left   := (F_Visu.Width - Paper.W) div 2;
      end;
      Bv_Visu.Visible := True;
    end
    else
    begin
      NumPageSection := NumPageSection - 1;
      Bv_Visu.Invalidate;
    end;
    E_NumPage.Text     := IntToStr(NumPage);
    ChangeButtons;
    E_NumSect.Text     := IntToStr(NumSection);
    L_NumPageSect.Text := IntToStr(NumPageSection);
    L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
  end;
end;

procedure TF_Visu.Bt_NextPageClick(Sender: TObject);
begin
  with FReport do
  begin
    NumPage := NumPage + 1;
    if NumPageSection = T_Section(Sections[Pred(NumSection)]).NbPages then
    begin
      NumSection      := NumSection + 1;
      NumPageSection  := 1;
      Bv_Visu.Visible := False;
      with T_Section(Sections[Pred(NumSection)]), F_Visu do
      begin
        Bv_Visu.Height := Paper.H;
        Bv_Visu.Width  := Paper.W;
        Bv_Visu.Top    := 50 + (F_Visu.Height - 50 - Paper.H) div 2;
        Bv_Visu.Left   := (F_Visu.Width - Paper.W) div 2;
      end;
      Bv_Visu.Visible := True;
    end
    else
    begin
      NumPageSection := NumPageSection + 1;
      Bv_Visu.Invalidate;
    end;
    E_NumPage.Text     := IntToStr(NumPage);
    ChangeButtons;
    E_NumSect.Text     := IntToStr(NumSection);
    L_NumPageSect.Text := IntToStr(NumPageSection);
    L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
  end;
end;

procedure TF_Visu.Bt_LastPageClick(Sender: TObject);
begin
  with FReport do
  begin
    NumPage         := T_Section(Sections[Pred(Sections.Count)]).TotPages;
    NumSection      := Sections.Count;
    NumPageSection  := T_Section(Sections[Pred(Sections.Count)]).NbPages;
    E_NumPage.Text  := IntToStr(NumPage);
    Bv_Visu.Visible := False;
    with T_Section(Sections[Pred(NumSection)]), F_Visu do
    begin
      Bv_Visu.Height := Paper.H;
      Bv_Visu.Width  := Paper.W;
      Bv_Visu.Top    := 50 + (F_Visu.Height - 50 - Paper.H) div 2;
      Bv_Visu.Left   := (F_Visu.Width - Paper.W) div 2;
    end;
    Bv_Visu.Visible := True;
    ChangeButtons;
    E_NumSect.Text     := IntToStr(NumSection);
    L_NumPageSect.Text := IntToStr(NumPageSection);
    L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
  end;
end;

procedure TF_Visu.Bt_PrecSectClick(Sender: TObject);
begin
  with FReport do
  begin
    NumSection      := NumSection - 1;
    NumPage         := T_Section(Sections[Pred(NumSection)]).FirstPage;
    NumPageSection  := 1;
    E_NumPage.Text  := IntToStr(NumPage);
    Bv_Visu.Visible := False;
    with T_Section(Sections[Pred(NumSection)]), F_Visu do
    begin
      Bv_Visu.Height := Paper.H;
      Bv_Visu.Width  := Paper.W;
      Bv_Visu.Top    := 50 + (F_Visu.Height - 50 - Paper.H) div 2;
      Bv_Visu.Left   := (F_Visu.Width - Paper.W) div 2;
    end;
    Bv_Visu.Visible := True;
    ChangeButtons;
    E_NumSect.Text     := IntToStr(NumSection);
    L_NumPageSect.Text := IntToStr(NumPageSection);
    L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
  end;
end;

procedure TF_Visu.Bt_NextSectClick(Sender: TObject);
begin
  with FReport do
  begin
    NumSection      := NumSection + 1;
    NumPage         := T_Section(Sections[Pred(NumSection)]).FirstPage;
    NumPageSection  := 1;
    E_NumPage.Text  := IntToStr(NumPage);
    Bv_Visu.Visible := False;
    with T_Section(Sections[Pred(NumSection)]), F_Visu do
    begin
      Bv_Visu.Height := Paper.H;
      Bv_Visu.Width  := Paper.W;
      Bv_Visu.Top    := 50 + (F_Visu.Height - 50 - Paper.H) div 2;
      Bv_Visu.Left   := (F_Visu.Width - Paper.W) div 2;
    end;
    Bv_Visu.Visible := True;
    ChangeButtons;
    E_NumSect.Text     := IntToStr(NumSection);
    L_NumPageSect.Text := IntToStr(NumPageSection);
    L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
  end;
end;

procedure TF_Visu.ChangeButtons;
begin
  with FReport do
    if T_Section(Sections[Pred(Sections.Count)]).TotPages > 1 then
      if NumPage = 1 then
      begin
        Bt_FirstPage.Enabled := False;
        Bt_PrecPage.Enabled  := False;
        Bt_NextPage.Enabled  := True;
        Bt_LastPage.Enabled  := True;
        Bt_PrecSect.Enabled  := False;
        if Sections.Count > 1 then
          Bt_NextSect.Enabled := True
        else
          Bt_NextSect.Enabled := False;
      end
      else if NumPage = T_Section(Sections[Pred(Sections.Count)]).TotPages then
      begin
        Bt_FirstPage.Enabled := True;
        Bt_PrecPage.Enabled  := True;
        Bt_NextPage.Enabled  := False;
        Bt_LastPage.Enabled  := False;
        if Sections.Count > 1 then
          Bt_PrecSect.Enabled := True
        else
          Bt_PrecSect.Enabled := False;
        Bt_NextSect.Enabled := False;
      end
      else
      begin
        Bt_FirstPage.Enabled := True;
        Bt_PrecPage.Enabled  := True;
        Bt_NextPage.Enabled  := True;
        Bt_LastPage.Enabled  := True;
        if Sections.Count > 1 then
          if NumSection = 1 then
          begin
            Bt_PrecSect.Enabled := False;
            Bt_NextSect.Enabled := True;
          end
          else if NumSection = Sections.Count then
          begin
            Bt_PrecSect.Enabled := True;
            Bt_NextSect.Enabled := False;
          end
          else
          begin
            Bt_PrecSect.Enabled := True;
            Bt_NextSect.Enabled := True;
          end
        else
        begin
          Bt_PrecSect.Enabled := False;
          Bt_NextSect.Enabled := False;
        end;
      end
    else
    begin
      Bt_FirstPage.Enabled := False;
      Bt_PrecPage.Enabled  := False;
      Bt_NextPage.Enabled  := False;
      Bt_LastPage.Enabled  := False;
      Bt_PrecSect.Enabled  := False;
      Bt_NextSect.Enabled  := False;
    end;
end;

procedure TF_Visu.E_NumPageKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: Boolean);
var
  CptSect, CptPage, CptPageSect: integer;
begin
  if (KeyCode = KeyReturn) or (KeyCode = KeyPEnter) then
    with FReport do
    begin
      if E_NumPage.Value > T_Section(Sections[Pred(Sections.Count)]).TotPages then
        NumPage := T_Section(Sections[Pred(Sections.Count)]).TotPages
      else if E_NumPage.Value = 0 then
        NumPage := 1
      else
        NumPage := E_NumPage.Value;
      E_NumPage.Value := NumPage;
      CptSect := 0;
      CptPage := 0;
      repeat
        Inc(CptSect);
        CptPageSect := 0;
        repeat
          Inc(CptPage);
          Inc(CptPageSect);
        until (CptPage = NumPage) or (CptPage = T_Section(Sections[Pred(Cptsect)]).NbPages);
      until CptPage = NumPage;
      NumSection         := CptSect;
      NumPageSection     := CptPagesect;
      Bv_Visu.Invalidate;
      ChangeButtons;
      E_NumSect.Text     := IntToStr(NumSection);
      L_NumPageSect.Text := IntToStr(NumPageSection);
      L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
    end;
end;

procedure TF_Visu.E_NumSectKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: Boolean);
begin
  if (KeyCode = KeyReturn) or (KeyCode = KeyPEnter) then
    with FReport do
    begin
      if E_NumSect.Value > Sections.Count then
        NumSection := Sections.Count
      else if E_NumSect.Value = 0 then
        NumSection := 1
      else
        NumSection := E_NumSect.Value;
      E_NumSect.Value := NumSection;
      NumPage := T_Section(Sections[Pred(NumSection)]).FirstPage;
      NumPageSection := 1;
      E_NumPage.Value    := NumPage;
      Bv_Visu.Invalidate;
      ChangeButtons;
      L_NumPageSect.Text := IntToStr(NumPageSection);
      L_NbrPageSect.Text := IntToStr(T_Section(Sections[Pred(NumSection)]).NbPages);
    end;
end;

constructor TF_Visu.Create(AOwner: TComponent; AImprime: T_Report);
begin
  inherited Create(AOwner);
  FReport        := AImprime;
  Name           := 'F_Visu';
  WindowTitle    := 'Preview';
  WindowPosition := wpUser;
  SetPosition(0, 0, fpgApplication.ScreenWidth - 2, fpgApplication.ScreenHeight - 66);
  BackgroundColor := fpgColor(51,51,51); // black/brown or should be use clShadow2 for theming abilities??
  OnShow         := @FormShow;

  CreateReportImages;

  Bv_Command     := CreateBevel(Self, 0, 0, Width, 50, bsBox, bsRaised);
  Bt_Close       := CreateButton(Bv_Command, 10, 10, 26, '', @Bt_CloseClick, 'stdimg.exit');
  Bt_Print       := CreateButton(Bv_Command, 50, 10, 26, '', @Bt_PrintClick, 'stdimg.print');
  Bt_Print.Enabled := False;
  Bt_Printer     := CreateButton(Bv_Command, 90, 10, 26, '', @Bt_PrinterClick, 'repimg.Printer');
  Bt_Printer.Enabled := False;
  Bt_Stop        := CreateButton(Bv_Command, 130, 10, 26, '', @Bt_StopClick, 'repimg.Stop');
  Bt_Pdf         := CreateButton(Bv_Command, 170, 10, 26, '', @Bt_PdfClick, 'stdimg.Adobe_pdf');
  Bt_Pdf.ImageMargin := 0;
  Bv_Pages       := CreateBevel(Bv_Command, 220, 5, 300, 40, bsBox, bsLowered);
  Bv_Sections    := CreateBevel(Bv_Command, 540, 5, 500, 40, bsBox, bsLowered);

  Bt_FirstPage   := CreateButton(Bv_Pages, 54, 6, 26, '', @Bt_FirstPageClick, 'repimg.First');
  Bt_PrecPage    := CreateButton(Bv_Pages, 80, 6, 26, '', @Bt_PrecPageClick, 'repimg.Precedent');
  E_NumPage      := CreateEditInteger(Bv_Pages, 110, 6, 60, 0);
  E_NumPage.OnKeyPress := @E_NumPageKeypress;
  Bt_NextPage    := CreateButton(Bv_Pages, 174, 6, 26, '', @Bt_NextPageClick, 'repimg.Next');
  Bt_LastPage    := CreateButton(Bv_Pages, 200, 6, 26, '', @Bt_LastPageClick, 'repimg.Last');
  L_Pages        := CreateLabel(Bv_Pages, 5, E_NumPage.Top, 'Page', 45, E_NumPage.Height, taLeftJustify, tlcenter);
  L_FromPage     := CreateLabel(Bv_Pages, 235, E_NumPage.Top, 'of', 30, E_NumPage.Height, taLeftJustify, tlcenter);
  L_NbrPages     := CreateLabel(Bv_Pages, 265, E_NumPage.Top, ' ', 30, E_NumPage.Height, taCenter, tlcenter);

  Bt_PrecSect    := CreateButton(Bv_Sections, 90, 6, 26, '', @Bt_PrecSectClick, 'repimg.Precedent');
  E_NumSect      := CreateEditInteger(Bv_Sections, 120, 6, 60, 0);
  E_NumSect.OnKeyPress := @E_NumSectKeyPress;
  Bt_NextSect    := CreateButton(Bv_Sections, 184, 6, 26, '', @Bt_NextSectClick, 'repimg.Next');
  L_Sections     := CreateLabel(Bv_Sections, 5, E_NumSect.Top, 'Section', 75, E_NumSect.Height, taLeftJustify, tlcenter);
  L_FromSect     := CreateLabel(Bv_Sections, 250, E_NumSect.Top, 'of', 30, E_NumSect.Height, taLeftJustify, tlcenter);
  L_NbrSect      := CreateLabel(Bv_Sections, 280, E_NumSect.Top, '-', 30, E_NumSect.Height, taLeftJustify, tlcenter);
  L_PageSect     := CreateLabel(Bv_Sections, 320, E_NumSect.Top, 'Page', 45, E_NumSect.Height, taLeftJustify, tlcenter);
  L_NumPageSect  := CreateLabel(Bv_Sections, 365, E_NumSect.Top, '-', 30, E_NumSect.Height, taLeftJustify, tlcenter);
  L_FromPageSect := CreateLabel(Bv_Sections, 410, E_NumSect.Top, 'of', 30, E_NumSect.Height, taLeftJustify, tlcenter);
  L_NbrPageSect  := CreateLabel(Bv_Sections, 440, E_NumSect.Top, '-', 30, E_NumSect.Height, taLeftJustify, tlcenter);
end;

destructor TF_Visu.Destroy;
begin
  DeleteReportImages;
  inherited Destroy;
end;

end.

