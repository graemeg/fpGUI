{ This is a basic fpReport preview dialog for fpGUI. }
unit fpreport_export_preview;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpreport,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_panel,
  fpg_button,
  fpg_label,
  fpg_widget,
  fpreport_export_fpgui;

type

  TCustomFPReportPreviewForm = class(TfpgForm)
  private
    FReport: TFPCustomReport;
    FReportPages: TFPList;
  protected
    function    GetEnableHyperLinks: Boolean ; virtual;
    procedure   SetEnableHyperLinks(AValue: Boolean); virtual;
    procedure   SetReport(AValue: TFPCustomReport); virtual;
    property    ReportPages: TFPList read FReportPages;
  public
    property    Report: TFPCustomReport read FReport write SetReport;
  published
    property    EnableHyperLinks: Boolean read GetEnableHyperLinks write SetEnableHyperLinks;
  end;


  TCustomFPReportPreviewFormClass = class of TCustomFPReportPreviewForm;


  { This is a default, minimalistic implementation. It is shown by default.
    For a better implementation, add the fpreportpreview form to your project
    uses clause. }
  TFPReportPreviewForm = class(TCustomFPReportPreviewForm)
  private
    {@VFD_HEAD_BEGIN: ReportPreviewForm}
    FToolbar: TfpgBevel;
    FBClose: TfpgButton;
    FBPrevious: TfpgButton;
    FBNext: TfpgButton;
    FPaintBox: TfpgWidget;
    {@VFD_HEAD_END: ReportPreviewForm}
    FRender: TFPReportExportCanvas;
    procedure UpdateButtonState;
  protected
    procedure SetReport(AValue: TFPCustomReport); override;
  public
    procedure AfterCreate; override;
    procedure DoCloseAction(Sender: TObject);
    procedure DoNextAction(Sender: TObject);
    procedure DoPaintReport(Sender: TObject);
    procedure DoPreviousAction(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;


  TFPreportPreviewExport = class(TFPReportExporter)
  protected
    procedure DoExecute(const ARTObjects: TFPList); override;
  public
    class var
       DefaultPreviewFormClass: TCustomFPReportPreviewFormClass;
    class function Name: string; override;
    class function Description: string; override;
  end;


implementation

{ TCustomFPReportPreviewForm }

procedure TCustomFPReportPreviewForm.SetEnableHyperLinks(AValue: Boolean);
begin
//
end;

function TCustomFPReportPreviewForm.GetEnableHyperLinks: Boolean;
begin
  Result:=False;
end;


procedure TCustomFPReportPreviewForm.SetReport(AValue: TFPCustomReport);
begin
  if FReport=AValue then Exit;
  FReport:=AValue;
end;

{ TFPreportPreviewExport }

procedure TFPreportPreviewExport.DoExecute(const ARTObjects: TFPList);
Var
  R: TCustomFPreportPreviewFormClass;
  F: TCustomFPReportPreviewForm;
begin
  R := Self.DefaultPreviewFormClass;
  if R = Nil then
    R := TFPReportPreviewForm;
  F := R.Create(nil);
  try
     F.Report := Self.Report;
     F.FReportPages := ARTObjects;
     F.Show;
     fpgApplication.Run;
  finally
     F.Free;
  end;
end;

class function TFPreportPreviewExport.Name: String;
begin
  Result := 'Preview';
end;

class function TFPreportPreviewExport.Description: String;
begin
  Result := 'Preview on screen';
end;

{ TFPReportPreviewForm }

procedure TFPReportPreviewForm.UpdateButtonState;
begin
  FBNext.Enabled := (FRender.PageNumber < ReportPages.Count);
  FBPrevious.Enabled := (FRender.PageNumber > 1);
end;

procedure TFPReportPreviewForm.SetReport(AValue: TFPCustomReport);
begin
  inherited SetReport(AValue);
  FRender.Report:=AValue;
//  If Assigned(AValue) then
//    FRender.Execute;
end;

procedure TFPReportPreviewForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: ReportPreviewForm}
  Name := 'ReportPreviewForm';
  SetPosition(357, 276, 800, 600);
  WindowTitle := 'fpGUI Report Preview';
  Hint := '';
  IconName := '';
  WindowPosition := wpOneThirdDown;
  OnShow := @FormShow;

  FToolbar := TfpgBevel.Create(self);
  with FToolbar do
  begin
    Name := 'FToolbar';
    SetPosition(0, 0, 300, 24);
    Align := alTop;
    Hint := '';
  end;

  FBClose := TfpgButton.Create(FToolbar);
  with FBClose do
  begin
    Name := 'FBClose';
    SetPosition(0, 0, 80, 24);
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @DoCloseAction;
  end;

  FBPrevious := TfpgButton.Create(FToolbar);
  with FBPrevious do
  begin
    Name := 'FBPrevious';
    SetPosition(81, 0, 80, 24);
    Text := 'Previous';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @DoPreviousAction;
  end;

  FBNext := TfpgButton.Create(FToolbar);
  with FBNext do
  begin
    Name := 'FBNext';
    SetPosition(161, 0, 80, 24);
    Text := 'Next';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @DoNextAction;
  end;

  FPaintBox := TfpgWidget.Create(self);
  with FPaintBox do
  begin
    Name := 'FPaintBox';
    SetPosition(12, 32, 272, 204);
    Name := 'FPaintBox';
    Align := alClient;
    OnPaint := @DoPaintReport;
  end;

  {@VFD_BODY_END: ReportPreviewForm}

  FRender := TFPReportExportCanvas.Create(Self);
  FRender.Canvas := FPaintBox.Canvas;
end;

procedure TFPReportPreviewForm.DoCloseAction(Sender: TObject);
begin
  Close;
end;

procedure TFPReportPreviewForm.DoNextAction(Sender: TObject);
begin
  FRender.PageNumber := FRender.PageNumber+1;
  FPaintBox.Invalidate;
  UpdateButtonState;
end;

procedure TFPReportPreviewForm.DoPaintReport(Sender: TObject);
begin
  FRender.Execute;
end;

procedure TFPReportPreviewForm.DoPreviousAction(Sender: TObject);
begin
  if FRender.PageNumber = 1 then
    Exit;
  FRender.PageNumber := FRender.PageNumber-1;
  FPaintBox.Invalidate;
  UpdateButtonState;
end;

procedure TFPReportPreviewForm.FormShow(Sender: TObject);
begin
  UpdateButtonState;
end;


initialization
  TFPReportPreviewExport.RegisterExporter;

end.

