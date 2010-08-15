program beveltest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  typinfo,
  fpg_main,
  fpg_form,
  fpg_button,
  fpg_panel,
  fpg_label,
  fpg_checkbox;
  
type
  TMainForm = class(TfpgForm)
  private
    bevel: TfpgBevel;
    btnQuit: TfpgButton;
    btnStyles: TfpgButton;
    btnShapes: TfpgButton;
    lblTitle: TfpgLabel;
    lblStyle: TfpgLabel;
    lblShape: TfpgLabel;
    lblNext: TfpgLabel;
    chkDouble: TfpgCheckBox;
    procedure   chkDoubleChanged(Sender: TObject);
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnStylesClick(Sender: TObject);
    procedure   btnShapesClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.chkDoubleChanged(Sender: TObject);
begin
  if chkDouble.Checked then
    Bevel.BorderStyle := bsDouble
  else
    Bevel.BorderStyle := bsSingle;
end;

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnStylesClick(Sender: TObject);
begin
  if Bevel.Style = bsRaised then
  begin
    Bevel.Style     := bsLowered;
    lblStyle.Text   := 'Bevel is bsLowered';
    btnStyles.Text  := 'bsRaised';
  end
  else
  begin
    Bevel.Style     := bsRaised;
    lblStyle.Text   := 'Bevel is bsRaised';
    btnStyles.Text  := 'bsLowered';
  end;
end;

procedure TMainForm.btnShapesClick(Sender: TObject);
var
  next: TPanelShape;
begin
  if Bevel.Shape = High(TPanelShape) then
    Bevel.Shape := Low(TPanelShape)
  else
    Bevel.Shape := TPanelShape(Ord(Bevel.Shape) + 1);
  lblShape.Text := 'Shape is ' + GetEnumName(TypeInfo(TPanelShape), Ord(Bevel.Shape));
  
  if Bevel.Shape = High(TPanelShape) then
    next := Low(TPanelShape)
  else
    next := TPanelShape(Ord(Bevel.Shape) + 1);
  btnShapes.Text := GetEnumName(TypeInfo(TPanelShape), Ord(next));
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Bevel test';
  SetPosition(100, 100, 300, 250);
  
  lblTitle  := CreateLabel(self, 6, 6, 'Click buttons to change properties');
  bevel     := CreateBevel(self, 20, 30, 150, 150, bsBox, bsRaised);

  btnQuit   := CreateButton(self, 210, 220, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.quit';
  btnQuit.ShowImage := True;
  
  lblNext   := CreateLabel(self, 200, 80, 'Next value is...');
  btnShapes := CreateButton(self, 200, 100, 90, 'bsFrame', @btnShapesClick);
  btnStyles := CreateButton(self, 200, 130, 90, 'bsLowered', @btnStylesClick);
  chkDouble := CreateCheckBox(self, 200, 160, 'Double Line');
  chkDouble.OnChange  := @chkDoubleChanged;

  lblShape  := CreateLabel(self, 6, 190, 'Shape is bsBox');
  lblStyle  := CreateLabel(self, 6, 210, 'Style is bsRaised');
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.

