unit frmCompilerOpt;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, fpgui;
  

type

  { TCompilerOptForm }

  TCompilerOptForm = Class(TFForm)
  private
    FGroupBoxStyle: TStyleAbs;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CompOptFormOnActivate(Sender: TObject);
    procedure SmartLinkClick(Sender: TObject);
  published
    procedure btnCloseClick(Sender: TObject);
    Box1: TFBoxLayout;
    Box2: TFBoxLayout;
    Box3: TFBoxLayout;
    Box4: TFBoxLayout;
    Box5: TFBoxLayout;
    Box6: TFBoxLayout;
    btnOK, btnCancel, btnShowOp, btnTest, btnLoadSave : TFButton;
    grpBox1: TFGroupBox;
      grpBox1VBox1: TFBoxLayout;
        cbSmartLink: TFCheckbox;
    grpBox2: TFGroupBox;
      grpBox2VBox1: TFBoxLayout;
        rbIO: TFRadioButton;
        rbOverflow: TFRadioButton;
        rbRange: TFRadioButton;
        rbStack: TFRadioButton;
    grpBox3: TFGroupBox;
      grpBox3VBox1: TFBoxLayout;
        edHeapSize: TFEdit;
    grpBox4: TFGroupBox;
      grpBox4VBox1: TFBoxLayout;
        rbNormal: TFRadioButton;
        rbFaster: TFRadioButton;
        rbSmaller: TFRadioButton;
    grpBox5: TFGroupBox;
      grpBox5VBox1: TFBoxLayout;
        lblTarget1: TFLabel;
        lblTarget2: TFLabel;
        lblTarget3: TFLabel;
    grpBox6: TFGroupBox;
      grpBox6VBox1: TFBoxLayout;
        rbLevel0: TFRadioButton;
        rbLevel1: TFRadioButton;
        rbLevel2: TFRadioButton;
        rbLevel3: TFRadioButton;
        rbKeepVarReg: TFCheckbox;
        rbUncOpt: TFCheckbox;
  end;

var
  CompOpt: TCompilerOptForm;

implementation
//uses
//  OpenSoftStyle;

{ TCompilerOptForm }



constructor TCompilerOptForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FGroupBoxStyle := TOpenSoftStyle.Create(Application.Display);
//  Style := FGroupBoxStyle;
//  Style := FOpenSoftStyle;
  OnActivate := @CompOptFormOnActivate;
end;


destructor TCompilerOptForm.Destroy;
begin
  FGroupBoxStyle.Free;
  inherited Destroy;
end;


procedure TCompilerOptForm.CompOptFormOnActivate(Sender: TObject);
begin
  cbSmartLink.OnClick := @SmartLinkClick;

{
  grpBox1.CanExpandHeight := True;
  grpBox1VBox1.CanExpandHeight := True;
  grpBox1.CanExpandWidth := True;
  grpBox2.CanExpandWidth := True;
  grpBox3.CanExpandWidth := True;
  grpBox4.CanExpandWidth := True;
  grpBox5.CanExpandWidth := True;
  grpBox6.CanExpandWidth := True;
  
  grpBox6VBox1.CanExpandWidth := True;
}
  grpBox5VBox1.VertAlign := vertFill;
//  grpBox1.Style := FGroupBoxStyle;
//  grpBox6.Style := FGroupBoxStyle;
end;


procedure TCompilerOptForm.SmartLinkClick(Sender: TObject);
begin
  grpBox1.Style := nil;
  self.Redraw;
//  grpBox1.Redraw;
end;


procedure TCompilerOptForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


initialization
finalization
  CompOpt.Free;

end.

