unit frmCompilerOpt;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, fpgui;
  

type

  { TCompilerOptForm }

  TCompilerOptForm = Class(TForm)
  private
    FGroupBoxStyle: TStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CompOptFormOnActivate(Sender: TObject);
    procedure SmartLinkClick(Sender: TObject);
  published
    procedure btnCloseClick(Sender: TObject);
    Box1: TBoxLayout;
    Box2: TBoxLayout;
    Box3: TBoxLayout;
    Box4: TBoxLayout;
    Box5: TBoxLayout;
    Box6: TBoxLayout;
    btnOK, btnCancel, btnShowOp, btnTest, btnLoadSave : TButton;
    grpBox1: TGroupBox;
      grpBox1VBox1: TBoxLayout;
        cbSmartLink: TCheckbox;
    grpBox2: TGroupBox;
      grpBox2VBox1: TBoxLayout;
        rbIO: TRadioButton;
        rbOverflow: TRadioButton;
        rbRange: TRadioButton;
        rbStack: TRadioButton;
    grpBox3: TGroupBox;
      grpBox3VBox1: TBoxLayout;
        edHeapSize: TEdit;
    grpBox4: TGroupBox;
      grpBox4VBox1: TBoxLayout;
        rbNormal: TRadioButton;
        rbFaster: TRadioButton;
        rbSmaller: TRadioButton;
    grpBox5: TGroupBox;
      grpBox5VBox1: TBoxLayout;
        lblTarget1: TLabel;
        lblTarget2: TLabel;
        lblTarget3: TLabel;
    grpBox6: TGroupBox;
      grpBox6VBox1: TBoxLayout;
        rbLevel0: TRadioButton;
        rbLevel1: TRadioButton;
        rbLevel2: TRadioButton;
        rbLevel3: TRadioButton;
        rbKeepVarReg: TCheckbox;
        rbUncOpt: TCheckbox;
  end;

var
  CompOpt: TCompilerOptForm;

implementation
uses
  OpenSoftStyle;

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

