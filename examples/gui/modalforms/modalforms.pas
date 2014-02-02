{
  Demo of modal forms in action.
  
  NOTE:    Model form are not 100% implemented yet!
}
program modalforms;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpg_main,
  fpg_form,
  fpg_dialogs,
  fpg_button,
  fpg_label,
  fpg_popupcalendar,
  fpg_combobox,
  fpg_edit;
  
type
  // forward declaration
  TForm1 = class;
  TForm2 = class;

  TMainForm = class(TfpgForm)
  private
    cal: TfpgComboBox;
    btnClose: TfpgButton;
    btnOpenForm1: TfpgButton;
    edtTest: TfpgEdit;
    procedure   btnCloseClick(Sender: TObject);
    procedure   btnOpenForm1Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;
  

  TForm1 = class(TfpgForm)
  private
    cal: TfpgComboBox;
    Label1: TfpgLabel;
    btnClose: TfpgButton;
    btnOpenForm2: TfpgButton;
    edtTest: TfpgEdit;
    procedure   btnCloseClick(Sender: TObject);
    procedure   btnOpenForm2Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
  TForm2 = class(TfpgForm)
  private
    Label1: TfpgLabel;
    btnClose: TfpgButton;
    edtTest: TfpgEdit;
    procedure   btnCloseClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TForm2 }

procedure TForm2.btnCloseClick(Sender: TObject);
begin
  Close;
end;

constructor TForm2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Form2';
  Sizeable := False;
  SetPosition(200, 200, 200, 200);

  Label1 := CreateLabel(self, 10, 10, 'This is Form2');

  btnClose := CreateButton(self, 110, 170, 80, 'Quit', @btnCloseClick);
  btnClose.ImageName := 'stdimg.Quit';
  btnClose.ShowImage := True;
  
  edtTest := CreateEdit(self, 10, 60, 100, 23);

end;

{ TForm1 }

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnOpenForm2Click(Sender: TObject);
var
  frm: TForm2;
begin
  frm := TForm2.Create(nil);
  try
    frm.ShowModal;
    writeln('Form2: This should only appear after the form closes.');
    edtTest.SetFocus;
    ActivateWindow;
  finally
    frm.Free;
  end;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Form1';
  Sizeable := False;
  SetPosition(150, 150, 200, 200);

  Label1 := CreateLabel(self, 10, 10, 'This is Form1');
  
  btnClose := CreateButton(self, 110, 170, 80, 'Quit', @btnCloseClick);
  btnClose.ImageName := 'stdimg.Quit';
  btnClose.ShowImage := True;
  
  btnOpenForm2 := CreateButton(self, 70, 100, 80, 'Open Form2', @btnOpenForm2Click);
  
  cal := TfpgComboBox.Create(self);
  cal.SetPosition(10, 10, 120, 23);
  
  edtTest := CreateEdit(self, 10, 60, 100, 23);

end;


{ TMainForm }

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnOpenForm1Click(Sender: TObject);
var
  frm: TForm1;
begin
  frm := TForm1.Create(nil);
  try
    frm.ShowModal;
    writeln('Form1: This should only appear after the form closes.');
    ActivateWindow;
  finally
    frm.Free;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Modal Form Demo';
  Sizeable := False;
  SetPosition(100, 100, 400, 200);

  btnClose := CreateButton(self, 310, 170, 80, 'Quit', @btnCloseClick);
  btnClose.ImageName := 'stdimg.Quit';
  btnClose.ShowImage := True;
  
  btnOpenForm1 := CreateButton(self, 100, 100, 80, 'Open Form1', @btnOpenForm1Click);
  
  cal := TfpgComboBox.Create(self);
  cal.SetPosition(10, 10, 120, 23);
  
  edtTest := CreateEdit(self, 10, 60, 100, 23);
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

