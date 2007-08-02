program tabtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpgfx, gfx_widget, gfxbase, gui_form, gui_tab, gui_button,
  fpgui_package;

type
  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    pcMain: TfpgPageControl;
    tsOne: TfpgTabSheet;
    tsTwo: TfpgTabSheet;
    tsThree: TfpgTabSheet;
    procedure   btnQuitClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Tab control test';
  SetPosition(100, 100, 566, 350);
  
  btnQuit := CreateButton(self, 476, 320, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];
  
  pcMain := TfpgPageControl.Create(self);
  pcMain.Top := 10;
  pcMain.Left := 10;
  pcMain.Width := Width - 20;
  pcMain.Height := 300;
  pcMain.Anchors := [anLeft, anTop, anRight, anBottom];

  tsOne := TfpgTabSheet.Create(pcMain);
  tsOne.Text := 'Tab One';
  tsOne.Top := 50;

  tsTwo := TfpgTabSheet.Create(pcMain);
  tsTwo.Text := 'Tab Two';
  tsTwo.Top := 50;

  tsThree := TfpgTabSheet.Create(pcMain);
  tsThree.Text := 'Tab Three';
  tsThree.Top := 50;

end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
end;

begin
  MainProc;
end.

