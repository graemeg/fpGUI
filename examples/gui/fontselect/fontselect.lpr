program fontselect;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  fpgfx,
  gui_form,
  gui_dialogs,
  gui_button;


type
  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    btnSelectFont: TfpgButton;
    procedure   btnQuitClick(Sender: TObject);
    procedure   btnSelectFontClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnSelectFontClick(Sender: TObject);
var
  frm: TfpgFontSelect;
begin
  frm := TfpgFontSelect.Create(nil);
  try
    if frm.ShowModal = 1 then
    begin
      // query font selected in dialog
    end;
  finally
    frm.Free;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Font selection test';
  SetPosition(100, 100, 500, 400);

  btnQuit := CreateButton(self, 415, 370, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];
  
  btnSelectFont := CreateButton(self, 10, 20, 110, 'Select Font...', @btnSelectFontClick);
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

