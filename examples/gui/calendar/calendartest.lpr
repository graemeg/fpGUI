{
   This is still under development!!!!!!!!!!!!!!!!!
}

program calendartest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fpgfx, gui_form, gui_popupcalendar, gui_edit,
  gui_button, gui_label, gfx_popupwindow, gui_combobox;

type
  TMainForm = class(TfpgForm)
  private
    procedure   btnDownClicked(Sender: TObject);
    procedure   DoDropDown;
  public
    {@VFD_HEAD_BEGIN: MainForm}
    edtName1: TfpgEdit;
    btnName1: TfpgButton;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    cbName1: TfpgComboBox;
    {@VFD_HEAD_END: MainForm}
    FDropDown: TfpgPopupCalendar;
    procedure   AfterCreate; override;
  end;
  
{@VFD_NEWFORM_DECL}

{ TMainForm }

procedure TMainForm.btnDownClicked(Sender: TObject);
begin
  DoDropDown;
end;

procedure TMainForm.DoDropDown;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FDropDown     := TfpgPopupCalendar.Create(nil);
    FDropDown.ShowAt(self, edtName1.Left, edtName1.Top+edtName1.Height);
    FDropDown.PopupFrame:= True;
    FDropDown.grdName1.SetFocus;
  end
  else
  begin
    FDropDown.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(286, 234, 417, 270);
  WindowTitle := 'fpGUI Calendar Test';
  WindowPosition := wpUser;

  edtName1 := TfpgEdit.Create(self);
  with edtName1 do
  begin
    Name := 'edtName1';
    SetPosition(84, 48, 120, 22);
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnName1 := TfpgButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(204, 48, 19, 22);
    Text := '';
    FontDesc := '#Label1';
    ImageName := 'sys.sb.down';
    OnClick := @btnDownClicked;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(84, 32, 80, 16);
    Text := 'Enter a date:';
    FontDesc := '#Label1';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(68, 116, 276, 16);
    Text := '*****   This is still Work-In-Progress  *****';
    FontDesc := '#Label2';
  end;

  cbName1 := TfpgComboBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(124, 184, 120, 23);
    Items.Add('line1');
    Items.Add('line2');
    Items.Add('line3');
    Items.Add('line4');
    Items.Add('line5');
    Items.Add('line6');
    FontDesc := '#List';
  end;

  {@VFD_BODY_END: MainForm}
end;


{@VFD_NEWFORM_IMPL}

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.



