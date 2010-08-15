program cursordemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_label;

type

  TMainForm = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: MainForm}
    Panel1: TfpgPanel;
    Panel2: TfpgPanel;
    Panel3: TfpgPanel;
    Panel4: TfpgPanel;
    Panel5: TfpgPanel;
    Panel6: TfpgPanel;
    Panel7: TfpgPanel;
    Panel8: TfpgPanel;
    Panel9: TfpgPanel;
    Panel10: TfpgPanel;
    Panel11: TfpgPanel;
    Panel12: TfpgPanel;
    Panel13: TfpgPanel;
    Label1: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(388, 200, 311, 204);
  WindowTitle := 'Mouse Cursor Demo';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(20, 4, 256, 16);
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Available mouse cursors in fpGUI';
  end;

  Panel1 := TfpgPanel.Create(self);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(8, 32, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcDefault';
    MouseCursor := mcDefault;
  end;

  Panel2 := TfpgPanel.Create(self);
  with Panel2 do
  begin
    Name := 'Panel2';
    SetPosition(108, 32, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcArrow';
    MouseCursor := mcArrow;
  end;

  Panel3 := TfpgPanel.Create(self);
  with Panel3 do
  begin
    Name := 'Panel3';
    SetPosition(208, 32, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcCross';
    MouseCursor := mcCross;
  end;

  Panel4 := TfpgPanel.Create(self);
  with Panel4 do
  begin
    Name := 'Panel4';
    SetPosition(8, 64, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcIBeam';
    MouseCursor := mcIBeam;
  end;

  Panel5 := TfpgPanel.Create(self);
  with Panel5 do
  begin
    Name := 'Panel5';
    SetPosition(108, 64, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeEW';
    MouseCursor := mcSizeEW;
  end;

  Panel6 := TfpgPanel.Create(self);
  with Panel6 do
  begin
    Name := 'Panel6';
    SetPosition(208, 64, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeNS';
    MouseCursor := mcSizeNS;
  end;

  Panel7 := TfpgPanel.Create(self);
  with Panel7 do
  begin
    Name := 'Panel7';
    SetPosition(8, 96, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeNWSE';
    MouseCursor := mcSizeNWSE;
  end;

  Panel8 := TfpgPanel.Create(self);
  with Panel8 do
  begin
    Name := 'Panel8';
    SetPosition(108, 96, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeNESW';
    MouseCursor := mcSizeNESW;
  end;

  Panel9 := TfpgPanel.Create(self);
  with Panel9 do
  begin
    Name := 'Panel9';
    SetPosition(208, 96, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeSWNE';
    MouseCursor := mcSizeSWNE;
  end;

  Panel10 := TfpgPanel.Create(self);
  with Panel10 do
  begin
    Name := 'Panel10';
    SetPosition(8, 128, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeSENW';
    MouseCursor := mcSizeSENW;
  end;

  Panel11 := TfpgPanel.Create(self);
  with Panel11 do
  begin
    Name := 'Panel11';
    SetPosition(108, 128, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcMove';
    MouseCursor := mcMove;
  end;

  Panel12 := TfpgPanel.Create(self);
  with Panel12 do
  begin
    Name := 'Panel12';
    SetPosition(208, 128, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcHourGlass';
    MouseCursor := mcHourGlass;
  end;

  Panel13 := TfpgPanel.Create(self);
  with Panel13 do
  begin
    Name := 'Panel13';
    SetPosition(8, 160, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcHand';
    MouseCursor := mcHand;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


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

