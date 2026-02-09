unit frm_containers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_checkbox, fpg_panel,
  fpg_flowlayout,
  fpg_layouttypes;

type

  TFlowWithContainers = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: FlowWithContainers}
    Panel2: TfpgPanel;
    chkToggleBtn5: TfpgCheckBox;
    Panel1: TfpgPanel;
    {@VFD_HEAD_END: FlowWithContainers}
    procedure chkClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TFlowWithContainers.AfterCreate;
var
  FlowLayout: ILayoutManager;
  i: integer;
  Btn: TfpgButton;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FlowWithContainers}
  Name := 'FlowWithContainers';
  SetPosition(511, 461, 300, 250);
  WindowTitle := 'Flow With Containers';
  Hint := '';
  IconName := 'stdimg.windowicon';

  Panel2 := TfpgPanel.Create(self);
  with Panel2 do
  begin
    Name := 'Panel2';
    SetPosition(0, 210, 300, 40);
    Align := alBottom;
    Text := '';
  end;

  chkToggleBtn5 := TfpgCheckBox.Create(Panel2);
  with chkToggleBtn5 do
  begin
    Name := 'chkToggleBtn5';
    SetPosition(8, 12, 120, 16);
    Checked := True;
    TabOrder := 1;
    Text := 'Toggle button 5';
    OnClick := @chkClicked;
  end;

  Panel1 := TfpgPanel.Create(self);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(0, 0, 300, 250);
    Align := alClient;
    Text := 'Client Panel';
    TextColor := clGray;
  end;

  {@VFD_BODY_END: FlowWithContainers}
  {%endregion}

  // Create the FlowLayoutManager and assign it to the client panel
  FlowLayout := TfpgFlowLayoutManager.Create as ILayoutManager;
  Panel1.LayoutManager := FlowLayout;

  // Add some buttons to demonstrate wrapping
  for i := 1 to 10 do
  begin
    Btn := TfpgButton.Create(Panel1);
    Btn.Name := 'Btn' + IntToStr(i);
    Btn.Text := Format('Button %d', [i]);
    Btn.Width := 60 + (i * 5); // Vary width to force wrapping
    Btn.Height := 30;
    Btn.TextColor := clBlack;
    if i = 5 then
      Btn.BackgroundColor := clMoneyGreen;
    FlowLayout.AddLayoutComponent(Btn, TfpgLayoutConstraint.Create());
  end;
end;

procedure TFlowWithContainers.chkClicked(Sender: TObject);
var
  btn: TfpgButton;
begin
  btn := TfpgButton(Panel1.Components[4]);
  btn.Visible := not btn.Visible;
end;


end.
