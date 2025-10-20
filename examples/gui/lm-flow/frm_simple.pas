unit frm_simple;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button,
  fpg_flowlayout,
  fpg_layouttypes;

type

  TSimpleFlowForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: SimpleFlowForm}
    {@VFD_HEAD_END: SimpleFlowForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TSimpleFlowForm.AfterCreate;
var
  FlowLayout: ILayoutManager;
  i: integer;
  Btn: TfpgButton;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: SimpleFlowForm}
  Name := 'SimpleFlowForm';
  SetPosition(518, 365, 300, 250);
  WindowTitle := 'Simple Flow Form';
  Hint := '';
  IconName := 'stdimg.windowicon';

  {@VFD_BODY_END: SimpleFlowForm}
  {%endregion}

  // Create the FlowLayoutManager and assign it to the form
  FlowLayout := TfpgFlowLayoutManager.Create as ILayoutManager;
  LayoutManager := FlowLayout;

  // Add some buttons to demonstrate wrapping
  for i := 1 to 10 do
  begin
    Btn := TfpgButton.Create(Self);
    Btn.Name := 'Btn' + IntToStr(i);
    Btn.Text := Format('Button %d', [i]);
    Btn.Width := 60 + (i * 5); // Vary width to force wrapping
    Btn.Height := 30;
    FlowLayout.AddLayoutComponent(Btn, TfpgLayoutConstraint.Create());
  end;
end;


end.
