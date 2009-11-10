unit frm_configuration;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form;

type

  TConfigurationForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ConfigurationForm}
    {@VFD_HEAD_END: ConfigurationForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure ShowConfigForm;

implementation


procedure ShowConfigForm;
var
  frm: TConfigurationForm;
begin
  frm := TConfigurationForm.Create(nil);
  try
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TConfigurationForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ConfigurationForm}
  Name := 'ConfigurationForm';
  SetPosition(300, 150, 373, 263);
  WindowTitle := 'Configuration';
  WindowPosition := wpOneThirdDown;

  {@VFD_BODY_END: ConfigurationForm}
  {%endregion}
end;


end.
