unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_ledmatrix;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    LEDMatrix1: TfpgLEDMatrix;
    {@VFD_HEAD_END: MainForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(464, 211, 490, 258);
  WindowTitle := 'LEDMatrix demo';
  Hint := '';
  IconName := '';
  WindowPosition := wpOneThirdDown;

  LEDMatrix1 := TfpgLEDMatrix.Create(self);
  with LEDMatrix1 do
  begin
    Name := 'LEDMatrix1';
    SetPosition(70, 30, 150, 30);
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
