unit ${UNITNAME};

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  fpg_base,
  fpg_main,
  fpg_form;

type

  T${FORMNAME} = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ${FORMNAME}}
    {@VFD_HEAD_END: ${FORMNAME}}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure T${FORMNAME}.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ${FORMNAME}}
  Name := '${FORMNAME}';
  SetPosition(374, 228, 490, 258);
  WindowTitle := 'New Form';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  {@VFD_BODY_END: ${FORMNAME}}
  {%endregion}
end;


end.
