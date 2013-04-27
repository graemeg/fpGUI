unit fra_liveview;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, 
  Classes, 
  fpg_base, 
  fpg_main, 
  fpg_form, 
  fpg_panel,
  fpg_grid;

type

  TLiveViewFrame = class(TfpgFrame)
  private
    {@VFD_HEAD_BEGIN: fra_liveview}
    Grid1: TfpgStringGrid;
    {@VFD_HEAD_END: fra_liveview}
  public
    procedure AfterCreate; override;
    property Grid: TfpgStringGrid read Grid1;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TLiveViewFrame.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: fra_liveview}
  Name := 'fra_liveview';
  SetPosition(359, 215, 442, 104);
  WindowTitle := 'fra_liveview';
  Hint := '';

  Grid1 := TfpgStringGrid.Create(self);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(0, 4, 444, 98);
    Anchors := [anLeft,anRight,anTop,anBottom];
    BackgroundColor := TfpgColor($80000002);
    AddColumn('Desc', 100, taLeftJustify);
    AddColumn('Value', 310, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    ShowHeader := False;
    TabOrder := 1;
  end;

  {@VFD_BODY_END: fra_liveview}
  {%endregion}
end;


end.
