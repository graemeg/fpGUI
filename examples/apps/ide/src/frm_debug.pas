{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit frm_debug;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_tab, fpg_grid;

type

  TDebugForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: DebugForm}
    PageControl1: TfpgPageControl;
    TabSheet1: TfpgTabSheet;
    TabSheet2: TfpgTabSheet;
    TabSheet3: TfpgTabSheet;
    TabSheet4: TfpgTabSheet;
    TabSheet5: TfpgTabSheet;
    TabSheet6: TfpgTabSheet;
    TabSheet7: TfpgTabSheet;
    Grid1: TfpgStringGrid;
    Grid2: TfpgStringGrid;
    Grid3: TfpgStringGrid;
    {@VFD_HEAD_END: DebugForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

var
  DebugForm: TDebugForm;

implementation

{@VFD_NEWFORM_IMPL}

procedure TDebugForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: DebugForm}
  Name := 'DebugForm';
  SetPosition(690, 193, 512, 247);
  WindowTitle := 'Debug Window';
  Hint := '';

  PageControl1 := TfpgPageControl.Create(self);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(4, 4, 506, 240);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 0;
  end;

  TabSheet1 := TfpgTabSheet.Create(PageControl1);
  with TabSheet1 do
  begin
    Name := 'TabSheet1';
    SetPosition(3, 24, 500, 213);
    Text := 'Watches';
  end;

  TabSheet2 := TfpgTabSheet.Create(PageControl1);
  with TabSheet2 do
  begin
    Name := 'TabSheet2';
    SetPosition(3, 24, 500, 213);
    Text := 'BreakPoints';
  end;

  TabSheet3 := TfpgTabSheet.Create(PageControl1);
  with TabSheet3 do
  begin
    Name := 'TabSheet3';
    SetPosition(3, 24, 500, 213);
    Text := 'Local Vars';
  end;

  TabSheet4 := TfpgTabSheet.Create(PageControl1);
  with TabSheet4 do
  begin
    Name := 'TabSheet4';
    SetPosition(3, 24, 500, 213);
    Text := 'Call Stack';
  end;

  TabSheet5 := TfpgTabSheet.Create(PageControl1);
  with TabSheet5 do
  begin
    Name := 'TabSheet5';
    SetPosition(3, 24, 500, 213);
    Text := 'Registers';
  end;

  TabSheet6 := TfpgTabSheet.Create(PageControl1);
  with TabSheet6 do
  begin
    Name := 'TabSheet6';
    SetPosition(3, 24, 500, 213);
    Text := 'Asm';
  end;

  TabSheet7 := TfpgTabSheet.Create(PageControl1);
  with TabSheet7 do
  begin
    Name := 'TabSheet7';
    SetPosition(3, 24, 500, 213);
    Text := 'GDB output';
  end;

  Grid1 := TfpgStringGrid.Create(TabSheet1);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(0, 4, 496, 204);
    AddColumn('Expression', 100, taLeftJustify);
    AddColumn('Value', 350, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 0;
  end;

  Grid2 := TfpgStringGrid.Create(TabSheet2);
  with Grid2 do
  begin
    Name := 'Grid2';
    SetPosition(0, 4, 496, 204);
    AddColumn('State', 50, taLeftJustify);
    AddColumn('Filename/Addres', 120, taLeftJustify);
    AddColumn('Line/Length', 85, taLeftJustify);
    AddColumn('Condition', 70, taLeftJustify);
    AddColumn('Action', 50, taLeftJustify);
    AddColumn('Count', 50, taLeftJustify);
    AddColumn('Group', 80, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 0;
  end;

  Grid3 := TfpgStringGrid.Create(TabSheet3);
  with Grid3 do
  begin
    Name := 'Grid3';
    SetPosition(0, 4, 496, 204);
    AddColumn('Name', 150, taLeftJustify);
    AddColumn('Value', 250, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 0;
  end;

  {@VFD_BODY_END: DebugForm}
  {%endregion}
end;


end.
