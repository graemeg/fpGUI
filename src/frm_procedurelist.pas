unit frm_procedurelist;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_label,
  fpg_edit, fpg_combobox, fpg_grid;

type

  TProcedureListForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ProcedureListForm}
    Bevel1: TfpgBevel;
    Bevel2: TfpgBevel;
    lblSearch: TfpgLabel;
    edtSearch: TfpgEdit;
    cbObjects: TfpgComboBox;
    lblObjects: TfpgLabel;
    grdProcedures: TfpgStringGrid;
    {@VFD_HEAD_END: ProcedureListForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TProcedureListForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ProcedureListForm}
  Name := 'ProcedureListForm';
  SetPosition(325, 166, 564, 285);
  WindowTitle := 'Procedure List';
  Hint := '';
  ShowHint := True;
  WindowPosition := wpOneThirdDown;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 0, 564, 32);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsSpacer;
  end;

  Bevel2 := TfpgBevel.Create(self);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(0, 33, 564, 32);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    Shape := bsSpacer;
  end;

  lblSearch := TfpgLabel.Create(Bevel2);
  with lblSearch do
  begin
    Name := 'lblSearch';
    SetPosition(4, 8, 47, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Search';
  end;

  edtSearch := TfpgEdit.Create(Bevel2);
  with edtSearch do
  begin
    Name := 'edtSearch';
    SetPosition(52, 4, 264, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    Hint := '';
    TabOrder := 2;
    Text := '';
    FontDesc := '#Edit1';
  end;

  cbObjects := TfpgComboBox.Create(Bevel2);
  with cbObjects do
  begin
    Name := 'cbObjects';
    SetPosition(376, 4, 184, 22);
    Anchors := [anRight,anTop];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 3;
  end;

  lblObjects := TfpgLabel.Create(Bevel2);
  with lblObjects do
  begin
    Name := 'lblObjects';
    SetPosition(324, 8, 51, 16);
    Anchors := [anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Objects';
  end;

  grdProcedures := TfpgStringGrid.Create(self);
  with grdProcedures do
  begin
    Name := 'grdProcedures';
    SetPosition(4, 68, 556, 212);
    AddColumn('', 30, taLeftJustify);
    AddColumn('Procedure', 300, taLeftJustify);
    AddColumn('Type', 130, taLeftJustify);
    AddColumn('Line', 70, taRightJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 3;
  end;

  {@VFD_BODY_END: ProcedureListForm}
  {%endregion}
end;


end.
