unit frm_procedurelist;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_panel, fpg_label,
  fpg_edit, fpg_combobox, fpg_grid, pparser, pastree;

type

  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement; override;
    function FindElement(const AName: String): TPasElement; override;
  end;


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
    FFilename: TfpgString;
    procedure   FormShow(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

function DisplayProcedureList(const AFilename: TfpgString): boolean;


implementation

uses
  ideconst;


function DisplayProcedureList(const AFilename: TfpgString): boolean;
var
  frm: TProcedureListForm;
begin
  try
    frm := TProcedureListForm.Create(nil);
    frm.FFilename := AFilename;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TProcedureListForm.FormShow(Sender: TObject);
var
  M: TPasModule;
  E: TPasTreeContainer;
  I: Integer;
  Decls: TList;
  p: TPasElement;
begin
  E := TSimpleEngine.Create;
  try
    M := ParseSource(E, FFilename, OSTarget, CPUTarget);

    { Cool, we successfully parsed the unit.
      Now output some info about it. }
    Decls := M.InterfaceSection.Declarations;
    for I := 0 to Decls.Count - 1 do
    begin
      p := TObject(Decls[I]) as TPasElement;
      Writeln('Interface item ', I, ': ' + p.Name + ' [line ' + IntToStr(p.SourceLinenumber) + ']');

    end;
    FreeAndNil(M);
  finally
    FreeAndNil(E)
  end;
end;

constructor TProcedureListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnShow  := @FormShow;
end;

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


{ TSimpleEngine }

function TSimpleEngine.CreateElement(AClass: TPTreeElement;
  const AName: String; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
end;

end.
