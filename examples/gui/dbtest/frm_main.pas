unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_main,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_listbox, fpg_panel, fpgui_db, db, dbf, u_reportimages;

type

  TMainForm = class(TfpgForm)
  private
    DataSet: TDBF;
    DataSource: TDataSource;
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnFirstClick(Sender: TObject);
    procedure   btnPrevClick(Sender: TObject);
    procedure   btnNextClick(Sender: TObject);
    procedure   btnLastClick(Sender: TObject);
    procedure   ButtonEnter(Sender: TObject);
    procedure   ButtonExit(Sender: TObject);
    procedure   FormShow(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    btnQuit: TfpgButton;
    btnFirst: TfpgButton;
    btnPrev: TfpgButton;
    btnNext: TfpgButton;
    btnLast: TfpgButton;
    lstName1: TfpgListBox;
    dblblName: TfpgDBLabel;
    dblblEMail: TfpgDBLabel;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    pnlName1: TfpgBevel;
    lblName3: TfpgLabel;
    lblName4: TfpgLabel;
    lblStatusBar: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnFirstClick(Sender: TObject);
begin
  DataSet.First;
end;

procedure TMainForm.btnPrevClick(Sender: TObject);
begin
  DataSet.Prior;
end;

procedure TMainForm.btnNextClick(Sender: TObject);
begin
  DataSet.Next;
end;

procedure TMainForm.btnLastClick(Sender: TObject);
begin
  DataSet.Last;
end;

procedure TMainForm.ButtonEnter(Sender: TObject);
begin
  lblStatusBar.Text := TfpgButton(Sender).Hint;
end;

procedure TMainForm.ButtonExit(Sender: TObject);
begin
  lblStatusBar.Text := '';
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  x: Integer;
  s: string;
begin
  dblblName.DataSource    := DataSource;
  dblblName.DataField     := 'Name';
  dblblEMail.DataSource   := DataSource;
  dblblEMail.DataField    := 'Address';

  DataSet.Open;
  while not DataSet.EOF do
  begin
    SetLength(s, 0);
    for x := 0 to DataSet.FieldCount - 2 do
      s := s + DataSet.Fields[x].AsString + ', ';
    s := s + DataSet.Fields[DataSet.FieldCount - 1].AsString;
    lstName1.Items.Add(s);
    DataSet.Next;
  end;
  DataSet.First;
end;

constructor TMainForm.Create(AOwner: TComponent);
//var
//  fields: TDbfFieldDefs;
begin
  inherited Create(AOwner);
  CreateReportImages;
  DataSet             := TDBF.Create(Self);
  DataSet.TableName   := 'test.dbf';
  
  // If you wanted to create a new DBF table
{
  fields := TDbfFieldDefs.Create(self);
  fields.Add('Name', ftString, 50);
  fields.Add('Address', ftString, 150);
  DataSet.CreateTableEx(fields); // <== Now we have an empty db table

  DataSet.Open;
  
  Dataset.Insert;  // <== Start inserting data
  Dataset.FieldByName('Name').AsString := 'Graeme Geldenhuys';
  Dataset.FieldByName('Address').AsString := 'graemeg@nospam.co.za';
  DataSet.Post;
}
  
  DataSource          := TDataSource.Create(Self);
  DataSource.DataSet  := DataSet;

  OnShow :=@FormShow;
end;

destructor TMainForm.Destroy;
begin
  DataSet.Close;
  DataSource.Free;
  DataSet.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(461, 212, 417, 315);
  WindowTitle := 'fpGUI DB controls test';
  Hint := '';
  IconName := '';
  WindowPosition := wpOneThirdDown;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(332, 264, 75, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.quit';
    TabOrder := 1;
    OnClick := @btnQuitClicked;
  end;

  btnFirst := TfpgButton.Create(self);
  with btnFirst do
  begin
    Name := 'btnFirst';
    SetPosition(8, 264, 30, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'First record';
    ImageName := 'repimg.first';
    TabOrder := 2;
    OnClick := @btnFirstClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  btnPrev := TfpgButton.Create(self);
  with btnPrev do
  begin
    Name := 'btnPrev';
    SetPosition(40, 264, 30, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Previous record';
    ImageName := 'repimg.previous';
    TabOrder := 3;
    OnClick := @btnPrevClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  btnNext := TfpgButton.Create(self);
  with btnNext do
  begin
    Name := 'btnNext';
    SetPosition(72, 264, 30, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Next record';
    ImageName := 'repimg.next';
    TabOrder := 4;
    OnClick := @btnNextClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  btnLast := TfpgButton.Create(self);
  with btnLast do
  begin
    Name := 'btnLast';
    SetPosition(104, 264, 30, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := 'Last record';
    ImageName := 'repimg.last';
    TabOrder := 5;
    OnClick := @btnLastClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  lstName1 := TfpgListBox.Create(self);
  with lstName1 do
  begin
    Name := 'lstName1';
    SetPosition(8, 24, 400, 156);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 6;
  end;

  dblblName := TfpgDBLabel.Create(self);
  with dblblName do
  begin
    Name := 'dblblName';
    SetPosition(104, 208, 304, 20);
  end;

  dblblEMail := TfpgDBLabel.Create(self);
  with dblblEMail do
  begin
    Name := 'dblblEMail';
    SetPosition(104, 228, 304, 20);
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(20, 208, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Name:';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(20, 228, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'E-mail:';
  end;

  pnlName1 := TfpgBevel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(0, 296, 416, 18);
    Anchors := [anLeft,anRight,anBottom];
    Hint := '';
    Style := bsLowered;
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 4, 400, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Available DB Records:';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(8, 188, 168, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Current record:';
  end;

  lblStatusBar := TfpgLabel.Create(pnlName1);
  with lblStatusBar do
  begin
    Name := 'lblStatusBar';
    SetPosition(5, 1, 404, 16);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  {@VFD_BODY_END: MainForm}
end;


end.
