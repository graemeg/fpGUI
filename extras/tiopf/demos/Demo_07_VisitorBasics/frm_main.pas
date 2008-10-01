unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_main, fpg_form, fpg_button,
  Client_BOM;

type

  TMainForm = class(TfpgForm)
  private
    FClientList: TClientList;
    procedure btnAddClientClick(Sender: TObject);
    procedure btnShowListClick(Sender: TObject);
    procedure btnRunClientVisitorClick(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    btnAddClient: TfpgButton;
    btnShowList: TfpgButton;
    btnRunClientVisitor: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiUtils
  ,tiDialogs
  ,tiOPFManager
  ;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnAddClientClick(Sender: TObject);
var
  lClient: TClient;
begin
  lClient := TClient.Create;
  lClient.OID.AsString  := IntToStr(tiGetTickCount); // Not how you do it in real life!
  lClient.ClientName    := 'Test ' + DateTimeToStr(Now);
  lClient.ClientID      := IntToStr(tiGetTickCount);
  FClientList.Add(lClient);
end;

procedure TMainForm.btnShowListClick(Sender: TObject);
begin
  tiShowString(FClientList.AsDebugString);
end;

procedure TMainForm.btnRunClientVisitorClick(Sender: TObject);
var
  lVis: TClientVisitor;
begin
  lVis := TClientVisitor.Create;
  try
    FClientList.Iterate(lVis);
  finally
    lVis.Free;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientList := TClientList.Create;
end;

destructor TMainForm.Destroy;
begin
//  FClientList.Save;
  FClientList.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(854, 117, 244, 166);
  WindowTitle := 'Visitor Basics';
  WindowPosition := wpScreenCenter;
  Sizeable := False;

  btnAddClient := TfpgButton.Create(self);
  with btnAddClient do
  begin
    Name := 'btnAddClient';
    SetPosition(68, 24, 107, 24);
    Text := 'Add client';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnAddClientClick;
  end;

  btnShowList := TfpgButton.Create(self);
  with btnShowList do
  begin
    Name := 'btnShowList';
    SetPosition(68, 56, 107, 24);
    Text := 'Show list';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnShowListClick;
  end;

  btnRunClientVisitor := TfpgButton.Create(self);
  with btnRunClientVisitor do
  begin
    Name := 'btnRunClientVisitor';
    SetPosition(68, 88, 107, 24);
    Text := 'Run client visitor';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnRunClientVisitorClick;
  end;

  {@VFD_BODY_END: MainForm}
end;


end.

