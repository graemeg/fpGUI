unit frmMain;

{$mode objfpc}{$H+}
  {$ASMMODE intel}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main,
  fpg_form, fpg_label, fpg_button,
  tiRtfReport;

type

  //Add extra functions in your rtfreport
  TRtfPrivateParser = class(TtiRtfParser)
  protected
    procedure AddFunctions; override;
    procedure UdfBla(AArgument: TRtfArgument);
    procedure UdfCentreAddress(AArgument: TRtfArgument);
  end;


  TMainForm = class(TfpgForm)
  private
    ResultFile: string;
    TemplateFile: string;
    procedure btnEditClicked(Sender: TObject);
    procedure btnParseClicked(Sender: TObject);
    procedure btnShowClicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    Label1: TfpgLabel;
    btnParse: TfpgButton;
    btnEdit: TfpgButton;
    btnShow: TfpgButton;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}


implementation

uses
  tiObject, process;


type
  // Simple BOM structure, nothing fancy
  TtiNestedDemoItem = class(TtiObject)
  private
    FId: integer;
    FName: string;
  published
    property Id: integer read FId write FId;
    property Name: string read FName write FName;
  end;


  TtiNestedDemoItems = class(TtiObjectList)
  protected
    function GetItems(Idx: integer): TtiNestedDemoItem; reintroduce;
  public
    property Items[Idx: integer]: TtiNestedDemoItem read GetItems;
  end;


  TtiDemoItem = class(TtiObject)
  private
    FId: integer;
    FName: string;
    FData: TtiNestedDemoItem;
    FList: TtiNestedDemoItems;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Id: integer read FId write FId;
    property Name: string read FName write FName;
    property oData: TtiNestedDemoItem read FData;
    property oList: TtiNestedDemoItems read FList;
  end;


  TtiDemoItems = class(TtiObjectList)
  protected
    function GetItems(Idx: integer): TtiDemoItem; reintroduce;
  public
    procedure Populate;
    property Items[Idx: integer]: TtiDemoItem read GetItems;
  end;


{ TtiNestedDemoItems }

function TtiNestedDemoItems.GetItems(Idx: integer): TtiNestedDemoItem;
begin
  Result := TtiNestedDemoItem(inherited GetItems(Idx));
end;


{ TtiDemoItem }

constructor TtiDemoItem.Create;
begin
  inherited;
  FData := TtiNestedDemoItem.Create;
  FList := TtiNestedDemoItems.Create;
end;

destructor TtiDemoItem.Destroy;
begin
  FData.Free;
  FList.Free;
  inherited;
end;


{ TtiDemoItems }

function TtiDemoItems.GetItems(Idx: integer): TtiDemoItem;
begin
  Result := TtiDemoItem(inherited GetItems(Idx));
end;

procedure TtiDemoItems.Populate;
var
  i, j: integer;
  ADemoItem: TtiDemoItem;
  ANestedDemoItem: TtiNestedDemoItem;
begin
  for i := 1 to 20 do begin
    ADemoItem := TtiDemoItem.Create;
    ADemoItem.Id := i;
    ADemoItem.Name := Format('This is demo item %d',[i]);
    ADemoItem.oData.Id := i * 1000;
    ADemoItem.oData.Name := Format('This is demo data item %d',[i * 1000]);
    Add(ADemoItem);

    for j := 1 to Random(5) do begin
      ANestedDemoItem := TtiNestedDemoItem.Create;
      ANestedDemoItem.Id := j;
      ANestedDemoItem.Name := Format('This is nested demo item %d',[j]);
      ADemoItem.oList.Add(ANestedDemoItem);
    end;
  end;
end;


{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnEditClicked(Sender: TObject);
var
  p: TProcess;
begin
  p := TProcess.Create(nil);
  try
    p.CommandLine := 'xdg-open ' + TemplateFile;
    p.Execute;
  finally
    p.Free;
  end;
end;

procedure TMainForm.btnParseClicked(Sender: TObject);
var
  AStart: TDateTime;
  FParser: TRtfPrivateParser;
  ADemoItems: TtiDemoItems;
begin
  ADemoItems := TtiDemoItems.Create;
  try
    ADemoItems.Populate;

    try
      AStart := Now;

      Label1.Text := 'working';
      btnShow.Enabled := false;
      btnParse.Enabled := false;
      MouseCursor := mcHourGlass;
      FParser := TRtfPrivateParser.Create;
      try
//        FParser.OnPictureAttr := OnPictureAttr;
//        FParser.OnCreateDataset := OnCreateDataset;
        FParser.Datasets.Add(ADemoItems, 'DemoItems');
//        FParser.Datasets.Add(tbBioLife, 'BioLife');
        FParser.LoadFromFile(TemplateFile);
        FParser.Execute;
        FParser.SaveToFile(ResultFile);
      finally
        FParser.Free;
        MouseCursor := mcDefault;
        btnShow.Enabled := true;
        btnParse.Enabled := true;
      end;

      Label1.Text := Format('Session completed in %s',[FormatDateTime('hh:nn:ss:zzz', Now - AStart)]);
      btnShow.Click;
    except
      on E: Exception do begin
        fpgApplication.HandleException(E);
      end;
    end;

  finally
    ADemoItems.Free;
  end;
end;

procedure TMainForm.btnShowClicked(Sender: TObject);
var
  p: TProcess;
begin
  p := TProcess.Create(nil);
  try
    p.CommandLine := 'xdg-open ' + ResultFile;
    p.Execute;
  finally
    p.Free;
  end;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TemplateFile := 'demo.rtf';
//  TemplateFile := 'demo_ms.rtf';
  ResultFile := 'result.rtf';
  ShortDateFormat := 'yyyy-mm-dd';
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(333, 208, 300, 124);
  WindowTitle := 'RTF Reporting Demo';

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(20, 16, 244, 16);
    FontDesc := '#Label1';
    Text := '--';
  end;

  btnParse := TfpgButton.Create(self);
  with btnParse do
  begin
    Name := 'btnParse';
    SetPosition(28, 80, 75, 24);
    Text := 'Parse';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnParseClicked;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(112, 80, 75, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnEditClicked;
  end;

  btnShow := TfpgButton.Create(self);
  with btnShow do
  begin
    Name := 'btnShow';
    SetPosition(196, 80, 75, 24);
    Text := 'Show';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnShowClicked;
  end;

  {@VFD_BODY_END: MainForm}
end;


{ TRtfPrivateParser }

procedure TRtfPrivateParser.AddFunctions;
begin
  inherited AddFunctions;
  Functions.Add(etFunction, 'Bla', 0, 0, @UdfBla);
  Functions.Add(etFunction, 'CentreAddress', 0, 0, @UdfCentreAddress);
end;

procedure TRtfPrivateParser.UdfBla(AArgument: TRtfArgument);
begin
  AArgument.Token := etLitString;
  AArgument.Value := 'Best value is 1.5e+400';
end;

procedure TRtfPrivateParser.UdfCentreAddress(AArgument: TRtfArgument);
begin
  AArgument.Token := etLitString;
  AArgument.Value := '8 Stellendal Road, Somerset West, 7130';
end;

end.
