unit frm_main;

{$mode objfpc}{$H+}

// You need to enable these in tiOPF's tiDefines.inc as well.
{.$Define object_tracking}
{$Define reference_counting}


interface

uses
  SysUtils, Classes, gfxbase, fpgfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_bevel, gui_popupcalendar, gui_gauge;

type

  TMainForm = class(TfpgForm)
  private
    procedure   PerformanceTestNoReferenceCounting(Sender: TObject);
    procedure   PerformanceTestReferenceCounting(Sender: TObject);
    procedure   btnTestValidClick(Sender: TObject);
    procedure   Log(const AMessage: string);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    btnNoRefCount: TfpgButton;
    btnRefCount: TfpgButton;
    btnTestValid: TfpgButton;
    memName1: TfpgMemo;
    lblName1: TfpgLabel;
    lblName2: TfpgLabel;
    memLog: TfpgMemo;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiUtils,
  tiBaseObject;
  
const
  CTestRunTime = 5; // Seconds
  CTestCount = 1000000; // 1 million

{@VFD_NEWFORM_IMPL}

procedure TMainForm.PerformanceTestNoReferenceCounting(Sender: TObject);
var
  LO: TtiBaseObject;
  LStart: Cardinal;
  LCount: Cardinal;
begin
  LCount := 0;
  LStart := tiGetTickCount;
  while LCount < CTestCount do
  begin
    LO := TtiBaseObject.Create;
    LO.Free;
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d ms (no reference counting)',
      [tiIntToCommaStr(LCount), tiGetTickCount - LStart]));
end;

procedure TMainForm.PerformanceTestReferenceCounting(Sender: TObject);
{$ifdef reference_counting}
var
//  LO: IInterface;
  LO: TtiBaseObject;  // We are testing object creation only. Hence a class and not interface reference.
  LStart: Cardinal;
  LCount: Cardinal;
begin
  LCount := 0;
  LStart := tiGetTickCount;
  while LCount < CTestCount do
  begin
    LO := TtiBaseObject.CreateWithRefCounting;
    LO.Free;    // we are testing object creation. This remove the garbage collector.
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d ms (reference counting)',
      [tiIntToCommaStr(LCount), tiGetTickCount - LStart]));
{$else}
begin
  Assert(False, 'reference_counting not enabled');
{$endif}
end;

procedure TMainForm.btnTestValidClick(Sender: TObject);
var
  LO: TtiBaseObject;
begin
  LO := TtiBaseObject.Create;
  Assert(LO.TestValid);
  LO.Free;
  Assert(not LO.TestValid);
end;

procedure TMainForm.Log(const AMessage: string);
begin
  memLog.Lines.Add(AMessage);
  memLog.Invalidate;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  memLog.Lines.Clear;
  {$ifdef object_tracking}
    Log('object_tracking is ON');
  {$else}
    btnTestValid.Enabled := False;
    Log('object_tracking is OFF');
  {$endif}
  {$ifdef reference_counting}
    Log('reference_counting is ON');
  {$else}
    Log('reference_counting is OFF');
    btnRefCount.Enabled := False;
  {$endif}
  Log('---');
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(307, 319, 357, 290);
  WindowTitle := 'TtiBaseObject Performance Demo';
  WindowPosition := wpScreenCenter;

  btnNoRefCount := TfpgButton.Create(self);
  with btnNoRefCount do
  begin
    Name := 'btnNoRefCount';
    SetPosition(16, 28, 155, 23);
    Text := 'No reference counting';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @PerformanceTestNoReferenceCounting;
  end;

  btnRefCount := TfpgButton.Create(self);
  with btnRefCount do
  begin
    Name := 'btnRefCount';
    SetPosition(16, 56, 155, 23);
    Text := 'Reference counting';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @PerformanceTestReferenceCounting;
  end;

  btnTestValid := TfpgButton.Create(self);
  with btnTestValid do
  begin
    Name := 'btnTestValid';
    SetPosition(16, 112, 155, 23);
    Text := 'TtiBaseObject.TestValid';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnTestValidClick;
  end;

  memName1 := TfpgMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(176, 28, 176, 125);
    Lines.Add('Toggle the conditional defines');
    Lines.Add('REFERENCE_COUNTING ');
    Lines.Add('and OBJECT_TRACKING on ');
    Lines.Add('and off to examine behaviour.');
    FontDesc := '#Edit1';
    Enabled := False;
  end;

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 160, 15);
    Text := 'Performance';
    FontDesc := '#Label2';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 92, 160, 15);
    Text := 'TtiBaseObject.TestValid';
    FontDesc := '#Label2';
  end;

  memLog := TfpgMemo.Create(self);
  with memLog do
  begin
    Name := 'memLog';
    SetPosition(8, 164, 344, 121);
    Lines.Add('');
    FontDesc := '#Edit1';
  end;

  {@VFD_BODY_END: MainForm}
end;


end.
