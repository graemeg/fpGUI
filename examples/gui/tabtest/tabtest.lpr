program tabtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fpgfx, gfx_widget, gfxbase, gui_form, gui_tab, gui_button,
  gui_label, gui_edit, gui_checkbox;

type
  TMainForm = class(TfpgForm)
  private
    btnQuit: TfpgButton;
    pcMain: TfpgPageControl;
    tsOne: TfpgTabSheet;
    tsTwo: TfpgTabSheet;
    tsThree: TfpgTabSheet;
    tsFour: TfpgTabSheet;
    btn2, btn3, btn4: TfpgButton;
    chkSort: TfpgCheckBox;
    procedure   btnQuitClick(Sender: TObject);
    procedure   btn2Click(Sender: TObject);
    procedure   btn3Click(Sender: TObject);
    procedure   chkSortChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btn2Click(Sender: TObject);
begin
  pcMain.ActivePageIndex := 0;
end;

procedure TMainForm.btn3Click(Sender: TObject);
var
  i: integer;
begin
  i := tsFour.PageIndex + 1;
  if i > pcMain.PageCount-1 then  // we reached the end so start from front
    i := 0;
    
  tsFour.PageIndex := i;
end;

procedure TMainForm.chkSortChange(Sender: TObject);
begin
  pcMain.SortPages := chkSort.Checked;
  btn3.Enabled := not chkSort.Checked;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Tab control test';
  SetPosition(100, 100, 566, 350);
  
  btnQuit := CreateButton(self, 476, 320, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];

  pcMain := TfpgPageControl.Create(self);
  pcMain.Top      := 10;
  pcMain.Left     := 10;
  pcMain.Width    := Width - 20;
  pcMain.Height   := 300;
  pcMain.Anchors  := [anLeft, anTop, anRight, anBottom];

  // Tab One
  tsOne := TfpgTabSheet.Create(pcMain);
  tsOne.Text := 'Tab One';
  CreateLabel(tsOne, 15, 50, 'TabSheet One');
  CreateLabel(tsOne, 15, 30, 'Resize form to see PageControl left/right buttons in action.');
  CreateEdit(tsOne, 15, 100, 150, 25);

  // Tab Two
  tsTwo := TfpgTabSheet.Create(pcMain);
  tsTwo.Text := 'Tab Two';
  CreateLabel(tsTwo, 50, 50, 'TabSheet Two');
  CreateButton(tsTwo, 50, 100, 80, 'Button1', nil);

  // Tab Three
  tsThree := TfpgTabSheet.Create(pcMain);
  tsThree.Text := 'Tab Three';
  CreateLabel(tsThree, 80, 50, 'TabSheet Three');
  
  // Tab Four
  tsFour := TfpgTabSheet.Create(pcMain);
  tsFour.Text := 'This is one long text caption';
  tsFour.BackgroundColor := clMediumSeaGreen;
  
  pcMain.ActivePage := tsOne;

  btn2 := CreateButton(self, 10, 320, 80, 'Page 1', @btn2Click);
  btn2.Anchors := [anLeft, anBottom];

  btn3 := CreateButton(self, 100, 320, 80, 'Reorder Tab', @btn3Click);
  btn3.Anchors := [anLeft, anBottom];
  
  chkSort := CreateCheckBox(self, 190, 320, 'Sort Tabs');
  chkSort.OnChange := @chkSortChange;
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.

