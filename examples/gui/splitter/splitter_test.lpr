program splitter_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, gfxbase, fpgfx,
  gui_form, gui_memo, gui_listbox,
  gui_panel, gui_progressbar, gui_splitter, fpgui_toolkit;

type
  { TfrmSplitterTest }

  TfrmSplitterTest = class(TfpgForm)
  public
    {@VFD_HEAD_BEGIN: frmSplitterTest}
    lstChoice: TfpgListBox;
    spl1: TfpgSplitter;
    mmSource: TfpgMemo;
    spl2: TfpgSplitter;
    mmDest: TfpgMemo;
    pnlName1: TfpgPanel;
    spl3: TfpgSplitter;
    pbName1: TfpgProgressBar;
    spl4: TfpgSplitter;
    {@VFD_HEAD_END: frmSplitterTest}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmSplitterTest.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmSplitterTest}
  Name := 'frmSplitterTest';
  SetPosition(292, 184, 553, 290);
  WindowTitle := 'Splitter Demo';

  lstChoice := TfpgListBox.Create(self);
  with lstChoice do
  begin
    Name := 'lstChoice';
    SetPosition(-1, 0, 160, 211);
    FontDesc := '#List';
    Items.Add('List item #1');
    Items.Add('List item #2');
    TabOrder := 3;
    Align := alLeft;
  end;

  spl1 := TfpgSplitter.Create(self);
  with spl1 do
  begin
    Name := 'spl1';
    SetPosition(159, 0, 2, 212);
    Align := alLeft;
  end;

  mmSource := TfpgMemo.Create(self);
  with mmSource do
  begin
    Name := 'mmSource';
    SetPosition(164, 0, 257, 90);
    Lines.Add('Memo1 Line #1');
    Lines.Add('Memo1 Line #2');
    FontDesc := '#Edit1';
    TabOrder := 2;
    Align := alTop;
  end;

  spl2 := TfpgSplitter.Create(self);
  with spl2 do
  begin
    Name := 'spl2';
    SetPosition(165, 90, 257, 2);
    Align := alTop;
  end;

  mmDest := TfpgMemo.Create(self);
  with mmDest do
  begin
    Name := 'mmDest';
    SetPosition(165, 94, 256, 116);
    Lines.Add('Memo2 Line #1');
    Lines.Add('Memo2 Line #2');
    FontDesc := '#Edit1';
    TabOrder := 1;
    Align := alClient;
  end;

  pnlName1 := TfpgPanel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(425, 0, 128, 208);
    Text := 'Panel';
    Align := alRight;
  end;

  spl3 := TfpgSplitter.Create(self);
  with spl3 do
  begin
    Name := 'spl3';
    SetPosition(422, 0, 2, 208);
    Align := alRight;
  end;

  pbName1 := TfpgProgressBar.Create(self);
  with pbName1 do
  begin
    Name := 'pbName1';
    SetPosition(0, 213, 554, 78);
    Position := 100;
    Align := alBottom;
  end;

  spl4 := TfpgSplitter.Create(self);
  with spl4 do
  begin
    Name := 'spl4';
    SetPosition(0, 211, 554, 2);
    Align := alBottom;
  end;
  
  // vvzh: the form appears unaligned under Linux, so we have to add the following line:
  Self.Realign;

  {@VFD_BODY_END: frmSplitterTest}
end;

procedure MainProc;
var
  frmSplitterTest: TfrmSplitterTest;
begin
  fpgApplication.Initialize;
  frmSplitterTest := TfrmSplitterTest.Create(nil);
  try
    frmSplitterTest.Show;
    fpgApplication.Run;
  finally
    frmSplitterTest.Free;
  end;
end;

begin
  MainProc;
end.
