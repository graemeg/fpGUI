unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  contnrs,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_label,
  fpg_grid,
  fpg_button,
  fpg_listbox,
  fpg_editbtn,
  fpg_trackbar,
  syncobjs;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Label1: TfpgLabel;
    TrackBar1: TfpgTrackBar;
    btnStart: TfpgButton;
    edtStartDirectory: TfpgDirectoryEdit;
    lblDirectory: TfpgLabel;
    grdFiles: TfpgStringGrid;
    btnStop: TfpgButton;
    lbThreads: TfpgListBox;
    lblProgress: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    FMaxThreads: Integer;
    FThreads: TObjectList;
    FInUpdate: Boolean;
    FCritSection: TCriticalSection;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure TrackbarChanged(Sender: TObject; APosition: integer);
    procedure HandleFileQueued(Sender: TObject);
    procedure HandleThreadTerminate(Sender: TObject);
    procedure ToggleButtons;
  public
    procedure AfterCreate; override;
    procedure Add(Filename: TfpgString; Checksum: TfpgString);
  end;


  TProcessQueueForm = class(TfpgForm)
  private
    FOnFileQueued: TNotifyEvent;
    {@VFD_HEAD_BEGIN: ProcessQueueForm}
    ListBox1: TfpgListBox;
    {@VFD_HEAD_END: ProcessQueueForm}
    FQueue: TStringList;
    FCritSection: TCriticalSection;
    procedure SetOnFileQueued(AValue: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterCreate; override;
    { Thread-safe: adds a filename to the internal queue. }
    procedure Queue(FileName: TfpgString);
    { Thread-safe: removes and returns the first filename, or '' if empty. }
    function Pop: TfpgString;
    { Thread-safe: returns number of queued items. }
    function Count: Integer;
    { Main thread only: syncs the internal queue to the listbox display. }
    procedure UpdateDisplay;
    property OnFileQueued: TNotifyEvent read FOnFileQueued write SetOnFileQueued;
  end;

{@VFD_NEWFORM_DECL}

var
  QueueForm: TProcessQueueForm;


implementation

uses
  fpg_utils,
  fpg_dialogs,
  fpg_progressbar,
  RecursiveSearchThread,
  WorkerThread;

{@VFD_NEWFORM_IMPL}

{ TProcessQueueForm }

procedure TProcessQueueForm.SetOnFileQueued(AValue: TNotifyEvent);
begin
  if FOnFileQueued = AValue then
    Exit;
  FOnFileQueued := AValue;
end;

constructor TProcessQueueForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQueue := TStringList.Create;
  FCritSection := TCriticalSection.Create;
end;

destructor TProcessQueueForm.Destroy;
begin
  FCritSection.Free;
  FQueue.Free;
  inherited Destroy;
end;

procedure TProcessQueueForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ProcessQueueForm}
  Name := 'ProcessQueueForm';
  SetPosition(797, 82, 300, 621);
  WindowTitle := 'File Process Queue';
  Hint := '';
  WindowPosition := wpUser;

  ListBox1 := TfpgListBox.Create(self);
  with ListBox1 do
  begin
    Name := 'ListBox1';
    SetPosition(0, 0, 300, 621);
    Align := alClient;
    FontDesc := '#List';
    Hint := '';
    TabOrder := 1;
  end;

  {@VFD_BODY_END: ProcessQueueForm}
  {%endregion}
end;

procedure TProcessQueueForm.Queue(FileName: TfpgString);
begin
  FCritSection.Enter;
  try
    FQueue.Add(FileName);
  finally
    FCritSection.Leave;
  end;
  if Assigned(FOnFileQueued) then
    FOnFileQueued(Self);
end;

function TProcessQueueForm.Pop: TfpgString;
begin
  FCritSection.Enter;
  try
    if FQueue.Count > 0 then
    begin
      Result := FQueue[0];
      FQueue.Delete(0);
    end
    else
      Result := '';
  finally
    FCritSection.Leave;
  end;
end;

function TProcessQueueForm.Count: Integer;
begin
  FCritSection.Enter;
  try
    Result := FQueue.Count;
  finally
    FCritSection.Leave;
  end;
end;

procedure TProcessQueueForm.UpdateDisplay;
var
  i: Integer;
begin
  FCritSection.Enter;
  try
    ListBox1.Items.Assign(FQueue);
  finally
    FCritSection.Leave;
  end;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FThreads := TObjectList.Create(False);
  FInUpdate := False;
  FCritSection := TCriticalSection.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  try
    for i := FThreads.count - 1 downto 0 do
    begin
      with TWorkerThread(FThreads[i]) do
      begin
        OnTerminate := nil;
        ProgressBar.Free;
        Terminate;
        WaitFor;
      end;
      FThreads.Delete(i);
    end;
  finally
    FThreads.Free;
  end;
  FCritSection.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  fpgApplication.CreateForm(TProcessQueueForm, QueueForm);
  QueueForm.OnFileQueued := @HandleFileQueued;
  edtStartDirectory.Directory := fpgExtractFilePath(ParamStr(0));
  FMaxThreads := 2;
  TrackBar1.Position := FMaxThreads;
  TrackBar1.OnChange := @TrackbarChanged;
end;

procedure TMainForm.btnStartClick(Sender: TObject);
var
  t: TRecursiveSearchThread;
begin
  if edtStartDirectory.Directory = '' then
  begin
    ShowMessage('Please select a starting directory');
    exit;
  end;

  if not fpgDirectoryExists(edtStartDirectory.Directory) then
  begin
    ShowMessage(format('Directory <%s> does not exist.',[edtStartDirectory.Directory]));
    exit;
  end;

  QueueForm.Show;

  grdFiles.RowCount := 0;
  QueueForm.ListBox1.Items.Clear;
  t := TRecursiveSearchThread.Create(True);
  with t do
  begin
    StartDir := edtStartDirectory.Directory;
    FreeOnTerminate := True;
//    OnTerminate := @HandleThreadTerminate;
//    FCritSection.Enter;
//    FThreads.Add(t);
//    FCritSection.Leave;
    Start;
  end;
  ToggleButtons;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
var
  i: integer;
begin
  for i := FThreads.Count - 1 downto 0 do
    TThread(FThreads[i]).Terminate;
  QueueForm.Hide;
end;

procedure TMainForm.TrackbarChanged(Sender: TObject; APosition: integer);
begin
  FMaxThreads := APosition;
  HandleFileQueued(self);
end;

procedure TMainForm.HandleFileQueued(Sender: TObject);
var
  t: TWorkerThread;
  p: TfpgProgressBar;
  c: integer;
begin
  writeln('>>> HandleFileQueued');
  FCritSection.Enter;
// writeln('   - successfully acquired critical section');
  c := FThreads.Count;
//  if FThreads.Count > FMaxThreads then
//    TThread(FThreads[FThreads.Count-1]).Terminate;

  if (FThreads.Count < FMaxThreads) and (QueueForm.Count > 0) then
  begin
    writeln('Creating Worker thread');
    t := TWorkerThread.CreateCustom(True, self);
    with t do
    begin
      writeln('   1');
      FreeOnTerminate := True;
      OnTerminate := @HandleThreadTerminate;
      writeln('   2');
      p := TfpgProgressBar.Create(self);
      writeln('   3');
      ProgressBar := p;
      FThreads.Add(t);
      writeln('   starting thread');
      Start;
      writeln('   thread started');
    end;
  end;
  FCritSection.Leave;
  writeln('<<< HandleFileQueued');
end;

procedure TMainForm.HandleThreadTerminate(Sender: TObject);
var
  i: integer;
begin
  writeln('> HandleThreadTerminate');
  if Sender is TWorkerThread then
    TWorkerThread(Sender).ProgressBar.Free;

  FCritSection.Enter;
  for i := 0 to FThreads.Count-1 do
  begin
    if Sender = FThreads[i] then
    begin
      FThreads.Delete(i);
      break;
    end;
  end;
  FCritSection.Leave;

  ToggleButtons;
end;

procedure TMainForm.ToggleButtons;
begin
  btnStart.Enabled := FThreads.Count = 0;
  btnStop.Enabled := not btnStart.Enabled;
  if btnStop.Enabled and not FInUpdate then
  begin
//    grdFiles.BeginUpdate;
    FInUpdate := True;
  end
  else
  begin
//    grdFiles.EndUpdate;
    FInUpdate := False;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(101, 83, 679, 480);
  WindowTitle := 'fpGUI Thread Demo - File Checksums';
  Hint := '';
  WindowPosition := wpUser;
  OnDestroy := @FormDestroy;
  OnCreate := @FormCreate;
  OnShow := @FormShow;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(556, 4, 108, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Max Threads:';
  end;

  TrackBar1 := TfpgTrackBar.Create(self);
  with TrackBar1 do
  begin
    Name := 'TrackBar1';
    SetPosition(556, 20, 112, 23);
    Hint := '';
    Max := 20;
    Min := 1;
    Position := 6;
    ShowPosition := True;
    TabOrder := 2;
  end;

  btnStart := TfpgButton.Create(self);
  with btnStart do
  begin
    Name := 'btnStart';
    SetPosition(404, 20, 70, 23);
    Text := 'Start';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnStartClick;
  end;

  edtStartDirectory := TfpgDirectoryEdit.Create(self);
  with edtStartDirectory do
  begin
    Name := 'edtStartDirectory';
    SetPosition(4, 20, 392, 23);
    Directory := '';
    ExtraHint := '';
    RootDirectory := '';
    TabOrder := 4;
  end;

  lblDirectory := TfpgLabel.Create(self);
  with lblDirectory do
  begin
    Name := 'lblDirectory';
    SetPosition(4, 4, 228, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Starting Directory:';
  end;

  grdFiles := TfpgStringGrid.Create(self);
  with grdFiles do
  begin
    Name := 'grdFiles';
    SetPosition(4, 48, 392, 428);
    BackgroundColor := TfpgColor($80000002);
    AddColumn('File', 270, taLeftJustify);
    AddColumn('Checksum', 85, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 6;
  end;

  btnStop := TfpgButton.Create(self);
  with btnStop do
  begin
    Name := 'btnStop';
    SetPosition(477, 20, 70, 23);
    Text := 'Stop';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnStopClick;
  end;

  lbThreads := TfpgListBox.Create(self);
  with lbThreads do
  begin
    Name := 'lbThreads';
    SetPosition(404, 68, 272, 408);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 8;
  end;

  lblProgress := TfpgLabel.Create(self);
  with lblProgress do
  begin
    Name := 'lblProgress';
    SetPosition(404, 52, 80, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Progress:';
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;

procedure TMainForm.Add(Filename: TfpgString; Checksum: TfpgString);
begin
  { Called on the main thread (via Queue). No lock needed for GUI access. }
  grdFiles.RowCount := grdFiles.RowCount + 1;
  grdFiles.Cells[0, grdFiles.RowCount-1] := Filename;
  grdFiles.Cells[1, grdFiles.RowCount-1] := Checksum;
end;


end.
