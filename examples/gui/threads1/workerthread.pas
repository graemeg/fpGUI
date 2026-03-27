unit WorkerThread;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SyncObjs,
  fpg_base,
  fpg_main,
  fpg_progressbar,
  fpg_form;

type

  TWorkerThread = class(TThread)
  private
    FFile: TfpgString;
    FMaxValue: Integer;
    FProgress: Integer;
    FCheckSum: TfpgString;
    FProgressBar: TfpgProgressBar;
    FForm: TfpgForm;
    FResultQueue: TStringList;
    FResultLock: TCriticalSection;
    procedure DoFlushResults;
  protected
    procedure Execute; override;
    procedure Checksum;
  public
    constructor CreateCustom(CreateSuspended: Boolean; AForm: TfpgForm);
    destructor Destroy; override;
    property Progress: Integer read FProgress write FProgress;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property ProcessFile: TfpgString read FFile write FFile;
    property ProgressBar: TfpgProgressBar read FProgressBar write FProgressBar;
  end;

implementation

uses
  frm_main,
  sha1;

const
  RecSize = 2048;

{ TWorkerThread }

constructor TWorkerThread.CreateCustom(CreateSuspended: Boolean; AForm: TfpgForm);
begin
  Create(CreateSuspended);
  FForm := AForm;
  FResultQueue := TStringList.Create;
  FResultLock := TCriticalSection.Create;
end;

destructor TWorkerThread.Destroy;
begin
  FResultLock.Free;
  FResultQueue.Free;
  inherited Destroy;
end;

procedure TWorkerThread.DoFlushResults;
var
  snapshot: TStringList;
  i: Integer;
  parts: TStringList;
begin
  FResultLock.Acquire;
  try
    if FResultQueue.Count = 0 then
      Exit;
  finally
    FResultLock.Release;
  end;
  snapshot := TStringList.Create;
  try
    FResultLock.Acquire;
    try
      snapshot.Assign(FResultQueue);
      FResultQueue.Clear;
    finally
      FResultLock.Release;
    end;
    parts := TStringList.Create;
    try
      parts.Delimiter := '|';
      parts.StrictDelimiter := True;
      for i := 0 to snapshot.Count - 1 do
      begin
        parts.DelimitedText := snapshot[i];
        if parts.Count = 2 then
          TMainForm(FForm).Add(parts[0], parts[1]);
      end;
    finally
      parts.Free;
    end;
  finally
    snapshot.Free;
  end;
end;

procedure TWorkerThread.Execute;
begin
  writeln(' - TWorkerThread.Execute');
  while not Terminated do
  begin
    { QueueForm.Pop is thread-safe — call directly, no Synchronize needed }
    FFile := QueueForm.Pop;
    if FFile = '' then
      Break;
    writeln('processing: ', FFile);
    Checksum;
  end;
end;

procedure TWorkerThread.Checksum;
var
  s: TSHA1Digest;
  hash: string;
begin
  s := SHA1File(FFile);
  hash := SHA1Print(s);
  { Queue the result for main-thread delivery }
  FResultLock.Acquire;
  try
    FResultQueue.Add(FFile + '|' + hash);
  finally
    FResultLock.Release;
  end;
  Queue(@DoFlushResults);
end;

end.
