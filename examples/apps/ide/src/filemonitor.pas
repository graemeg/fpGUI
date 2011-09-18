unit filemonitor;

{$mode objfpc}

interface

uses
  Classes, SysUtils, fpg_main, fpg_base, contnrs;

type

  TFileMonitorEventType = (fmeFileCreated,
                         fmeFileChanged,
                         fmeFileDeleted,
                         fmeFileRenamed,
                         fmeUnknownChange);

  TFileMonitorEventData = record
    EventType: TFileMonitorEventType;
    FileName: TfpgString;
//    OldFileName: TfpgString;
//    UserData: Pointer;
  end;


  TFileChangedEvent = procedure(Sender: TObject; AData: TFileMonitorEventData) of object;




  TMonitoredFile = class(TObject)
  private
    FName: TfpgString;
    FSize: Int64;
    FDate: TDateTime;
    FSHA1: TfpgString;
    function AsString: TfpgString;
  public
    function GetNewSHA1: TfpgString;
    procedure UpdateInfo;
    property Name: TfpgString read FName write FName;
    property Size: Int64 read FSize write FSize;
    property Date: TDateTime read FDate write FDate;
    property SHA1: TfpgString read FSHA1 write FSHA1;
  end;


  TFileMonitor = class(TThread)
  private
    FInterval: LongWord;
    FFileList: TObjectList;
    FOnFileChanged: TFileChangedEvent;
    FCurrent: TMonitoredFile;
    FCurrentState: TFileMonitorEventType;
    procedure DoFileChangeNotification;
  public
    constructor CreateCustom;
    destructor Destroy; override;
    procedure Execute; override;
    property  Interval: LongWord read FInterval write FInterval;
    procedure AddFile(const AFilename: TfpgString);
    procedure RemoveFile(const AFilename: TfpgString);
    property  OnFileChanged: TFileChangedEvent read FOnFileChanged write FOnFileChanged;
  end;


implementation

uses
  fpg_utils,
  sha1;

function ReadFileDate(const AFileName: string): TDateTime;
var
  fa: LongInt;
begin
  fa := FileAge(AFileName);
  Result := FileDateToDateTime(fa);
end;

function ReadFileSize(const AFileName: string): DWord;
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LFileStream.Size;
  finally
    LFileStream.Free;
  end;
end;

procedure ReadFileDateSize(const AFileName: string; var ADateTime: TDateTime; var AFileSize: Int64);
begin
  ADateTime  := ReadFileDate(AFileName);
  AFileSize := ReadFileSize(AFileName);
end;


{ TMonitoredFile }

function TMonitoredFile.AsString: TfpgString;
const
  OBJ_AS_STRING = '%s %s %d';
begin
  Result := Format(OBJ_AS_STRING, [FName, FormatDateTime('yyyy-mm-dd hh:mm:ss', FDate), FSize]);
end;

function TMonitoredFile.GetNewSHA1: TfpgString;
var
  lFile: TMonitoredFile;
  s: Int64;
  d: TDateTime;
begin
  s := 0;
  d := 0.0;
  lFile := TMonitoredFile.Create;
  try
    lFile.Name := FName;
    ReadFileDateSize(lFile.Name, d, s);
    lFile.Size := s;
    lFile.Date := d;

    Result := SHA1ofStr(lFile.AsString);
  finally
    lFile.Free;
  end;
end;

procedure TMonitoredFile.UpdateInfo;
var
  s: Int64;
  d: TDateTime;
begin
  ReadFileDateSize(Name, d, s);
  Size := s;
  Date := d;
  SHA1 := SHA1ofStr(AsString);
end;

{ TFileMonitor }

procedure TFileMonitor.DoFileChangeNotification;
var
  rec: TFileMonitorEventData;
begin
  if Assigned(FOnFileChanged) then
  begin
    rec.EventType := FCurrentState;
    rec.FileName := FCurrent.Name;
    FOnFileChanged(self, rec);
  end;
end;

constructor TFileMonitor.CreateCustom;
begin
  Create(True);
  FFileList := TObjectList.create(True);
  FInterval := 500;
end;

destructor TFileMonitor.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

procedure TFileMonitor.Execute;
var
  i: integer;
  lFile: TMonitoredFile;
begin
  while not Terminated do
  begin
    if Assigned(FOnFileChanged) then
    begin
      for i := FFileList.Count-1 downto 0 do
      begin
        lFile := TMonitoredFile(FFileList[i]);
        if fpgFileExists(lFile.Name) then
        begin
          if lFile.SHA1 <> lFile.GetNewSHA1 then
          begin
            FCurrent := lFile;
            FCurrentState := fmeFileChanged;
            Synchronize(@DoFileChangeNotification);
            lFile.UpdateInfo;
          end;
        end
        else
        begin
          FCurrent := lFile;
          FCurrentState := fmeFileDeleted;
          Synchronize(@DoFileChangeNotification);
          FFileList.Remove(lFile);
        end;
      end;
    end;
    sleep(FInterval);
  end;
end;

procedure TFileMonitor.AddFile(const AFilename: TfpgString);
var
  lFile: TMonitoredFile;
  s: Int64;
  d: TDateTime;
begin
  s := 0;
  d := 0.0;
  lFile := TMonitoredFile.Create;
  lFile.Name := AFileName;
  ReadFileDateSize(AFileName, d, s);
  lFile.Size := s;
  lFile.Date := d;
  lFile.SHA1 := SHA1ofStr(lFile.AsString);
  FFileList.Add(lFile);
end;

procedure TFileMonitor.RemoveFile(const AFilename: TfpgString);
begin
  //
end;

end.

