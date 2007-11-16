{
  This program tests the speed of Reference Counted and non-Reference Counted
  objects over a set time period. Default of 5 seconds.
}
program performancetest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp;

const
  CTestCount = 1000000;  // 1 million

type
  TMyApplication = class(TCustomApplication)
  protected
    procedure   DoRun; override;
    procedure   TestRefCountedObjects;
    procedure   TestNonRefCountedObjects;
    procedure   TestRefCountedMyObjects;
    procedure   TestNonRefCountedMyObjects;
    procedure   Log(const AMessage: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   WriteHelp; virtual;
  end;
  

  TMyBaseObject = class(TObject, IUnknown)
  private
    FRefCounting: Boolean;
    FRefCount: Integer;
  protected
    function    QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
    function    _AddRef: longint; stdcall;
    function    _Release: longint; stdcall;
  public
    constructor Create;
    destructor  Destroy; override;
    constructor CreateWithRefCounting;
    procedure   AfterConstruction; override;
    procedure   BeforeDestruction; override;
    class function NewInstance: TObject; override;
  end;

{ TMyBaseObject }

function TMyBaseObject.QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TMyBaseObject._AddRef: longint; stdcall;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TMyBaseObject._Release: longint; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if FRefCounting then
    if Result = 0 then
      Destroy;
end;

constructor TMyBaseObject.Create;
begin
  FRefCounting := False;
end;

destructor TMyBaseObject.Destroy;
begin
  inherited Destroy;
end;

constructor TMyBaseObject.CreateWithRefCounting;
begin
  Create;
  FRefCounting := True;
end;

procedure TMyBaseObject.AfterConstruction;
begin
  inherited AfterConstruction;
  // Release the constructor's implicit refcount
  if FRefCounting then
    InterlockedDecrement(FRefCount);
end;

procedure TMyBaseObject.BeforeDestruction;
begin
  if FRefCounting then
    if FRefCount <> 0 then
      System.Error(reInvalidPtr);
  inherited BeforeDestruction;
end;

class function TMyBaseObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TMyBaseObject(Result).FRefCount := 1;
end;
  
function tiGetTickCount: Cardinal;
begin
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;


{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h','help');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Halt;
  end;

  // parse parameters
  if HasOption('h','help') then
  begin
    WriteHelp;
    Halt;
  end;

//  TestNonRefCountedMyObjects;
  TestNonRefCountedObjects;
  TestRefCountedObjects;
  TestNonRefCountedMyObjects;
  TestRefCountedMyObjects;

  // stop program loop
  Terminate;
end;

procedure TMyApplication.TestRefCountedObjects;
var
  LO: TInterfacedObject;
  LStart: Cardinal;
  LCount: Cardinal;
begin
  LCount := 0;
  LStart := tiGetTickCount;
  while LCount < CTestCount do
  begin
    LO := TInterfacedObject.Create;
    LO.Free;
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d ms (reference counting with TInterfacedObject)',
      [IntToStr(LCount), tiGetTickCount - LStart]));
end;

procedure TMyApplication.TestNonRefCountedObjects;
var
  LO: TObject;
  LStart: Cardinal;
  LCount: Cardinal;
begin
  LCount := 0;
  LStart := tiGetTickCount;
  while LCount < CTestCount do
  begin
    LO := TObject.Create;
    LO.Free;
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d ms (no reference counting with TObject)',
      [IntToStr(LCount), tiGetTickCount - LStart]));
end;

procedure TMyApplication.TestRefCountedMyObjects;
var
  LO: TMyBaseObject;
  LStart: Cardinal;
  LCount: Cardinal;
begin
  LCount := 0;
  LStart := tiGetTickCount;
  while LCount < CTestCount do
  begin
    LO := TMyBaseObject.CreateWithRefCounting;
    LO.Free;
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d ms (reference counting with MyBaseObject)',
      [IntToStr(LCount), tiGetTickCount - LStart]));
end;

procedure TMyApplication.TestNonRefCountedMyObjects;
var
  LO: TMyBaseObject;
  LStart: Cardinal;
  LCount: Cardinal;
begin
  LCount := 0;
  LStart := tiGetTickCount;
  while LCount < CTestCount do
  begin
    LO := TMyBaseObject.Create;
    LO.Free;
    Inc(LCount);
  end;
  Log(Format('%s iterations in %d ms (no reference counting with MyBaseObject)',
      [IntToStr(LCount), tiGetTickCount - LStart]));
end;

procedure TMyApplication.Log(const AMessage: string);
begin
  writeln(AMessage);
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.

