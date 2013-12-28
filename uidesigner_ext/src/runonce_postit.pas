(* 
RunOnce_PostIt allows your application to run only once.
If you try to run it again, you may post a message to the first running application.

It works for lcl, fpGUI, msei and console applications.

RunOnce procedure is (a lot of) inspired by LSRunOnce of LazSolutions, created 
by Silvio Clecio :  http://silvioprog.com.br 

Fred van Stappen
fiens@hotmail.com
*)

unit RunOnce_PostIt;

interface

uses
  {$IFDEF MSWINDOWS}
  JwaTlHelp32,
{$ENDIF}
   {$IF DEFINED(LCL)}
  ExtCtrls,       /// for lcl timer
     {$else}
  fpg_main, /// for fpgui timer
    {$endif}
  SysUtils, Classes, Process;

type
  TProc = procedure of object;

type
  TOncePost = class(TObject)
  private
    TheProc: TProc;
{$IF DEFINED(LCL)}
    ATimer: TTimer;         /// for lcl timer
    procedure InitMessage(AOwner: TComponent);
    {$else}
    ATimer: Tfpgtimer;             /// for fpGUI
    procedure InitMessage;
   {$endif}
    function ExecProcess(const ACommandLine: string): string;
    procedure ListProcess(const AProcess: TStringList; const AShowPID: boolean = False;
      const ASorted: boolean = True; const AIgnoreCurrent: boolean = False);
    function PostIt: string;
  const
    CLSUtilsProcessNameValueSeparator: char = '=';

    procedure onTimerPost(Sender: TObject);
    procedure RunOnce(AMessage: string);
    function IsRunningIDE(AProcess : string) :boolean;

  end;

procedure RunOnce(AMessage: string);

{$IF DEFINED(LCL)}
procedure InitMessage(AOwner: TComponent);    /// LcL
    {$else}
procedure InitMessage;      /// fpgui
   {$endif}
function IsRunningIDE(AProcess : string) :boolean;
procedure FreeRunOnce;
procedure StopMessage;
procedure StartMessage(AProc: Tproc; const AInterval: integer = 1000);

var
   TheOncePost: TOncePost;
   TheMessage: string;

implementation

procedure RunOnce(AMessage: string);
begin
  TheOncePost := TOncePost.Create;
  TheOncePost.RunOnce(AMessage);
end;

function IsRunningIDE(AProcess : string) :boolean;
begin
  if assigned(TheOncePost) then
 result := TheOncePost.IsRunningIDE(AProcess)
else
 result := true ;
end;

{$IF DEFINED(LCL)}
  procedure InitMessage(AOwner: TComponent);
begin
   if assigned(TheOncePost.ATimer) = false then  TheOncePost.InitMessage(AOwner);
end;
    {$else}
  procedure InitMessage ;         /// fpgui
begin
   if assigned(TheOncePost.ATimer) = false then  TheOncePost.InitMessage;
end;
   {$endif}


procedure StopMessage;
begin
  if assigned(TheOncePost.ATimer) then
   begin
   TheOncePost.ATimer.Enabled:=false;
   end;
end;

procedure StartMessage(AProc: Tproc; const AInterval: integer = 1000);
begin
  TheOncePost.ATimer.Enabled := false;
  TheOncePost.TheProc := AProc;
 TheOncePost.ATimer.Interval := AInterval;
 TheOncePost.ATimer.OnTimer := @TheOncePost.onTimerPost;
 TheOncePost.ATimer.Enabled := True;
end;

procedure FreeRunOnce;
begin
   if assigned(TheOncePost.ATimer) then
   begin
   TheOncePost.ATimer.Enabled:=false;
   TheOncePost.ATimer.Free;
   end;
  TheOncePost.Free;
end;


function TOncePost.PostIt: string;
var
  f: textfile;
begin
  if fileexists(GetTempDir + '.postit.tmp') then
  begin
    AssignFile(f, PChar(GetTempDir + '.postit.tmp'));
    Reset(F);
    Readln(F, TheMessage);
    CloseFile(f);
    DeleteFile(GetTempDir + '.postit.tmp');
    Result := TheMessage;
  end;
end;

procedure TOncePost.onTimerPost(Sender: TObject);
begin
  ATimer.Enabled:=false;
  fpgapplication.ProcessMessages;
  if PostIt <> '' then
    if TheProc <> nil then
      TheProc;
 // fpgapplication.ProcessMessages;
   ATimer.Enabled:=true;
end;

{$IF DEFINED(LCL)}
procedure TOncePost.InitMessage(AOwner: TComponent);
begin
   ATimer := TTimer.Create(AOwner);         /// for lcl timer
   ATimer.Enabled := false;
end;
{$else}
procedure TOncePost.InitMessage;
begin
   ATimer := TfpgTimer.Create(1000);           /// for fpGUI
   ATimer.Enabled := false;
 end;
{$endif}

function TOncePost.ExecProcess(const ACommandLine: string): string;
const
  READ_BYTES = 2048;
var
  VStrTemp: TStringList;
  VMemoryStream: TMemoryStream;
  VProcess: TProcess;
  I64: longint;
  VBytesRead: longint;
begin
  VMemoryStream := TMemoryStream.Create;
  VProcess := TProcess.Create(nil);
  VStrTemp := TStringList.Create;
  try
    VBytesRead := 0;
{$WARN SYMBOL_DEPRECATED OFF}
    VProcess.CommandLine := ACommandLine;
{$WARN SYMBOL_DEPRECATED ON}
    VProcess.Options := [poUsePipes, poNoConsole];
    VProcess.Execute;
    while VProcess.Running do
    begin
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64)
      else
        Sleep(100);
    end;
    repeat
      VMemoryStream.SetSize(VBytesRead + READ_BYTES);
      I64 := VProcess.Output.Read((VMemoryStream.Memory + VBytesRead)^, READ_BYTES);
      if I64 > 0 then
        Inc(VBytesRead, I64);
    until I64 <= 0;
    VMemoryStream.SetSize(VBytesRead);
    VStrTemp.LoadFromStream(VMemoryStream);
    Result := Trim(VStrTemp.Text);
  finally
    VStrTemp.Free;
    VProcess.Free;
    VMemoryStream.Free;
  end;
end;

procedure TOncePost.ListProcess(const AProcess: TStringList;
  const AShowPID: boolean; const ASorted: boolean; const AIgnoreCurrent: boolean);
var
{$IFDEF UNIX}
  I, J: integer;
  VOldNameValueSeparator: char;
{$ENDIF}
{$IFDEF MSWINDOWS}
  VSnapshotHandle: THandle;
  VProcessEntry32: TProcessEntry32;
{$ENDIF}
begin
{$IFDEF UNIX}
  VOldNameValueSeparator := AProcess.NameValueSeparator;
  AProcess.NameValueSeparator := CLSUtilsProcessNameValueSeparator;
  AProcess.Text := ExecProcess('sh -c "ps -A | awk ''{ print $4 "=" $1 }''"');
  J := AProcess.Count;
  for I := AProcess.Count downto 1 do
  begin
    if (I > J - 3) or (AIgnoreCurrent and
      (StrToIntDef(AProcess.ValueFromIndex[I - 1], -1) = integer(GetProcessID))) then
    begin
      AProcess.Delete(I - 1);
      Continue;
    end;
    if not AShowPID then
      AProcess.Strings[I - 1] := AProcess.Names[I - 1];
  end;
  AProcess.NameValueSeparator := VOldNameValueSeparator;
{$ENDIF}
{$IFDEF MSWINDOWS}
  try
    VSnapshotHandle := CreateToolHelp32SnapShot(TH32CS_SNAPALL, 0);
    VProcessEntry32.dwSize := SizeOf(TProcessEntry32);
    Process32First(VSnapshotHandle, VProcessEntry32);
    repeat
      if AIgnoreCurrent and (GetProcessID = VProcessEntry32.th32ProcessID) then
        Continue;
      if AShowPID then
        AProcess.Add(VProcessEntry32.szExeFile + CLSUtilsProcessNameValueSeparator +
          IntToStr(VProcessEntry32.th32ProcessID))
      else
        AProcess.Add(VProcessEntry32.szExeFile);
    until (not Process32Next(VSnapshotHandle, VProcessEntry32));
  except

  end;
{$ENDIF}
  if AProcess.Count > 0 then
    AProcess.Delete(0);
  AProcess.Sorted := ASorted;
end;

procedure TOncePost.RunOnce(AMessage: string);
var
  VProcess: TStringList;
  x, y: integer;
  f: textfile;
begin
  x := 0;
  y := 0;
  VProcess := TStringList.Create;
  ListProcess(VProcess, False, False, False);
  while x < VProcess.Count do
  begin
    if pos(ApplicationName, VProcess.Strings[x]) > 0 then
      Inc(y);
    if y > 1 then
    begin
      if AMessage <> '' then
      begin
        AssignFile(f, PChar(GetTempDir + '.postit.tmp'));
        rewrite(f);
        append(f);
        if Amessage = 'clear' then
        writeln(f, 'close') else  writeln(f, AMessage) ;
        Flush(f);
        CloseFile(f);
      end;
     Halt;
    end;
    Inc(x);
  end;
      if (ParamStr(1) <> '') and  (Amessage <> 'clear') then
     begin
        AssignFile(f, PChar(GetTempDir + '.postit.tmp'));
        rewrite(f);
        append(f);
        writeln(f,AMessage);
        Flush(f);
        CloseFile(f);
      end;
  VProcess.Free;
end;

function TOncePost.IsRunningIDE(AProcess : string) :boolean;
var
  VProcess: TStringList;
  x : integer;
  begin
  x := 0;
  result := false;
  VProcess := TStringList.Create;
  ListProcess(VProcess, False, False, False);
  while x < VProcess.Count do
  begin
    if pos(AProcess, VProcess.Strings[x]) > 0 then
     begin
     result := true;
     exit;
     end;
   inc(x);
 end;
  VProcess.Free;
end;

end.
                                                                   
