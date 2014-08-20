{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2005 by Michael Van Canneyt, member of
    the Free Pascal development team
  Copyright (C) 2013 by Graeme Geldenhuys

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    Originally from the Free Pascal FCL. Since then the code has
    diverged and was customised for fpGUI usage.
    
    This is the Client Interface for the debug server, which is
    based on SimpleIPC.
}
unit fpg_dbugintf;

{$mode objfpc}{$h+}

interface

uses
  Classes, 
  fpg_base;

Type
  TDebugLevel = (dlStop, dlInformation, dlWarning, dlError, dlIdentify, dlLive);

procedure SendBoolean(const Identifier: string; const Value: Boolean);
procedure SendDateTime(const Identifier: string; const Value: TDateTime);
procedure SendInteger(const Identifier: string; const Value: Integer; HexNotation: Boolean = False);
procedure SendPoint(const Identifier: string; const Value: TPoint; const ADbgLevel: TDebugLevel = dlLive);
procedure SendPointer(const Identifier: string; const Value: Pointer);
procedure SendRect(const Identifier: string; const Value: TRect; const ADbgLevel: TDebugLevel = dlInformation);
procedure SendRect(const Identifier: string; const Value: TfpgRect; const ADbgLevel: TDebugLevel = dlInformation);
procedure SendDebugEx(const Msg: string; MType: TDebugLevel);
procedure SendDebug(const Msg: string);
procedure SendMethodEnter(const MethodName: string);
procedure SendMethodExit(const MethodName: string);
procedure SendSeparator;
procedure SendDebugFmt(const Msg: string; const Args: array of const);
procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TDebugLevel; const ATitle: string = '');

procedure SetDebuggingEnabled(const AValue : boolean);
function GetDebuggingEnabled : Boolean;

{ low-level routines }

Function  StartDebugServer : integer;
Function InitDebugClient : Boolean;
Function DebugMessageName(msgType: TDebugLevel) : String;

Const
  SendError       : String = '';

ResourceString
  SProcessID = 'Process %s';
  SEntering = '> Entering ';
  SExiting  = '< Exiting ';
  SSeparator = '>-=-=-=-=-=-=-=-=-=-=-=-=-=-=-<';
  SServerStartFailed = 'Failed to start debugserver. (%s)';

implementation

Uses 
  SysUtils, 
  process, 
  simpleipc,
  fpg_dbugmsg;

const
  IndentChars    = 2;
  
var
  DebugClient : TSimpleIPCClient = nil;
  MsgBuffer : TMemoryStream = Nil;
  ServerID : Integer;
  DebugDisabled : Boolean = False;
  Indent : Integer = 0;
  
function RectToStr(const ARect: TRect): String;
begin
  with ARect do
    Result := Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)', [Left, Top, Right, Bottom]);
end;

function fpgRectToStr(const ARect: TfpgRect): String;
begin
  with ARect do
    Result := Format('(Left: %d; Top: %d; Width: %d; Height: %d)', [Left, Top, Width, Height]);
end;

function PointToStr(const APoint: TPoint): String;
begin
  with APoint do
    Result := Format('(X: %d; Y: %d)', [X, Y]);
end;
  
procedure WriteMessage(Const Msg : TDebugMessage);
begin
  MsgBuffer.Seek(0, soFromBeginning);
  WriteDebugMessageToStream(MsgBuffer, Msg);
  DebugClient.SendMessage(mtUnknown, MsgBuffer);
end;

procedure SendDebugMessage(Var Msg : TDebugMessage);
begin
  if DebugDisabled then exit;
  try
    If (DebugClient=Nil) then
      if InitDebugClient = false then exit;
    if (Indent > 0) then
      Msg.Msg := StringOfChar(' ', Indent) + Msg.Msg;
    WriteMessage(Msg);
  except
    On E: Exception do
      SendError := E.Message;
  end;
end;

procedure SendBoolean(const Identifier: string; const Value: Boolean);
const
  Booleans : Array[Boolean] of string = ('False','True');
begin
  SendDebugFmt('%s = %s',[Identifier,Booleans[value]]);
end;

procedure SendDateTime(const Identifier: string; const Value: TDateTime);
begin
  SendDebugFmt('%s = %s',[Identifier,DateTimeToStr(Value)]);
end;

procedure SendInteger(const Identifier: string; const Value: Integer; HexNotation: Boolean = False);
const
  Msgs : Array[Boolean] of string = ('%s = %d','%s = %x');
begin
  SendDebugFmt(Msgs[HexNotation],[Identifier,Value]);
end;

procedure SendPoint(const Identifier: string; const Value: TPoint; const ADbgLevel: TDebugLevel);
begin
  SendDebugFmtEx('%s = %s',[Identifier, PointToStr(Value)], ADbgLevel);
end;

procedure SendPointer(const Identifier: string; const Value: Pointer);
begin
  SendDebugFmt('%s = %p',[Identifier,Value]);
end;

procedure SendRect(const Identifier: string; const Value: TRect; const ADbgLevel: TDebugLevel);
begin
  SendDebugFmtEx('%s',[RectToStr(Value)], ADbgLevel, Identifier);
end;

procedure SendRect(const Identifier: string; const Value: TfpgRect; const ADbgLevel: TDebugLevel);
begin
  SendDebugFmtEx('%s',[fpgRectToStr(Value)], ADbgLevel, Identifier);
end;

procedure SendDebugEx(const Msg: string; MType: TDebugLevel);
var
  Mesg : TDebugMessage;
begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=Ord(MTYpe);
  Mesg.Msg:=Msg;
  SendDebugMessage(Mesg);
end;

procedure SendDebug(const Msg: string);
var
  Mesg : TDebugMessage;
begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:=Ord(dlInformation);
  Mesg.Msg:=Msg;
  SendDebugMessage(Mesg);
end;

procedure SendMethodEnter(const MethodName: string);
begin
  SendDebug(SEntering+MethodName);
  inc(Indent,IndentChars);
end;

procedure SendMethodExit(const MethodName: string);
begin
  Dec(Indent,IndentChars);
  If (Indent<0) then
    Indent:=0;
  SendDebug(SExiting+MethodName);
end;

procedure SendSeparator;
begin
  SendDebug(SSeparator);
end;

procedure SendDebugFmt(const Msg: string; const Args: array of const);
var
  Mesg : TDebugMessage;
begin
  Mesg.MsgTimeStamp:=Now;
  Mesg.MsgType:= Ord(dlInformation);
  Mesg.Msg:=Format(Msg,Args);
  SendDebugMessage(Mesg);
end;

procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TDebugLevel; const ATitle: string);
var
  Mesg: TDebugMessage;
begin
  Mesg.MsgTimeStamp := Now;
  Mesg.MsgType := Ord(mType);
  if MType = dlLive then
    Mesg.MsgTitle := ATitle
  else
    Mesg.MsgTitle := ' ';
  Mesg.Msg := Format(Msg,Args);
  SendDebugMessage(Mesg);
end;

procedure SetDebuggingEnabled(const AValue: boolean);
begin
  DebugDisabled := not AValue;
end;

function GetDebuggingEnabled: Boolean;
begin
  Result := not DebugDisabled;
end;

function StartDebugServer : Integer;
begin
  With TProcess.Create(Nil) do
    begin
    Try
      CommandLine:='dbugsrv';
      Execute;
      Result:=ProcessID;
    Except On E: Exception do
      begin
      SendError := Format(SServerStartFailed,[E.Message]);
      Result := 0;
      end;
    end;
    Free;
    end;
end;

procedure FreeDebugClient;
var
  msg : TDebugMessage;
begin
  try
    If (DebugClient<>Nil) and
       (DebugClient.ServerRunning) then
      begin
      Msg.MsgType := Ord(dlStop);
      Msg.MsgTimeStamp := Now;
      Msg.Msg := Format(SProcessID,[ApplicationName]);
      WriteMessage(Msg);
      end;
    if assigned(MsgBuffer) then 
      FreeAndNil(MsgBuffer);
    if assigned(DebugClient) then 
      FreeAndNil(DebugClient);
  except
  end;
end;

function InitDebugClient : Boolean;
var
  msg : TDebugMessage;
  I : Integer;
begin
  Result := False;
  DebugClient:=TSimpleIPCClient.Create(Nil);
  DebugClient.ServerID:=DebugServerID;
  If not DebugClient.ServerRunning then
    begin
    ServerID:=StartDebugServer;
    if ServerID = 0 then
      begin
      DebugDisabled := True;
      FreeAndNil(DebugClient);
      Exit;
      end
    else
      DebugDisabled := False;
    I:=0;
    While (I<10) and not DebugClient.ServerRunning do
      begin
      Inc(I);
      Sleep(100);
      end;
    end;
  try
    DebugClient.Connect;
  except
    FreeAndNil(DebugClient);
    DebugDisabled:=True;
    Raise;
  end;
  MsgBuffer := TMemoryStream.Create;
  Msg.MsgType := Ord(dlIdentify);
  Msg.MsgTimeStamp := Now;
  Msg.Msg := Format(SProcessID,[ApplicationName]);
  WriteMessage(Msg);
  Result := True;
end;

Function DebugMessageName(msgType : TDebugLevel) : String;
begin
  Case MsgType of
    dlStop        : Result := 'Stop';
    dlInformation : Result := 'Information';
    dlWarning     : Result := 'Warning';
    dlError       : Result := 'Error';
    dlIdentify    : Result := 'Identify';
    dlLive        : Result := 'LiveView';
  else
    Result := 'Unknown';
  end;
end;


finalization
  FreeDebugClient;

end.
