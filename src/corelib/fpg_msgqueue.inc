{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      fpGUI internal message queue implementation

}

{%mainunit fpg_main.pas}

type
  // a simlpe linked list implementation
  TMessageListElement = class(TObject)
  protected
    Next: TMessageListElement;
    Prev: TMessageListElement;
  public
    msg: TfpgMessageRec;
  end;

var
  // to make things easier we have a few global variable that will get set
  UsedFirstMessage: TMessageListElement;
  UsedLastMessage: TMessageListElement;
  FreeFirstMessage: TMessageListElement;
  FreeLastMessage: TMessageListElement;

procedure MsgListInsertElement(Elem: TMessageListElement; var First: TMessageListElement; var Last: TMessageListElement);
begin
  Elem.Prev := nil;
  Elem.Next := nil;

  if First = nil then
  begin
    First := Elem;
    Last  := Elem;
  end
  else
  begin
    Last.Next := Elem;
    Elem.Prev := Last;
    Last      := Elem;
  end;
end;

procedure MsgListRemoveElement(Elem: TMessageListElement; var First: TMessageListElement; var Last: TMessageListElement);
begin
  if Elem = First then
    First := Elem.Next;

  if Elem = Last  then
    Last  := Elem.Prev;

  if Elem.Next <> nil then
    Elem.Next.Prev := Elem.Prev;

  if Elem.Prev <> nil then
    Elem.Prev.Next := Elem.Next;
end;

function fpgAllocateMessage: PfpgMessageRec;
var
  e: TMessageListElement;
begin
  e := FreeFirstMessage;
  if e <> nil then
  begin
    MsgListRemoveElement(e, FreeFirstMessage, FreeLastMessage);
    MsgListInsertElement(e, UsedFirstMessage, UsedLastMessage);
    result := @(e.msg);
  end
  else
    result := nil;
end;

procedure fpgDeleteFirstMessage;
var
  e: TMessageListElement;
begin
  fpgApplication.Lock;
  try
    e := UsedFirstMessage;
    if e <> nil then
    begin
      MsgListRemoveElement(e, UsedFirstMessage, UsedLastMessage);
      MsgListInsertElement(e, FreeFirstMessage, FreeLastMessage);
    end;
  finally
    fpgApplication.Unlock;
  end;
end;

procedure fpgDeleteMessagesForTarget(Dest: TObject; MsgCode: integer);
var
  e: TMessageListElement;
begin
  if not Assigned(Dest) then
    Exit; // ==>

  fpgApplication.Lock;
  try
    e := UsedFirstMessage;
    while (e <> nil) do
    begin
      if (e.msg.Dest = Dest) and ((MsgCode = -1) or (MsgCode = e.msg.MsgCode)) then
      begin
        //WriteLn(Format('fpgDeleteMessagesForTarget: Deleted message %d for %s', [e.msg.MsgCode, e.msg.Dest.ClassName]));
        MsgListRemoveElement(e, UsedFirstMessage, UsedLastMessage);
        MsgListInsertElement(e, FreeFirstMessage, FreeLastMessage);
      end;
      if e = UsedLastMessage then
        break;
      e := e.Next;
    end;
  finally
    fpgApplication.Unlock;
  end;
end;

function fpgGetFirstMessage: PfpgMessageRec;
begin
  fpgApplication.Lock;
  try
    if UsedFirstMessage <> nil then
      Result := @(UsedFirstMessage.msg)
    else
      Result := nil;
  finally
    fpgApplication.Unlock;
  end;
end;

procedure fpgInitMsgQueue;
var
  n: integer;
  e: TMessageListElement;
begin
  UsedFirstMessage := nil;
  UsedLastMessage  := nil;
  FreeFirstMessage := nil;
  FreeLastMessage  := nil;

  uMsgQueueList := TList.Create;

  for n := 1 to cMessageQueueSize do
  begin
    e := TMessageListElement.Create;
    uMsgQueueList.Add(e); // so we can free it off later
    MsgListInsertElement(e,FreeFirstMessage,FreeLastMessage);
  end;
end;

procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TfpgMessageParams); overload;
var
  p: PfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>
  fpgApplication.Lock;
  try
    p := fpgAllocateMessage;
    if p <> nil then
    begin
      p^.MsgCode := MsgCode;
      p^.Sender  := Sender;
      p^.Dest    := Dest;
      p^.Params  := aparams;
    end
    else
      raise Exception.Create('THE fpGUI MESSAGE QUEUE IS FULL.');
  finally
    fpgApplication.Unlock;
  end;
end;

procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer); overload;
var
  p: PfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  fpgApplication.Lock;
  try
    p := fpgAllocateMessage;
    if p <> nil then
    begin
      p^.MsgCode := MsgCode;
      p^.Sender  := Sender;
      p^.Dest    := Dest;
      p^.Stop    := False;
      FillChar(p^.Params, SizeOf(p^.Params), 0);
    end
    else
      raise Exception.Create('THE fpGUI MESSAGE QUEUE IS FULL.');
  finally
    fpgApplication.Unlock;
  end;
end;

procedure fpgSendMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TfpgMessageParams); overload;
var
  m: TfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;
  m.Params  := aparams;

  fpgDeliverMessage(m);
end;

procedure fpgSendMessage(Sender, Dest: TObject; MsgCode: integer); overload;
var
  m: TfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  m.MsgCode := MsgCode;
  m.Sender  := Sender;
  m.Dest    := Dest;
  FillChar(m.Params, SizeOf(m.Params), 0);

  fpgDeliverMessage(m);
end;

function fpgPeekMessage(Dest: TObject; MsgCode: integer; Msg: PfpgMessageRec = nil): Boolean;
var
  e: TMessageListElement;
begin
  Result := False;
  fpgApplication.Lock;
  try
    e := UsedLastMessage;
    while e <> nil do
    begin
      if (e.msg.Dest = Dest) and (e.msg.MsgCode = MsgCode) then
      begin
        Result := True;
        if Assigned(Msg) then
          Msg^ := e.msg;
        Exit;
      end;
      if e = UsedFirstMessage then
        break;
      e := e.Next;
    end;

  finally
    fpgApplication.Unlock;
  end;
end;

procedure fpgDeliverMessage(var msg: TfpgMessageRec);
var
  oItem: TMsgHookItem;
  i: integer;
begin
  if msg.MsgCode = FPGM_KILLME then
    msg.Dest.Free
  else
  begin
    msg.Dest.Dispatch(msg);
    if fpgApplication.FMessageHookList.Count > 0 then
    begin
      for i := 0 to fpgApplication.FMessageHookList.Count - 1 do
      begin
        oItem := TMsgHookItem(fpgApplication.FMessageHookList.Items[i]);
        if (msg.Dest = oItem.Dest) and (msg.MsgCode = oItem.MsgCode) then
          oItem.Listener.Dispatch(msg);
      end;
    end;
  end;
end;

procedure fpgDeliverMessages;
var
  mp: PfpgMessageRec;
  m: TfpgMessageRec;
begin
  repeat
    mp := fpgGetFirstMessage;
    if mp <> nil then
    begin
      m := mp^;
      fpgDeleteFirstMessage;
      fpgDeliverMessage(m);
    end;
  until mp = nil;
end;

