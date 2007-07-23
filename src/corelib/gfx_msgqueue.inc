
// fpGFX internal message queue implementation

type
  // a simlpe linked list implementation
  TMessageListElement = class
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

function pgfAllocateMessage: PfpgMessageRec;
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
  e := UsedFirstMessage;
  if e <> nil then
  begin
    MsgListRemoveElement(e, UsedFirstMessage, UsedLastMessage);
    MsgListInsertElement(e, FreeFirstMessage, FreeLastMessage);
  end;
end;

function fpgGetFirstMessage: PfpgMessageRec;
begin
  if UsedFirstMessage <> nil then
    Result := @(UsedFirstMessage.msg)
  else
    Result := nil;
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

  for n := 1 to cMessageQueueSize do
  begin
    e := TMessageListElement.Create;
    MsgListInsertElement(e,FreeFirstMessage,FreeLastMessage);
  end;
end;

procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TfpgMessageParams); overload;
var
  p: PfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  p := pgfAllocateMessage;
  if p <> nil then
  begin
    p^.MsgCode := MsgCode;
    p^.Sender  := Sender;
    p^.Dest    := Dest;
    p^.Params  := aparams;
  end
  else
    Writeln('THE MESSAGE QUEUE IS FULL.');
end;

procedure fpgPostMessage(Sender, Dest: TObject; MsgCode: integer); overload;
var
  p: PfpgMessageRec;
begin
  if Dest = nil then
    Exit; //==>

  p := pgfAllocateMessage;
  if p <> nil then
  begin
    p^.MsgCode := MsgCode;
    p^.Sender  := Sender;
    p^.Dest    := Dest;
  end
  else
    Writeln('THE MESSAGE QUEUE IS FULL.');
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

  m.Dest.Dispatch(m)
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

  m.Dest.Dispatch(m)
end;

procedure fpgDeliverMessage(var msg: TfpgMessageRec);
begin
  if msg.MsgCode = FPGM_KILLME then
    msg.Dest.Free
  else
    msg.Dest.Dispatch(msg);
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

