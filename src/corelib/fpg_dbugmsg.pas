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
}
unit fpg_dbugmsg;

{$mode objfpc}{$h+}

interface

uses Classes;

Const
  DebugServerID  : String = 'fpgDebugServer';

Type
  TDebugMessage = record
    MsgType      : Integer;
    MsgTimeStamp : TDateTime;
    MsgTitle     : string;
    Msg          : string;
  end;

Procedure ReadDebugMessageFromStream(AStream : TStream; Var Msg : TDebugMessage);
Procedure WriteDebugMessageToStream(AStream : TStream; Const Msg : TDebugMessage);


implementation


procedure ReadDebugMessageFromStream(AStream : TStream; Var Msg : TDebugMessage);
var
  MsgSize: Integer;
begin
  MsgSize := 0;
  with AStream do
  begin
    ReadBuffer(Msg.MsgType, SizeOf(Integer));
    ReadBuffer(Msg.MsgTimeStamp, SizeOf(TDateTime));

    ReadBuffer(MsgSize, SizeOf(Integer));
    SetLength(Msg.MsgTitle, MsgSize);
    if (MsgSize<>0) then
      ReadBuffer(Msg.MsgTitle[1], MsgSize);

    ReadBuffer(MsgSize, SizeOf(Integer));
    SetLength(Msg.Msg, MsgSize);
    if (MsgSize<>0) then
      ReadBuffer(Msg.Msg[1], MsgSize);
  end;
end;

procedure WriteDebugMessageToStream(AStream : TStream; Const Msg : TDebugMessage);
var
  MsgSize : Integer;
  lTitle: string;
begin
  with AStream do
  begin
    WriteBuffer(Msg.MsgType, SizeOf(Integer));
    WriteBuffer(Msg.MsgTimeStamp, SizeOf(TDateTime));

    MsgSize := Length(Msg.MsgTitle);
    if MsgSize = 0 then // fake a title
    begin
      MsgSize := 1;
      lTitle := ' ';
    end
    else
      lTitle := Msg.MsgTitle;
    WriteBuffer(MsgSize, SizeOf(Integer));
    WriteBuffer(lTitle[1], MsgSize);

    MsgSize := Length(Msg.Msg);
    WriteBuffer(MsgSize, SizeOf(Integer));
    WriteBuffer(Msg.Msg[1], MsgSize);
  end;
end;


end.
