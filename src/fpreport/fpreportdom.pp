{
    This file is part of the Free Component Library.
    Copyright (c) 2008 Michael Van Canneyt, member of the Free Pascal development team

    Stream report definition to/from XML Stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

Type
  { TFPReportDOM }
  TXMLPropertyStorage = (psAttr,psElement);

Const
  DefPropertyStorage = psAttr;

Type
  TFPReportDOM = Class(TObject)
  private
    FCurrentElement: TDomElement;
    FDocument: TXMLDocument;
    FRootNode: TDomElement;
    FStack : TFPList;
    function PushCurrentElement: TDomElement;
    procedure SetCurrentElement(const AValue: TDomElement);
  Public
    Constructor Create(ADocument : TXMLDocument; ARootNode : TDomElement = Nil);
    Destructor Destroy; override;
    Function StreamToHex(S : TStream) : String;
    function HexToStringStream(S: String): TStringStream;
    Function StreamsEqual(S1,S2 : TStream) : Boolean;
    Function NewDOMElement(Const AName : DOMString) : TDomElement;
    Function PushElement(Const AName : DOMString) : TDomElement;
    Function PushElement(AElement : TDomElement) : TDomElement;
    Function PopElement : TDomElement;
    Function FindChild(Const AName: DOMString) : TDomElement;

    // Writing properties as attributes of the current element
    Procedure WriteInteger(AName : DOMString; AValue : Integer; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteString(AName : DOMString; AValue : AnsiString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteWideString(AName : DOMString; AValue : WideString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteFloat(AName : DOMString; AValue : Extended; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteDateTime(AName : DOMString; AValue : TDateTime; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteBoolean(AName : DOMString; AValue : Boolean; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteStream(AName : DOMString; AValue : TStream; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    // Writing properties but only when different from original
    Procedure WriteIntegerDiff(AName : DOMString; AValue,AOriginal : Integer; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteStringDiff(AName : DOMString; AValue,AOriginal : AnsiString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteWideStringDiff(AName : DOMString; AValue,AOriginal : WideString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteFloatDiff(AName : DOMString; AValue,AOriginal : Extended; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteDateTimeDiff(AName : DOMString; AValue,AOriginal : TDateTime; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteBooleanDiff(AName : DOMString; AValue,AOriginal : Boolean; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
    Procedure WriteStreamDiff(AName : DOMString; AValue,AOriginal : TStream; UseStorage : TXMLPropertyStorage = DefPropertyStorage);

    // Writing properties as attributes of the current element
    Function ReadInteger(AName : DOMString; ADefault : Integer; UseStorage : TXMLPropertyStorage = DefPropertyStorage) : Integer;
    Function ReadString(AName : DOMString; ADefault : Ansistring; UseStorage : TXMLPropertyStorage = DefPropertyStorage) : Ansistring;
    Function ReadWideString(AName : DOMString; ADefault : WideString; UseStorage : TXMLPropertyStorage = DefPropertyStorage) : WideString;
    Function ReadFloat(AName : DOMString; ADefault : Extended; UseStorage : TXMLPropertyStorage = DefPropertyStorage) : Extended;
    Function ReadDateTime(AName : DOMString; ADefault : TDateTime; UseStorage : TXMLPropertyStorage = DefPropertyStorage) : TDateTime;
    Function ReadBoolean(AName : DOMString; ADefault : Boolean; UseStorage : TXMLPropertyStorage = DefPropertyStorage) : Boolean;
    Function ReadStream(AName : DOMString; AValue : TStream; UseStorage : TXMLPropertyStorage = DefPropertyStorage) : Boolean;

    Property CurrentElement : TDomElement read FCurrentElement write SetCurrentElement;
    Property Document : TXMLDocument Read FDocument;
    Property RootNode : TDomElement Read FRootNode;
  end;

  EReportDOM = Class(Exception);

implementation

Resourcestring
  SErrStackEmpty = 'Element stack is empty';
  SErrNoCurrentElement = 'No current element to find node %s below';
  SErrNodeNotElement = 'Node %s is not an element node';

{ TFPReportDOM }

procedure TFPReportDOM.SetCurrentElement(const AValue: TDomElement);
begin
  if FCurrentElement=AValue then exit;
  FCurrentElement:=AValue;
end;

constructor TFPReportDOM.Create(ADocument: TXMLDocument; ARootNode: TDomElement
  );
begin
  FDocument:=ADocument;
  If (ARootNode=Nil) then
    FRootNode:=FDocument.DocumentElement
  else
    FRootNode:=ARootNode;
  FCurrentElement:=FRootNode;
end;

destructor TFPReportDOM.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

function TFPReportDOM.StreamToHex(S: TStream): String;

Var
  T : TMemoryStream;
  P,PD : PChar;
  I,L : Integer;
  h : String[2];

begin
  If(S is TMemoryStream) then
    T:=S as TMemoryStream
  else
    begin
    T:=TMemoryStream.Create;
    T.CopyFrom(S,0);
    end;
  try
    L:=T.Size;
    SetLength(Result,L*2);
    PD:=PChar(Result);
    P:=PChar(T.Memory);
    For I:=1 to L do
      begin
      H:=HexStr(Ord(P^),2);
      PD^:=H[1];
      Inc(PD);
      PD^:=H[2];
      Inc(P);
      Inc(PD);
      end;
  finally
    S.Position:=0;
  end;
end;

function TFPReportDOM.StreamsEqual(S1, S2: TStream): Boolean;

Var
  S : TStringStream;
  T : String;

begin
  Result:=(S1=S2);
  If not Result then
    begin
    Result:=(S1.Size=S2.Size);
    If Result then
      begin
      S:=TStringStream.Create('');
      try
        S.CopyFrom(S1,0);
        T:=S.DataString;
        S.Size:=0;
        S.CopyFrom(S2,0);
        Result:=(T=S.DataString);
      finally
        S.Free;
      end;
      end;
    end;
end;

function TFPReportDOM.NewDOMElement(Const AName: DOMString): TDomElement;
begin
  Result:=FDocument.CreateElement(AName);
  FCurrentElement.AppendChild(Result);
  FCurrentElement:=Result;
end;

function TFPReportDOM.PushCurrentElement: TDomElement;

begin
  If Not Assigned(FStack) then
    FStack:=TFPList.Create;
  FStack.Add(FCurrentElement);
  Result:=FCurrentElement;
end;

function TFPReportDOM.PushElement(Const AName: DOMString): TDomElement;
begin
  Result:=PushCurrentElement;
  NewDomElement(AName);
end;

function TFPReportDOM.PushElement(AElement: TDomElement): TDomElement;
begin
  Result:=PushCurrentElement;
  CurrentElement:=AElement;
end;

function TFPReportDOM.PopElement: TDomElement;
begin
  If (FStack=Nil) or (FStack.Count=0) then
    Raise EReportDOM.Create(SErrStackEmpty);
  Result:=FCurrentElement;
  FCurrentElement:=TDomElement(FStack[FStack.Count-1]);
  FStack.Delete(FStack.Count-1);
  If (FStack.Count=0) then
    FreeAndNil(FStack);
end;

function TFPReportDOM.FindChild(const AName: DOMString): TDomElement;

Var
  N : TDomNode;

begin
  If Assigned(FCurrentElement) then
    N:=FCurrentElement.FindNode(AName)
  else
    Raise EReportDom.CreateFmt(SErrNoCurrentElement,[AName]);
  If N=Nil then
    Result:=Nil
  else if N is TDOMElement then
    Result:=N as TDomElement
  else
    Raise EReportDom.CreateFmt(SErrNodeNotElement,[AName]);
end;

Procedure TFPReportDOM.WriteInteger(AName: DOMString; AValue: Integer; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  WriteString(AName,IntToStr(AValue),UseStorage);
end;

Procedure TFPReportDOM.WriteString(AName: DOMString; AValue: AnsiString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);

Var
  el : TDomElement;

begin
  If UseStorage=psAttr then
    FCurrentElement[AName]:=AValue
  else
    begin
    el:=FDocument.CreateElement(AName);
    FCurrentElement.AppendChild(el);
    el.AppendChild(FDocument.CreateTextNode(AValue));
    end
end;

Procedure TFPReportDOM.WriteWideString(AName: DOMString; AValue: WideString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);

Var
  el : TDomElement;

begin
  If UseStorage=psAttr then
    FCurrentElement[AName]:=AValue
  else
    begin
    el:=FDocument.CreateElement(AName);
    FCurrentElement.AppendChild(el);
    el.AppendChild(FDocument.CreateTextNode(AValue));
    end;
end;

procedure TFPReportDOM.WriteFloat(AName: DOMString; AValue: Extended; UseStorage : TXMLPropertyStorage = DefPropertyStorage);

Var
  S : String;

begin
  Str(AValue:18,S);
  WriteString(AName,TrimLeft(S),UseStorage);
end;

procedure TFPReportDOM.WriteDateTime(AName: DOMString; AValue: TDateTime; UseStorage : TXMLPropertyStorage = DefPropertyStorage  );

Var
  S : String;

begin
  If Frac(AValue)=0 then  // Only date
    S:='yyyymmdd'
  else if Trunc(AValue)=0 then // Only time
    S:='00000000hhnnsszzz'
  else
    S:='yyyymmddhhnnsszzz'; // Full date-time
  S:=FormatDateTime(S,AValue);
  WriteString(AName,S,UseStorage);
end;

procedure TFPReportDOM.WriteBoolean(AName: DOMString; AValue: Boolean; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  WriteString(AName,IntToStr(Ord(Avalue)),UseStorage);
end;

procedure TFPReportDOM.WriteStream(AName: DOMString; AValue: TStream; UseStorage : TXMLPropertyStorage = DefPropertyStorage);

begin
  WriteString(AName,StreamToHex(AValue),UseStorage);
end;

procedure TFPReportDOM.WriteIntegerDiff(AName: DOMString; AValue, AOriginal: Integer; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  If (AValue<>AOriginal) then
    WriteInteger(AName,AValue,UseStorage);
end;

procedure TFPReportDOM.WriteStringDiff(AName: DOMString; AValue, AOriginal: AnsiString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  If (AValue<>AOriginal) then
    WriteString(AName,AValue,UseStorage);
end;

procedure TFPReportDOM.WriteWideStringDiff(AName: DOMString; AValue, AOriginal: WideString; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  If (AValue<>AOriginal) then
    WriteWideString(AName,AValue,UseStorage);
end;

procedure TFPReportDOM.WriteFloatDiff(AName: DOMString; AValue, AOriginal: Extended; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  If (AValue<>AOriginal) then
    WriteFloat(AName,AValue,UseStorage);
end;

procedure TFPReportDOM.WriteDateTimeDiff(AName: DOMString; AValue, AOriginal: TDateTime; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  If (AValue<>AOriginal) then
    WriteDateTime(AName,AValue,UseStorage);
end;

procedure TFPReportDOM.WriteBooleanDiff(AName: DOMString; AValue, AOriginal: Boolean; UseStorage : TXMLPropertyStorage = DefPropertyStorage);
begin
  If (AValue<>AOriginal) then
    WriteBoolean(AName,AValue,UseStorage);
end;

procedure TFPReportDOM.WriteStreamDiff(AName: DOMString; AValue, AOriginal: TStream; UseStorage : TXMLPropertyStorage = DefPropertyStorage);

begin
  If StreamsEqual(AValue,AOriginal) then
    Exit;
  WriteStream(AName,AValue,UseStorage);
end;


function TFPReportDOM.ReadInteger(AName: DOMString; ADefault: Integer;
  UseStorage: TXMLPropertyStorage): Integer;

begin
  Result:=StrToIntDef(ReadString(AName,'',UseStorage),ADefault);
end;

function TFPReportDOM.ReadString(AName: DOMString; ADefault: Ansistring;
  UseStorage: TXMLPropertyStorage): Ansistring;

Var
  N : TDomNode;

begin
  If (UseStorage=psAttr) then
    begin
    N:=FCurrentElement.GetAttributeNode(AName);
    If Assigned(N) then
      Result:=TDomAttr(N).NodeValue
    else
      Result:=ADefault;
    end
  else
    begin
    N:=FCurrentElement.FindNode(AName);
    If Assigned(N) and (N is TDomElement) then
      Result:=TDomElement(N).FirstChild.NodeValue
    else
      Result:=ADefault;
    end;
end;

function TFPReportDOM.ReadWideString(AName: DOMString; ADefault: WideString;
  UseStorage: TXMLPropertyStorage): WideString;

Var
  N : TDomNode;

begin
  If (UseStorage=psAttr) then
    begin
    N:=FCurrentElement.GetAttributeNode(AName);
    If Assigned(N) then
      Result:=TDomAttr(N).NodeValue
    else
      Result:=ADefault;
    end
  else
    begin
    N:=FCurrentElement.FindNode(AName);
    If Assigned(N) and (N is TDomElement) then
      Result:=TDomElement(N).FirstChild.NodeValue
    else
      Result:=ADefault;
    end;
end;

function TFPReportDOM.ReadFloat(AName: DOMString; ADefault: Extended;
  UseStorage: TXMLPropertyStorage): Extended;

Var
  S : String;

  C : Integer;

begin
  S:=ReadString(AName,'',UseStorage);
  If (S='') then
    Result:=ADefault
  else
    begin
    Val(S,Result,C);
    If (C<>0) then
      Result:=ADefault;
    end;
end;

function TFPReportDOM.ReadDateTime(AName: DOMString; ADefault: TDateTime;
  UseStorage: TXMLPropertyStorage): TDateTime;

  Function NextNumber(Var S : String; Digits : Integer; Out AValue : Word) : Boolean;

  Var
    I : Integer;

  begin
    AValue:=0;
    Result:=TryStrToInt(Copy(S,1,Digits),I);
    If Result then
      begin
      Result:=(I>=0);
      If Result then
        AValue:=I;
      end;
    system.Delete(S,1,Digits);
  end;

Var
  S : String;
  D : TDateTime;
  W1,W2,W3,W4 : Word;

begin
  S:=ReadString(AName,'',UseStorage);
  If (S='') then
    Result:=ADefault
  else If Not (NextNumber(S,4,W1) and NextNumber(S,2,W2) and NextNumber(S,2,W3)) Then
    Result:=ADefault
  else If Not TryEncodeDate(W1,W2,W3,Result) then
    Result:=ADefault
  else
    begin
    // is there a time part ?
    If (S<>'') then
      begin
      if NextNumber(S,2,W1) and NextNumber(S,2,W2) and NextNumber(S,2,W3) then
        begin
        // Check milliseconds
        If (S<>'') then
          begin
          if not NextNumber(S,3,W4) then
            begin
            Result:=ADefault;
            Exit;
            end;
          end
        else
          W4:=0;
        if Not TryEncodeTime(W1,W2,W3,W4,D) then
          Result:=ADefault
        else
          Result:=Result+D;
        end
      else // If time formatted wrong
        Result:=ADefault;
      end;
    end;
end;

function TFPReportDOM.ReadBoolean(AName: DOMString; ADefault: Boolean;
  UseStorage: TXMLPropertyStorage): Boolean;

Var
  S : String;

begin
  S:=ReadString(AName,'',UseStorage);
  Result:=ADefault;
  if (S='1') then
    Result:=True
  else If (S='0') then
    Result:=False;
end;

function TFPReportDOM.HexToStringStream(S : String) : TStringStream;

Var
  T : String;
  I,J : Integer;
  B : Byte;
  P : Pchar;
  H : String[3];

begin
  Result:=Nil;
  SetLength(H,3);
  H[1]:='$';
  if (S<>'') then
    begin
    SetLength(T,Length(S) div 2);
    P:=PChar(T);
    I:=1;
    While I<Length(S) do
      begin
      H[2]:=S[i];
      Inc(I);
      H[3]:=S[i];
      Inc(I);
      Val(H,B,J);
      If (J=0) then
        P^:=Char(B)
      else
        P^:=#0;
      Inc(P);
      end;
    Result:=TStringStream.Create(T);
    end;
end;

function TFPReportDOM.ReadStream(AName: DOMString; AValue: TStream;
  UseStorage: TXMLPropertyStorage): Boolean;

Var
  S: String;
  SS : TStringStream;

begin
  S:=ReadString(AName,'',UseStorage);
  Result:=(S<>'');
  If Result then
    begin
    SS:=HexToStringStream(S);
    try
      AValue.CopyFrom(SS,0);
    Finally
      SS.Free;
    end;
    end;
end;

end.

