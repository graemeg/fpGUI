{
    This file is part of the Free Component Library.
    Copyright (c) 2008 Michael Van Canneyt, member of the Free Pascal development team
    Portions (C) 2016 WISA b.v.b.a.

    Stream report definition to/from JSON Stream.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fpReportStreamer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson;

type

  { Using an abstract class in case we want to support multiple output writers. eg: JSON, XML etc }
  TFPReportStreamer = class(TComponent)
  public
    function PushCurrentElement: TObject; virtual; abstract;
    function PushElement(const AName: String): TObject; virtual; abstract;
    function PushElement(AElement: TObject): TObject; virtual; abstract;
    function PopElement: TObject; virtual; abstract;
    function FindChild(const AName: String): TObject; virtual; abstract;
    function NewElement(const AName: String): TObject; virtual; abstract;
    function ChildCount: integer; virtual; abstract;
    function GetChild(AIndex: Integer): TObject; virtual; abstract;
    function CurrentElementName: string; virtual; abstract;

    // Writing properties of the current element
    procedure   WriteInteger(AName: String; AValue: Integer); virtual; abstract;
    procedure   WriteFloat(AName: String; AValue: Extended); virtual; abstract;
    procedure   WriteString(AName: String; AValue: String); virtual; abstract;
    procedure   WriteBoolean(AName: String; AValue: Boolean); virtual; abstract;
    procedure   WriteDateTime(AName: String; AValue: TDateTime); virtual; abstract;
    procedure   WriteStream(AName: String; AValue: TStream); virtual; abstract;
    // Writing properties but only when different from original
    procedure   WriteIntegerDiff(AName: String; AValue, AOriginal: Integer); virtual; abstract;
    procedure   WriteFloatDiff(AName: String; AValue, AOriginal: Extended); virtual; abstract;
    procedure   WriteStringDiff(AName: String; AValue, AOriginal: String); virtual; abstract;
    procedure   WriteBooleanDiff(AName: String; AValue, AOriginal: Boolean); virtual; abstract;
    procedure   WriteDateTimeDiff(AName: String; AValue, AOriginal: TDateTime); virtual; abstract;
    procedure   WriteStreamDiff(AName: String; AValue, AOriginal: TStream); virtual; abstract;
    // Reading properties
    function    ReadInteger(AName: String; ADefault: Integer): Integer; virtual; abstract;
    function    ReadFloat(AName: String; ADefault: Extended): Extended; virtual; abstract;
    function    ReadString(AName: String; ADefault: String): String; virtual; abstract;
    function    ReadDateTime(AName: String; ADefault: TDateTime): TDateTime; virtual; abstract;
    function    ReadBoolean(AName: String; ADefault: Boolean): Boolean; virtual; abstract;
    function    ReadStream(AName: String; AValue: TStream) : Boolean; virtual; abstract;
  end;


  { TFPReportJSONStreamer }

  TFPReportJSONStreamer = class(TFPReportStreamer)
  private
    Fjson: TJSONObject;
    FCurrentElement: TJSONObject;
    FOwnsJSON: Boolean;
    FStack: TFPList;
    procedure   SetCurrentElement(AValue: TJSONObject);
    function    DateTimeAsIntlDateStor(const ADateTime: TDateTime): string;
    function    IntlDateStorAsDateTime(const AValue: string): TDateTime;
    procedure   InitialiseCurrentElement;
    procedure   SetJSON(AValue: TJSONObject);
    procedure   SetOwnsJSON(AValue: Boolean);
  public
    // FPReportStreamer interface
    procedure   WriteInteger(AName: String; AValue: Integer); override;
    procedure   WriteFloat(AName: String; AValue: Extended); override;
    procedure   WriteString(AName: String; AValue: String); override;
    procedure   WriteBoolean(AName: String; AValue: Boolean); override;
    procedure   WriteDateTime(AName: String; AValue: TDateTime); override;
    procedure   WriteStream(AName: String; AValue: TStream); override;
    procedure   WriteIntegerDiff(AName: String; AValue, AOriginal: Integer); override;
    procedure   WriteFloatDiff(AName: String; AValue, AOriginal: Extended); override;
    procedure   WriteStringDiff(AName: String; AValue, AOriginal: String); override;
    procedure   WriteBooleanDiff(AName: String; AValue, AOriginal: Boolean); override;
    procedure   WriteDateTimeDiff(AName: String; AValue, AOriginal: TDateTime); override;
    procedure   WriteStreamDiff(AName: String; AValue, AOriginal: TStream); override;
    function    ReadInteger(AName: String; ADefault: Integer): Integer; override;
    function    ReadFloat(AName: String; ADefault: Extended): Extended; override;
    function    ReadString(AName: String; ADefault: String): String; override;
    function    ReadDateTime(AName: String; ADefault: TDateTime): TDateTime; override;
    function    ReadBoolean(AName: String; ADefault: Boolean): Boolean; override;
    function    ReadStream(AName: String; AValue: TStream) : Boolean; override;
    function    PushCurrentElement: TObject; override;
    function    PushElement(const AName: String): TObject; override;
    function    PushElement(AElement: TObject): TObject; override;
    function    PopElement: TObject; override;
    function    FindChild(const AName: String): TObject; override;
    function    NewElement(const AName: String): TObject; override;
    function    ChildCount: integer; override;
    function    GetChild(AIndex: Integer): TObject; override;
    function    CurrentElementName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    StreamToHex(S: TStream): String;
    function    StreamsEqual(S1, S2: TStream): Boolean;
    function    HexToStringStream(S: String): TStringStream;
    property    JSON: TJSONObject read Fjson write SetJSON;
    Property    OwnsJSON : Boolean Read FOwnsJSON Write SetOwnsJSON;
    property    CurrentElement: TJSONObject read FCurrentElement write SetCurrentElement;
  end;


  EReportDOM = class(Exception);


implementation

resourcestring
  SErrStackEmpty = 'Element stack is empty';
  SErrNoCurrentElement = 'No current element to find node %s below';
  SErrNodeNotElement = 'Node %s is not an element node';

const
  { Summary of ISO 8601  http://www.cl.cam.ac.uk/~mgk25/iso-time.html }
  cIntlDateTimeStor = 'yyyymmdd"T"hhnnss';    // for storage


{ TFPReportJSONStreamer }

procedure TFPReportJSONStreamer.SetCurrentElement(AValue: TJSONObject);
begin
  if FCurrentElement = AValue then Exit;
  FCurrentElement := AValue;
end;

{ Borrowed implementation from tiOPF's tiUtils unit. }
function TFPReportJSONStreamer.DateTimeAsIntlDateStor(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime(cIntlDateTimeStor, ADateTime);
  if Pos('18991230', Result) = 1 then
    Result := StringReplace(Result, '18991230', '00000000', [rfReplaceAll]);
end;

function TFPReportJSONStreamer.IntlDateStorAsDateTime(const AValue: string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS: Word;
begin
  if Trim(AValue) = '' then
  begin
    Result := 0;
    Exit; //==>
  end;

    //          1         2
    // 12345678901234567890123
    // yyyymmddThhnnss
  lY := StrToInt(Copy(AValue, 1, 4));
  lM := StrToInt(Copy(AValue, 5, 2));
  lD := StrToInt(Copy(AValue, 7, 2));
  lH := StrToInt(Copy(AValue, 10, 2));
  lMi := StrToInt(Copy(AValue, 12, 2));
  lS := StrToInt(Copy(AValue, 14, 2));

  { Cannot EncodeDate if any part equals 0. EncodeTime is okay. }
  if (lY = 0) or (lM = 0) or (lD = 0) then
    Result := EncodeTime(lH, lMi, lS, 0)
  else
    Result := EncodeDate(lY, lM, lD) + EncodeTime(lH, lMi, lS, 0);
end;

procedure TFPReportJSONStreamer.InitialiseCurrentElement;
begin
  FCurrentElement := Fjson;
end;

procedure TFPReportJSONStreamer.SetJSON(AValue: TJSONObject);
begin
  if Fjson = AValue then
    Exit;
  if Assigned(Fjson) and OwnsJSON then
    FreeAndNil(FJson);
  Fjson := AValue;
  InitialiseCurrentElement;
end;

procedure TFPReportJSONStreamer.SetOwnsJSON(AValue: Boolean);
begin
  if FOwnsJSON=AValue then Exit;
  FOwnsJSON:=AValue;
  if Not FOwnsJSON then // We no longer own, so free and nil
    FreeAndNil(FJSON);
end;

procedure TFPReportJSONStreamer.WriteInteger(AName: String; AValue: Integer);
begin
  CurrentElement.Add(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteFloat(AName: String; AValue: Extended);
begin
  CurrentElement.Add(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteString(AName: String; AValue: String);
begin
  CurrentElement.Add(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteBoolean(AName: String; AValue: Boolean);
begin
  CurrentElement.Add(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteDateTime(AName: String; AValue: TDateTime);
begin
  CurrentElement.Add(AName, DateTimeAsIntlDateStor(AValue));
end;

procedure TFPReportJSONStreamer.WriteStream(AName: String; AValue: TStream);
begin
  WriteString(AName, StreamToHex(AValue));
end;

procedure TFPReportJSONStreamer.WriteIntegerDiff(AName: String; AValue, AOriginal: Integer);
begin
  if (AValue <> AOriginal) then
    WriteInteger(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteFloatDiff(AName: String; AValue, AOriginal: Extended);
begin
  if (AValue <> AOriginal) then
    WriteFloat(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteStringDiff(AName: String; AValue, AOriginal: String);
begin
  if (AValue <> AOriginal) then
    WriteString(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteBooleanDiff(AName: String; AValue, AOriginal: Boolean);
begin
  if (AValue <> AOriginal) then
    WriteBoolean(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteDateTimeDiff(AName: String; AValue, AOriginal: TDateTime);
begin
  if (AValue <> AOriginal) then
    WriteDateTime(AName, AValue);
end;

procedure TFPReportJSONStreamer.WriteStreamDiff(AName: String; AValue, AOriginal: TStream);
begin
  if StreamsEqual(AValue, AOriginal) then
    Exit;
  WriteStream(AName, AValue);
end;

function TFPReportJSONStreamer.ReadInteger(AName: String; ADefault: Integer): Integer;
var
  d: TJSONData;
begin
  d := FindChild(AName) as TJSONData;
  if d = nil then
    Result := ADefault
  else
  begin
    if d.JSONType = jtNumber then
      Result := d.AsInt64
    else
      Result := ADefault;
  end;
end;

function TFPReportJSONStreamer.ReadFloat(AName: String; ADefault: Extended): Extended;
var
  d: TJSONData;
begin
  d := FindChild(AName) as TJSONData;
  if d = nil then
    Result := ADefault
  else
  begin
    if d.JSONType = jtNumber then
      Result := d.AsFloat
    else
      Result := ADefault;
  end;
end;

function TFPReportJSONStreamer.ReadString(AName: String; ADefault: String): String;
var
  d: TJSONData;
begin
  d := FindChild(AName) as TJSONData;
  if d = nil then
    Result := ADefault
  else
  begin
    if d.JSONType = jtString then
      Result := d.AsString
    else
      Result := ADefault;
  end;
end;

function TFPReportJSONStreamer.ReadDateTime(AName: String; ADefault: TDateTime): TDateTime;
var
  d: TJSONData;
begin
  d := FindChild(AName) as TJSONData;
  if d = nil then
    Result := ADefault
  else
  begin
    if d.JSONType = jtString then
    begin
      try
        Result := IntlDateStorAsDateTime(d.AsString)
      except
        on E: EConvertError do
          Result := ADefault;
      end
    end
    else
      Result := ADefault;
  end;
end;

function TFPReportJSONStreamer.ReadBoolean(AName: String; ADefault: Boolean): Boolean;
var
  d: TJSONData;
begin
  d := FindChild(AName) as TJSONData;
  if d = nil then
    Result := ADefault
  else
  begin
    if d.JSONType = jtBoolean then
      Result := d.AsBoolean
    else
      Result := ADefault;
  end;
end;

function TFPReportJSONStreamer.ReadStream(AName: String; AValue: TStream): Boolean;
var
  S: string;
  SS: TStringStream;
begin
  S := ReadString(AName, '');
  Result := (S <> '');
  if Result then
  begin
    SS := HexToStringStream(S);
    try
      AValue.CopyFrom(SS, 0);
    finally
      SS.Free;
    end;
  end;
end;

function TFPReportJSONStreamer.PushCurrentElement: TObject;
begin
  if not Assigned(FStack) then
    FStack := TFPList.Create;
  FStack.Add(FCurrentElement);
  Result := FCurrentElement;
end;

function TFPReportJSONStreamer.PushElement(const AName: String): TObject;
begin
  PushCurrentElement;
  Result := NewElement(AName);
end;

function TFPReportJSONStreamer.PushElement(AElement: TObject): TObject;
begin
  PushCurrentElement;
  CurrentElement := TJSONObject(AElement);
  Result := CurrentElement;
end;

function TFPReportJSONStreamer.PopElement: TObject;
begin
  if (FStack = nil) or (FStack.Count = 0) then
    raise EReportDOM.Create(SErrStackEmpty);
  Result := FCurrentElement;
  FCurrentElement := TJSONObject(FStack[FStack.Count - 1]);
  FStack.Delete(FStack.Count - 1);
  if (FStack.Count = 0) then
    FreeAndNil(FStack);
end;

function FindRecursive(AData: TJSONObject; AName: string): TJSONData;
var
  i: integer;
  d: TJSONData;
  o: TJSONObject;
begin
  Result := AData.Find(AName);
  if Result <> nil then // we found it
    Exit
  else
  begin
    for i := 0 to AData.Count-1 do
    begin
      d := AData.Items[i];
      if d.JSONType = jtObject then
      begin
        o := TJSONObject(d);
        Result := FindRecursive(o, AName);
        if Result <> nil then
          exit;
      end;
    end;
  end;
end;

function TFPReportJSONStreamer.FindChild(const AName: String): TObject;
var
  i: integer;
  d: TJSONData;
  o: TJSONObject;
begin
  Result := CurrentElement.Find(AName);
  if Result = nil then
  begin
    for i := 0 to CurrentElement.Count-1 do
    begin
      d := CurrentElement.Items[i];
      if d.JSONType = jtObject then
      begin
        o := TJSONObject(d);
        Result := FindRecursive(o, AName);
        if Result <> nil then
          exit;
      end;
    end;
  end
end;

function TFPReportJSONStreamer.NewElement(const AName: String): TObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  FCurrentElement.Add(AName, obj);
  FCurrentElement := obj;
  Result := FCurrentElement;
end;

function TFPReportJSONStreamer.ChildCount: integer;
begin
  Result := FCurrentElement.Count;
end;

function TFPReportJSONStreamer.GetChild(AIndex: Integer): TObject;
begin
  if (ChildCount = 0) or (AIndex > ChildCount-1) then
    result := nil
  else
  begin
    Result := FCurrentElement.Items[AIndex];
  end;
end;

function TFPReportJSONStreamer.CurrentElementName: string;
begin
  if Assigned(FCurrentElement) then
    Result := TJSONObject(FCurrentElement).Names[0]
  else
    Result := '';
end;

constructor TFPReportJSONStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwnsJSON:=True;
  Fjson := TJSONObject.Create;
  InitialiseCurrentElement;
end;

destructor TFPReportJSONStreamer.Destroy;
begin
  FreeAndNil(FStack);
  If OwnsJSON then
    FreeAndNil(Fjson);
  inherited Destroy;
end;

function TFPReportJSONStreamer.StreamToHex(S: TStream): String;
var
  T: TMemoryStream;
  P, PD: PChar;
  I, L: integer;
  h: string[2];
begin
  if (S is TMemoryStream) then
    T := S as TMemoryStream
  else
  begin
    T := TMemoryStream.Create;
    T.CopyFrom(S, 0);
  end;
  try
    L := T.Size;
    SetLength(Result, L * 2);
    PD := PChar(Result);
    P := PChar(T.Memory);
    for I := 1 to L do
    begin
      H := HexStr(Ord(P^), 2);
      PD^ := H[1];
      Inc(PD);
      PD^ := H[2];
      Inc(P);
      Inc(PD);
    end;
  finally
    S.Position := 0;
  end;
end;

function TFPReportJSONStreamer.StreamsEqual(S1, S2: TStream): Boolean;
var
  S: TStringStream;
  T: string;
begin
  Result := (S1 = S2);
  if not Result then
  begin
    Result := (S1.Size = S2.Size);
    if Result then
    begin
      S := TStringStream.Create('');
      try
        S.CopyFrom(S1, 0);
        T := S.DataString;
        S.Size := 0;
        S.CopyFrom(S2, 0);
        Result := (T = S.DataString);
      finally
        S.Free;
      end;
    end;
  end;
end;

function TFPReportJSONStreamer.HexToStringStream(S: String): TStringStream;
var
  T: string;
  I, J: integer;
  B: byte;
  P: PChar;
  H: string[3];
begin
  Result := nil;
  SetLength(H, 3);
  H[1] := '$';
  if (S <> '') then
  begin
    SetLength(T, Length(S) div 2);
    P := PChar(T);
    I := 1;
    while I < Length(S) do
    begin
      H[2] := S[i];
      Inc(I);
      H[3] := S[i];
      Inc(I);
      Val(H, B, J);
      if (J = 0) then
        P^ := char(B)
      else
        P^ := #0;
      Inc(P);
    end;
    Result := TStringStream.Create(T);
  end;
end;

end.
