unit iterator_impl;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ,Regex            { to be used with filtered string iterator }
  ,iterator_intf
  ,contnrs
  ;

type

  TTBStringsIterator = class(TInterfacedObject, ITBStringIterator, ITBStringAndObjectIterator)
  private
    FStrings: TStrings;
    FCursor: Integer;
    { Interface methods should always be private because
      we will only ever access them via an Interface,
      never via an Object instance }

    { Interface: ITBStringIterator and ITBStringAndObjectIterator }
    function    HasNext: Boolean;
    function    Next: string;
    function    HasPrevious: Boolean;
    function    Previous: string;
    { Interface: ITBStringAndObjectIterator }
    function    HasNextObject: Boolean;
    function    NextObject: TObject;
    function    HasPreviousObject: Boolean;
    function    PreviousObject: TObject;
  public
    constructor CreateCustom(const ASource: TStrings); virtual;
  end;


  TTBListIterator = class(TInterfacedObject, ITBIterator)
  private
    FList: TList;
    FCursor: Integer;
    { Interface: ITBIterator }
    function    HasNext: Boolean;
    function    Next: TObject;
    function    HasPrevious: Boolean;
    function    Previous: TObject;
  public
    constructor CreateCustom(const ASource: TList); virtual;
  end;


  TTBCollectionIterator = class(TInterfacedObject, ITBIterator)
  private
    FCollection: TCollection;
    FCursor: Integer;
    { Interface: ITBIterator }
    function    HasNext: Boolean;
    function    Next: TObject;
    function    HasPrevious: Boolean;
    function    Previous: TObject;
  public
    constructor CreateCustom(const ASource: TCollection); virtual;
  end;


  TTBInterfaceListIterator = class(TInterfacedObject, ITBInterfaceIterator)
  private
    FList: TInterfaceList;
    FCursor: integer;
    { Interface: ITBinterfaceIterator }
    function    HasNext: Boolean;
    function    Next: IInterface;
    function    HasPrevious: Boolean;
    function    Previous: IInterface;
  public
    constructor CreateCustom(const ASource: TInterfaceList); virtual;
  end;


  TTBFilteredStringsIterator = class(TTBStringsIterator, ITBFilteredStringIterator)
  private
    FNextIndex: Integer;
    FRegex: TRegexEngine;
    { Interface: ITBFilteredStringIterator }
    function    GetFilter: string;
    procedure   SetFilter(const AValue: string);
    { Interface: ITBStringIterator and ITBStringAndObjectIterator }
    function    HasNext: Boolean;
    function    Next: string;
    function    HasPrevious: Boolean;
    function    Previous: string;
  public
    constructor CreateCustom(const ASource: TStrings); override;
    destructor  Destroy; override;
  end;


  TTBObjectListIterator = class(TInterfacedObject, ITBIterator)
  private
    FList: TObjectList;
    FCursor: Integer;
    { Interface: ITBIterator }
    function    HasNext: Boolean;
    function    Next: TObject;
    function    HasPrevious: Boolean;
    function    Previous: TObject;
  public
    constructor CreateCustom(const ASource: TObjectList); virtual;
  end;



implementation


{ TTBStringsIterator }

function TTBStringsIterator.HasNext: Boolean;
begin
  Result := False;
  if Assigned(FStrings) then
    if FCursor < FStrings.Count - 1 then
      Result := True;
end;

function TTBStringsIterator.Next: string;
begin
  Result := '';
  if HasNext then
  begin
    Inc(FCursor, 1);
    Result := FStrings.Strings[FCursor];
  end;
end;

function TTBStringsIterator.HasPrevious: Boolean;
begin
  Result := False;
  if Assigned(FStrings) then
    if FCursor > 0 then
      Result := True;
end;

function TTBStringsIterator.Previous: string;
begin
  Result := '';
  if HasPrevious then
  begin
    Dec(FCursor, 1);
    Result := FStrings.Strings[FCursor];
  end;
end;

function TTBStringsIterator.HasNextObject: Boolean;
begin
  Result := False;
  if Assigned(FStrings) then
    if FCursor < FStrings.Count - 1 then
      Result := FStrings.Objects[FCursor] <> nil;
end;

function TTBStringsIterator.NextObject: TObject;
begin
  Result := nil;
  if HasNextObject then
    // Note that Next(...) increments the FCursor
    Result := FStrings.Objects[FCursor];
end;

function TTBStringsIterator.HasPreviousObject: Boolean;
begin
  Result := False;
  if Assigned(FStrings) then
    if FCursor > 0 then
      Result := FStrings.Objects[FCursor] <> nil;
end;

function TTBStringsIterator.PreviousObject: TObject;
begin
  Result := nil;
  if HasPreviousObject then
    // Note that Previous(...) decrements the FCursor
    Result := FStrings.Objects[FCursor];
end;

constructor TTBStringsIterator.CreateCustom(const ASource: TStrings);
begin
  inherited Create;
  FStrings  := ASource;
  FCursor   := -1;
end;


{ TTBListIterator }

function TTBListIterator.HasNext: Boolean;
begin
  Result := False;
  if Assigned(FList) then
    if FCursor < FList.Count - 1 then
      Result := True;
end;

function TTBListIterator.Next: TObject;
begin
  Result := nil;
  if HasNext then
  begin
    Inc(FCursor, 1);
    result := TObject(FList.Items[FCursor]);
  end;
end;

function TTBListIterator.HasPrevious: Boolean;
begin
  Result := False;
  if Assigned(FList) then
  begin
    if FCursor > 0 then
      Result := True;
  end;
end;

function TTBListIterator.Previous: TObject;
begin
  Result := nil;
  if HasPrevious then
  begin
    Dec(FCursor, 1);
    Result := TObject(FList.Items[FCursor]);
  end;
end;

constructor TTBListIterator.CreateCustom(const ASource: TList);
begin
  inherited Create;
  FList := ASource;
  FCursor := -1;
end;


{ TTBCollectionIterator }

function TTBCollectionIterator.HasNext: Boolean;
begin
  Result := False;
  if Assigned(FCollection) then
    if FCursor < FCollection.Count - 1 then
      Result := True;
end;

function TTBCollectionIterator.Next: TObject;
begin
  Result := nil;
  if HasNext then
  begin
    Inc(FCursor, 1);
    result := FCollection.Items[FCursor];
  end;
end;

function TTBCollectionIterator.HasPrevious: Boolean;
begin
  Result := False;
  if Assigned(FCollection) then
    if FCursor > 0 then
      Result := True;
end;

function TTBCollectionIterator.Previous: TObject;
begin
  Result := nil;
  if HasPrevious then
  begin
    Dec(FCursor, 1);
    Result := FCollection.Items[FCursor];
  end;
end;

constructor TTBCollectionIterator.CreateCustom(const ASource: TCollection);
begin
  inherited Create;
  FCollection := ASource;
  FCursor := -1;
end;


{ TTBFilteredStringsIterator }

function TTBFilteredStringsIterator.GetFilter: string;
begin
  Result := FRegex.RegexString;
end;

procedure TTBFilteredStringsIterator.SetFilter(const AValue: string);
const
  cFilterErr = 'Error in Filter string at position %d with ErrorCode %d. Filter string <%s>';
var
  LErrorCode: TRegexError;
  LErrorPos: integer;
begin
  if AValue <> FRegex.RegexString then
  begin
    FRegex.RegexString := AValue;
    if not FRegex.Parse(LErrorPos, LErrorCode) then
      raise Exception.CreateFmt(cFilterErr, [LErrorPos, Ord(LErrorCode), AValue]);
  end;
  FNextIndex := -1;
end;

function TTBFilteredStringsIterator.HasNext: Boolean;
var
  LIndex: integer;
  LMatchPos: integer;
  LOffset: integer;
begin
  Result := False;
  if GetFilter = '' then
  begin
    Result := inherited HasNext;
    if Result then
      FNextIndex := FCursor + 1;
  end
  else
  begin
    if FCursor < FStrings.Count - 1 then
    begin
      { If we haven't already calculated the next matching item }
      if FNextIndex = -1 then
      begin
        LIndex := FCursor + 1;
        { Peek ahead to find the next matching string }
        while (LIndex < FStrings.Count) and (FNextIndex = -1) do
        begin
          { reset MatchString parameters }
          LOffset   := 0;
          LMatchPos := 0;
          if FRegex.MatchString(FStrings.Strings[LIndex], LMatchPos, LOffset) then
            FNextIndex := LIndex;
          Inc(LIndex);
        end;
      end;
      if FNextIndex <> -1 then
        Result := True;
    end;
  end; { if..else }
end;

function TTBFilteredStringsIterator.Next: string;
begin
  Result := '';
  if HasNext then
  begin
    FCursor     := FNextIndex;
    FNextIndex  := -1;
    Result      := FStrings.Strings[FCursor];
  end;
end;

function TTBFilteredStringsIterator.HasPrevious: Boolean;
begin
  Result := False;  // Filtered String is uni-directional
end;

function TTBFilteredStringsIterator.Previous: string;
begin
  Result := '';
  raise EUniDirectionalIterator.Create('Filtered String Iterator is uni-directional (forward) only.');
end;

constructor TTBFilteredStringsIterator.CreateCustom(const ASource: TStrings);
begin
  inherited CreateCustom(ASource);
  FRegex := TRegexEngine.Create('');
  FRegex.IgnoreCase := True;
  FNextIndex := -1;
end;

destructor TTBFilteredStringsIterator.Destroy;
begin
  FRegex.Free;
  inherited Destroy;
end;


{ TTBInterfaceListIterator }

function TTBInterfaceListIterator.HasNext: Boolean;
begin
  Result := False;
  if Assigned(FList) then
    if FCursor < FList.Count - 1 then
      Result := True;
end;

function TTBInterfaceListIterator.Next: IInterface;
begin
  Result := nil;
  if HasNext then
  begin
    Inc(FCursor, 1);
    Result := FList.Items[FCursor];
  end;
end;

function TTBInterfaceListIterator.HasPrevious: Boolean;
begin
  Result := False;
  if Assigned(FList) then
    if FCursor > 0 then
      Result := True;
end;

function TTBInterfaceListIterator.Previous: IInterface;
begin
  Result := nil;
  if HasPrevious then
  begin
    Dec(FCursor, 1);
    Result := FList.Items[FCursor];
  end;
end;

constructor TTBInterfaceListIterator.CreateCustom(const ASource: TInterfaceList);
begin
  inherited Create;
  FList   := ASource;
  FCursor := -1;
end;

{ TTBObjectListIterator }

function TTBObjectListIterator.HasNext: Boolean;
begin
  Result := False;
  if Assigned(FList) then
    if FCursor < FList.Count - 1 then
      Result := True;
end;

function TTBObjectListIterator.Next: TObject;
begin
  result := nil;
  if HasNext then
  begin
    Inc(FCursor);
    result := FList.Items[FCursor];
  end;
end;

function TTBObjectListIterator.HasPrevious: Boolean;
begin
  Result := False;
  if Assigned(FList) then
    if FCursor > 0 then
      Result := True;
end;

function TTBObjectListIterator.Previous: TObject;
begin
  result := nil;
  if HasPrevious then
  begin
    Dec(FCursor);
    result := FList.Items[FCursor];
  end;
end;

constructor TTBObjectListIterator.CreateCustom(const ASource: TObjectList);
begin
  inherited Create;
  FList := ASource;
  FCursor := -1;
end;


end.

