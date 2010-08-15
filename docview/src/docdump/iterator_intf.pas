unit iterator_intf;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,SysUtils
  ;

type
  { A custom exception class }
  ENoIteratorImpl = class(Exception);
  EUniDirectionalIterator = class(Exception);


  { Standard iterators }

  ITBIterator = interface(IInterface)
  ['{9C2BC10D-54C8-4B59-88B5-A564921CF0E3}']
    function    HasNext: Boolean;
    function    Next: TObject;
    function    HasPrevious: Boolean;
    function    Previous: TObject;
  end;


  ITBStringIterator = interface(IInterface)
  ['{B2A449B4-5D0A-4F14-AC11-CA055EDA3ED7}']
    function    HasNext: Boolean;
    function    Next: string;
    function    HasPrevious: Boolean;
    function    Previous: string;
  end;


  ITBStringAndObjectIterator = interface(ITBStringIterator)
  ['{287373DC-A90D-400E-BAEE-C85474C317A8}']
    function    HasNextObject: Boolean;
    function    NextObject: TObject;
    function    HasPreviousObject: Boolean;
    function    PreviousObject: TObject;
  end;


  ITBInterfaceIterator = interface
  ['{9B599C5B-4BBB-43F6-AF8E-09FEE9AE0E20}']
    function    HasNext: Boolean;
    function    Next: IInterface;
    function    HasPrevious: Boolean;
    function    Previous: IInterface;
  end;

  { TODO:
    More interfaces could be added for collections like:
    TTreeView, TStringGrid etc... }


  { Filtered iterators }

  ITBFilteredStringIterator = interface(ITBStringIterator)
  ['{CF1B9E2D-DD05-4D15-95C6-686EAFA4ED82}']
    function    GetFilter: string;
    procedure   SetFilter(const AValue: string);
    property    Filter: string read GetFilter write SetFilter;
  end;


  { TODO:
    More filtered versions of the standard iterators could
    be added here... }



  { Iterator Factory }

  TTBIteratorFactory = class(TObject)
    function    Iterator(const ASource: TObject): ITBIterator;
    function    StringIterator(const ASource: TObject): ITBStringIterator;
    function    StringAndObjectIterator(const ASource: TObject): ITBStringAndObjectIterator;
    function    InterfaceIterator(const ASource: TObject): ITBInterfaceIterator;
    function    FilteredStringIterator(const ASource: TObject; const AFilter: string): ITBFilteredStringIterator;
  end;


{ Global iterator factory singleton }
function gIteratorFactory: TTBIteratorFactory;


implementation

uses
  iterator_impl;

var
  uIteratorFactory: TTBIteratorFactory;

const
  cNoIteratorImpl = 'No Iterator implementation found for <%s>';


{ The lazy mans singleton implementation, but it does the job just fine. }
function gIteratorFactory: TTBIteratorFactory;
begin
  if not Assigned(uIteratorFactory) then
    uIteratorFactory := TTBIteratorFactory.Create;
  Result := uIteratorFactory;
end;


{ TTBIteratorFactory }

function TTBIteratorFactory.Iterator(const ASource: TObject): ITBIterator;
begin
  if ASource is TList then
    Result := TTBListIterator.CreateCustom(TList(ASource))
  else if ASource is TCollection then
    Result := TTBCollectionIterator.CreateCustom(TCollection(ASource))
  //else if ASource is TTreeNodes then
    //Result := TTBTreeNodesIterator.CreateCustom(TTreeNodes(ASource))
  else
    raise ENoIteratorImpl.CreateFmt(cNoIteratorImpl, [ASource.ClassName]);
end;

function TTBIteratorFactory.StringIterator(const ASource: TObject): ITBStringIterator;
begin
  if ASource is TStrings then
    Result := TTBStringsIterator.CreateCustom(TStrings(ASource))
  else
    raise ENoIteratorImpl.CreateFmt(cNoIteratorImpl, [ASource.ClassName]);
end;

function TTBIteratorFactory.StringAndObjectIterator(const ASource: TObject): ITBStringAndObjectIterator;
begin
  if ASource is TStrings then
    Result := TTBStringsIterator.CreateCustom(TStrings(ASource))
  else
    raise ENoIteratorImpl.CreateFmt(cNoIteratorImpl, [ASource.ClassName]);
end;

function TTBIteratorFactory.InterfaceIterator(const ASource: TObject): ITBInterfaceIterator;
begin
  if ASource is TInterfaceList then
    Result := TTBInterfaceListIterator.CreateCustom(TInterfaceList(ASource))
  else
    raise ENoIteratorImpl.CreateFmt(cNoIteratorImpl, [ASource.ClassName]);
end;

function TTBIteratorFactory.FilteredStringIterator(const ASource: TObject; const AFilter: string): ITBFilteredStringIterator;
begin
  if ASource is TStrings then
  begin
    Result := TTBFilteredStringsIterator.CreateCustom(TStrings(ASource));
    Result.Filter := AFilter;
  end
  else
    raise ENoIteratorImpl.CreateFmt(cNoIteratorImpl, [ASource.ClassName]);
end;


initialization
  uIteratorFactory := nil;

finalization
  uIteratorFactory.Free;

end.

