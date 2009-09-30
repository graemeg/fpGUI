Unit HelpFile;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Encapsulates the basic reading of a help file's structure.

uses
  Classes, SysUtils, DataTypes,
  HelpFileHeader, HelpTopic,
//  HelpBitmap,
  TextSearchQuery, SearchTable, CompareWordUnit,
  // this unit is used to fake image support untill fpGUI has this implemented.
  nvNullObjects;

Type

  EHelpFileException = class( Exception )
  end;

type
  THelpFile = class(TObject)
  protected
    _Data: pointer;
    _DataLen: longint;
    _pSlotData: pInt16;
    _SlotDataSize: longint;
    _FileName: string;
    _Title: string;
    _Header: THelpFileHeader;
    _Topics: TList; // of TTopic
    _Dictionary: TStringList;
    _SlotOffsets: Int32ArrayPointer;
    _Index: TStringList;
    _SearchTable: TSearchTable;

    procedure InitMembers;
    procedure ReadFile(const AFilename: string );

    procedure ReadHeader;
    procedure ReadContents;
    procedure ReadDictionary;

    procedure ReadIndex;

    procedure ReadFontTable;

    function GetTopic( Index: longint ): TTopic;
    function GetTopicCount: longint;

    function GetDictionaryCount: longint;
    function GetDictionaryWord( Index: longint ): string;

  public
    HighlightWords: Int32ArrayPointer;
    constructor Create( const FileName: string;
                        UpdateProgress: TProgressCallback );

    destructor Destroy; override;

    property Title: string read _Title;
    property Topics[ Index: longint ]: TTopic read GetTopic;
    property TopicCount: longint read GetTopicCount;
    property Index: TStringList read _Index;
    property Filename: string read _FileName;

    procedure GetImages( ImageOffsets: TList;
                         Images: TImageList );

    property DictionaryCount: longint read GetDictionaryCount;
    property DictionaryWords[ Index: longint ]: string read GetDictionaryWord;

    function IndexOfTopic( Topic: TTopic ): longint;

    property SearchTable: TSearchTable read _SearchTable;
  end;

// Returns helpfile that the given topic is within
Function TopicFile( Topic: TTopic ): THelpFile;


Implementation

uses
  //ACLFileUtility, ACLStringUtility, ACLFileIOUtility, ACLProfile,
  //ACLPCharUtility, ACLDialogs,
  HelpWindow, nvUtilities;

// Load "missing" bitmap
//{.$R Images}

Function TopicFile( Topic: TTopic ): THelpFile;
Begin
  Result := Topic.HelpFile as THelpFile;
end;

procedure THelpFile.InitMembers;
begin
  _pSlotData := nil;
  _SlotDataSize := 0;

  _Dictionary:= TStringList.Create;
  _Topics:= TList.Create;
  _Index:= TStringList.Create;
end;

procedure THelpFile.ReadFile(const AFilename: string );
var
  AFile: File of char;
begin
  _FileName:= AFilename;
  if not FileExists( AFilename ) then
    raise EHelpFileException.CreateFmt('File <%s> not found', [AFilename]);

  AssignFile(AFile, Filename);
  try
    FileMode := fmOpenRead; // read-only
    {$i-}
    Reset(AFile);
    {$i+}
    if IOResult <> 0 then
    begin
      case IOResult of
        2:      raise EHelpFileException.Create( 'File not found' );
        5:      raise EHelpFileException.Create( 'Access denied' );
        else
          raise EHelpFileException.CreateFmt( 'File open error. IO Error is <%d>', [IOResult] );
      end;
    end;

    _DataLen := FileSize(AFile);  // in bytes
    _Data := nil;
    GetMem( _Data, _DataLen );    // allocate enough memory
    BlockRead(AFile, _Data^, _DataLen);
  finally
    CloseFile(AFile);
  end;
end;

procedure THelpFile.ReadHeader;
begin
ProfileEvent('THelpFile.ReadHeader >>>>');
  Move(_Data^, _Header, SizeOf(_Header));
ProfileEvent('title=' + _Header.title);

  if _Header.ID <> $5348 then  // 'HS'
    raise EHelpFileException.Create( 'File doesn''t appear to be an OS/2 Help document (header ID not correct)' );

  _Title := _Header.Title;
ProfileEvent('THelpFile.ReadHeader <<<<<');
end;

constructor THelpFile.Create( const FileName: string;
                              UpdateProgress: TProgressCallback );
var
  SearchTableOffset: longint;
  SearchTableRecordLengthIs16Bit: boolean;
begin
  ProfileEvent( 'Helpfile Load: ' + FileName );

  InitMembers;

  UpdateProgress( 1, 100, 'Reading file' );

  ReadFile( FileName );

  UpdateProgress( 20, 100, 'Interpreting file' );

  ProfileEvent( 'Read header' );
  ReadHeader;

  UpdateProgress( 40, 100, 'Reading contents' );
  ProfileEvent( 'Read contents' );
  ReadContents;

  UpdateProgress( 60, 100, 'Reading dictionary' );
  ProfileEvent( 'Read dictionary' );
  ReadDictionary;

  UpdateProgress( 80, 100, 'Reading index' );
  ProfileEvent( 'Read index' );
  ReadIndex;

  UpdateProgress( 90, 100, 'Reading search table' );
  ProfileEvent( 'Read search table' );
  SearchTableOffset := _Header.SearchStart and $7fffffff;
  SearchTableRecordLengthIs16Bit := _Header.SearchStart and $80000000 > 0;
  _SearchTable := TSearchTable.Create( _Data + SearchTableOffset,
                                       SearchTableRecordLengthIs16Bit,
                                       _Dictionary.Count,
                                       _Topics.Count );

  UpdateProgress( 100, 100, 'Done' );

  GetMem( HighlightWords,
          _Dictionary.Count * sizeof( longint ) );

end;

destructor THelpFile.Destroy;
var
  TopicIndex: longint;
begin
  FreeMem( HighlightWords,
           _Dictionary.Count * sizeof( longint ) );

  FreeMem( _Data, _DataLen );

  for TopicIndex:= 0 to _Topics.Count - 1 do
    TTopic( _Topics[ TopicIndex ] ).Destroy;
  _Topics.Free;

  _Index.Free;

  _Dictionary.Free;

  _SearchTable.Free;
end;

procedure THelpFile.ReadContents;
var
  Topic: TTopic;
  EntryIndex: longint;
  pEntry: pTTOCEntryStart;
  tocarray: array of Int32;
  p: PByte;
begin
  _Topics.Capacity := _Header.ntoc;

//--------------------------------- experimental >>
  SetLength(tocarray, _Header.ntoc);
  p := _Data + _Header.tocoffsetsstart;

  Move(p, tocarray, SizeOf(tocarray));
  for EntryIndex := 0 to _Header.ntoc-1 do
  begin
    pEntry := _Data + tocarray[EntryIndex];
    writeln('pEntry^.length = ', IntToStr(pEntry^.Length) + ' NumSlots=' + IntToStr(pEntry^.numSlots));
    Topic := TTopic.Create( _Data,
                           _Header,
                           _Dictionary,
                           pEntry );
  end;
  SetLength(tocarray, 0);
  exit;
//----------------------------------

  pEntry:= _Data + _Header.tocstart;

writeln('----------- old method ');
  for EntryIndex := 0 to _Header.ntoc-1 do
  begin
    Topic:= TTopic.Create( _Data,
                           _Header,
                           _Dictionary,
                           pEntry );

    Topic.HelpFile := Self;
    Topic.Index := EntryIndex;

    _Topics.Add( Topic );

    inc( pEntry, pEntry^.Length );
    writeln('Topic ' + IntToStr(EntryIndex) + ' length = ' + IntToStr(pEntry^.Length));
  end;
end;

procedure THelpFile.ReadDictionary;
var
  i: longint;
  p: pbyte;
  c: array[0..255] of char;
  b: byte;
  s: string;
begin
  p := _Data + _Header.dictstart;   // set starting position
  for i := 0 to _header.ndict-1 do
  begin
    FillChar(c, sizeof(c),0);       // fill string with NUL chars
    Move(p^, b, sizeof(b));         // read string length value
    Inc(p, sizeof(b));              // move pointer
    Move(p^, c, b-1);               // read string of dictionary
    Inc(p, b-1);                    // move pointer
    s := StrPas(@c);
    _Dictionary.Add(s);
  end;
end;

type
  TIndexEntryHeader = record
    TextLength: int8;
    Flags: int8;
    NumberOfRoots: int8;
    TOCIndex: int16;
  end;

procedure THelpFile.ReadIndex;
var
  IndexIndex: longint;
  pEntryHeader: ^TIndexEntryHeader;
  EntryText: string;
  IndexTitleLen: longint;
  p: pointer;
begin
  p := _Data + _Header.indexstart;

  for IndexIndex:= 0 to longint( _Header.nindex ) - 1 do
  begin
    pEntryHeader := p;
    IndexTitleLen:= pEntryHeader^.TextLength;
    inc( p, sizeof( TIndexEntryHeader ) );

    { TODO -oGraeme : Double check this later }
    EntryText := PChar(p);
//    GetMemString( p, EntryText, IndexTitleLen );

    if ( pEntryHeader^.flags and 2 ) > 0 then
      EntryText:= '- ' + EntryText;
    if pEntryHeader^.TOCIndex < _Topics.Count then
      _Index.AddObject( EntryText, TTopic(_Topics[ pEntryHeader^.TOCIndex ]) )
    else
//      raise EHelpFileException.Create( 'Error reading help file index - out of range topic reference' );
      ; // pass! something special
    inc( p, IndexTitleLen + pEntryHeader^.NumberOfRoots ); // skip 'roots' for index search
  end;

end;

type
  HelpFontSpec = class
    FaceName: array[ 0..32 ] of char;
    Height: int16;
    Width: int16;
    Codepage: int16;
  end;

procedure THelpFile.ReadFontTable;
begin
  { TODO : ReadFontTable }
end;

procedure THelpFile.GetImages( ImageOffsets: TList;
                               Images: TImageList );
var
  ListIndex: longint;
  ImageOffset: longint;
  Bitmap: THelpBitmap;
begin
  Images.Clear;
  for ListIndex:= 0 to ImageOffsets.Count - 1 do
  begin
    { TODO -oGraeme : Double check pointer conversion }
    ImageOffset := longint( ImageOffsets[ ListIndex ] );
    try
      Bitmap:= THelpBitmap.CreateFromHelpFile( _Data
                                               + _Header.imgstart
                                               + ImageOffset );
    except
      on e: EHelpBitmapException do
{        raise EHelpFileException.Create( 'Error loading help bitmap at'
                                         + IntToStr( ImageOffset )
                                         + ': '
                                         + e.Message );}
      begin
        Bitmap:= THelpBitmap.Create;
        Bitmap.LoadFromResourceName( 'MissingBitmap' );
      end;
    end;

    Images.Add( Bitmap, nil );
    Bitmap.Destroy;
  end;
end;

function THelpFile.GetTopic( Index: longint ): TTopic;
begin
  Result:= TTopic(_Topics[ Index ]);
end;

function THelpFile.GetTopicCount: longint;
begin
  Result:= _Topics.Count;
end;

function THelpFile.IndexOfTopic( Topic: TTopic ): longint;
begin
  Result:= _Topics.IndexOf( Topic );
end;

function THelpFile.GetDictionaryCount: longint;
begin
  Result := _Dictionary.Count;
end;

function THelpFile.GetDictionaryWord( Index: longint ): string;
begin
  Result := _Dictionary[Index];
end;

Initialization
End.

