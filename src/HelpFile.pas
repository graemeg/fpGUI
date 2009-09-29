Unit HelpFile;

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Encapsulates the basic reading of a help file's structure.

uses
  classes, BseDos, os2def, SysUtils, Graphics,
  DataTypes,
  HelpFileHeader, HelpTopic, HelpBitmap, ACLUtility,
  TextSearchQuery, SearchTable, CompareWordUnit;

Type

  EHelpFileException = class( Exception )
  end;

type
  THelpFile = class
  protected
    _Data: pointer;
    _DataLen: longint;

    _pSlotData: pInt16;
    _SlotDataSize: longint;

    _FileName: string;
    _Title: string;
    _Header: THelpFileHeader;

    _Topics: TList; // of TTopics

    _Dictionary: TList; // pointers to strings.

    _SlotOffsets: Int32ArrayPointer;
    _Index: TStringList;

    _SearchTable: TSearchTable;

    procedure InitMembers;
    procedure ReadFile( Filename: string );

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

    HighlightWords: Int32ArrayPointer;
  end;

// Returns helpfile that the given topic is within
Function TopicFile( Topic: TTopic ): THelpFile;

Implementation

uses
  Dialogs, Forms,
  BseErr,
  ACLFileUtility, ACLStringUtility, ACLFileIOUtility, ACLProfile,
  ACLPCharUtility, ACLDialogs,
  HelpWindow;

// Load "missing" bitmap
{$R Images}

Function TopicFile( Topic: TTopic ): THelpFile;
Begin
  Result := Topic.HelpFile as THelpFile;
end;

procedure THelpFile.InitMembers;
begin
  _pSlotData := nil;
  _SlotDataSize := 0;

//  _Dictionary:= TStringList.Create;
  _Dictionary:= TList.Create;
  _Topics:= TList.Create;
  _Index:= TStringList.Create;
end;

procedure THelpFile.ReadFile( Filename: string );
var
  OpenAction: ULong;
  rc: APIRET;
  szName: Cstring;
  F: HFILE;
  FileInfo: FILESTATUS3;
begin
  _FileName:= Filename;
  if not FileExists( Filename ) then
    raise EHelpFileException.Create( 'File not found' );

  szName:= FileName;
  rc:= DosOpen( szName,
                F,
                OpenAction,
                0, // file size - irrelevant, not creating,
                0, // attrs - ''
                OPEN_ACTION_OPEN_IF_EXISTS,
                OPEN_SHARE_DENYNONE + OPEN_ACCESS_READONLY,
                nil ); // no eas
  if rc<> 0 then
  begin
    case rc of
      ERROR_FILE_NOT_FOUND: // crap, this doesn't actually occur!
        raise EHelpFileException.Create( 'File not found' );

      ERROR_ACCESS_DENIED:
        raise EHelpFileException.Create( 'Access denied' );

      ERROR_SHARING_VIOLATION:
        raise EHelpFileException.Create( 'File in use by another program' );

      else
        raise EHelpFileException.Create( 'File open error' );
    end;
  end;

  DosQueryFileInfo( F,
                    FIL_STANDARD,
                    FileInfo,
                    sizeof( FileInfo ) );
  _DataLen:= FileInfo.cbFile; // file size
  GetMem( _Data, _DataLen );
  MyRead( F, _Data, _DataLen );
  DosClose( F );
end;

procedure THelpFile.ReadHeader;
begin
  MemCopy( _Data, Addr( _Header ), sizeof( _Header ) );

  if _Header.ID <> $5348 then
    raise EHelpFileException.Create( 'File doesn''t appear to be an OS/2 Help document (header ID not correct)' );

  _Title:= StrPas( _Header.Title );
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
  _Topics.Destroy;

  _Index.Destroy;

  _Dictionary.Destroy;

  _SearchTable.Destroy;

end;

procedure THelpFile.ReadContents;
var
  Topic: TTopic;
  EntryIndex: longint;
  pEntry: pTTOCEntryStart;
begin
  _Topics.Capacity:= _Header.ntoc;

  pEntry:= _Data + _Header.tocstart;

  for EntryIndex:= 0 to integer( _Header.ntoc ) - 1 do
  begin
    Topic:= TTopic.Create( _Data,
                           _Header,
                           _Dictionary,
                           pEntry );

    Topic.HelpFile:= Self;
    Topic.Index:= EntryIndex;

    _Topics.Add( Topic );

    inc( pEntry, pEntry ^. Length );
  end;
end;

procedure THelpFile.ReadDictionary;
var
  i: longint;
  Len: int8;
  p: pbyte;
begin
  P:= _Data + _Header.dictstart;
  for i:= 0 to integer( _Header.ndict ) - 1 do
  begin
    Len:= p^ - 1;
    p^ := Len; // adjust so we can use as  a string
//    S:= StrNPas( P, RecordLen-1 );
    _Dictionary.Add( P );
    inc( P, Len + 1 );
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
  IndexIndex: longint; // I can't resist :-)
  pEntryHeader: ^TIndexEntryHeader;
  EntryText: string;
  IndexTitleLen: longint;
  p: pointer;
begin
  p:= _Data + _Header.indexstart;

  for IndexIndex:= 0 to longint( _Header.nindex ) - 1 do
  begin
    pEntryHeader:= p;
    IndexTitleLen:= pEntryHeader^.TextLength;
    inc( p, sizeof( TIndexEntryHeader ) );

    GetMemString( p, EntryText, IndexTitleLen );
    if ( pEntryHeader^.flags and 2 ) > 0 then
      EntryText:= '- ' + EntryText;
    if pEntryHeader^.TOCIndex < _Topics.Count then
      _Index.AddObject( EntryText, _Topics[ pEntryHeader^.TOCIndex ] )
    else
//      raise EHelpFileException.Create( 'Error reading help file index - out of range topic reference' );
      ; // pass! something special
    inc( p, IndexTitleLen
            + pEntryHeader^.NumberOfRoots ); // skip 'roots' for index search
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
  Result:= _Topics[ Index ];
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
  Result := pstring( _Dictionary[ Index ] )^;
end;

Initialization
End.

