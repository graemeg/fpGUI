unit HelpFile;

{$mode objfpc}{$H+}

interface

// Encapsulates the basic reading of a help file's structure.

uses
  Classes
  ,SysUtils
  ,fpg_imagelist
  ,IPFFileFormatUnit
  ,HelpTopic
  ,HelpBitmap
  ,SearchTable
  ;

type

  TFontEncoding = (encUTF8, encCP437, encCP850, encIBMGraph);


  TIndexEntry = class(TObject)
  private
    name: String;
    topic: TTopic;
    flags: uint8;
  public
    constructor Create(aName: String; aTopic: TTopic; aFlags: uint8);
    destructor  Destroy; override;
    property    getTopic: TTopic read topic;
    function    getLabel: String;
    function    isGlobal: boolean;
    function    getLevel: integer;
  end;


  TIndex = class(TObject)
  private
    entries: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Count: longint;
    function    GetLabels: TStringList;
    function    GetTopic(aPos: longint): TTopic;
    procedure   Add(anIndexEntry: TIndexEntry);
  end;


  THelpFile = class(TObject)
  private
    function GetFormatVersion: string;
    function GetStringResourceIDCount: integer;
    function GetNumericResourceIDCount: integer;
  protected
    _Filename : string;
    _FileSize : longint;
    _Handle: TFileStream;

    _pSlotData: pUInt16;
    _SlotDataSize: longint;

    _Title: string;

    _Topics: TList; // of TTopics

    _Dictionary: TStringList; // pointers to strings.

    _Index: TIndex;

    _SearchTable: TSearchTable;

    _ReferencedFiles: TStringList;

    _FontTable: TList;

    _pHeader: TPHelpFileHeader;
    _pExtendedHeader: TPExtendedHelpFileHeader;
    _pContentsData: pointer;
    _pResourceData: pointer;
    _pSearchData: pointer;
    _pHighlightWords: UInt32ArrayPointer;
    _pSlotOffsets: Uint32ArrayPointer;
    _pDictionaryData: pointer;
    _pFontTableData: pointer;
    _pTopicNameData: pointer;
    _pTopicGlobalNamesData: pointer;

    procedure InitMembers;
    procedure Open;
    procedure Close;
    procedure ReadFileBlock( Var Dest: pointer;
                             const StartPosition: LongWord;
                             const Length: LongWord);

    procedure ReadHeader;
    procedure ReadContents;
    procedure ReadDictionary;
    procedure ReadSearchTable;

    procedure ReadIndex;

    procedure ReadReferencedFilesTable;
    procedure ReadFontTableData;
    procedure ParseFontTable;

    function GetTopic( AIndex: longint ): TTopic;
    function GetTopicCount: longint;

    function GetDictionaryCount: longint;
    function GetDictionaryWord( AIndex: longint ): string;

    function GetHighlightWords: UInt32ArrayPointer;

    function GetSearchTable: TSearchTable;

    // Lookup global or local panel name list
    function FindTopicByName( const Name: string;
                              Var pData: pointer;
                              Count: longint;
                              Offset: longint ): TTopic;

  public
    constructor Create( const aFileName: string );

    destructor Destroy; override;

    function GetIndex: TIndex;

    property Title: string read _Title;
    property Topics[ Index: longint ]: TTopic read GetTopic;
    property TopicList: TList read _Topics;
    property TopicCount: longint read GetTopicCount;
    property StringResourceIDCount: integer read GetStringResourceIDCount;
    property NumericResourceIDCount: integer read GetNumericResourceIDCount;
    property Index: TIndex read GetIndex;
    property Filename: string read _FileName;
    property FormatVersion: string read GetFormatVersion;

    property ReferencedFiles: TStringList read _ReferencedFiles;

    procedure GetImages( ImageOffsets: TList; Images: TfpgImageList );

    function GetImage( ImageOffset: longint ): THelpBitmap;

    property DictionaryCount: longint read GetDictionaryCount;
    property DictionaryWords[ AIndex: longint ]: string read GetDictionaryWord;

    function IndexOfTopic( Topic: TTopic ): longint;

    property SearchTable: TSearchTable read GetSearchTable;

    function FindTopicByResourceID( ID: uint16 ): TTopic;

    function FindTopicByLocalName( const Name: string ): TTopic;
    function FindTopicByGlobalName( const Name: string ): TTopic;

    function FindTopicByTitleStartsWith( const SearchText: string ): TTopic;
    function FindTopicByTitleContains( const SearchText: string ): TTopic;

    function FindTopicByIndexStartsWith( const SearchText: string ): TTopic;
    function FindTopicByIndexContains( const SearchText: string ): TTopic;

    procedure FindResourceIDsForTopic( Topic: TTopic;
                                       ResourceIDs: TList );

    property HighlightWords: UInt32ArrayPointer read GetHighlightWords;

    property FileSize: longint read _FileSize;

    procedure SetupFontSubstitutes( Substitutions: string );
  public
    NotesLoaded: boolean; // used externally
    Encoding: TFontEncoding;
  end;

// Returns helpfile that the given topic is within
Function TopicFile( Topic: TTopic ): THelpFile;

function GetHelpFileTitle( const Filename: string ): string;

Implementation

uses
//  BseErr,
//  StringUtilsUnit,
//  CharUtilsUnit,
//  DebugUnit,
//  ACLFileIOUtility,
//  ACLLanguageUnit;
  fpg_main
  ,fpg_utils
  ,fpg_stringutils
  ,nvUtilities
  ,ACLStringUtility
  ;


const
  FileErrorNotFound = 'File not found ';
  FileErrorAccessDenied = 'File access denied';
  FileErrorInUse = 'File in use';
  FileErrorInvalidHeader = 'Invalid file header';


  // -----------
  // TIndexEntry
  // -----------

  CONSTRUCTOR TIndexEntry.Create(aName: String; aTopic: TTopic; aFlags: uint8);
  begin
    LogEvent(LogObjConstDest, 'TIndexEntry.Create');
    name := aName;
    topic := aTopic;
    flags := aFlags;
  end;


  DESTRUCTOR TIndexEntry.Destroy;
  begin
    LogEvent(LogObjConstDest, 'TIndexEntry.Destroy');
    topic := nil;
    inherited Destroy;
  end;


  FUNCTION TIndexEntry.getLabel: String;
  begin
    result := name;

    // index level check (level 1 or 2)
    if (getLevel) > 1 then
    begin
      result := '- ' + result;
    end;

    if isGlobal then
    begin
      result := result + ' (g)';
    end;
  end;


  FUNCTION TIndexEntry.isGlobal: boolean;
  begin
    result := (flags and 64) > 0
  end;


  FUNCTION TIndexEntry.getLevel: integer;
  begin
    result := 1;

    // index level check (level 1 or 2)
    if (flags and 2 ) > 0 then
    begin
      result := 2;
    end;
  end;




  // -----------
  // TIndex
  // -----------
  CONSTRUCTOR TIndex.Create;
  begin
    inherited Create;

    entries := TStringList.Create;
    // labels := nil; // lazy
  end;


  DESTRUCTOR TIndex.Destroy;
  var
    i : longint;
    tmpEntry : TIndexEntry;
  begin
    LogEvent(LogObjConstDest, 'TIndex.Destroy (size:' + IntToStr(entries.Count) + ')');

    for i := 0 to entries.Count - 1 do
    begin
      tmpEntry := TIndexEntry(entries.Objects[i]);
      if tmpEntry <> nil then
      begin
        tmpEntry.Free;
        entries.Objects[i] := nil;
      end;
    end;
    entries.Free;

    inherited Destroy;
  end;


  FUNCTION TIndex.Count: longint;
  begin
    result := entries.Count;
  end;


  FUNCTION TIndex.GetLabels: TStringList;
  begin
    result := entries;
  end;


  FUNCTION TIndex.GetTopic(aPos: longint): TTopic;
  begin
    result := TIndexEntry(entries.Objects[aPos]).getTopic;
  end;


  PROCEDURE TIndex.add(anIndexEntry: TIndexEntry);
  begin
//    LogEvent(LogDebug, 'TIndex.add(' + anIndexEntry.getLabel + ', ' + anIndexEntry.ClassName + ')');
    entries.AddObject(anIndexEntry.getLabel, anIndexEntry);
  end;

Function TopicFile( Topic: TTopic ): THelpFile;
Begin
  Result := Topic.HelpFile as THelpFile;
end;

function THelpFile.GetFormatVersion: string;
begin
  Result := Format('%d.%d', [_pHeader^.version_hi, _pHeader^.version_lo]);
end;

function THelpFile.GetStringResourceIDCount: integer;
begin
  Result := _pHeader^.nname;
end;

function THelpFile.GetNumericResourceIDCount: integer;
begin
  Result := _pHeader^.nres;
end;

procedure THelpFile.InitMembers;
begin
  _SlotDataSize := 0;

  _pHeader := nil;
  _pExtendedHeader := nil;
  _pContentsData := nil;
  _pSlotOffsets := nil;
  _pResourceData := nil;
  _pSearchData := nil;
  _pDictionaryData := nil;
//  _pIndexData := nil;
  _pFontTableData := nil;

  _pHighlightWords := nil;

  _Dictionary:= TStringList.Create;
  _Topics := TList.Create;
// _Index := TStringList.Create;
  _ReferencedFiles := TStringList.Create;
  _FontTable := TList.Create;

  NotesLoaded := false;
end;


constructor THelpFile.Create(const aFileName: string);
var
  i: integer;
  lText: string;
  lTopic: TTopic;
begin
  LogEvent(LogObjConstDest, 'THelpFile.Create (file:' + aFileName + ')');
  LogEvent(LogParse, 'Helpfile Load: ' + aFileName);

  _FileName := aFileName;
  Encoding := encUTF8;

  InitMembers;
  Open;

  // we always need these basics:
  try
    ReadHeader;
    ReadContents;
    ReadDictionary;
    ReadFontTableData;
    ParseFontTable;
    ReadReferencedFilesTable;
    
    // Fix text encoding
    for i := 0 to TopicCount-1 do
    begin
      lText := TTopic(TopicList[i]).Title;
      // apply encoding conversion
      case Encoding of
        encUTF8:      lText := IPFToUTF8(lText);
        encCP437:     lText := CP437ToUTF8(lText);
        encCP850:     lText := CP850ToUTF8(lText);
        encIBMGraph:  lText := IBMGraphToUTF8(lText);
      else
        lText := IPFToUTF8(lText);
      end;
      TTopic(TopicList[i]).Title := lText;
    end;
  except
    Close;
    raise;
  end;
  // the rest is loaded on demand
end;


destructor THelpFile.Destroy;
begin
  LogEvent(LogObjConstDest, 'THelpFile.Destroy');
  Dispose( _pHeader );
  Dispose( _pExtendedHeader );
  FreeMem( _pContentsData );
  FreeMem( _pSlotOffsets );
  FreeMem( _pResourceData );
  FreeMem( _pSearchData );
  FreeMem( _pDictionaryData );
//  DeallocateMemory( _pIndexData );
  FreeMem( _pFontTableData );

  FreeMem( _pHighlightWords );

  // index entries are pointing to topics
  // so let us clean them first
  if Assigned( _Index ) then
    _Index.Free;

  if Assigned( _Topics ) then
    DestroyListAndObjects( _Topics );

  _Dictionary.Free;
  _SearchTable.Free;
  _ReferencedFiles.Free;
  _FontTable.Free;

  _Handle.Free;
end;

procedure THelpFile.Open;
begin
  LogEvent(LogDebug, 'Open File >>');
  if not FileExists( _Filename ) then
    raise EHelpFileException.Create( FileErrorNotFound );

  try
    _Handle := TFileStream.Create(_FileName, fmOpenRead or fmShareDenyWrite);
  except
    on E: Exception do
      raise EHelpFileException.Create(E.Message);
  end;
    //case rc of
    //  ERROR_FILE_NOT_FOUND: // crap, this doesn't actually occur!
    //    raise EHelpFileException.Create( FileErrorNotFound );
    //
    //  ERROR_ACCESS_DENIED:
    //    raise EHelpFileException.Create( FileErrorAccessDenied );
    //
    //  ERROR_SHARING_VIOLATION:
    //    raise EHelpFileException.Create( FileErrorInUse );
    //
    //  else
    //    raise EHelpFileException.Create( SysErrorMessage( rc ) );
    //end;

  _FileSize := GetFileSize(_Filename);
  LogEvent(LogDebug, 'Open File  <<');
end;

procedure THelpFile.Close;
begin
  _Handle.Free;
  _Handle := nil;
end;

procedure THelpFile.ReadFileBlock(var Dest: pointer;
    const StartPosition: LongWord; const Length: LongWord);
var
  bytes: LongWord;
begin
  if Length = 0 then
    exit; // nothing to read - go home!

  _Handle.Seek(StartPosition, soBeginning);

  // we allocate early so this should never happen
  if Dest = nil then
    Dest := GetMem(Length);

  bytes := _Handle.Read(Dest^, Length);
  if bytes <> Length then
    raise EHelpFileException.Create(ErrorCorruptHelpFile);
end;

procedure THelpFile.ReadHeader;
begin
  LogEvent(LogParse, 'Read header');
  New(_pHeader);
  ReadFileBlock( _pHeader,
                 0,
                 sizeof( THelpFileHeader ) );
  if _pHeader^.ID <> INF_HEADER_ID then
  begin
    // not an OS/2 help file.
    if (Byte(_pHeader^.ID[0]) = $5f) and (Byte(_pHeader^.ID[1]) = $3f) then
      raise EWindowsHelpFormatException.Create( 'It seems we have a Win16 help file!' );

    raise EHelpFileException.Create( FileErrorInvalidHeader );
  end;

  _Title := _pHeader^.Title;

  if _pHeader^.extstart > 0 then
  begin
    New(_pExtendedHeader);
    // read extended header
    ReadFileBlock( _pExtendedHeader,
                   _pHeader^.extstart,
                   sizeof( _pExtendedHeader^ ) );
  end;
end;

procedure THelpFile.ReadContents;
var
  Topic: TTopic;
  EntryIndex: longint;
  pEntry: pTTOCEntryStart;
  pEnd: pbyte;
  tocarray: UInt32ArrayPointer;
  pData: Pointer;
  p: PByte;
begin
  LogEvent(LogParse, 'Read contents');

  if _pHeader^.ntoc = 0 then
    exit; // explicit check required since ntoc is unsigned

  // Presize the topics list to save reallocation time
  _Topics.Capacity := _pHeader^.ntoc;

  // read toc offsets array
  //ReadFileBlock( tocarray,
  //              _pHeader^.tocoffsetsstart,
  //              _pHeader^.ntoc * SizeOf(uint32) );

  // read slots first so that Topics can refer to it.
  ReadFileBlock( _pSlotOffsets,
                 _pHeader^.slotsstart,
                 _pHeader^.nslots * sizeof( uint32 ) );

  ReadFileBlock( _pContentsData,
                 _pHeader^.tocstart,
                 _pHeader^.toclen );

  pEntry := _pContentsData;
  pEnd := _pContentsData + _pHeader^.toclen;
  p := PByte(pEntry);

  for EntryIndex := 0 to _pHeader^.ntoc - 1 do
  begin
//    pEntry := _Handle.Seek(tocarray[EntryIndex], soBeginning);
//    pEntry := tocarray[EntryIndex];
    if p >= pEnd then
      // runs off end of data!
      raise EHelpFileException.Create( ErrorCorruptHelpFile );

    Topic := TTopic.Create( _Handle,
                            _pSlotOffsets,
                            _Dictionary,
                            pEntry,
                            _FontTable,
                            _ReferencedFiles );

        
    Topic.HelpFile := Self;
    Topic.Index := EntryIndex;

    _Topics.Add( Topic );
    p := PByte(pEntry);
    inc(p, pEntry^.Length);
    pEntry := pTTOCentryStart(p);
  end;
end;

procedure THelpFile.ReadDictionary;
var
  i: longint;
  Len: uint8;
  p: pbyte;
  pEnd: pbyte;
  s: string;
  c: array[0..255] of char;
begin
  LogEvent(LogParse, 'Read dictionary');

  if _pHeader^.ndict = 0 then
    exit; // explicit check required since ndict is unsigned

  ReadFileBlock( _pDictionaryData,
                 _pHeader^.dictstart,
                 _pHeader^.dictlen );

  P := _pDictionaryData;
  pEnd := _pDictionaryData + _pHeader^.dictlen;

  // Presize the dictionary to save reallocation
  _Dictionary.Capacity := _pHeader^.ndict;
  for i := 0 to _pHeader^.ndict - 1 do
  begin
    // adjust length so we can use as a Pascal string
    // (file uses length including length byte,
    //  Pascal string have length excluding length byte)
    if p >= pEnd then
      // ran off end of data
      raise EHelpFileException.Create( 'Error reading help file dictionary' );


    FillChar(c, sizeof(c), 0);      // fill string with NUL chars
    Len := p^ - 1;                  // read string length value (corrected length)
    Inc(p, sizeof(byte));           // move pointer
    Move(p^, c, Len);               // read string of dictionary
    s := c;                         // convert PChar to String type

    _Dictionary.Add( s );
    Inc(p, Len);                    // move pointer to next item
  end;
end;


function THelpFile.GetIndex: TIndex;
begin
  if _Index = nil then
  begin
    ReadIndex;
  end;
  Result := _Index;
end;

type
  TIndexEntryHeader = packed record
    TextLength: uint8;
    Flags: uint8;
    NumberOfRoots: uint8;
    TOCIndex: uint16;
  end;
  pTIndexEntryHeader = ^TIndexEntryHeader;

procedure THelpFile.ReadIndex;
var
  IndexIndex: longint;
  pEntryHeader: pTIndexEntryHeader;
  EntryText: string;
  IndexTitleLen: longint;
  p: pByte;
  pEnd: pByte;
  pIndexData: pointer;
  tmpIndexEntry: TIndexEntry;
  lText: string;
begin
  LogEvent(LogParse, 'Read index');
  _Index := TIndex.Create;
  if _pHeader^.nindex = 0 then
    exit; // explicit check required since ndict is unsigned

  pIndexData := nil;
  ReadFileBlock( pIndexData,
                 _pHeader^.indexstart,
                 _pHeader^.indexlen );

  P := pIndexData;
  pEnd := pIndexData + _pHeader^.indexlen;

  for IndexIndex := 0 to _pHeader^.nindex - 1 do
  begin
    if p >= pEnd then
      // ran off end of data
      raise EHelpFileException.Create( 'Error reading help file index' );

    pEntryHeader := pTIndexEntryHeader(p);
    IndexTitleLen := pEntryHeader^.TextLength;
    inc( p, sizeof( TIndexEntryHeader ) );

    EntryText := '';
    SetString(EntryText, PChar(p), IndexTitleLen);

    if pEntryHeader^.TOCIndex < _Topics.Count then
    begin
      // apply encoding conversion
      case Encoding of
        encUTF8:      lText := IPFToUTF8(EntryText);
        encCP437:     lText := CP437ToUTF8(EntryText);
        encCP850:     lText := CP850ToUTF8(EntryText);
        encIBMGraph:  lText := IBMGraphToUTF8(EntryText);
      else
        lText := IPFToUTF8(EntryText);
      end;
      EntryText := lText;
      tmpIndexEntry := TIndexEntry.Create(EntryText, TTopic(_Topics[pEntryHeader^.TOCIndex]), pEntryHeader^.flags);
      _Index.Add(tmpIndexEntry);
    end
    else
//      raise EHelpFileException.Create( 'Error reading help file index - out of range topic reference' );
      ; // pass! something special

    inc( p, IndexTitleLen
            + pEntryHeader^.NumberOfRoots
              * sizeof( uint32 ) ); // skip 'roots' for index search
  end;

  FreeMem( pIndexData );
end;

function THelpFile.GetSearchTable: TSearchTable;
begin
  if _SearchTable = nil then
    ReadSearchTable;
  Result := _SearchTable;
end;

procedure THelpFile.ReadSearchTable;
var
  SearchTableOffset: longint;
  SearchTableRecordLengthIs16Bit: boolean;
begin
  LogEvent(LogParse, 'Read search table');

  if _pHeader^.SearchLen = 0 then
  begin
    LogEvent(LogParse, 'Read search table (len = 0');
    exit;
  end;

  SearchTableOffset := _pHeader^.SearchStart and $7fffffff;
  SearchTableRecordLengthIs16Bit := (_pHeader^.SearchStart and $80000000) > 0;
  ReadFileBlock( _pSearchData,
                 SearchTableOffset,
                 _pHeader^.SearchLen );

  _SearchTable := TSearchTable.Create( _pSearchData,
                                       SearchTableRecordLengthIs16Bit,
                                       _Dictionary.Count,
                                       _Topics.Count );
end;

function THelpFile.GetHighlightWords: UInt32ArrayPointer;
begin
  if _pHighlightWords = nil then
    _pHighlightWords := GetMem( _Dictionary.Count * sizeof( UInt32 ) );
  Result := _pHighlightWords;
end;

function THelpFile.FindTopicByResourceID( ID: uint16 ): TTopic;
var
  i: longint;
  pResourceIDs: UInt16ArrayPointer;
  pTopicIndices: UInt16ArrayPointer;
  FileResourceID: uint16;
  TopicIndex: uint16;
begin
  Result := nil;

  if _pHeader^.nres = 0 then
    // since nres is unsigned
    exit;

  if _pResourceData = nil then
  begin
    ReadFileBlock( _pResourceData,
                   _pHeader^.resstart,
                   (_pHeader^.nres * sizeof( uint16 )) * 2 ); // list of IDs, list of topics
  end;

  pResourceIDs := _pResourceData;
  pTopicIndices := _pResourceData
                   + _pHeader^.nres * sizeof( uint16 );

  for i := 0 to _pHeader^.nres - 1 do
  begin
    FileResourceID := pResourceIDs^[ i ];
    if FileResourceID = ID then
    begin
      // found
      TopicIndex := pTopicIndices^[ i ];
      Result := TTopic(_Topics[ TopicIndex ]);
      exit;
    end;
  end;
end;

// Look up a local "panel name" and return associated topic, if any.
function THelpFile.FindTopicByLocalName( const Name: string ): TTopic;
begin
  Result := FindTopicByName( Name,
                             _pTopicNameData,
                             _pHeader^.nname,
                             _pHeader^.namestart );
end;

function THelpFile.FindTopicByGlobalName( const Name: string ): TTopic;
begin
  Result := nil;

  if _pExtendedHeader = nil then
    // no extended header - no global list to lookup
    exit;

  Result := FindTopicByName( Name,
                             _pTopicGlobalNamesData,
                             _pExtendedHeader ^. EntryInGNameTable,
                             _pExtendedHeader ^. HelpPanelGNameTblOffset );

end;

// The text of the names are stored in the (global) dictionary
// with a table referencing them.
// We could use a binary search here... but whatever...
function THelpFile.FindTopicByName( const Name: string;
                                    Var pData: pointer;
                                    Count: longint;
                                    Offset: longint ): TTopic;
var
  i: longint;
  pNameTable: UInt16ArrayPointer;
  pTopicIndices: UInt16ArrayPointer;
  TopicIndex: uint16;

  TopicNameWordIndex: uint16;
  TopicName: string;
begin
  Result := nil;

  if Count = 0 then
    // since it's unsigned
    exit;

  if pData = nil then
    ReadFileBlock( pData,
                   Offset,
                   Count * sizeof( uint16 ) * 2 ); // list of name words, list of topics

  // get pointers to the two parts of the table
  pNameTable := pData;
  pTopicIndices := pData
                   + Count * sizeof( uint16 );

  for i := 0 to Count - 1 do
  begin
    TopicNameWordIndex := pNameTable^[ i ];
    TopicName := DictionaryWords[ TopicNameWordIndex ];

    if CompareText( TopicName, Name ) = 0 then
    begin
      // found
      TopicIndex := pTopicIndices^[ i ];
      Result := TTopic(_Topics[ TopicIndex ]);
      exit;
    end;
  end;
end;


// TODO move to index class
function THelpFile.FindTopicByIndexStartsWith( const SearchText: string ): TTopic;
var
  i: longint;
  tmpLabel: String;
begin
  result := nil;
  GetIndex; // make sure it's read

  for i := 0 to _Index.Count - 1 do
  begin
    tmpLabel := _Index.GetLabels[i];
    if SameText(tmpLabel, SearchText) then
    begin
      // found
      result := Index.getTopic(i);
      exit;
    end;
  end;
end;


function THelpFile.FindTopicByIndexContains(const SearchText: string): TTopic;
var
  i: longint;
  tmpLabel: String;
begin
  result := nil;
  GetIndex; // make sure it's read

  for i := 0 to _Index.Count - 1 do
  begin
    tmpLabel := _Index.GetLabels[i];
    if Pos(UpperCase(SearchText), UpperCase(tmpLabel)) > 0 then
    begin
      // found
      result := Index.getTopic(i);
      exit;
    end;
  end;
end;


function THelpFile.FindTopicByTitleStartsWith( const SearchText: string ): TTopic;
var
  i: longint;
  tmpTopic: TTopic;
  tmpLevel : integer;
  tmpMore : boolean;
begin
  result := nil;

  tmpLevel := 0;
  repeat
    tmpMore := false;
    inc(tmpLevel);
    for i := 0 to _Topics.Count - 1 do
    begin
      tmpTopic := TTopic(_Topics[i]);
      if tmpLevel = tmpTopic.ContentsLevel then
      begin
        if StrStartsWithIgnoringCase(tmpTopic.Title, SearchText) then
        begin
          result := tmpTopic;
          exit;
        end;
      end;
      if tmpLevel < tmpTopic.ContentsLevel then
      begin
        tmpMore := True;
      end;
    end;
  until NOT tmpMore;
end;

function THelpFile.FindTopicByTitleContains( const SearchText: string ): TTopic;
var
  i: longint;
  tmpTopic: TTopic;
  tmpLevel : integer;
  tmpMore : boolean;
begin
  result := nil;

  tmpLevel := 0;
  repeat
    tmpMore := false;
    inc(tmpLevel);
    for i := 0 to _Topics.Count - 1 do
    begin
      tmpTopic := TTopic(_Topics[i]);
      if tmpLevel = tmpTopic.ContentsLevel then
      begin
        if CaseInsensitivePos( SearchText, tmpTopic.Title) > 0 then
        begin
          result := tmpTopic;
          exit;
        end;
      end;
      if tmpLevel < tmpTopic.ContentsLevel then
      begin
        tmpMore := True;
      end;
    end;
  until NOT tmpMore;
end;

procedure THelpFile.FindResourceIDsForTopic( Topic: TTopic;
                                             ResourceIDs: TList );
var
  i: longint;
  pResourceIDs: UInt16ArrayPointer;
  pTopicIndices: UInt16ArrayPointer;
begin
  ResourceIDs.Clear;

  if _pHeader^.nres = 0 then
    // since nres is unsigned
    exit;

  if _pResourceData = nil then
    ReadFileBlock( _pResourceData,
                   _pHeader^.resstart,
                   _pHeader^.nres * sizeof( uint16 ) * 2 ); // list of IDs, list of topics

  pResourceIDs := _pResourceData;
  pTopicIndices := _pResourceData
                   + _pHeader^.nres * sizeof( uint16 );

  for i := 0 to _pHeader^.nres - 1 do
  begin
    if pTopicIndices^[ i ] = Topic.Index then
    begin
      // found
      ResourceIDs.Add( pointer( pResourceIDs^[ i ] ) );
    end;
  end;
end;

procedure THelpFile.ReadReferencedFilesTable;
var
  i: longint;
  p: pointer;
  pData: pointer;
  DatabaseName: string;
  pLength: pByte;
begin
  if _pExtendedHeader = nil then
    // no extended header -> no referenced files table
    exit;

  if _pExtendedHeader^.Numdatabase = 0 then
    exit;

  pData := nil; // please allocate...
  ReadFileBlock( pData,
                 _pExtendedHeader^.DatabaseOffset,
                 _pExtendedHeader^.DatabaseSize );

  p := pData;
  for i := 0 to _pExtendedHeader^.Numdatabase - 1 do
  begin
    pLength := p; // length byte, including itself
    SetString(DatabaseName, p+1, pLength^-1); // use length value minus the length byte to get the string length
    _ReferencedFiles.Add( DatabaseName );
    inc( p, pLength^ ); // skip to next entry using full length (including length byte)
  end;
  FreeMem( pData );
end;

procedure THelpFile.ReadFontTableData;
begin
  if _pExtendedHeader = nil then
    // no extended header -> no font table
    exit;

  if _pExtendedHeader^.NumFontEntry = 0 then
    exit;

  ReadFileBlock( _pFontTableData,
                 _pExtendedHeader^.FontTableOffset,
                 _pExtendedHeader^.NumFontEntry * sizeof( THelpFontSpec ) );
end;

procedure THelpFile.ParseFontTable;
var
  i: longint;
  p: pointer;
  pFontSpec: pTHelpFontSpec;
begin
  _FontTable.Clear;

  p := _pFontTableData;
  if p = nil then
    exit; // no data

  for i := 0 to _pExtendedHeader^.NumFontEntry - 1 do
  begin
    pFontSpec := p + i * sizeof( THelpFontSpec );
    _FontTable.Add( pFontSpec );
    if pFontSpec^.CodePage = 850 then
      Encoding := encCP850
    else if pFontSpec^.CodePage = 437 then
      Encoding := encCP437;
  end;
end;

procedure THelpFile.GetImages( ImageOffsets: TList; Images: TfpgImageList );
var
  ListIndex: longint;
  ImageOffset: longint;
  Bitmap: THelpBitmap;
begin
  Images.Clear;

  for ListIndex := 0 to ImageOffsets.Count - 1 do
  begin
    ImageOffset := longint( ImageOffsets[ ListIndex ] );
    try
      Bitmap := THelpBitmap.CreateFromHelpFile( _Handle,
                                               _pHeader^.imgstart
                                               + ImageOffset );
    except
      on e: EHelpBitmapException do
{        raise EHelpFileException.Create( 'Error loading help bitmap at'
                                         + IntToStr( ImageOffset )
                                         + ': '
                                         + e.Message );}
      begin
        Bitmap := THelpBitmap(fpgImages.GetImage('stdimg.dlg.critical'));
      end;
    end;

    Images.AddImage(Bitmap);
  end;
end;

function THelpFile.GetImage( ImageOffset: longint ): THelpBitmap;
begin
  try
    Result := THelpBitmap.CreateFromHelpFile( _Handle,
                                              _pHeader^.imgstart
                                              + ImageOffset );
  except
    on e: EHelpBitmapException do
    begin
      result := nil;
        raise EHelpFileException.Create( 'Error loading help bitmap at'
                                       + IntToStr( ImageOffset )
                                       + ': '
                                       + e.Message );
    end;
  end;
end;

function THelpFile.GetTopic( AIndex: longint ): TTopic;
begin
  if    ( AIndex < 0 )
     or ( AIndex > _Topics.Count - 1 ) then
    Result := nil
  else
    Result := TTopic(_Topics[ AIndex ]);
end;

function THelpFile.GetTopicCount: longint;
begin
  Result := _Topics.Count;
end;

function THelpFile.IndexOfTopic( Topic: TTopic ): longint;
begin
  Result := _Topics.IndexOf( Topic );
end;

function THelpFile.GetDictionaryCount: longint;
begin
  Result := _Dictionary.Count;
end;

function THelpFile.GetDictionaryWord( AIndex: longint ): string;
begin
  Result := _Dictionary[ AIndex ];
end;


// Looks for fonts that should be substitued to the
// users selected fixed font
// doesn't make a lot of sense for this to be here...
procedure THelpFile.SetupFontSubstitutes( Substitutions: string );
var
  Item: string;
  FontName: string;
  SpacePos: longint;
  W: longint;
  H: longint;
  i: longint;
  pFontSpec: pTHelpFontSpec;
  tmpSubstitutionItems : TStrings;
  tmpCounter : integer;
  tmpDimensionParts : TStrings;
  s: string;
  PointSize: word;
  cp: integer;
begin
  ParseFontTable; // (re)load table from raw data

  tmpSubstitutionItems := TStringList.Create;
  StrExtractStrings(tmpSubstitutionItems, Substitutions, [';'], #0);

  for tmpCounter := 0 to tmpSubstitutionItems.Count - 1 do
  begin
    Item := tmpSubstitutionItems[tmpCounter];
    try
      if Item <> '' then
      begin
        // Look for space in xxxx WxH
        SpacePos := LastDelimiter(' ', Item);
        if SpacePos > 0 then
        begin
          // fontname comes before
          FontName := StrLeft( Item, SpacePos - 1 );
          Delete( Item, 1, SpacePos );

          // width and height after, with an X between
          tmpDimensionParts := TStringList.Create;
          StrExtractStrings(tmpDimensionParts, Item, ['x'], #0);
          W := StrToInt(tmpDimensionParts[0]);
          H := StrToInt(tmpDimensionParts[1]);
          tmpDimensionParts.Destroy;
          if ( W > 0 ) and ( H > 0 ) then
          begin
            // Now look through the font table for matches
            for i := 0 to _FontTable.Count - 1 do
            begin
              pFontSpec := _FontTable[ i ];
              cp := pFontSpec^.Codepage;
              s := StrNPas( pFontSpec^.FaceName, sizeof( pFontSpec^.FaceName ) );
              if s = FontName then
              begin
                // same face name...
                // this formula seems to give a simulated pointsize compared to
                // what the original VIEW program intended.
                PointSize := (pFontSpec^.Height * 2) div 3;
                if ( H = PointSize ) then
                begin
                  // match
                  pFontSpec^.Codepage := High(word); // font substitute marker added
//                  _FontTable[ i ] := SubstituteFixedFont;
                end;
              end;
            end;
          end;
        end;
      end;
    except
    end;
  end;

  tmpSubstitutionItems.Free;
end;


// -------------------------------------------------------------
// Get the title only from specific help file (if possible)

function GetHelpFileTitle( const Filename: string ): string;
var
  Header: THelpFileHeader;
  fstream: TFileStream;
  Ext: string;
begin
  Ext := fpgExtractFileExt( Filename );
  Result := '';

  if    SameText( Ext, '.inf' )
     or SameText( Ext, '.hlp' ) then
  begin
    try
      try
        fstream := TFileStream.Create(Filename, fmOpenRead);
        fstream.Position := 0;
        FillChar( Header, sizeof( Header ), 0 );
        fstream.Read(Header, SizeOf(Header));
        if Header.ID = INF_HEADER_ID then
          Result := StrPas(Header.title);
      except
        // silently ignore errors - it's not to critical at this point.
      end;
    finally
      fstream.Free;
    end;
  end;
end;


end.

