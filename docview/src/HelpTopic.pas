Unit HelpTopic;

{$mode objfpc}{$H+}

Interface

// This is it - the monster which decodes IPF data.
// It's created with a reference to the contents data defining it.
// It gets relevant pointers out of that. When GetText is called
// it decodes the data and spits out formatted text to suit
// RichTextView.

uses
  Classes,
  HelpWindowDimensions,
  IPFFileFormatUnit;

const
  DefaultGroupIndex = 0;

  RTF_NewLine = #10;

var
  { TODO -oGraeme -cPointers : I don't like this - double check alternatives later }
  // placeholder for font table entry, indiciating user fixed font should be substituted
  SubstituteFixedFont: pointer = Pointer(1);

type
  THelpLink = class(TObject)
  public
    HelpFile: TObject;     // file this link is within
    // Even though it doesn't do anything,
    // we have to have a constructor to allow
    // virtual constructors to work
    constructor Create; virtual;
  end;


  THelpTopicSlot = class(TObject)
  public
    pData: pUInt8;                            // Pointer to actual Slot structure in INF file.
    Size: longint;                            // Number of bytes in the text for this Slot (slotheader.ntext)
    pLocalDictionary: UInt16ArrayPointer;     // Pointer to Slot's local dictionary
    LocalDictSize: uint8;                     // Number of entries in the local dictionary
    destructor Destroy; override;
  end;


  THelpLinkClass = class of THelpLink;


  TFootnoteHelpLink = class(THelpLink)
  public
    TopicIndex: longint;
    Title: string; // from text within link
  end;


  TWindowedHelpLink = class(THelpLink)
  public
    GroupIndex: longint;   // DefaultGroupIndex if not specified.
                           // Note: Overrides contents group index of topic
    Automatic: boolean;    // link should be automatically followed on topic display
    Split: boolean;        // link should open the window within the parent
    ViewPort: boolean;     // link should always open a new window
    Dependent: boolean;    // window opened by link should be closed
                           // when current topic is closed
    Rect: THelpWindowRect; // Display window with this rectangle.
                           // Note: overrides contents rect
    constructor Create; override;
    destructor Destroy; override;
  end;


  TInternalHelpLink = class(TWindowedHelpLink)
  public
    TopicIndex: longint;
  end;


  THelpLinkByResourceID = class(TWindowedHelpLink)
  public
    ResourceID: longint;
  end;


  SlotArray = array[0..0] of THelpTopicSlot;
  pSlotArray = ^SlotArray;


  TFontState = (fsNormal, fsFixed, fsCustom);
  TIPFTextAlignment = (itaLeft, itaRight, itaCenter, itaCenterOnePara);


  TParseState = record
    Alignment: TIPFTextAlignment;
    ForegroundColorTag: string;
    BackgroundColorTag: string;
    Spacing: boolean;
    FontState: TFontState;
    InCharGraphics: boolean;
    LinkIndex: longint;
    StartOfTextBlock: longint;
    TextBlock: string;
    FootnoteLink: TFootnoteHelpLink;
    StyleCode: longint;
  end;


  TTopic = class(TObject)
  protected
    _FileHandle: TFileStream;
    _pTOCEntry: pTTOCEntryStart;
    _pSlotOffsets: UInt32ArrayPointer;
    _Slots: TList;
    _pSlotNumbers: puint16;
    _NumSlots: longint;
    _Title: string;
    _GlobalDictionary: TStringList;
    _ShowInContents: boolean;
    _ContentsLevel: integer;
    _ContentsGroupIndex: longint;
    _FontTable: TList;
    _ReferencedFiles: TStrings;
    procedure SetTitle( const NewValue: string );
    function GetTitle: string;

    // Returns the tag texts for the given bitmap ref
    function GetImageText( CurrentAlignment: TIPFTextAlignment;
                           BitmapOffset: longint;
                           BitmapFlags: longint;
                           ImageOffsets: TList ): string;

    Procedure ProcessLinkedImage( Var State: TParseState;
                                  Var pData: pByte;
                                  Var OutputString: string;
                                  ImageOffsets: TList );
    procedure TranslateIPFEscapeCode( Var State: TParseState;
                                      Var pData: pUInt8;
                                      var AText: String;
                                      Var WordsOnLine: longint;
                                      ImageOffsets: TList );

    function CreateLink( Var LinkIndex: longint;
                         Var Link: THelpLink;
                         LinkClass: THelpLinkClass ): boolean;

    procedure EnsureSlotsLoaded;

    // returns true if the escape code at pData results in whitespace.
    function IPFEscapeCodeSpace( Var State: TParseState; Var pData: pUInt8 ): boolean;

    function GetNextIPFTextItem( Var SlotIndex: longint;
                             Var pData: pUInt8;
                             Var State: TParseState ): longint;

    function CheckForSequence( WordSequences: TList;
                               SlotIndex: longint;
                               pData: pUint8;
                               State: TParseState;
                               GlobalDictIndex: longint
                             ): longint;

  public
    HelpFile: TObject;
    Index: longint;
    SearchRelevance: longint;
    Links: TList; // only valid after GetText
    constructor Create( var FileHandle: TFileStream;
                        pSlotOffsets: UInt32ArrayPointer;
                        Dictionary: TStringList;
                        var pTOCEntry: pTTOCEntryStart;
                        FontTable: TList;
                        ReferencedFiles: TStrings );
    destructor Destroy; override;
    property Title: string read GetTitle write SetTitle;
    procedure SetTitleFromMem( const p: pointer; const Len: byte );
    // Main function for retrieving text for topic.
    // HighlightSequences: list of sequences to highlight
    // if nil then ignored.
    // ShowCodes: indicates debugging: hex output of escape
    //   codes will be included
    // ShowWordSeparators: | will be included after each dictionary
    //   word inserted
    // Text: The output is written to here. IS NOT CLEARED FIRST.
    // ImageOffsets: For each image that occurs in the text,
    //   the help file offset will be written to this list.
    // HighlightMatches: if not nil, and HighlightSequences is not nil,
    // will return offsets to each highlight match
    procedure GetText( HighlightSequences: TList;
                       ShowCodes: boolean;
                       ShowWordSeparators: boolean;
                       var Text: String;
                       ImageOffsets: TList;
                       HighlightMatches: TList );
    // if StopAtFirstOccurrence true, returns 0 or 1
    // if false, returns count of occurrences of word
    function SearchForWord( DictIndex: integer;
                            StopAtFirstOccurrence: boolean ): longint;
    // searches for sequences out of those listed in WordSequence
    // Each element of WordSequence contains a pointer to an array
    // of flags for each dictionary word, indicating whether that word
    // is to be a possible match.
    function SearchForWordSequences( WordSequence: TList; StopAtFirstOccurrence: boolean ): longint;
    procedure GetContentsWindowRect( ContentsRect: THelpWindowRect );
    // search for binary data including codes
    function SearchForData( Data: pbyte; DataLen: integer ): boolean;
    procedure SaveIPFEscapeCode( Var State: TParseState;
                                 Var pData: pUInt8;
                                 Var F: TextFile;
                                 ImageOffsets: TList );
    procedure SaveToIPF( Var f: TextFile; ImageOffsets: TList );
    property ShowInContents: boolean read _ShowInContents;
    property ContentsLevel: integer read _ContentsLevel;
    property ContentsGroupIndex: longint read _ContentsGroupIndex;
    function CountWord( DictIndex: integer ): longint;
    function ContainsWord( DictIndex: integer ): boolean;
  end;


// Compares two topics for purposes of sorting by
// search match relevance
function TopicRelevanceCompare( Item1, Item2: pointer ): longint;

// Compares two topics for purposes of sorting by title
function TopicTitleCompare( Item1, Item2: pointer ): longint;


implementation


uses
  SysUtils
  ,dvConstants
  ,nvUtilities
  ,ACLStringUtility
  ,SettingsUnit
  ;

const
  IPFColors: array[ 0..15 ] of string =
  (
    //rrggbb
    '', // default
    '#0000ff', // blue
    '#ff0000', // red
    '#ff00ff', // pink (purple)
    '#00ff00', // green
    '#00ffff', // cyan
    '#ffff00', // yellow
    '#808000', // neutral = brown
    '#404040', // dark gray
    '#000080', // dark blue
    '#800000', // dark red
    '#800080', // dark pink (purple)
    '#008000', // dark green
    '#008080', // dark cyan
    '#000000', // black
    '#c0c0c0'  // pale gray
  );

  // for ecHighlight1
  IPFHighlight1Tags : array [ 0..6 ] of string =
  (
    '</i></b></u></color>',  // normal
    '<i>',           // hp1 italitc
    '<b>',           // hp2 bold
    '<b><i>',        // hp3 bold italic
    '<u>',           // hp5 underline
    '<u><i>',        // hp6 underline italic
    '<u><b>'         // hp7 underline bold
  );

  // for ecHighlight2
  IPFHighlight2Tags : array [ 0..3 ] of string =
  (
    '</i></b></u></color>',  // normal
    '<color blue>',  // hp4 blue
    '<color red>',   // hp8 red
    '<color purple>' // hp9 purple
  );

  BlankString: string = '';

var
  DefaultTitle: string;


function GetBeginLink( LinkIndex: longint ): string;
begin
  Result := '<link ' + IntToStr( LinkIndex ) + '>'
end;

function GetEndLinkTags( const State: TParseState ): string;
begin
  Result := '</link>' + State.ForegroundColorTag;
end;

// Even though it doesn't do anything,
// we have to have a constructor to allow
// virtual constructors to work
constructor THelpLink.Create;
begin
  // do nothing
end;

constructor TWindowedHelpLink.Create;
begin
  GroupIndex := DefaultGroupIndex;
  Automatic := false;
  ViewPort := false;
  Dependent := false;
  Rect := THelpWindowRect.Create;
end;

destructor TWindowedHelpLink.Destroy;
begin
  Rect.Destroy;
end;

destructor THelpTopicSlot.Destroy;
begin
  { TODO -ograeme -ccleanup memory : Double check this }
  FreeMem(pData);//  DeallocateMemory( pData );
  FreeMem(pLocalDictionary); // DeallocateMemory( pLocalDictionary );
end;

constructor TTopic.Create( var FileHandle: TFileStream;
                           pSlotOffsets: UInt32ArrayPointer;
                           Dictionary: TStringList;
                           var pTOCEntry: pTTOCEntryStart;
                           FontTable: TList;
                           ReferencedFiles: TStrings );
var
  pExtendedInfo: pExtendedTOCEntry;
  titleLen: integer;
  XY: THelpXYPair;
  p: pbyte;
  Flags: byte;
begin
  _FileHandle := FileHandle;
  _pSlotOffsets := pSlotOffsets;

  _Title := '';
  _GlobalDictionary := Dictionary;
  _ContentsGroupIndex := 0;

  _pTOCEntry := pTOCEntry;
  _NumSlots := pTOCEntry^.numslots;

  Flags := _pTOCEntry^.flags;
  p := pByte( _pTOCEntry ) + sizeof( TTOCEntryStart );

  if ( Flags and TOCEntryExtended ) = TOCEntryExtended then
  begin
    pExtendedInfo := pExtendedTOCEntry( p );
    inc( p, sizeof( TExtendedTOCEntry ) );

    if ( pExtendedInfo^.w1 and 1 ) > 0 then
      // skip position
      inc( p, sizeof( XY ) );

    if ( pExtendedInfo^.w1 and 2 ) > 0 then
      // skip size
      inc( p, sizeof( XY ) );

    if ( pExtendedInfo^.w1 and 8 ) > 0 then
      // skip window controls
      inc( p, sizeof(word) );    // increment by 2

    if ( pExtendedInfo^.w1 and $40 ) > 0 then
      // skip something else, unknown... style? 2 bytes
      inc( p, sizeof(word) );    // increment by 2

    if ( pExtendedInfo^.w2 and 4 ) > 0 then
    begin
      _ContentsGroupIndex := pUInt16(p)^;
      // read group
      inc( p, sizeof( uint16 ) );
    end;
  end;

  // skip slot numbers for now.
  _pSlotNumbers := pUInt16(p);
  inc( p, _NumSlots * sizeof( uint16 ) );

  // Calculate the remainder of the tocentry length - that is the bytes used for TOC topic (title) text
  titleLen := _pTOCEntry^.length - ( longword( p ) - longword( _pTOCEntry ) );

  // Read title
  if TitleLen > 0 then
    SetTitleFromMem( p, TitleLen )
  else
    Title := DefaultTitle;

  _ContentsLevel := ( Flags and TOCEntryLevelMask );
  _ShowInContents := Flags and TOCEntryHidden = 0;
  if _ContentsLevel = 0 then
    _ShowInContents := false; // hmmm....

  _FontTable := FontTable;
  _ReferencedFiles := ReferencedFiles;
end;

destructor TTopic.Destroy;
begin
  LogEvent(LogObjConstDest, 'TTopic.Destroy');
  DestroyListAndObjects( Links );
  DestroyListAndObjects( _Slots );
  inherited Destroy;
end;

procedure TTopic.SetTitle( const NewValue: string );
begin
  _Title := NewValue;
end;

procedure TTopic.SetTitleFromMem( const p: pointer; const Len: byte );
begin
  //FreePString( _Title );
  //GetMem( _Title, Len + 1 );
  //_Title^[ 0 ] := char( Len );
  //MemCopy( p, _Title + 1, Len );
  SetString(_Title, p, Len);
end;

function TTopic.GetTitle: string;
begin
  Result := _Title;
end;

// Replace < and > characters with doubles << and >>
// for compatibility with richtextview.
// This works in place, assuming that instances of > or < are
// actually rare. In practice, IPF normally would insert these
// two characters as distinct words, but I don't want to assume that.
procedure SubstituteAngleBrackets( Var s: string );
var
  i: integer;
begin
  i := 1;
  while i <= Length( S ) do
  begin
    case S[ i ] of
      '<':
      begin
        Insert( '<', s, i );
        inc( i );
      end;

      '>':
      begin
        Insert( '>', s, i );
        inc( i );
      end;
    end;
    inc( i );
  end;
end;

function TTopic.GetImageText( CurrentAlignment: TIPFTextAlignment;
                              BitmapOffset: longint;
                              BitmapFlags: longint;
                              ImageOffsets: TList ): string;
var
  BitmapIndex: longint;
  OriginalAlignTag: string;
  ImageTag: string;
  AlignTag: string;
begin
  BitmapIndex := ImageOffsets.IndexOf( pointer( BitmapOffset ) );
  if BitmapIndex = -1 then
    BitmapIndex := ImageOffsets.Add( pointer( BitmapOffset ) );

  ImageTag := '<image '
              + IntToStr( BitmapIndex )
              + '>';

  if ( BitmapFlags and $08 ) > 0 then
  begin
    // stretch to fit - not implemented
  end;

  // aligned
  case CurrentAlignment of
    itaLeft:
      OriginalAlignTag := '<align left>';
    itaRight:
      OriginalAlignTag := '<align right>';
    itaCenter,
    itaCenterOnePara:
      OriginalAlignTag := '<align center>';
  end;

  case BitmapFlags and 7 of
    0, // curious - should not occur? does in dbexpert.hlp
    1: // left
      AlignTag := '<align left>';
    2: // right
      AlignTag := '<align right>';
    4,5: // centre (4 is official, 5 seems to occur too)
      AlignTag := '<align center>';
  end;

  Result := AlignTag
            + ImageTag
            + OriginalAlignTag;

  if ( BitmapFlags and $10 ) = 0 then
  begin
    // NOT runin, new lines before and after
    Result := RTF_NewLine + Result + RTF_NewLine;
  end;

end;

Procedure SaveImageText( BitmapOffset: longint;
                         BitmapFlags: longint;
                         Var F: TextFile;
                         ImageOffsets: TList );
var
  ImageIndex: longint;
begin
  ImageIndex := ImageOffsets.IndexOf( pointer( BitmapOffset ) );
  if ImageIndex = -1 then
    ImageIndex := ImageOffsets.Add( pointer( BitmapOffset ) );

  Write( F, ':artwork name=' );
  Write( F, StrInSingleQuotes('img' + IntToStr(ImageIndex) + '.bmp') );

  case BitmapFlags and 7 of
    2: // right
      Write( F, ' align=right' );
    4,5: // centre (4 is official, 5 seems to occur too)
      Write( F, ' align=center' );
  end;

  if ( BitmapFlags and $10 ) > 0 then
  begin
    // runin
    Write( F, ' runin' );
  end;

  // fit ...
  Write( F, '.' );
end;

Procedure TTopic.ProcessLinkedImage( Var State: TParseState;
                                     Var pData: pByte;
                                     Var OutputString: string;
                                     ImageOffsets: TList );
var
  EscapeLen: uint8;
  EscapeCode: uint8;
  SubEscapeCode: uint8;
  BitmapOffset: longword;
  BitmapFlags: uint8;
  Link: THelpLink;//TInternalHelpLink;
  LinkTopicIndex: integer;
begin
  LinkTopicIndex := -1;
  while true do
  begin
    EscapeLen := pData^;
    SubEscapeCode := ( pData + 2 )^;
    case SubEscapeCode of
      HPART_DEFINE:
      begin
        BitmapFlags := ( pData + 3 )^;
        BitmapOffset := pUInt32( pData + 4 )^;
      end;

      HPART_HDREF: // define whole bitmap topic link?
      begin
        LinkTopicIndex := pUInt16( pData + 3 )^;
      end;
    end;
    inc( pData, EscapeLen );

    // Now pData points at next code or item
    if pData^ <> IPF_ESC then
      // not an escape code, done
      break;
    EscapeCode := (pData + 2) ^;
    if EscapeCode <> ecLinkedImage then
      // not a hyperlink code, done
      break;
    // another linked image code is coming up.
    SubEscapeCode := ( pData + 3 )^;
    if SubEscapeCode = HPART_DEFINE then
      // started another linked image.
      break;
    inc( pData ); // move pointer to escape code len.
  end;

  OutputString := GetImageText( State.Alignment,
                                BitmapOffset,
                                BitmapFlags,
                                ImageOffsets );

  // Don't make it a link if we didn't find a
  // overall link code, i.e. degrade gracefully.
  if LinkTopicIndex > -1 then
  begin    
    if CreateLink( State.LinkIndex, Link, TInternalHelpLink ) then
    begin
      TInternalHelpLink(Link).TopicIndex := LinkTopicIndex;
    end;

    OutputString := GetBeginLink( State.LinkIndex )
                    + OutputString
                    + GetEndLinkTags( State );

    inc( State.LinkIndex );
  end;

end;

Procedure SaveLinkedImage( Var pData: pByte;
                           Var F: TextFile;
                           ImageOffsets: TList );
var
  EscapeLen: uint8;
  EscapeCode: uint8;
  SubEscapeCode: uint8;
  BitmapOffset: longword;
  BitmapFlags: uint8;
  LinkTopicIndex: integer;
begin
  LinkTopicIndex := -1;
  while true do
  begin
    EscapeLen := pData^;
    SubEscapeCode := ( pData + 2 )^;
    case SubEscapeCode of
      HPART_DEFINE:
      begin
        BitmapFlags := ( pData + 3 )^;
        BitmapOffset := pUInt32( pData + 4 )^;
      end;

      HPART_HDREF: // define whole bitmap topic link?
      begin
        LinkTopicIndex := pUInt16( pData + 3 )^;
      end;
    end;
    inc( pData, EscapeLen );

    // Now pData points at next code or item
    if pData^ <> IPF_ESC then
      // not an escape code, done
      break;
    EscapeCode := (pData + 2) ^;
    if EscapeCode <> ecLinkedImage then
      // not a hyperlink code, done
      break;
    // another linked image code is coming up.
    SubEscapeCode := ( pData + 3 )^;
    if SubEscapeCode = HPART_DEFINE then
      // started another linked image.
      break;
    inc( pData ); // move pointer to escape code len.
  end;

  SaveImageText( BitmapOffset,
                 BitmapFlags,
                 F,
                 ImageOffsets );

  // Don't make it a link if we didn't find a
  // overall link code, i.e. degrade gracefully.
  if LinkTopicIndex > -1 then
  begin
    WriteLn( F, '' );
    WriteLn( F, ':artlink.' );
    Write( F, ':link reftype=hd' );
    Write( F, ' refid=' + IntToStr( LinkTopicIndex ) );
    WriteLn( F, '.' );
    WriteLn( F, ':eartlink.' );
  end;

end;

Procedure GetExtraLinkData( Link: TWindowedHelpLink;
                            pData: pUInt8 );
var
  LinkFlags1: uint8;
  LinkFlags2: uint8;
  LinkDataIndex: longint;
  pLinkXY: pHelpXYPair;
  pLinkData: pUInt8;
begin
  LinkFlags1 := ( pData + 0 ) ^;
  LinkFlags2 := ( pData + 1 ) ^;

  pLinkData := pData + 2;

  if ( LinkFlags1 and 1 ) > 0 then
  begin
    // position specified
    pLinkXY := pHelpXYPair( pLinkData );
    ReadHelpPosition( pLinkXY^, Link.Rect );
    inc( pLinkData, sizeof( THelpXYPair ) );
  end;

  if ( LinkFlags1 and 2 ) > 0 then
  begin
    // size specified
    pLinkXY := pHelpXYPair( pLinkData );
    ReadHelpSize( pLinkXY^, Link.Rect );
    inc( pLinkData, sizeof( THelpXYPair ) );
  end;

  if ( LinkFlags1 and 8 ) > 0 then
  begin
    // window controls specified - skip
    inc( pLinkData, 2 );
  end;

  if ( LinkFlags2 and 4 ) > 0 then
  begin
    // group specified
    Link.GroupIndex := pUInt16( pLinkData )^;
    inc( LinkDataIndex, sizeof( uint16 ) );
  end;

  if ( LinkFlags1 and 64 ) > 0 then
  begin
    Link.Automatic := true;
  end;

  if ( LinkFlags1 and 4 ) > 0 then
    Link.ViewPort := true;

  if ( LinkFlags2 and 2 ) > 0 then
    Link.Dependent := true;

  if ( LinkFlags1 and 128 ) > 0 then
    Link.Split := true;

  // cant be bothered with the others.
end;

// If the given link has already been decoded
// ie. the topic has been displayed before,
// then return the already decoded link & return false
// Otherwise, create a new link object & return true
function TTopic.CreateLink( Var LinkIndex: longint;
                            Var Link: THelpLink;
                            LinkClass: THelpLinkClass ): boolean;
begin
  if LinkIndex >= Links.Count then
  begin
    Link := LinkClass.Create;
    Link.HelpFile := HelpFile;
    Links.Add( Link );
    Result := true;
  end
  else
  begin
    Link := THelpLink(Links[ LinkIndex ]);
    Result := false;
  end;
end;

const
  // size of the original View's default font
  AverageViewCharWidth = 8;

procedure GetMarginTag( const Margin: longint;
                        FontState: TFontState;
                        Var MarginString: string;
                        BreakIfPast: boolean );
begin
  MarginString := '<leftmargin ';
  if FontState <> fsCustom then
    // for standard fonts, scale margins to match font
    MarginString := MarginString + IntToStr( Margin )
  else
    // for custom fonts, since the IPF margins were always in
    // terms of the standard font size, set the margin to a width based on that.
    MarginString := MarginString + IntToStr( Margin * AverageViewCharWidth ) + ' pixels';

  if BreakIfPast then
    MarginString := MarginString + ' breakifpast';

  MarginString := MarginString + '>';
end;

// TODO
function FullDoubleQuote( const s: string ): string;
begin
  Result := StrDoubleQuote
            + StrEscapeAllCharsBy(s, [], CharDoubleQuote)
            + StrDoubleQuote;
end;

// End URL, if it has been started. Go back and insert the start tag,
// and add the end tag.
procedure CheckForAutoURL( var Text: string; var State: TParseState );
var
  T: string;
begin
  if State.StartOfTextBlock = -1 then
    // haven't got any text yet
    exit;

  TrimPunctuation( State.TextBlock );

  if not CheckAndEncodeURL( State.TextBlock ) then
  begin
    // not a URL we know
    State.TextBlock := '';
    exit;
  end;

  // It's a URL. Insert link at start of URL
  T := '<color blue><link ' + PARAM_LINK_URL + ' "';
  T := T + State.TextBlock;
  T := T + '">';
  Insert(T, Text, State.StartOfTextBlock);
  Text := Text + GetEndLinkTags(State);

  State.TextBlock := '';
  State.StartOfTextBlock := -1;
end;

procedure TTopic.TranslateIPFEscapeCode( Var State: TParseState;
                                         Var pData: pUInt8;
                                         var AText: String;
                                         Var WordsOnLine: longint;
                                         ImageOffsets: TList );
var
  EscapeLen: uint8;
  EscapeCode: uint8;

  Link: THelpLink;              //TInternalHelpLink;
  FootnoteLink: THelpLink;      //TFootnoteHelpLink;
  LinkByResourceID: THelpLink;  //THelpLinkByResourceID;

  Margin: integer;

  BitmapOffset: longword;
  BitmapFlags: uint8;

  ColorCode: uint8;
  StyleCode: uint8;

  FontIndex: uint8;
  pFontSpec: pTHelpFontSpec;

  FaceName: string;
  PointSize: longint;
  QuotedFaceName: string;

  ExternalLinkFileIndex: uint8;
  ExternalLinkTopicID: string;

  ProgramLink: string;
  ProgramPath: string;
  ProgramFilename: string;
  lURL: string;
  ProgramInfo : TSerializableStringList;
  tmpProgramLinkParts : TStrings;

  OutputString: string;
begin
  EscapeLen := pData^;
  EscapeCode := (pData + 1)^;
  OutputString := '';

  case EscapeCode of

    ecSetLeftMargin:
    begin
      CheckForAutoURL( AText, State );
      Margin := integer( ( pData + 2 )^ );
      GetMarginTag( Margin, State.FontState, OutputString, false );
    end;

    ecSetLeftMarginNewLine:
    begin
      CheckForAutoURL( AText, State );
      Margin := integer( ( pData + 2 )^ );
      GetMarginTag( Margin, State.FontState, OutputString, false );
      OutputString := OutputString  + RTF_NewLine;
    end;

    ecSetLeftMarginFit:
    begin
      CheckForAutoURL( AText, State );
      Margin := integer( ( pData + 2 )^ );
      GetMarginTag( Margin, State.FontState, OutputString, true );
      // note that this will cause following tex to be "tabbed" across to the
      // new margin position, if not yet there.
      // if we are already past this margin then a new line should be started.

    end;

    ecSetLeftMarginHere:
    begin
      OutputString := '<leftmargin here>';
    end;

    ecHighlight1:
    begin
      StyleCode := ( pData + 2 )^;
      if StyleCode <= High( IPFHighlight1Tags ) then
        OutputString := IPFHighlight1Tags[ StyleCode ];
      if StyleCode = 0 then
        State.ForegroundColorTag := '</color>';
    end;

    ecHighlight2:
    begin
      StyleCode := ( pData + 2 )^;
      if StyleCode <= High( IPFHighlight2Tags ) then
        OutputString := IPFHighlight2Tags[ StyleCode ];

      if StyleCode = 0 then
        State.ForegroundColorTag := '</color>'
      else
        State.ForegroundColorTag := OutputString; // only colours
    end;

    ecLinkStart:
    begin
      CheckForAutoURL( AText, State );
      if CreateLink( State.LinkIndex, Link, TInternalHelpLink ) then
      begin
        TInternalHelpLink(Link).TopicIndex := pUInt16( pData + 2 )^;

        if EscapeLen >= 6 then
        begin
          GetExtraLinkData( TInternalHelpLink(Link), pData + 4 );
        end;
      end;

      // If it's not an automatic link
      // then put code in to show it.
      if not TInternalHelpLink(Link).Automatic then
      begin
        OutputString := '<color blue>'
                        + GetBeginLink( State.LinkIndex );
      end;

      inc( State.LinkIndex );
    end;

    ecFootnoteLinkStart:
    begin
      CheckForAutoURL( AText, State );
      if CreateLink( State.LinkIndex, FootnoteLink, TFootnoteHelpLink ) then
      begin
        TFootnoteHelpLink(FootnoteLink).TopicIndex := pUInt16( pData + 2 )^;
        State.FootnoteLink := TFootnoteHelpLink(FootnoteLink);
      end;

      OutputString := '<color blue>' + GetBeginLink( State.LinkIndex );

      inc( State.LinkIndex );
    end;

    ecStartLinkByResourceID:
    begin
      CheckForAutoURL( AText, State );
      if CreateLink( State.LinkIndex, LinkByResourceID, THelpLinkByResourceID ) then
      begin
        THelpLinkByResourceID(LinkByResourceID).ResourceID := pUInt16( pData + 2 )^;

        if EscapeLen >= 6 then
        begin
          GetExtraLinkData( THelpLinkByResourceID(LinkByResourceID), pData + 4 );
        end;
      end;

      OutputString := '<color blue>' + GetBeginLink( State.LinkIndex );

      inc( State.LinkIndex );
    end;

    ecExternalLink:
    begin
      CheckForAutoURL( AText, State );
      // :link reftype=hd refid=... database=<filename>
      ExternalLinkFileIndex := ( pData + 2 )^;
      ExternalLinkTopicID := StrNPas( pchar(pData + 4), (pData + 3)^ );
      OutputString := '<color blue><link ' + PARAM_LINK_EXTERNAL + ' '
                      + IntToStr( ExternalLinkFileIndex )
                      + ' '
                      + ExternalLinkTopicID
                      + '>'

    end;

    ecProgramLink:
    begin
      CheckForAutoURL( AText, State );
      ProgramLink := StrNPas( pchar(pData + 3), EscapeLen-3 );

      tmpProgramLinkParts := TStringList.Create;
      StrExtractStrings(tmpProgramLinkParts, ProgramLink, [' '], #0);
      ProgramPath := tmpProgramLinkParts[0];
      lURL := tmpProgramLinkParts[1];
      tmpProgramLinkParts.Destroy;

      ProgramFilename := ExtractFilename( ProgramPath );

      if    StrStartsWithIgnoringCase(ProgramFilename, PRGM_EXPLORER)
         or StrStartsWithIgnoringCase(ProgramFilename, PRGM_NETSCAPE)
         or StrStartsWithIgnoringCase(ProgramFilename, PRGM_MOZILLA)
         or StrStartsWithIgnoringCase(ProgramFilename, PRGM_FIREFOX)
         then
      begin
        OutputString := '<color blue><link ' + PARAM_LINK_URL + ' '
                        + FullDoubleQuote( lURL )
                        + '>';
      end
      else
      begin
        ProgramInfo := TSerializableStringList.create;
        ProgramInfo.add(ProgramPath);
        ProgramInfo.add(ProgramLink);
        OutputString := '<color blue><link ' + PARAM_LINK_PROGRAM + ' '
                        + ProgramInfo.getSerializedString
                        + '>';
        ProgramInfo.destroy;
      end;
    end;

    ecLinkEnd:
    begin
      OutputString := GetEndLinkTags( State );
      if State.FootnoteLink <> nil then
        State.FootnoteLink := nil;
    end;

    ecStartCharGraphics:
    begin
      State.FontState := fsFixed;
      State.InCharGraphics := true;
      OutputString := RTF_NewLine + RTF_NewLine + '<tt><nowrap>';
      State.Spacing := false;
      WordsOnLine := 0;
    end;

    ecEndCharGraphics:
    begin
      State.FontState := fsNormal;
      State.InCharGraphics := false;
      OutputString := '</nowrap></tt>';// + RTF_NewLine;
      State.Spacing := true;
    end;

    ecImage:
    begin
      CheckForAutoURL( AText, State );
      BitmapFlags := ( pData + 2 )^;
      BitmapOffset := pUInt32( pData + 3 )^;

      OutputString := GetImageText( State.Alignment,
                                    BitmapOffset,
                                    BitmapFlags,
                                    ImageOffsets );

      if State.Spacing
         AND (OutputString[Length(OutputString)] <> RTF_NewLine) // no space after a line break
      then
        OutputString := OutputString + ' ';
    end;

    ecLinkedImage:
    begin
      CheckForAutoURL( AText, State );
      ProcessLinkedImage( State,
                          pData,
                          OutputString,
                          ImageOffsets );
      if State.Spacing then
        OutputString := OutputString + ' ';

      // Note! Early exit, since the procedure
      // will update pData.
      AText := AText + OutputString;
      exit;
    end;

    ecStartLines:
    begin
      CheckForAutoURL( AText, State );
      // aligned text
      case ( pData + 2 )^ of
        0, // just in case - to match image alignment oddities
        1:
        begin
          OutputString := RTF_NewLine + '<align left>';
          State.Alignment := itaLeft;
        end;

        2:
        begin
          OutputString := RTF_NewLine + '<align right>';
          State.Alignment := itaRight;
        end;

        4:
        begin
          OutputString := RTF_NewLine + '<align center>';
          State.Alignment := itaCenter;
        end;
      end;
      OutputString := OutputString + '<nowrap>';
      WordsOnLine := 0;
    end;

    ecEndLines:
    begin
      CheckForAutoURL( AText, State );
      // supposed to turn word wrap on, default font
      OutputString := {'<align left>}'</nowrap>'; // I guess...
      State.Alignment := itaLeft;
    end;

    ecForegroundColor:
    begin
      ColorCode := ( pData + 2 )^;
      if ColorCode = 0 then
        State.ForegroundColorTag := '</color>'
      else if ColorCode <= High( IPFColors ) then
        State.ForegroundColorTag := '<color ' + IPFColors[ ColorCode ] + '>';
      OutputString := State.ForegroundColorTag;
    end;

    ecBackgroundColor:
    begin
      ColorCode := ( pData + 2 )^;
      if ColorCode = 0 then
        State.BackgroundColorTag := '</backcolor>'
      else if ColorCode <= High( IPFColors ) then
        State.BackgroundColorTag := '<backcolor ' + IPFColors[ ColorCode ] + '>';
      OutputString := State.BackgroundColorTag;
    end;

    ecFontChange:
    begin
      FontIndex := ( pData + 2 )^;
      if FontIndex = 0 then
      begin
        // back to default font
        OutputString := '</font>';
        State.FontState := fsNormal;
      end
      else if FontIndex < _FontTable.Count then
      begin
        // valid font index
        pFontSpec := _FontTable[ FontIndex ];

//        if pFontSpec = SubstituteFixedFont then
        if pFontSpec^.Codepage = High(word) then // Substitute Fixed Font detected
        begin
          OutputString := '<tt>';
          State.FontState := fsFixed;
        end
        else
        begin
//          pFontSpec := _FontTable[ FontIndex ];
          FaceName := StrNPas( pFontSpec^.FaceName, sizeof(pFontSpec^.FaceName) );
          // arbitrarily and capriciously use specified height * 2/3
          // as the point size - seems to correspond to what original
          // view wanted...  note this doesn't necessarily scale
          // correctly, since default font could be different. whatever.
          PointSize := (pFontSpec^.Height * 2) div 3;

          if PointSize < 8 then
            PointSize := 8;
          // quote font name, escape double quotes with duplicates
          // e.g. Bob's "Big" Font would become
          //      "Bob's ""Big"" Font"
          QuotedFaceName := FullDoubleQuote( FaceName );
          OutputString := '<font '
                          + QuotedFaceName
                          + ' '
                          + IntToStr( PointSize )
                          + '>';
                          {
                           // for when (if ever) RTV allows setting font
                           // by precise dimensions
                          + '['
                          + IntToStr( pFontSpec ^. Width )
                          + 'x'
                          + IntToStr( pFontSpec ^. Height )
                          + ']';
                          }
          State.FontState := fsCustom;
        end;
      end;
    end
  end; // case escape code of...

  AText := AText + OutputString;
  inc( pData, EscapeLen );
end;

// returns true if the escape code results in whitespace
// also updates the bits of State that relate to spacing
// ie. .Spacing, and .InCharGraphics (which affects whether
// spacing is reset at paragraph ends etc)
function TTopic.IPFEscapeCodeSpace( Var State: TParseState;
                                    Var pData: pUInt8 ): boolean;
var
  EscapeLen: uint8;
  EscapeCode: uint8;

begin
  EscapeLen := pData^;
  EscapeCode := (pData + 1) ^;

  result := false; // for most
  case EscapeCode of
    ecSetLeftMargin,
    ecSetLeftMarginNewLine,
    ecSetLeftMarginFit:
      result := true;

    ecStartCharGraphics:
    begin
      result := true;
      State.InCharGraphics := true;
      State.Spacing := false;
    end;

    ecEndCharGraphics:
    begin
      result := true;
      State.InCharGraphics := false;
      State.Spacing := true;
    end;

    ecImage:
      result := State.Spacing;

    ecLinkedImage:
      result := State.Spacing;

    ecStartLines:
    begin
      result := true;
      State.Spacing := false;
    end;

    ecEndLines:
    begin
      result := true;
      // supposed to turn word wrap on, default font
      State.Spacing := true;
    end;
  end; // case escape code of...

  inc( pData, EscapeLen );
end;

procedure TTopic.EnsureSlotsLoaded;
var
  i: longint;
  pSlotNumber: puint16;
  SlotNumber: uint16;
  SlotHeader: TSlotHeader;
  Slot: THelpTopicSlot;
  bytes: integer;
  expected: integer;
begin
  if _Slots = nil then
  begin
    try
      _Slots := TList.Create;

      // Read slot data
      pSlotNumber := _pSlotNumbers;

      for i := 1 to _NumSlots do
      begin
        SlotNumber := pSlotNumber^;

        // Seek to start of slot
        try
          _FileHandle.Seek(_pSlotOffsets^[SlotNumber], soBeginning);
        except
          // not a valid offset
          raise EHelpFileException.Create( ErrorCorruptHelpFile );
        end;

        // Read header
        bytes := _FileHandle.Read(SlotHeader, SizeOf(TSlotHeader));
        if bytes <> SizeOf(TSlotHeader) then
          // couldn't read slot header
          raise EHelpFileException.Create( 'Failed to load Topic Slots.' );

        // Create slot object
        Slot := THelpTopicSlot.Create;

        Slot.LocalDictSize := SlotHeader.nLocalDict;
        Slot.Size := SlotHeader.ntext;

        // Allocate and read slot dictionary
        _FileHandle.Seek(SlotHeader.localDictPos, soBeginning);
        expected := uint32(Slot.LocalDictSize) * sizeof(uint16); // size we need
        if Slot.pLocalDictionary = nil then
          // allocate memory
          Slot.pLocalDictionary := GetMem(expected);
        bytes := _FileHandle.Read(Slot.pLocalDictionary^, expected);
        if bytes <> expected then
          raise EHelpFileException.Create('Failed to read complete slot dictionary');

        // Allocate and read slot data (text)
        _FileHandle.Seek(_pSlotOffsets^[SlotNumber] + sizeof(TSlotHeader), soBeginning);
        expected := Slot.Size; // size we need
        if Slot.pData = nil then
          // allocate memory
          Slot.pData := GetMem(expected);
        bytes := _FileHandle.Read(Slot.pData^, expected);
        if bytes <> expected then
          raise EHelpFileException.Create('Failed to read complete slot data (text)');

        _Slots.Add( Slot );
        inc( pByte(pSlotNumber), sizeof( UInt16 ) );
      end;
    except
      on E: EHelpFileException do
      begin
        DestroyListAndObjects( _Slots );
        raise;
      end;
    end;
  end;
end;

// returns a global dict index.
// or, -1 for a whitespace item.
// or, -2 for end of text.
function TTopic.GetNextIPFTextItem( Var SlotIndex: longint;
                                    Var pData: pUInt8;
                                    Var State: TParseState ): longint;
var
  Slot: THelpTopicSlot;
  pSlotEnd: pUInt8;

  LocalDictIndex: uint8;
begin
  while SlotIndex < _NumSlots do
  begin
    Slot := THelpTopicSlot(_Slots[ SlotIndex ]);
    pSlotEnd := Slot.pData + Slot.Size;

    while pData < pSlotEnd do
    begin
      LocalDictIndex := pData^;
      inc( pData );

      if LocalDictIndex < Slot.LocalDictSize then
      begin
        // Normal word lookup
        result := Slot.pLocalDictionary^[ LocalDictIndex ];
        exit;
      end;

      // special code
      case LocalDictIndex of
        IPF_END_PARA:
        begin
          result := -1;
          if not State.InCharGraphics then
            State.Spacing := true;
          exit;
        end;

        IPF_CENTER:
        begin
          result := -1;
          exit;
        end;

        IPF_INVERT_SPACING:
        begin
          State.Spacing := not State.Spacing;
        end;

        IPF_LINEBREAK:
        begin
          result := -1;
          if not State.InCharGraphics then
            State.Spacing := true;
          exit;
        end;

        IPF_SPACE:
        begin
          result := -1;
          exit;
        end;

        IPF_ESC:
        begin
          // escape sequence
          if IPFEscapeCodeSpace( State, pData ) then
            result := -1;
        end;
      end;
    end; // while in slot...
    inc( SlotIndex );
  end;
  Result := -2;
end;

// Checks to see if the given word (at pData)
// starts one of the given sequences, by looking forward
// If found, returns the length of the sequence.
function TTopic.CheckForSequence( WordSequences: TList;
                                  SlotIndex: longint;
                                  pData: pUint8;
                                  State: TParseState;
                                  GlobalDictIndex: longint
                                ): longint;
var
  WordSequence: TList;
  SequenceStepIndex: longint;
  pSequenceStepWords: Uint32ArrayPointer;

  SequenceIndex: longint;

  SlotIndexTemp: longint;
  pDataTemp: pUint8;
  StateTemp: TParseState;
//  s : string;
  DictIndex: longint;
begin
  result := 0; // if we don't find a match.

  for SequenceIndex := 0 to WordSequences.Count - 1 do
  begin
    WordSequence := TList(WordSequences[ SequenceIndex ]);
    pSequenceStepWords := WordSequence[ 0 ];

    if pSequenceStepWords^[ GlobalDictIndex ] > 0 then
    begin
      // matched first step in this sequence. Look ahead...

      SequenceStepIndex := 0;

      pDataTemp := pData;
      SlotIndexTemp := SlotIndex;
      StateTemp := State;
      while true do
      begin
        inc( SequenceStepIndex );
        if SequenceStepIndex = WordSequence.Count then
        begin
          // have a match for the sequence, insert start highlight
          Result := WordSequence.Count;
          break;
        end;

        // get words for next step in sequence
        pSequenceStepWords := WordSequence[ SequenceStepIndex ];

        DictIndex := GetNextIPFTextItem( SlotIndexTemp,
                                         pDataTemp,
                                         StateTemp );
        if DictIndex = -2 then
        begin
          // end of text - abort
          break;
        end;

        if DictIndex = -1 then
        begin
          // whitespace - abort
           // for multi-word phrase searching - count this and subsequent whitespace...
          break;
        end;

//        s := _GlobalDictionary[ DictIndex ]; // for debug only
        if not StrIsEmptyOrSpaces(_GlobalDictionary[ DictIndex ]) then
        begin
          if pSequenceStepWords^[ DictIndex ] = 0 then
          begin
            // word doesn't match - abort
            break;
          end;
        end;

      end; // while

    end;
    // else - doesn't match first step, do nothing
  end; // for sequenceindex ...
end;

// Main translation function. Turns the IPF data into
// a text string. Translates formatting codes into tags
// as for Rich Text Viewer.
procedure TTopic.GetText( HighlightSequences: TList;
                            // each element is a TList
                            //   containing a sequence of possible words
                            //     each element of each sequence
                            //     is an array of flags for the dictionary
                            //       indicating if the word is a allowed match at that step
                            // a match is any sequence that matches one or more words at each step.
                          ShowCodes: boolean;
                          ShowWordSeparators: boolean;
                          var Text: String;
                          ImageOffsets: TList;
                          HighlightMatches: TList );
var
  SlotIndex: integer;
  Slot: THelpTopicSlot;
  pData: pUInt8;
  pSlotEnd: pUInt8;

  GlobalDictIndex: uint32;

  WordsOnLine: longint;

  StringToAdd: string;
  LocalDictIndex: uint8;

  State: TParseState;

  EscapeLen: uint8;
  i: longint;

  SequenceStepIndex: longint;
begin
  if Links = nil then
    Links := TList.Create;

  if HighlightMatches <> nil then
    HighlightMatches.Clear;

  // Text.Clear;
  ImageOffsets.Clear;

  try
    EnsureSlotsLoaded;
  except
    on E: EHelpFileException do
    begin
      Text := Text + E.Message;
      exit;
    end;
  end;

  WordsOnLine := 0;

  State.LinkIndex := 0;
  State.FontState := fsNormal; // ? Not sure... this could be reset at start of slot
  State.InCharGraphics := false;
  State.Spacing := true;
  State.ForegroundColorTag := '</color>';
  State.BackgroundColorTag := '</backcolor>';
  State.StartOfTextBlock := -1;
  State.TextBlock := '';
  State.FootnoteLink := nil;
  Text := Text + '<leftmargin 1>';

  SequenceStepIndex := 0;

  for SlotIndex := 0 to _NumSlots - 1 do
  begin
    if not State.InCharGraphics then
      State.Spacing := true; // this is just a guess as to the exact view behaviour.
                             // inf.txt indicates that spacing is reset to true at
                             // slot (cell) start, but that doesn't seem to be the
                             // case when in character graphics... hey ho.

    Slot := THelpTopicSlot(_Slots[ SlotIndex ]);
    pData := Slot.pData;
    pSlotEnd := pData + Slot.Size;
    State.Alignment := itaLeft;

    while pData < pSlotEnd do
    begin
      LocalDictIndex := pData^;
      inc( pData );

      if LocalDictIndex < Slot.LocalDictSize then
      begin
        // Normal word lookup
        GlobalDictIndex := Slot.pLocalDictionary^[ LocalDictIndex ];

        if ShowWordSeparators then
          Text := Text + '{' + IntToStr( GlobalDictIndex )+ '}';

        // normal lookup
        if GlobalDictIndex < _GlobalDictionary.Count then
          StringToAdd := _GlobalDictionary[ GlobalDictIndex ]
        else
          StringToAdd := '';

        if StrIsEmptyOrSpaces( StringToAdd ) then
        begin
          // spaces only...
          CheckForAutoURL( Text, State );
        end
        else
        begin
          // really is a word, not a space.

          // store string into "word"
          if Length(State.TextBlock) = 0 then
            // store start of block
            State.StartOfTextBlock := Length(Text);

          State.TextBlock := State.TextBlock + StringToAdd;

          SubstituteAngleBrackets( StringToAdd );

          if HighlightSequences <> nil then
          begin
            if SequenceStepIndex > 0 then
            begin
              // currently highlighting a sequence.
              dec( SequenceStepIndex );
              if SequenceStepIndex = 0 then
              begin
                // now finished, insert end highlight
                StringToAdd := StringToAdd
                               + State.BackgroundColorTag;

              end;
            end
            else
            begin
              // not yet in a sequence, searching.
              SequenceStepIndex :=
                CheckForSequence( HighlightSequences,
                                  SlotIndex,
                                  pData,
                                  State,
                                  GlobalDictIndex );

              if SequenceStepIndex > 0 then
              begin
                // this word starts a sequence!
                if HighlightMatches <> nil then
                  HighlightMatches.Add( pointer( Length(Text) ) );
                StringToAdd := '<backcolor #'
                         + IntToHex( Settings.Colors[ SearchHighlightTextColorIndex ], 6 )
                         + '>'
                         + StringToAdd;
                dec( SequenceStepIndex );
                if SequenceStepIndex = 0 then
                  // and ends it.
                  StringToAdd := StringToAdd
                           + State.BackgroundColorTag;
              end;

            end;
          end; // if processing sequence
          inc( WordsOnLine );
        end;

        Text := Text + StringToAdd;

        if State.FootnoteLink <> nil then
        begin
          State.FootnoteLink.Title := State.FootnoteLink.Title + StringToAdd;
          if State.Spacing then
          begin
            State.FootnoteLink.Title := State.FootnoteLink.Title + ' ';
          end;
        end;

        if State.Spacing then
        begin
          CheckForAutoURL( Text, State );
          Text := Text + ' ';
        end;
      end
      else
      begin
        // special code

        if ShowCodes then
        begin
          Text := Text + '[' + IntToHex( LocalDictIndex, 2 );
          if LocalDictIndex = IPF_ESC then
          begin
            EscapeLen := pData^;
            for i := 1 to EscapeLen - 1 do
              Text := Text + ' ' + IntToHex( ( pData + i )^, 2 );
          end;
          Text := Text + ']';
        end;

        case LocalDictIndex of
          IPF_END_PARA:
          begin
            if SlotIndex = 0 then
              if pData - 1 = Slot.pData then
                // ignore first FA, not needed with RichTextView
                continue;

            CheckForAutoURL( Text, State );
            if State.Alignment = itaCenterOnePara then
            begin
              State.Alignment := itaLeft;
              Text := Text + '<align left>';
            end;
            Text := Text + RTF_NewLine;

            if WordsOnLine > 0 then
              Text := Text + RTF_NewLine;

            if not State.InCharGraphics then
              State.Spacing := true;

            WordsOnLine := 0;
          end;

          IPF_CENTER:
          begin
            CheckForAutoURL( Text, State );
            Text := Text + RTF_NewLine + '<align center>';
            State.Alignment := itaCenterOnePara;
          end;

          IPF_INVERT_SPACING:
          begin
            if not State.InCharGraphics then
              State.Spacing := not State.Spacing;
          end;

          IPF_LINEBREAK:
          begin
            CheckForAutoURL( Text, State );

            if State.Alignment = itaCenterOnePara then
            begin
              State.Alignment := itaLeft;
              Text := Text + '<align left>';
            end;
            Text := Text + RTF_NewLine;
            if not State.InCharGraphics then
              State.Spacing := true;
            WordsOnLine := 0;
          end;

          IPF_SPACE:
          begin
            CheckForAutoURL( Text, State );
            if State.Spacing then
              Text := Text + '  ';
          end;

          IPF_ESC:
          begin
            // escape sequence
            TranslateIPFEscapeCode( State,
                                    pData,
                                    Text,
                                    WordsOnLine,
                                    ImageOffsets );

          end;

        end; // case code of...
      end;
    end; // for slotindex = ...
  end;
  State.TextBlock := '';
end;

function TTopic.SearchForWord( DictIndex: integer;
                               StopAtFirstOccurrence: boolean )
  : longint;
var
  SlotIndex: integer;
  Slot: THelpTopicSlot;
  pData: pUInt8;
  pSlotEnd: pUInt8;

  EscapeLen: longint;

  GlobalDictIndex: uint32;

  LocalDictIndex: uint8;
begin
  EnsureSlotsLoaded;

  Result := 0;
  for SlotIndex := 0 to _NumSlots - 1 do
  begin
    Slot := THelpTopicSlot(_Slots[ SlotIndex ]);

    pData := Slot.pData;

    pSlotEnd := pData + Slot.Size;

    while pData < pSlotEnd do
    begin
      LocalDictIndex := pData^;

      if LocalDictIndex < Slot.LocalDictSize then
      begin
        // Normal word lookup
        GlobalDictIndex := Slot.pLocalDictionary^[ LocalDictIndex ];

        if GlobalDictIndex = DictIndex then
        begin
          inc( result );
          if StopAtFirstOccurrence then
            exit;
        end;
      end
      else
      begin
        // special code
        if LocalDictIndex = $ff then
        begin
          // escape string, skip it
          EscapeLen := ( pData + 1 ) ^;
          inc( pData, EscapeLen );
        end;
      end;

      inc( pData );
    end; // for slotindex = ...
  end;
end;

// Search for a sequence of bytes, including in escape codes
// this is for debugging to allow finding specific sequences
function TTopic.SearchForData( Data: pbyte;
                               DataLen: integer ): boolean;
var
  SlotIndex: integer;
  Slot: THelpTopicSlot;
  pData: pUInt8;
  pSlotEnd: pUInt8;

  pHold: pUint8;
  pSearch: pUint8;
begin
  EnsureSlotsLoaded;

  for SlotIndex := 0 to _NumSlots - 1 do
  begin
    Slot := THelpTopicSlot(_Slots[ SlotIndex ]);

    pSearch := Data;
    pHold := Slot.pData;
    pData := Slot.pData;
    pSlotEnd := Slot.pData + Slot.Size;

    while pHold < pSlotEnd do
    begin
      if pData^ = pSearch^ then
      begin
        // byte matches
        inc( pData );
        inc( pSearch );
        if ( pSearch >= Data + DataLen ) then
        begin
          // matches
          result := true;
          exit;
        end
      end
      else
      begin
        // no match
        pSearch := Data;
        inc( pHold );
        pData := pHold;
      end;
    end; // for slotindex = ...
  end;

  result := false; // not found
end;

function TTopic.SearchForWordSequences( WordSequence: TList;
    StopAtFirstOccurrence: boolean ): longint;
var
  SlotIndex: integer;
  Slot: THelpTopicSlot;
  pData: pUInt8;
  pSlotEnd: pUInt8;

  EscapeLen: longint;

  GlobalDictIndex: uint32;
  IsWord: boolean;
  WordRelevance: uint32;

  CurrentMatchRelevance: uint32; // total relevances for words matched so far
                                 // in the current sequence

//  CurrentMatch: string;  // useful for debugging only
  LocalDictIndex: uint8;

  SequenceIndex: longint;
  SequenceStartSlotIndex: longint;
  pSequenceStartData: pUInt8;

  pStepWordRelevances: UInt32ArrayPointer; // word relevances for the current step in the sequence

  // get the current slot start and end pointers
  procedure GetSlot;
  begin
    Slot := THelpTopicSlot(_Slots[ SlotIndex ]);
    pData := Slot.pData;
    pSlotEnd := pData + Slot.Size;
  end;

  // get pointer to the current set of word relevances
  procedure GetStepFlags;
  begin
    pStepWordRelevances := WordSequence[ SequenceIndex ];
  end;

  // store the current point as start of a sequence
  procedure StoreStartOfSequence;
  begin
    SequenceIndex := 0;
    SequenceStartSlotIndex := SlotIndex;
    pSequenceStartData := pData;
    CurrentMatchRelevance := 0;
//    CurrentMatch := '';
    GetStepFlags;
  end;

begin
  Result := 0;

  EnsureSlotsLoaded;

  if _NumSlots = 0 then
    // thar's nowt in yon topic, cannae be a match laid
    exit;

  SlotIndex := 0;

  GetSlot;

  StoreStartOfSequence;

  while true do
  begin
    LocalDictIndex := pData^;
    IsWord := false;
    if LocalDictIndex < Slot.LocalDictSize then
    begin
      IsWord := true;
      // Normal word lookup, so get the global dict idnex before we
      // (potentially) move to next slot
      GlobalDictIndex := Slot.pLocalDictionary^[ LocalDictIndex ];
    end;

    inc( pData );
    if pData >= pSlotEnd then
    begin
      // reached end of slot, next please
      inc( SlotIndex );
      if SlotIndex < _NumSlots then
        GetSlot;
      // else - there is nothing more to search
      // but we need to check this last item
    end;

    if IsWord then
    begin
      // Normal word lookup
      WordRelevance := 0;

      if GlobalDictIndex < _GlobalDictionary.Count then
        if not StrIsEmptyOrSpaces( _GlobalDictionary[ GlobalDictIndex ] ) then;
          WordRelevance := pStepWordRelevances^[ GlobalDictIndex ];

      if WordRelevance > 0 then
      begin
        // Found a matching word
        inc( CurrentMatchRelevance, WordRelevance );
// debug:
//        CurrentMatch := CurrentMatch +
//          pstring( _GlobalDictionary[ GlobalDictIndex ] )^;

        if SequenceIndex = 0 then
        begin
          // remember next start point
          SequenceStartSlotIndex := SlotIndex;
          pSequenceStartData := pData;
        end;

        inc( SequenceIndex );

        if SequenceIndex < WordSequence.Count then
        begin
          // get next set of flags.
          GetStepFlags;
        end
        else
        begin
          // found a complete sequence. Cool!

          inc( result, CurrentMatchRelevance );

          if StopAtFirstOccurrence then
            exit;

          // start looking from the beginning of the sequence again.
          StoreStartOfSequence;
        end;
      end
      else
      begin
        // not a match at this point, restart search
        if SequenceIndex > 0 then
        begin
          // we had matched one or more steps already,
          // back to start of sequence AND back to
          // point we started matching from (+1)
          SequenceIndex := 0;
          CurrentMatchRelevance := 0;
//          CurrentMatch := '';
          SlotIndex := SequenceStartSlotIndex;
          GetSlot;
          pData := pSequenceStartData;
          GetStepFlags;
        end
        else
        begin
          // haven't matched anything yet.
          // update start of sequence
          SequenceStartSlotIndex := SlotIndex;
          pSequenceStartData := pData;
        end;
      end;
    end
    else
    begin
      // special code
      if LocalDictIndex = $ff then
      begin
        // escape string, skip it
        EscapeLen := pData ^;
        inc( pData, EscapeLen );
      end;
    end;

    if SlotIndex >= _NumSlots then
    begin
      // finished searching topic
      break;
    end;

    // next item
  end;
end;


function TTopic.CountWord( DictIndex: integer ): longint;
begin
  Result := SearchForWord( DictIndex, false );
end;

function TTopic.ContainsWord( DictIndex: integer ): boolean;
begin
  Result := SearchForWord( DictIndex, true ) > 0;
end;

// Gets the window dimensions specified by this topic's
// contents header
procedure TTopic.GetContentsWindowRect( ContentsRect: THelpWindowRect );
var
  extendedinfo: TExtendedTOCEntry;
  XY: THelpXYPair;
  p: pbyte;

  Flags: byte;
begin
  Flags := _pTOCEntry ^.flags;
  p := pByte( _pTOCEntry + sizeof( TTOCEntryStart ) );

  ContentsRect.Left := 0;
  ContentsRect.Bottom := 0;
  ContentsRect.Width := 100;
  ContentsRect.Height := 100;

  if ( Flags and TOCEntryExtended ) > 0 then
  begin
    // have more details available...
    ExtendedInfo.w1 := p^;
    ExtendedInfo.w2 := ( p+1) ^;
    inc( p, sizeof( ExtendedInfo ) );

    if (  ExtendedInfo.w1 and 1 ) > 0 then
    begin
      // read origin
      XY := pHelpXYPair( p )^;
      inc( p, sizeof( XY ) );
      ReadHelpPosition( XY, ContentsRect );
    end;
    if ( ExtendedInfo.w1 and 2 ) > 0 then
    begin
      // read size
      XY := pHelpXYPair( p )^;
      inc( p, sizeof( XY ) );
      ReadHelpSize( XY, ContentsRect );
    end;
  end;
end;

const
  IPFColorNames: array[ 0..15 ] of string =
  (
    'default',
    'blue',
    'red',
    'pink',
    'green',
    'cyan',
    'yellow',
    'neutral',
//    'brown',  ??
    'darkgray',
    'darkblue',
    'darkred',
    'darkpink',
    'darkgreen',
    'darkcyan',
    'black',
    'palegray'
  );

Procedure SaveExtraLinkData( Link: TWindowedHelpLink;
                            pData: pUInt8 );
var
  LinkFlags1: uint8;
  LinkFlags2: uint8;
  LinkDataIndex: longint;
  pLinkXY: pHelpXYPair;
  pLinkData: pUInt8;
begin
  LinkFlags1 := ( pData + 0 ) ^;
  LinkFlags2 := ( pData + 1 ) ^;

  pLinkData := pData + 2;

  if ( LinkFlags1 and 1 ) > 0 then
  begin
    // position specified
    pLinkXY := pHelpXYPair( pLinkData );
    inc( pLinkData, sizeof( THelpXYPair ) );
  end;

  if ( LinkFlags1 and 2 ) > 0 then
  begin
    // size specified
    pLinkXY := pHelpXYPair( pLinkData );
    inc( pLinkData, sizeof( THelpXYPair ) );
  end;

  if ( LinkFlags1 and 8 ) > 0 then
  begin
    // window controls specified - skip
    inc( pLinkData, 2 );
  end;

  if ( LinkFlags2 and 4 ) > 0 then
  begin
    // group specified
    Link.GroupIndex := pUInt16( pLinkData )^;
    inc( LinkDataIndex, sizeof( uint16 ) );
  end;

  if ( LinkFlags1 and 64 ) > 0 then
  begin
    Link.Automatic := true;
  end;

  if ( LinkFlags1 and 4 ) > 0 then
    Link.ViewPort := true;

  if ( LinkFlags2 and 2 ) > 0 then
    Link.Dependent := true;

  if ( LinkFlags1 and 128 ) > 0 then
    Link.Split := true;

  // cant be bothered with the others.
end;

procedure TTopic.SaveIPFEscapeCode( Var State: TParseState;
                                    Var pData: pUInt8;
                                    Var F: TextFile;
                                    ImageOffsets: TList );
var
  EscapeLen: uint8;
  EscapeCode: uint8;

  Margin: integer;

  BitmapOffset: longword;
  BitmapFlags: uint8;

  ColorCode: uint8;
  StyleCode: uint8;

  FontIndex: uint8;
  pFontSpec: pTHelpFontSpec;

  FaceName: string;

  ExternalLinkFileIndex: uint8;
  ExternalLinkTopicID: string;

  ProgramLink: string;
  ProgramPath: string;
  tmpProgramLinkParts : TStrings;

  OutputString: string;
begin
  EscapeLen := pData^;
  EscapeCode := (pData + 1) ^;
  OutputString := '';

  case EscapeCode of

    ecSetLeftMargin:
    begin
      Margin := integer( ( pData + 2 )^ );
      GetMarginTag( Margin, State.FontState, OutputString, false );
    end;

    ecSetLeftMarginNewLine:
    begin
      Margin := integer( ( pData + 2 )^ );
      GetMarginTag( Margin, State.FontState, OutputString, false );
      OutputString := OutputString
                      + RTF_NewLine;
    end;

    ecSetLeftMarginFit:
    begin
      Margin := integer( ( pData + 2 )^ );
      GetMarginTag( Margin, State.FontState, OutputString, true );
      // note that this will cause following tex to be "tabbed" across to the
      // new margin position, if not yet there.
      // if we are already past this margin then a new line should be started.

    end;

    ecSetLeftMarginHere:
    begin
      OutputString := '<leftmargin here>';
    end;

    ecHighlight1: // hp1,2,3, 5,6,7
    begin
      StyleCode := ( pData + 2 ) ^;
      if StyleCode > 3 then
        StyleCode := StyleCode + 1; // 4, 8 and 9 are expressed in highlight2 code

      if StyleCode > 0 then
        Write( F, ':hp' + IntToStr( StyleCode ) + '.' )
      else
        Write( F, ':ehp' + IntToStr( State.StyleCode ) + '.' );
      State.StyleCode := StyleCode;
    end;

    ecHighlight2: // hp4, 8, 9
    begin
      StyleCode := ( pData + 2 ) ^;
      case StyleCode of
        1: StyleCode := 4;
        2: StyleCode := 8;
        3: StyleCode := 9;
      end;

      if StyleCode > 0 then
        Write( F, ':hp' + IntToStr( StyleCode ) + '.' )
      else
        Write( F, ':ehp' + IntToStr( State.StyleCode ) + '.' );
      State.StyleCode := StyleCode;
    end;

    ecLinkStart:
    begin
      Write( F, ':link reftype=hd' ); // link to heading

      Write( F, ' refid=' + IntToStr( pUInt16( pData + 2 )^ ) );

      {
        if EscapeLen >= 6 then
        begin
          GetExtraLinkData( Link, pData + 4 );
        end;}

//      if Link.Automatic then
//        Write( F, ' auto' );

      Write( F, '.' );

      inc( State.LinkIndex );
    end;

    ecFootnoteLinkStart:
    begin
      Write( F, ':link reftype=fn refid=fn'
             + IntToStr( pUInt16( pData + 2 )^ )
             + '.' );
      inc( State.LinkIndex );
    end;

    ecStartLinkByResourceID:
    begin
      Write( F, ':link reftype=hd res='
             + IntToStr( pUInt16( pData + 2 )^ )
             + '.' );

      inc( State.LinkIndex );
    end;

    ecExternalLink:
    begin
      ExternalLinkFileIndex := ( pData + 2 )^;
      ExternalLinkTopicID := StrNPas( pchar( pData + 4 ), ( pData + 3 )^ );
      Write( F, ':link reftype=hd '
             + ' refid=' + StrInSingleQuotes( ExternalLinkTopicID )
             + ' database=' + StrInSingleQuotes( _ReferencedFiles[ ExternalLinkFileIndex ] )
             + '.' );

    end;

    ecProgramLink:
    begin
      ProgramLink := StrNPas( pchar( pData + 3 ), EscapeLen - 3 );
      tmpProgramLinkParts := TStringList.Create;
      StrExtractStrings(tmpProgramLinkParts, ProgramLink, [' '], #0);
      ProgramPath := tmpProgramLinkParts[0];
      tmpProgramLinkParts.Destroy;

      Write( F, ':link reftype=launch'
             + ' object=' + StrInSingleQuotes( ProgramPath )
             + ' data=' + StrInSingleQuotes( ProgramLink )
             + '.' );
    end;

    ecLinkEnd:
    begin
      Write( F, ':elink.' );
      if State.FootnoteLink <> nil then
        State.FootnoteLink := nil;
    end;

    ecStartCharGraphics:
    begin
      State.FontState := fsFixed;
      State.InCharGraphics := true;
      WriteLn( F, '' );
      WriteLn( F, ':cgraphic.' );
      State.Spacing := false;
    end;

    ecEndCharGraphics:
    begin
      State.FontState := fsNormal;
      State.InCharGraphics := false;
      WriteLn( F, '' );
      WriteLn( F, ':ecgraphic.' );
      State.Spacing := true;
    end;

    ecImage:
    begin
      BitmapFlags := ( pData + 2 )^;
      BitmapOffset := pUInt32( pData + 3 )^;

      SaveImageText( BitmapOffset, BitmapFlags, F, ImageOffsets );

      if State.Spacing then
        Write( F, ' ' );
    end;

    ecLinkedImage:
    begin
      SaveLinkedImage( pData, F, ImageOffsets );
      // Note! Early exit, since the procedure
      // will update pData.
      exit;
    end;

    ecStartLines:
    begin
      WriteLn( F, '' );
      // aligned text
      case ( pData + 2 )^ of
        0, // just in case - to match image alignment oddities
        1:
        begin
          WriteLn( F, ':lines.' );
          State.Alignment := itaLeft;
        end;

        2:
        begin
          WriteLn( F, ':lines align=right.' );
          State.Alignment := itaRight;
        end;

        4:
        begin
          WriteLn( F, ':lines align=center.' );
          State.Alignment := itaCenter;
        end;
      end;
    end;

    ecEndLines:
    begin
      // supposed to turn word wrap on, default font
      WriteLn( F, '' );
      WriteLn( F, ':elines.' );
      State.Alignment := itaLeft;
    end;

    ecForegroundColor:
    begin
      ColorCode := ( pData + 2 )^;

      if ColorCode < High( IPFColorNames ) then
        Write( F, ':color fc=' + IPFColorNames[ ColorCode ] + '.' );
    end;

    ecBackgroundColor:
    begin
      ColorCode := ( pData + 2 )^;
      if ColorCode < High( IPFColorNames ) then
        Write( F, ':color bc=' + IPFColorNames[ ColorCode ] + '.' );
    end;

    ecFontChange:
    begin
      FontIndex := ( pData + 2 )^;
      if FontIndex = 0 then
      begin
        // back to default font
        Write( F, ':font facename=default.' );
        State.FontState := fsNormal;
      end
      else if FontIndex < _FontTable.Count then
      begin
        // valid font index
        pFontSpec := _FontTable[ FontIndex ];

        if pFontSpec = SubstituteFixedFont then
        begin
         // oops.
          OutputString := '<tt>';
          State.FontState := fsFixed;
        end
        else
        begin
          pFontSpec := _FontTable[ FontIndex ];
          FaceName := StrNPas( pFontSpec^.FaceName,
                               sizeof( pFontSpec^.FaceName ) );
          Write( F,
                 ':font facename=' + StrInSingleQuotes( FaceName )
                 + ' size=' + IntToStr( pFontSpec^.Height )
                   + 'x' + IntToStr( pFontSpec^.Width )
                 + '.' );
          State.FontState := fsCustom;
        end;
      end;
    end
  end; // case escape code of...

  // Write( F, OutputString );

  inc( pData, EscapeLen );
end;

procedure TTopic.SaveToIPF( Var f: TextFile;
                            ImageOffsets: TList );
var
  SlotIndex: integer;
  Slot: THelpTopicSlot;
  pData: pUInt8;
  pSlotEnd: pUInt8;
  GlobalDictIndex: uint32;
  StringToAdd: string;
  LocalDictIndex: uint8;
  State: TParseState;
  SequenceStepIndex: longint;
  LineLen: longint;
  c: char;
begin
  EnsureSlotsLoaded;

  State.LinkIndex := 0;
  State.FontState := fsNormal; // ? Not sure... this could be reset at start of slot
  State.InCharGraphics := false;
  State.Spacing := true;
  State.ForegroundColorTag := '</color>';
  State.BackgroundColorTag := '</backcolor>';

  State.StartOfTextBlock := -1;
  State.TextBlock := '';

  State.FootnoteLink := nil;

  State.StyleCode := 0;

  SequenceStepIndex := 0;

  LineLen := 0;

  for SlotIndex := 0 to _NumSlots - 1 do
  begin
    if not State.InCharGraphics then
      State.Spacing := true; // this is just a guess as to the exact view behaviour.
                             // inf.txt indicates that spacing is reset to true at
                             // slot (cell) start, but that doesn't seem to be the
                             // case when in character graphics... hey ho.

    Slot := THelpTopicSlot(_Slots[ SlotIndex ]);

    pData := Slot.pData;

    pSlotEnd := pData + Slot.Size;

    State.Alignment := itaLeft;

    while pData < pSlotEnd do
    begin
      LocalDictIndex := pData^;
      inc( pData );

      if LocalDictIndex < Slot.LocalDictSize then
      begin
        // Normal word lookup
        GlobalDictIndex := Slot.pLocalDictionary^[ LocalDictIndex ];

        // normal lookup
        if GlobalDictIndex < _GlobalDictionary.Count then
          StringToAdd := _GlobalDictionary[ GlobalDictIndex ]
        else
          StringToAdd := '';

        if (Length( StringToAdd ) = 1) and Settings.IPFTopicSaveAsEscaped then
        begin
          // could be symbol
          c := StringToAdd[ 1 ];
          case C of
            '&': StringToAdd := '&amp.';
            '''': StringToAdd := '&apos.';
            '*': StringToAdd := '&asterisk.';
            '@': StringToAdd := '&atsign.';
            '\': StringToAdd := '&bsl.';
            '^': StringToAdd := '&caret.';
            '"': StringToAdd := '&osq.';
            ':': StringToAdd := '&colon.';
            '.': StringToAdd := '&per.';
            '(': StringToAdd := '&lpar.';
            ')': StringToAdd := '&rpar.';
            '/': StringToAdd := '&slash.';
            ',': StringToAdd := '&comma.';
            '-': StringToAdd := '&hyphen.';
            '_': StringToAdd := '&us.';
            '~': StringToAdd := '&tilde.';
            '+': StringToAdd := '&plus.';
            '>': StringToAdd := '&gt.';
            ';': StringToAdd := '&semi.';
       Chr($da): StringToAdd := '+';
       Chr($c4): StringToAdd := '-';
       Chr($b3): StringToAdd := '|';
       Chr($c3): StringToAdd := '|';
       Chr($bf): StringToAdd := '+';
          end;
        end;

        inc( LineLen, Length( StringToAdd ) );
        if ( LineLen > 80 ) and ( not State.InCharGraphics ) then
        begin
          WriteLn( F );
          LineLen := 0;
        end;

        Write( F, StringToAdd );
{
        if State.FootnoteLink <> nil then
        begin
          State.FootnoteLink.Title := State.FootnoteLink.Title + StringToAdd;
          if State.Spacing then
          begin
            State.FootnoteLink.Title := State.FootnoteLink.Title + ' ';
          end;
        end;
 }
        if State.Spacing then
        begin
          Write( F, ' ' );
          inc( LineLen );
        end;
      end
      else
      begin
        // special code

        case LocalDictIndex of
          IPF_END_PARA:
          begin
            WriteLn( F, '' );
            Write( F, ':p.' );
            LineLen := 3;

            if not State.InCharGraphics then
              State.Spacing := true;
          end;

          IPF_CENTER:
          begin
            WriteLn( F, '' );
            Write( F, '.ce ' ); // remainder of this line is centered.
            LineLen := 4;
            State.Alignment := itaCenterOnePara;
          end;

          IPF_INVERT_SPACING:
          begin
            State.Spacing := not State.Spacing;
          end;

          IPF_LINEBREAK:
          begin
            WriteLn( F, '' );
            if not State.InCharGraphics then
              WriteLn( F, '.br ' ); // break must be the only thing on the line

            LineLen := 0;
            if not State.InCharGraphics then
              State.Spacing := true;
          end;

          IPF_SPACE:
          begin
            if State.Spacing then
              Write( F, '  ' )
            else
              Write( F, ' ' );
          end;

          IPF_ESC:
          begin
            // escape sequence
            SaveIPFEscapeCode( State,
                               pData,
                               F,
                               ImageOffsets );
          end;

        end; // case code of...
      end;
    end; // for slotindex = ...
  end;
  State.TextBlock := '';

end;

// Compares two topics for purposes of sorting by
// search match relevance
function TopicRelevanceCompare( Item1, Item2: pointer ): longint;
var
  Topic1, Topic2: TTopic;
begin
  Topic1 := TTopic(Item1);
  Topic2 := TTopic(Item2);

  if Topic1.SearchRelevance > Topic2.SearchRelevance then
    Result := -1
  else if Topic1.SearchRelevance < Topic2.SearchRelevance then
    Result := 1
  else
    Result := 0;
end;

// Compares two topics for purposes of sorting by title
function TopicTitleCompare( Item1, Item2: pointer ): longint;
begin
  Result := CompareText( TTopic( Item1 )._Title,
                         TTopic( Item2 )._Title );
end;


end.
