Unit HelpTopic;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2001 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// This is it - the monster which decodes IPF data.
// It's created with a reference to the contents data defining it.
// It gets relevant pointers out of that. When GetText is called
// it decodes the data and spits out formatted text to suit
// RichTextView.

uses
  Classes, DataTypes, HelpWindow, HelpFileHeader;

Type
  THelpLink = class(TObject)
    TopicIndex: longint;
    GroupIndex: longint; // -1 if not specified
    Automatic: boolean;
    Split: boolean;
    ViewPort: boolean;
    Dependent: boolean;
    Rect: THelpWindowRect;
    HelpFile: TObject;
    constructor Create;
    destructor Destroy; override;
  end;

  THelpTopicSlot = packed record
    pData: pInt8;
    Size: longint;
    pLocalDictionary: Int16ArrayPointer;
    LocalDictSize: int8;
  end;
  pHelpTopicSlot = ^ THelpTopicSlot;

  SlotArray = packed array[ 0..0 ] of THelpTopicSlot;

  pSlotArray = ^SlotArray;

  TTopic = class(TObject)
  protected
    _pTOCEntry: pTTOCEntryStart;
    _Slots: pSlotArray;
    _NumSlots: longint;
    _NumSlotsUsed: longint;
    _Title: string;
    _GlobalDictionary: TStringList;

    _ShowInContents: boolean;
    _ContentsLevel: integer;
    _ContentsGroupIndex: longint;

    procedure SetTitle( const NewValue: string );
    function GetTitle: string;

    // Returns the tag texts for the given bitmap ref
    function GetImageText( BitmapOffset: longint;
                           BitmapFlags: longint;
                           ImageOffsets: TList ): string;

    Procedure ProcessLinkedImage( Var pData: pByte;
                                  Var OutputString: string;
                                  Var DebugString: string;
                                  Var ImageOffsets: TList;
                                  Var LinkIndex: longint );
    procedure TranslateIPFEscapeCode( Var pData: pInt8;
                                      Var OutputString: string;
                                      Var DebugString: string;
                                      Var Spacing: boolean;
                                      Var InFixedFont: boolean;
                                      Var WordsOnLine: longint;
                                      Var ImageOffsets: TList;
                                      Var LinkIndex: longint );

  public
    constructor Create( FileData: pointer;
                        const FileHeader: THelpFileHeader;
                        Dictionary: TStringList;
                        pTOCEntry: pTTOCEntryStart );

    destructor Destroy; override;

    property Title: string read GetTitle write SetTitle;
    procedure SetTitleFromMem( const p: pointer; const Len: byte );

    // Main function for retrieving text for topic.
    // HighlightWords: array indicating whether words
    //   should be highlighted
    // ShowCodes: indicates debugging: hex output of escape
    //   codes will be included
    // Text: The output is written to here.
    // ImageOffsets: For each image that occurs in the text,
    //   the help file offset will be written to this list.
    procedure GetText( HighLightWords: Int32ArrayPointer;
                       ShowCodes: boolean;
                       Var Text: PChar;
                       ImageOffsets: TList );
    function SearchForWord( DictIndex: integer;
                            StopAtFirstOccurrence: boolean ): longint;

    procedure GetContentsWindowRect( ContentsRect: THelpWindowRect );

  public
    Links: TList; // only valid after GetText
    // Used externally
    HelpFile: TObject;
    Index: longint;

    FoundInSearch: boolean;
    ExcludedInSearch: boolean;

    SearchRelevance: longint;

    property ShowInContents: boolean read _ShowInContents;
    property ContentsLevel: integer read _ContentsLevel;
    property ContentsGroupIndex: longint read _ContentsGroupIndex;

    function CountWord( DictIndex: integer ): longint;
    function ContainsWord( DictIndex: integer ): boolean;
  end;

// Compares two topics for purposes of sorting by
// search match relevance
function TopicRelevanceCompare( Item1, Item2: pointer ): longint;

Implementation

uses
  SysUtils,
{
  ACLUtility, ACLStringUtility, ACLPCharUtility,
  ACLString, ACLProfile,
}
  IPFEscapeCodes;

const
  IPFStyleTags : array [ 0..6 ] of string =
  (
    '</i></b></u>',  // hp1
    '<i>',
    '<b>',
    '<b><i>',
    '<u>',
    '<u><i>',
    '<u><b>'
  );

constructor THelpLink.Create;
begin
  TopicIndex:= -1;
  GroupIndex:= -1;
  Automatic:= false;
  ViewPort:= false;
  Dependent:= false;

  Rect:= THelpWindowRect.Create;
end;

destructor THelpLink.Destroy;
begin
  Rect.Destroy;
end;

constructor TTopic.Create( FileData: pointer;
                           const FileHeader: THelpFileHeader;
                           Dictionary: TStringList;
                           pTOCEntry: pTTOCEntryStart );
var
  pExtendedInfo: pExtendedTOCEntry;
  titleLen: integer;
  i: longint;
  SlotNumber: int16;
  XY: THelpXYPair;
  p: pbyte;

  lFlags: byte;
  pSlotOffsets: Int32ArrayPointer;
  pSlotData: pSlotHeader;
  Slot: THelpTopicSlot;
begin
  _Title := '';
  _GlobalDictionary := Dictionary;
  _ContentsGroupIndex := 0;

  _pTOCEntry := pTOCEntry;
  _NumSlots:= pTOCEntry^.numslots;

  GetMem( _Slots, _NumSlots * sizeof(THelpTopicSlot));

  _NumSlotsUsed := 0;

  lFlags:= _pTOCEntry^.flags;
  p:= pInt8( _pTOCEntry ) + sizeof( TTOCEntryStart );

  if ( lFlags and TOCEntryExtended ) > 0 then
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
      inc( p, 2 );

    if ( pExtendedInfo^.w2 and 4 ) > 0 then
    begin
      _ContentsGroupIndex := pint16(p)^;
      // read group
      inc(p, sizeof(int16));
    end;
  end;

  // Read slot indices
  for i := 0 to _NumSlots-1 do
  begin
    SlotNumber:= pint16( p )^;
    if SlotNumber < FileHeader.nslots then
    begin
      pSlotOffsets := FileData + FileHeader.slotsstart;

      pSlotData := pSlotHeader( FileData + pSlotOffsets^[ SlotNumber ] );

      Slot.pData := pInt8( pSlotData + sizeof( TSlotHeader ) );
      Slot.pLocalDictionary := FileData + pSlotData^.localDictPos;
      Slot.LocalDictSize := pSlotData^.nLocalDict;
      Slot.Size := pSlotData^.ntext;

      _Slots^[ _NumSlotsUsed ] := Slot;
      inc( _NumSlotsUsed );

    end;
    inc( p, sizeof( int16 ) );
  end;

  titleLen := _pTOCEntry^.length - ( longword(p) - longword(_pTOCEntry) );

  // Read title
  if TitleLen > 0 then
    SetTitleFromMem( p, TitleLen )
  else
    Title:= '(No title)';

  _ContentsLevel:= ( lFlags and $f );
  _ShowInContents:= (lFlags and TOCEntryHidden) = 0;
  if _ContentsLevel = 0 then
    _ShowInContents := false; // hmmm....
end;

destructor TTopic.Destroy;
var
  LinkIndex: longint;
  Link: THelpLink;
begin
  if Links <> nil then
  begin
    for LinkIndex:= 0 to Links.Count - 1 do
    begin
      Link := THelpLink(Links[ LinkIndex ]);
      Link.Destroy;
    end;
    Links.Destroy;
  end;
  FreeMem( _Slots, _NumSlots * sizeof( THelpTopicSlot ) );
end;

procedure TTopic.SetTitle( const NewValue: string );
begin
  _Title := NewValue;
end;

procedure TTopic.SetTitleFromMem( const p: pointer; const Len: byte );
begin
  //DisposeStr( _Title );
  //GetMem( _Title, Len + 1 );
  //_Title^[ 0 ] := char( Len );
  SetString(_Title, p, Len);
//  MemCopy( p, _Title + 1, Len );
end;

function TTopic.GetTitle: string;
begin
  Result:= _Title;
end;

function SubstituteAngleBrackets( const s: string ): string;
var
  i: integer;
  C: Char;
begin
  Result:= '';
  for i:= 1 to Length( S ) do
  begin
    C:= S[ i ];
    Result:= Result + C;
    case C of
      '<':
        Result:= Result + '<';
      '>':
        Result:= Result + '>';
    end;
  end;
end;

function TTopic.GetImageText( BitmapOffset: longint;
                              BitmapFlags: longint;
                              ImageOffsets: TList ): string;
var
  BitmapIndex: longint;
begin
  BitmapIndex:= ImageOffsets.IndexOf( pointer( BitmapOffset ) );
  if BitmapIndex = -1 then
    BitmapIndex:= ImageOffsets.Add( pointer( BitmapOffset ) );

  Result := '<image '
            + IntToStr( BitmapIndex )
            + '>';
  if ( BitmapFlags and $10 ) > 0 then
  begin
    // runin...
  end
  else if ( BitmapFlags and $08 ) > 0 then
  begin
    // stretch to fit
  end
  else
  begin
    // aligned
    case BitmapFlags and 7 of
      1: // left
        Result := #10 + '<left>' + Result + #10;
      2: // right
        Result := #10 + '<right>' + Result + #10;
      4,5: // centre (4 is official, 5 seems to occur too)
        Result := #10 + '<center>' + Result + #10;
    end;
  end;
end;

Procedure TTopic.ProcessLinkedImage( Var pData: pByte;
                                     Var OutputString: string;
                                     Var DebugString: string;
                                     Var ImageOffsets: TList;
                                     Var LinkIndex: longint );
var
  EscapeLen: int8;
  EscapeCode: int8;
  SubEscapeCode: int8;
  BitmapOffset: longword;
  BitmapFlags: int8;
  Link: THelpLink;
  LinkTopicIndex: int16;
begin
  LinkTopicIndex := -1;
  while true do
  begin
    EscapeLen:= pData^;
    SubEscapeCode := ( pData + 2 )^;
    case SubEscapeCode of
      HPART_DEFINE:
      begin
        BitmapFlags := ( pData + 3 )^;
        BitmapOffset:= pint32( pData + 4 )^;
      end;

      HPART_HDREF: // define whole bitmap topic link?
      begin
        LinkTopicIndex := pInt16( pData + 3 )^;
      end
      else
      begin
        // ignore others for now
        DebugString := DebugString + ' ?';
      end;
    end;
    inc( pData, EscapeLen );

    if pData^ <> IPF_ESC then
      // not an escape code, done
      break;
    inc( pData ); // move pointer to escape code len.
    EscapeCode:= (pData + 1) ^;
    if EscapeCode <> $0f then
      // not a hyperlink code, done
      break;
  end;

  OutputString := GetImageText( BitmapOffset,
                                 BitmapFlags,
                                 ImageOffsets );

  // Don't make it a link if we didn't find a
  // overall link code, i.e. degrade gracefully.
  if LinkTopicIndex > -1 then
  begin
    if LinkIndex>= Links.Count then
    begin
      Link:= THelpLink.Create;
      Link.HelpFile := HelpFile;
      Link.TopicIndex:= LinkTopicIndex;
      Links.Add( Link );
    end
    else
      Link := THelpLink(Links[ LinkIndex ]);

    OutputString := '<link '
                 + IntToStr( LinkIndex )
                 + '>'
                 + OutputString
                 + '</link>';
  end;

end;

procedure TTopic.TranslateIPFEscapeCode( Var pData: pInt8;
                                         Var OutputString: string;
                                         Var DebugString: string;
                                         Var Spacing: boolean;
                                         Var InFixedFont: boolean;
                                         Var WordsOnLine: longint;
                                         Var ImageOffsets: TList;
                                         Var LinkIndex: longint );
var
  EscapeLen: int8;
  EscapeCode: int8;

  Link: THelpLink;
  LinkFlags1: int8;
  LinkFlags2: int8;
  LinkDataIndex: longint;
  pLinkXY: pHelpXYPair;

  Margin: int8;
  EscCodeDataIndex: longint;

  pLinkData: pInt8;

  BitmapOffset: longword;
  BitmapFlags: int8;

begin
  EscapeLen:= pData^;
  EscapeCode:= (pData + 1) ^;

  OutputString := '';

  DebugString := IntToHex( EscapeCode, 2 ) + ' ';
  for EscCodeDataIndex:= 2 to EscapeLen - 1 do
  begin
    DebugString := DebugString
                   + ' '
                   + IntToHex( ( pData + EscCodeDataIndex )^, 2 );
  end;

  case EscapeCode of
    $02:
    begin
      // set left margin
      Margin:= ( pData + 2 )^;
      OutputString := '<margin ' + IntToStr( Margin ) + '>';
    end;

    $04:
    begin
      // style change
      OutputString := IPFStyleTags[ ( pData + 2 ) ^ ];
    end;

    $05:
    begin
      // Link start
      if LinkIndex>= Links.Count then
      begin
        Link:= THelpLink.Create;
        Link.HelpFile := HelpFile;
        Link.TopicIndex:= pInt16( pData + 2 )^;
        if EscapeLen >= 6 then
        begin
          LinkFlags1:= ( pData + 4 ) ^;
          LinkFlags2:= ( pData + 5 ) ^;

          pLinkData := pData + 6;
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
            // window controls specified - skip
            inc( pLinkData, 2 );

          if ( LinkFlags2 and 4 ) > 0 then
          begin
            // group specified
            Link.GroupIndex:= pint16( pLinkData )^;
            inc( LinkDataIndex, sizeof( int16 ) );
          end;

          if ( LinkFlags1 and 64 ) > 0 then
            Link.Automatic:= true;
          if ( LinkFlags1 and 4 ) > 0 then
            Link.ViewPort:= true;
          if ( LinkFlags2 and 2 ) > 0 then
            Link.Dependent:= true;
          if ( LinkFlags1 and 128 ) > 0 then
            Link.Split:= true;

          // cant be bothered with the others.
        end;
        Links.Add( Link );
      end
      else
        Link := THelpLink(Links[ LinkIndex ]);

      // If it's not an automatic link
      // then put code in to show it.
      if not Link.Automatic then
      begin
        if InFixedFont then
        begin
          // only use blue color, in fixed font,
          // since bold would change spacing
          OutputString := '<blue>'
        end
        else
        begin
          OutputString := '<b><blue>';
        end;

        OutputString := OutputString
                       + '<link '
                       + IntToStr( LinkIndex )
                       + '>';
      end;

      inc( LinkIndex );
    end;

    $07:
    begin
      // Footnote Link start
      if LinkIndex>= Links.Count then
      begin
        Link:= THelpLink.Create;
        Link.HelpFile := HelpFile;
        Link.TopicIndex:= pInt16( pData + 2 )^;
        Link.GroupIndex := -2;
        SetFootnoteRect( Link.Rect );
        Links.Add( Link );
      end
      else
        Link := THelpLink(Links[ LinkIndex ]);

      if InFixedFont then
      begin
        // only use blue color, in fixed font,
        // since bold would change spacing
        OutputString := '<blue>'
      end
      else
      begin
        OutputString := '<b><blue>';
      end;

      OutputString := OutputString
                     + '<link '
                     + IntToStr( LinkIndex )
                     + '>';

      inc( LinkIndex );
    end;

    $08: // Link end
    begin
      OutputString := '</link><black></b>';
    end;

    $0b: // start fixed font
    begin
      InFixedFont:= true;
      OutputString := #10 + '<tt><unaligned>';
      Spacing:= false;
      WordsOnLine:= 0;
    end;

    $0c: // end fixed font
    begin
      InFixedFont:= false;
      OutputString := '</tt><left>';
      Spacing:= true;
    end;

    $0e: // bitmap
    begin
      BitmapFlags := ( pData + 2 )^;
      BitmapOffset:= pint32( pData + 3 )^;

      OutputString := GetImageText( BitmapOffset,
                                    BitmapFlags,
                                    ImageOffsets );
    end;

    $0f: // hyperlinked bitmap
    begin
      ProcessLinkedImage( pData,
                          OutputString,
                          DebugString,
                          ImageOffsets,
                          LinkIndex );
      // Note! Early exit, since the procedure
      // will update pData.
      exit;
    end;

    $1a:
    begin
      // aligned text (can't see how it is otherwise different)
      case ( pData + 2 )^ of
        1:
          OutputString := #10 + '<left>';
        2:
          OutputString := #10 + '<right>';
        4:
          OutputString := #10 + '<center>';
      end;
      Spacing:= false;
      WordsOnLine:= 0;
    end;

    $1b:
    begin
      OutputString := '<left>'; // I guess...
      Spacing:= true;
    end

    else
    begin
      // Unknown/unhandled code
      DebugString := DebugString + ' ?';
    end;
  end; // case escape code of...
  inc( pData, EscapeLen );
end;

// Main translation function. Turns the IPF data into
// a text string. Translates formatting codes into tags
// as for Rich Text Viewer.
// Uses TAString for speed without length limits
// - string is too short
// - PChar is slow to concatenate (unless you keep track of the insert point)
// - AnsiString is slow
procedure TTopic.GetText( HighLightWords: Int32ArrayPointer;
                          ShowCodes: boolean;
                          Var Text: PChar;
                          ImageOffsets: TList );
var
  SlotIndex: integer;
  Slot: THelpTopicSlot;
  pData: pInt8;
  pSlotEnd: pInt8;

  S: string;
  Word: string;

  GlobalDictIndex: int32;

  Spacing: boolean;
  InFixedFont: boolean;

  WordsOnLine: longint;

  StringToAdd: string;
  LocalDictIndex: int8;
  DebugString: string;
  EscapeDebugString: string;

  LinkIndex: longint;
begin
  if Links = nil then
    Links:= TList.Create;

  S := '';
{  Text:= StrNew( S.AsPChar );
  S.Destroy;
  exit;}

  ImageOffsets.Clear;

  LinkIndex:= 0;

  InFixedFont:= false;  // ? Not sure... this could be reset at start of slot

  WordsOnLine:= 0;

  for SlotIndex := 0 to _NumSlots - 1 do
  begin
    Spacing:= true;
    Slot := THelpTopicSlot(_Slots[ SlotIndex ]);

    pData := Slot.pData;

    pSlotEnd := pData + Slot.Size;

    while pData < pSlotEnd do
    begin
      LocalDictIndex := pData^;

      StringToAdd := '';

      if LocalDictIndex < Slot.LocalDictSize then
      begin
        // Normal word lookup
        GlobalDictIndex := Slot.pLocalDictionary^[ LocalDictIndex ];

        // normal lookup
        if GlobalDictIndex < _GlobalDictionary.Count then
          Word := _GlobalDictionary[ GlobalDictIndex ]
        else
          Word := '';

        Word:= SubstituteAngleBrackets( Word );
        if HighlightWords^[ GlobalDictIndex ] > 0 then
          StringToAdd := '<red>' + Word + '<black>'
        else
          StringToAdd := Word;

        if Spacing then
          StringToAdd := StringToAdd + ' ';

        inc( WordsOnLine );
        inc( pData );
      end
      else
      begin
        // special code
        DebugString := '[' + IntToHex( LocalDictIndex, 2 );
        case LocalDictIndex of
          IPF_END_PARA:
          begin
            if WordsOnLine > 0 then
              StringToAdd := #10 + #10
            else
              StringToAdd := #10;
            if not InFixedFont then
              Spacing:= true;
            WordsOnLine:= 0;
            inc( pData );
          end;

          IPF_CENTER:
          begin
            StringToAdd := #10 + '<center>';
            inc( pData );
          end;

          IPF_INVERT_SPACING:
          begin
            Spacing:= not Spacing;
            inc( pData );
          end;

          IPF_LINEBREAK:
          begin
            StringToAdd := #10;
            if not InFixedFont then
              Spacing:= true;
            WordsOnLine:= 0;
            inc( pData );
          end;

          IPF_SPACE:
          begin
            StringToAdd := ' ';
            inc( pData );
          end;

          IPF_ESC:
          begin
            // escape sequence
            inc( pData );
            TranslateIPFEscapeCode( pData,
                                    StringToAdd,
                                    EscapeDebugString,
                                    Spacing,
                                    InFixedFont,
                                    WordsOnLine,
                                    ImageOffsets,
                                    LinkIndex );

            DebugString := DebugString + ' ' + EscapeDebugString;
          end // case code of ff:

          else
          begin
            // Unrecongised code
            DebugString := DebugString + '?';
            inc( pData );
          end;

        end; // case code of...
        DebugString := DebugString + ']';
        if ShowCodes then
          S := S + DebugString;
      end;

      S := S + StringToAdd;
    end; // for slotindex = ...
  end;

  Text := PChar(S);
end;

function TTopic.SearchForWord( DictIndex: integer;
                               StopAtFirstOccurrence: boolean )
  : longint;
var
  SlotIndex: integer;
  Slot: THelpTopicSlot;
  pData: pInt8;
  pSlotEnd: pInt8;

  EscapeLen: longint;

  GlobalDictIndex: int32;

  LocalDictIndex: int8;
begin
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
        GlobalDictIndex:= Slot.pLocalDictionary^[ LocalDictIndex ];

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
        if GlobalDictIndex = $ff then
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

function TTopic.CountWord( DictIndex: integer ): longint;
begin
  Result := SearchForWord( DictIndex, false );
end;

function TTopic.ContainsWord( DictIndex: integer ): boolean;
begin
  Result := SearchForWord( DictIndex, true ) > 0;
end;

procedure TTopic.GetContentsWindowRect( ContentsRect: THelpWindowRect );
var
  extendedinfo: TExtendedTOCEntry;
  XY: THelpXYPair;
  p: pbyte;

  Flags: byte;
begin
  Flags:= _pTOCEntry ^.flags;
  p:= pByte( _pTOCEntry + sizeof( TTOCEntryStart ) );

  ContentsRect.Left:= 0;
  ContentsRect.Bottom:= 0;
  ContentsRect.Width:= 100;
  ContentsRect.Height:= 100;

  if ( Flags and TOCEntryExtended ) > 0 then
  begin
    ExtendedInfo.w1:= p^;
    ExtendedInfo.w2:= ( p+1) ^;
    inc( p, sizeof( ExtendedInfo ) );

    if (  ExtendedInfo.w1 and 1 ) > 0 then
    begin
      // read origin
      XY:= pHelpXYPair( p )^;
      inc( p, sizeof( XY ) );
      ReadHelpPosition( XY, ContentsRect );
    end;
    if ( ExtendedInfo.w1 and 2 ) > 0 then
    begin
      // read size
      XY:= pHelpXYPair( p )^;
      inc( p, sizeof( XY ) );
      ReadHelpSize( XY, ContentsRect );
    end;
  end;
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

Initialization
End.
