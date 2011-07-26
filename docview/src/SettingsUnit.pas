unit SettingsUnit;

{$mode objfpc}{$H+}

interface

// Defines settings (options) in a record and contains functions
// for loading and saving them to ini file.

Uses
  Classes
  ,contnrs
  ,fpg_base
  ,fpg_main
  ,CanvasFontManager
  ,HelpFile
  ;

Const
  ContentsBackgroundColorIndex          = 0;
  ContentsTextColorIndex                = 1;
  ContentsLinesColorIndex               = 2;
  IndexBackgroundColorIndex             = 3;
  IndexTextColorIndex                   = 4;
  SearchBackgroundColorIndex            = 5;
  SearchTextColorIndex                  = 6;
  NotesListBackgroundColorIndex         = 7;
  NotesListTextColorIndex               = 8;
  TopicBackgroundColorIndex             = 9;
  NotesTextColorIndex                   = 10;
  SearchHighlightTextColorIndex         = 11;
  NumColorSettings                      = 12;

  // already defined, but these values are slightly different
  //clLightYellow = $ffffc0;
  //clLightBlue = $e0e0ff;
  //clLightCyan = $c0ffff;
  //clLightGreen = $e0ffe0;

  DefaultColors: array[ 0 .. NumColorSettings - 1 ] of TfpgColor
   = ( clLightCyan,
       clBlack,
       clBlue,
       clLightGreen,
       clBlack,
       clLightBlue,
       clBlack,
       clWhite,
       clBlack,
       clWhite,
       clGreen,
       clYellow );

  ApplicationFontIndex = 0;
  NumFontSettings = 1;


type
  TIndexStyle = ( isAlphabetical, isFileOnly, isFull );
  TToolbarStyle = ( tsNone, tsImages, tsText, tsImagesAndText );
  TGlobalSearchLocation = ( gsHelpPaths, gsFixedDrives, gsSelectedHelpPaths, gsCustom );


  TMRUItem = class(TObject)
  public
    Title: string;
    Filenames: TStringList;
    constructor Create;
    destructor  Destroy; override;
  end;


  TSettings = record
    MRUList: TObjectList;
    LastOpenDirectory: string;
    LastSaveDirectory: string;
    StartupHelp: boolean;
    LeftPanelWidth: longint;
    ShowLeftPanel: boolean;
    ScrollDistance: integer;
    FileDialogSplit: Double;
    Colors: array[ 0..NumColorSettings - 1 ] of TfpgColor;
    NormalFontDesc: TfpgString;
    FixedFontDesc: TfpgString;
    Fonts: array[ 0..NumFontSettings - 1 ] of TfpgFont;
    FixedFontSubstitution: boolean;
    FixedFontSubstitutes: string;  // semi-colon seperated list of INF fonts eg: 'Courier 10x12;Mono 8x10'
    IndexStyle: TIndexStyle;
    SmoothScrolling: boolean;
    UseOriginalDialogs: boolean;
    OpenWithExpandedContents: boolean;
    ToolbarBackgroundImageFilename: string;
    ToolbarStyle: TToolbarStyle;
    ConfirmWinHelp: boolean;
    GlobalSearchLocation: TGlobalSearchLocation;
    SearchDirectories: TStringList;
    IPFTopicSaveAsEscaped: boolean;
    Encoding: TFontEncoding;
  end;


// global procs
procedure LoadSettings;
procedure SaveSettings;
procedure WriteSettingsDetailsTo(aStrings : TStrings);
procedure AddToMRUList( const Title: string; Filenames: TStrings );

var
  Settings: TSettings;

Implementation

Uses
  SysUtils
  ,fpg_iniutils
  ,ACLStringUtility
  ,nvUtilities
  ;

const
  GeneralSection = 'General';
  FontsSection = 'Fonts';
  ColoursSection = 'Colours';
  MRUSection = 'RecentFiles';
  MRUItemBaseSection = 'RecentFile';
  SearchSection = 'Search';

  DefaultWidth = 620;
  DefaultHeight = 460;

  MaxMRUListEntries = 9;

constructor TMRUItem.Create;
begin
  Title := '';
  Filenames := TStringList.Create;
end;

destructor TMRUItem.Destroy;
begin
  Filenames.Free;
  inherited Destroy;
end;

Procedure LoadSettings;
var
  ColorIndex: longint;
  DefaultColor: TfpgColor;
  FontName: string;
  SettingString: string;
  MRUItem: TMRUItem;
  MRUItemSection: string;
  MRUItemFileCount: longint;
  MRUItemFileIndex: longint;
  i: longint;
  Count: longint;
  MRUFilename: string;
//  MRUFileTitle: string;
begin
  LogEvent(LogSettings, 'LoadSettings' );
  with gINI do
  begin
    EraseSection( 'Windows' );
    with Settings do
    begin
      LastOpenDirectory := ReadString( GeneralSection, 'LastOpenDirectory', '' );
      LastSaveDirectory := ReadString( GeneralSection, 'LastSaveDirectory', '' );

      // Read split points, as units of 0.1%
      LeftPanelWidth := ReadInteger( GeneralSection, 'LeftPanelWidth', 225 );
      FileDialogSplit := ReadInteger( GeneralSection, 'FileDialogSplit', 500 ) / 1000;

      ShowLeftPanel := ReadBool( GeneralSection, 'ShowLeftPanel', true );

      ScrollDistance := ReadInteger(GeneralSection, 'ScrollDistance', 75);

      // Colours
      for ColorIndex := 0 to High( Colors ) do
      begin
        DefaultColor := DefaultColors[ ColorIndex ];
        Colors[ ColorIndex ] := ReadInteger( ColoursSection,
                                             'Color' + IntToStr( ColorIndex ),
                                             DefaultColor );
      end;

      // Most Recently Used files list...
      Count := ReadInteger( MRUSection, 'Count', 0 );
      for i := 0 to Count - 1 do
      begin
        MRUItemSection := MRUItemBaseSection + IntToStr( i );
        MRUItem := TMRUItem.Create;
        MRUItem.Title := ReadString( MRUItemSection, 'Title', '' );
        MRUItemFileCount := ReadInteger( MRUItemSection, 'FileCount', 0 );
        for MRUItemFileIndex := 0 to MRUItemFileCount - 1 do
        begin
          MRUFilename := ReadString( MRUItemSection,
                                     'File' + IntToStr( MRUItemFileIndex ),
                                     '' );
          if MRUFilename <> '' then
            MRUItem.Filenames.Add( MRUFilename );
        end;
        if ( MRUItem.Title <> '' ) and ( MRUItem.Filenames.Count > 0 ) then
          // valid MRU item
          MRUList.Add( MRUItem )
        else
        begin
          // not valid
          MRUItem.Free;
          MRUItem := nil;
        end;
      end;

      // Fonts
      NormalFontDesc := ReadString(FontsSection, 'NormalFont', DefaultTopicFont);
      FixedFontDesc := ReadString(FontsSection, 'FixedFont', DefaultTopicFixedFont);

      for i := 0 to NumFontSettings - 1 do
      begin
        FontName := 'Font' + IntToStr( i );
        Fonts[ i ] := nil;
        if ReadBool( FontsSection, FontName + 'Customised', false ) then
          Fonts[ i ] := fpgGetFont(ReadString(FontsSection, FontName + 'Desc', DefaultTopicFont));
      end;

      FixedFontSubstitution := ReadBool( FontsSection, 'FixedFontSubstitution', true );
      FixedFontSubstitutes := ReadString( FontsSection, 'FixedFontSubstitutes', 'Courier 18x12' );

      // Index style
      SettingString := ReadString( GeneralSection, 'IndexStyle', 'Full' );
      if SameText( SettingString, 'FileOnly' ) then
        IndexStyle := isFileOnly
      else if Sametext( SettingString, 'Alphabetical' ) then
        IndexStyle := isAlphabetical
      else
        IndexStyle := isFull;

      StartupHelp := ReadBool( GeneralSection, 'StartupHelp', true );

      SmoothScrolling := ReadBool( GeneralSection, 'SmoothScrolling', true );
//      UseOriginalDialogs := ReadBool( GeneralSection, 'UseOriginalDialogs', false );
      OpenWithExpandedContents := ReadBool( GeneralSection, 'OpenWithExpandedContents', false );

//      ToolBarBackgroundImageFilename := ReadString( GeneralSection, 'ToolbarBackground', '' );
      SettingString := ReadString( GeneralSection, 'ToolbarStyle', 'ImagesAndText' );

      if SameText( SettingString, 'None' ) then
        ToolbarStyle := tsNone
      else if SameText( SettingString, 'Images' ) then
        ToolbarStyle := tsImages
      else if SameText( SettingString, 'Text' ) then
        ToolbarStyle := tsText
      else
        ToolbarStyle := tsImagesAndText;

      ConfirmWinHelp := ReadBool( GeneralSection, 'ConfirmWinHelp', true );
      IPFTopicSaveAsEscaped := ReadBool(GeneralSection, 'IPFTopicSaveAsEscaped', true);

      Count := ReadInteger( SearchSection, 'CustomDirCount', 0 );

      SearchDirectories.Clear;
      for i := 0 to Count - 1 do
      begin
        SettingString := ReadString( SearchSection,
                                     'CustomDir' + IntToStr( i ),
                                     '' );
        if trim( SettingString ) <> '' then
          SearchDirectories.Add( SettingString );
      end;
      SettingString := ReadString( SearchSection,
                                   'Location',
                                   'HelpPaths' );
      if SameText( SettingString, 'HelpPaths' ) then
        GlobalSearchLocation := gsHelpPaths
      else if SameText( SettingString, 'FixedDrives' ) then
        GlobalSearchLocation := gsFixedDrives
      else if SameText( SettingString, 'SelectedHelpPaths' ) then
        GlobalSearchLocation := gsSelectedHelpPaths
      else
        GlobalSearchLocation := gsCustom;

      Encoding := encUTF8;
    end;
  end;
  LogEvent(LogSettings, ' Done' );
end;

procedure SaveSettings;
var
  ColorIndex: longint;
  FontIndex: longint;
  FontName: string;
  i: longint;
  MRUItemFileIndex: longint;
  SettingString: string;
  MRUItem: TMRUItem;
  MRUItemSection: string;
begin
  LogEvent(LogSettings, 'SaveSettings' );
  with gINI do
  begin
    with Settings do
    begin
      WriteString( GeneralSection, 'LastOpenDirectory', LastOpenDirectory );
      WriteString( GeneralSection, 'LastSaveDirectory', LastSaveDirectory );

      WriteInteger( GeneralSection, 'LeftPanelWidth', LeftPanelWidth );
      // Write split points, as units of 0.1%
      WriteInteger( GeneralSection, 'FileDialogSplit', Round( FileDialogSplit * 1000 ) );

      WriteBool( GeneralSection, 'ShowLeftPanel', ShowLeftPanel);
      WriteInteger(GeneralSection, 'ScrollDistance', ScrollDistance);

      // Colours
      for ColorIndex := 0 to High( Colors ) do
        WriteInteger( ColoursSection,
                      'Color' + IntToStr( ColorIndex ),
                      Colors[ ColorIndex ] );

      // MRU files
      WriteInteger( MRUSection, 'Count', MRUList.Count );
      for i := 0 to MRUList.Count - 1 do
      begin
        MRUItem := TMRUItem(MRUList[ i ]);
        MRUItemSection := MRUItemBaseSection + IntToStr( i );
        EraseSection( MRUItemSection );
        WriteString( MRUItemSection, 'Title', MRUItem.Title );
        WriteInteger( MRUItemSection, 'FileCount', MRUItem.Filenames.Count );
        for MRUItemFileIndex := 0 to MRUItem.Filenames.Count - 1 do
          WriteString( MRUItemSection,
                       'File' + IntToStr( MRUItemFileIndex ),
                       MRUItem.Filenames[ MRUItemFileIndex ] );
      end;

      // clear unused sections
      for i := MRUList.Count to MaxMRUListEntries do
      begin
        MRUItemSection := MRUItemBaseSection + IntToStr( i );
        EraseSection( MRUItemSection );
      end;

      // Fonts
      WriteString( FontsSection, 'NormalFont', NormalFontDesc );
      WriteString( FontsSection, 'FixedFont', FixedFontDesc );
      for FontIndex := 0 to NumFontSettings - 1 do
      begin
        FontName := 'Font' + IntToStr( FontIndex );
        WriteBool( FontsSection, FontName + 'Customised', Fonts[ FontIndex ] <> nil );
        if Fonts[ FontIndex ] <> nil then
          WriteString( FontsSection, FontName + 'Desc', Fonts[ FontIndex ].FontDesc );
      end;

      WriteBool( FontsSection, 'FixedFontSubstitution', FixedFontSubstitution );
      WriteString( FontsSection, 'FixedFontSubstitutes', FixedFontSubstitutes );

      case IndexStyle of
        isFileOnly:
          SettingString := 'FileOnly';
        isAlphabetical:
          SettingString := 'Alphabetical';
        isFull:
          SettingString := 'Full';
      end;
      WriteString( GeneralSection, 'IndexStyle', SettingString );

      WriteBool( GeneralSection, 'StartupHelp', StartupHelp );
      WriteBool( GeneralSection, 'SmoothScrolling', SmoothScrolling );
//      WriteBool( GeneralSection, 'UseOriginalDialogs', UseOriginalDialogs );
      WriteBool( GeneralSection, 'OpenWithExpandedContents', OpenWithExpandedContents );
//      WriteString( GeneralSection, 'ToolbarBackground', ToolbarBackgroundImageFilename );

      case ToolbarStyle of
        tsNone:
          SettingString := 'None';
        tsImages:
          SettingString := 'Images';
        tsText:
          SettingString := 'Text';
        tsImagesAndText:
          SettingString := 'ImagesAndText';
      end;

      WriteString( GeneralSection, 'ToolbarStyle', SettingString );
      WriteBool( GeneralSection, 'ConfirmWinHelp', ConfirmWinHelp );
      WriteBool( GeneralSection, 'IPFTopicSaveAsEscaped', IPFTopicSaveAsEscaped);
      WriteInteger( SearchSection, 'CustomDirCount', SearchDirectories.Count );

      SearchDirectories.Sorted := true;
      SearchDirectories.CaseSensitive := True;
      SearchDirectories.Duplicates := dupIgnore;

      for i := 0 to SearchDirectories.Count - 1 do
      begin
        WriteString( SearchSection,
                     'CustomDir' + IntToStr( i ),
                     SearchDirectories[ i ] );
      end;

      case GlobalSearchLocation of
        gsHelpPaths:
          SettingString := 'HelpPaths';

        gsFixedDrives:
          SettingString := 'FixedDrives';

        gsSelectedHelpPaths:
          SettingString := 'SelectedHelpPaths';

        gsCustom:
          SettingString := 'Custom';
      end;

      WriteString( SearchSection, 'Location', SettingString );
    end;
  end;
  LogEvent(LogSettings, ' Done' );
End;

Procedure AddToMRUList( const Title: string; Filenames: TStrings );
var
  MRUIndex: longint;
  PreviousMRUIndex: longint;
  MRUItem: TMRUItem;
begin
  PreviousMRUIndex := -1;
  for MRUIndex := 0 to Settings.MRUList.Count - 1 do
  begin
    MRUItem := TMRUItem(Settings.MRUList[ MRUIndex ]);
    if     ( MRUItem.Title = Title )
       and ( MRUItem.Filenames.Equals( Filenames ) ) then
    begin
      // found identical entry in the list already.
      PreviousMRUIndex := MRUIndex;
      break;
    end;
  end;

  if PreviousMRUIndex > -1 then
  begin
    // is already in list. Get instance so we can move it to the top of list
    MRUItem := TMRUItem(Settings.MRUList[PreviousMRUIndex]);
    Settings.MRUList.Extract(MRUItem);
  end
  else
  begin
    // not yet in list. Create new
    MRUItem := TMRUItem.Create;
    MRUItem.Title := Title;
    MRUItem.Filenames.Assign( Filenames );
  end;

  Settings.MRUList.Insert( 0, MRUItem );
  while Settings.MRUList.Count > MaxMRUListEntries do
  begin
    Settings.MRUList.Remove(Settings.MRUList.Last);
  end;
end;

procedure WriteSettingsDetailsTo(aStrings : TStrings);
Begin
  aStrings.Add('');
  aStrings.Add('---- Settings ----');
  aStrings.Add('info: Screenwidth=' + IntToStr(fpgApplication.ScreenWidth));
  aStrings.Add('info: Screenheight=' + IntToStr(fpgApplication.ScreenHeight));
  aStrings.Add('info: dpi=' + IntToStr(fpgApplication.Screen_dpi));

  aStrings.Add('LastOpenDirectory: ' + Settings.LastOpenDirectory);
  aStrings.Add('LastSaveDirectory: ' + Settings.LastSaveDirectory);
  aStrings.Add('StartupHelp:       ' + boolToStr(Settings.StartupHelp));
  // LeftPanelWidth: longint;
  aStrings.Add('ShowLeftPanel: ' + boolToStr(Settings.ShowLeftPanel));
  aStrings.Add('ScrollDistance: ' + IntToStr(Settings.ScrollDistance));
  // FileDialogSplit: real;
  // Colors: array[ 0..NumColorSettings - 1 ] of TColor;
  aStrings.Add('NormalFont: ' +  Settings.NormalFontDesc);
  aStrings.Add('FixedFont: ' + Settings.FixedFontDesc);
  // Fonts: array[ 0..NumFontSettings - 1 ] of TFont;
  aStrings.Add('FixedFontSubstitution: ' + boolToStr(Settings.FixedFontSubstitution));
  aStrings.Add('FixedFontSubstitutes: ' + Settings.FixedFontSubstitutes);
  // IndexStyle: TIndexStyle;
  aStrings.Add('SmoothScrolling: ' + boolToStr(Settings.SmoothScrolling));
//  aStrings.Add('UseOriginalDialogs: ' + boolToStr(Settings.UseOriginalDialogs));
  aStrings.Add('OpenWithExpandedContents: ' + boolToStr(Settings.OpenWithExpandedContents));
//  aStrings.Add('ToolbarBackgroundImageFilename: ' + Settings.ToolbarBackgroundImageFilename);
  // ToolbarStyle: TToolbarStyle;
  aStrings.Add('ConfirmWinHelp: ' + boolToStr(Settings.ConfirmWinHelp));
  aStrings.Add('IPFTopicSaveAsEscaped: ' + BoolToStr(Settings.IPFTopicSaveAsEscaped));
  // GlobalSearchLocation: TGlobalSearchLocation;
  // SearchDirectories: TStringList;
end;


Initialization
  Settings.MRUList := TObjectList.Create;
  Settings.SearchDirectories := TStringList.Create;

Finalization
  Settings.SearchDirectories.Free;
  Settings.MRUList.Free;

End.
