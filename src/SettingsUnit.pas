Unit SettingsUnit;

{$mode objfpc}{$H+}

// NewView - a new OS/2 Help Viewer
// Copyright 2003 Aaron Lawrence (aaronl at consultant dot com)
// This software is released under the Gnu Public License - see readme.txt

Interface

// Defines settings (options) in a record and contains functions
// for loading and saving them to ini file.

Uses
  Classes
  ,fpg_base
  ,fpg_main
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

  VGADefaultColors: array[ 0 .. NumColorSettings - 1 ] of TfpgColor
   = ( clBoxColor,
       clText1,
       clText1,
       clBoxColor,
       clText1,
       clBoxColor,
       clText1,
       clBoxColor,
       clText1,
       clBoxColor,
       clGreen,
       clYellow );

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

  DefaultTopicFont = 'Arial-10';
  DefaultTopicFixedFont = 'Courier New-10';


Type
  TIndexStyle = ( isFileOnly, isAlphabetical, isFull );
  TToolbarStyle = ( tsNone, tsImages, tsText, tsImagesAndText );
  TGlobalSearchLocation = ( gsHelpPaths, gsFixedDrives, gsSelectedHelpPaths, gsCustom );

  TMRUItem = class(TObject)
  public
    Title: string;
    Filenames: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;


  TSettings = record
    MRUList: TList;
    LastOpenDirectory: string;
    LastSaveDirectory: string;
    StartupHelp: boolean;
    LeftPanelWidth: longint;
    ShowLeftPanel_Help: boolean;
    ShowLeftPanel_Standalone: boolean;
    FileDialogSplit: Double;
    Colors: array[ 0..NumColorSettings - 1 ] of TfpgColor;
    NormalFont: TfpgFont;
    FixedFont: TfpgFont;
    Fonts: array[ 0..NumFontSettings - 1 ] of TfpgFont;
    FixedFontSubstitution: boolean;
    FixedFontSubstitutes: string;
    IndexStyle: TIndexStyle;
    SmoothScrolling: boolean;
    UseOriginalDialogs: boolean;
    OpenWithExpandedContents: boolean;
    ToolbarBackgroundImageFilename: string;
    ToolbarStyle: TToolbarStyle;
    ConfirmWinHelp: boolean;
    GlobalSearchLocation: TGlobalSearchLocation;
    SearchDirectories: TStringList;
  end;

// global procs
procedure LoadSettings;
procedure SaveSettings;
procedure writeSettingsDetailsTo(aStrings : TStrings);
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

Const
  IniFileName = 'NewView.ini';
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
  MRUFileTitle: string;
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

      ShowLeftPanel_Help := ReadBool( GeneralSection, 'ShowLeftPanel_Help', true );
      ShowLeftPanel_Standalone := ReadBool( GeneralSection, 'ShowLeftPanel_Standalone', true );

      // Colours
      for ColorIndex := 0 to High( Colors ) do
      begin
        //if GetScreenColorDepth > 8 then
           DefaultColor := DefaultColors[ ColorIndex ];
        //else
        //   DefaultColor := VGADefaultColors[ ColorIndex ];
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
          MRUItem.Destroy;
          MRUItem := nil;
        end;
      end;

      // Fonts
      NormalFont := fpgGetFont(ReadString(FontsSection, 'NormalFont', DefaultTopicFont));
      if NormalFont = nil then
        NormalFont := fpgStyle.DefaultFont;

      FixedFont := fpgGetFont(ReadString(FontsSection, 'FixedFont', DefaultTopicFixedFont));
      if FixedFont = nil then
        FixedFont := fpgStyle.FixedFont;

      for i := 0 to NumFontSettings - 1 do
      begin
        FontName := 'Font' + IntToStr( i );
        Fonts[ i ] := nil;
        if ReadBool( FontsSection, FontName + 'Customised', false ) then
          Fonts[ i ] := fpgGetFont(ReadString(FontsSection, FontName + 'Desc', DefaultTopicFont));
      end;

      FixedFontSubstitution := ReadBool( FontsSection, 'FixedFontSubstitution', true );
      FixedFontSubstitutes := ReadString( FontsSection, 'FixedFontSubstitutes', 'Mono-10' );

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

      WriteBool( GeneralSection, 'ShowLeftPanel_Help', ShowLeftPanel_Help );
      WriteBool( GeneralSection, 'ShowLeftPanel_Standalone', ShowLeftPanel_Standalone );

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
      WriteString( FontsSection, 'NormalFont', NormalFont.FontDesc );
      WriteString( FontsSection, 'FixedFont', FixedFont.FontDesc );
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
    // is already in list, move to top of list
    MRUItem := TMRUItem(Settings.MRUList[ PreviousMRUIndex ]);
    Settings.MRUList.Delete( PreviousMRUIndex );
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
    MRUItem := TMRUItem(Settings.MRUList[ MaxMRUListEntries ]);
    Settings.MRUList.Delete( MaxMRUListEntries );
    MRUItem.Destroy;
  end;
end;

procedure writeSettingsDetailsTo(aStrings : TStrings);
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
  aStrings.Add('ShowLeftPanel_Help: ' + boolToStr(Settings.ShowLeftPanel_Help));
  aStrings.Add('ShowLeftPanel_Standalone: ' + boolToStr(Settings.ShowLeftPanel_Standalone));
  // FileDialogSplit: real;
  // Colors: array[ 0..NumColorSettings - 1 ] of TColor;
  // NormalFont: TFont;
  // FixedFont: TFont;
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
  // GlobalSearchLocation: TGlobalSearchLocation;
  // SearchDirectories: TStringList;
end;


Initialization
  //Settings.NormalFont := fpgStyle.DefaultFont;
  //Settings.FixedFont := fpgStyle.FixedFont;
  //Settings.SearchDirectories := TStringList.Create;

Finalization
  Settings.NormalFont.Free;
  Settings.FixedFont.Free;
  Settings.SearchDirectories.Free;

End.
