Unit CanvasFontManager;

{$mode objfpc}{$H+}

Interface

Uses
  Classes
  ,fpg_base
  ,fpg_main
  ,fpg_widget
  ;

Const
  // This defines the fraction of a pixel that
  // font character widths will be given in
  FontWidthPrecisionFactor = 1; //256;  // 256 seems to be specific to OS/2 API

Type
  {Standard Font types}
  TFontType=(ftBitmap,ftOutline);

  {Standard Font Attributes}
  TFontAttributes=Set Of(faItalic,faUnderScore,faOutline,faStrikeOut,faBold);

  {Standard Font pitches}
  TFontPitch=(fpFixed,fpProportional);

  {Standard Font character Set}
  TFontCharSet=(fcsSBCS,fcsDBCS,fcsMBCS);  {Single,Double,mixed Byte}

  // a user-oriented specification of a font;
  TFontSpec = record
    FaceName: string[ 64 ];
    PointSize: integer; // if 0 then use x/y size
    XSize: integer;
    YSize: integer;
    Attributes: TFontAttributes; // set of faBold, faItalic etc
  end;

  // NOTE: Char widths are in 1/FontWidthPrecisionFactor units
  TCharWidthArray = array[ #0..#255 ] of longint;
  TPCharWidthArray = ^TCharWidthArray;

  // Used internally for storing full info on font
  TLogicalFont= Class( TfpgComponent )
  public
    FaceName: String; // user-selected name
    UseFaceName: String; // after substitutions.

    // Selected bits of FONTMETRICS
    fsSelection: word;

    FontType: TFontType;
    FixedWidth: boolean;
    PointSize: integer;
    ID: integer;
    Attributes: TFontAttributes;

    // this can be nil if not already fetched
    pCharWidthArray: TPCharWidthArray;
    lMaxbaselineExt: longint;
    lAveCharWidth: longint;
    lMaxCharInc: longint;
    lMaxDescender: longint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


  TFontFace = class(TObject)
  public
    Name: string;
    FixedWidth: boolean;
    FontType: TFontType;
    Sizes: TList; // relevant for bitmap fonts only - contains TLogicalFont objects
    constructor Create;
    destructor  Destroy; override;
  end;


  TCanvasFontManager = class(TObject)
  private
    FWidget: TfpgWidget;
  protected
    FCanvas: TfpgCanvas;
    FLogicalFonts: TList;
    FCurrentFontSpec: TFontSpec;
    FDefaultFontSpec: TFontSpec;
    FCurrentFont: TLogicalFont;
    FAllowBitmapFonts: boolean;
  protected
    function    CreateFont( const FontSpec: TFontSpec ): TLogicalFont;
    function    GetFont( const FontSpec: TFontSpec ): TLogicalFont;
    procedure   RegisterFont( Font: TLogicalFont );
    procedure   SelectFont( Font: TLogicalFont; Scale: longint );
    // Retrieve character widths for current font
    procedure   LoadMetrics;
    // load metrics if needed
    procedure   EnsureMetricsLoaded;
  public
    constructor Create( Canvas: TfpgCanvas; AllowBitmapFonts: boolean; AWidget: TfpgWidget ); reintroduce;
    destructor  Destroy; override;
    // Set the font for the associated canvas.
    procedure   SetFont( const FontSpec: TFontSpec );
    // Retrieve the width of the given char, in the current font
    function    CharWidth( const C: Char ): longint;
    function    AverageCharWidth: longint;
    function    MaximumCharWidth: longint;
    function    IsFixed: boolean;
    function    CharHeight: longint;
    function    CharDescender: longint;
    procedure   DrawString(var Point: TPoint; const Length: longint; const S: PChar);
    property    Canvas: TfpgCanvas read FCanvas;
    property    Widget: TfpgWidget read FWidget;
    property    DefaultFontSpec: TFontSpec read FDefaultFontSpec write FDefaultFontSpec;
  end;


// Convert a Sibyl font to a FontSpec (Color is left the same)
procedure FPGuiFontToFontSpec( Font: TfpgFont; Var FontSpec: TFontSpec );

  // Thoughts on how it works....

  // SelectFont looks for an existing logical font that
  // matches the request. If found selects that logical font
  // onto the canvas.

  // If not found it creates a logical font and selects that onto
  // the canvas.

  // For bitmap fonts the logical font definition includes pointsize
  // For outline fonts the defn is only face+attr; in this case
  // selectfont also ses the 'CharBox' according to the point size.
Implementation

uses
//  PMWin, PMGpi, OS2Def, PmDev,
  SysUtils
  ,ACLStringUtility
  ;

{
Imports
  Function GpiQueryCharStringPosAt( PS: Hps;
                              StartPoint: PPointL;
                              Options: ULONG;
                              Count: LONG;
                              TheString: PChar;
                              IncrementsArray: PLONG;
                              CharacterPoints: PPointL ): BOOL;
    ApiEntry; 'PMGPI' Index 585;
  Function GpiQueryCharStringPos( PS: Hps;
                              Options: ULONG;
                              Count: LONG;
                              TheString: PChar;
                              IncrementsArray: PLONG;
                              CharacterPoints: PPointL ): BOOL;
    ApiEntry; 'PMGPI' Index 584;
end;
}

Type
  // A little pretend window to send font name.size
  // and get definite font info back. (See .CreateFont)
  TFontWindow = class( TfpgWidget )
  protected
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); override;
  public
//    procedure CreateWnd; override;
    function SetPPFontNameSize( Const FNS: String ): Boolean;
  end;

var
  FontFaces: TList = nil; // of TFontface
  //FontWindow: TFontWindow;

  DefaultOutlineFixedFace: TFontFace;
  DefaultOutlineProportionalFace: TFontFace;

// TFontFace
//------------------------------------------------------------------------

constructor TFontface.Create;
begin
  Sizes := TList.Create;
  FontType := ftOutline; // in fpGUI we treat all fonts as scalable (preference)
end;

destructor TFontface.Destroy;
begin
  Sizes.Destroy;
end;

// TLogicalFont
//------------------------------------------------------------------------

constructor TLogicalFont.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FontType := ftOutline;
  PointSize := 10;
  Attributes := [];
  FixedWidth := False;
end;

// frees allocated memory, if any.
// Note - does not delete the Gpi Logical Font
destructor TLogicalFont.Destroy;
begin
  if pCharWidthArray <> nil then
    FreeMem( pCharWidthArray,
             sizeof( TCharWidthArray ) );

  inherited Destroy;
end;

// TFontWindow
//------------------------------------------------------------------------

procedure TFontWindow.DoAllocateWindowHandle(AParent: TfpgWindowBase);
begin
  inherited DoAllocateWindowHandle(AParent);
end;

Function TFontWindow.SetPPFontNameSize( Const FNS: String ): Boolean;
//Var
//  CS: Cstring;
Begin
  Result := True;
  { TODO -ograemeg -cAPI call : port API to fpGUI }
  //CS := FNS;
  //Result := WinSetPresParam( Handle,
  //                           PP_FONTNAMESIZE,
  //                           Length( CS ) + 1,
  //                           CS );
End;

//------------------------------------------------------------------------

// Convert a fpGUI Toolkit font to a FontSpec
//------------------------------------------------------------------------
procedure FPGuiFontToFontSpec( Font: TfpgFont; Var FontSpec: TFontSpec );
var
  s: string;
  facename: string;
  cp: integer;
  c: char;
  token: string;
  prop, propval: string;
  desc: string;

  function NextC: char;
  begin
    Inc(cp);
    if cp > length(desc) then
      c := #0
    else
      c := desc[cp];
    Result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ', 'a'..'z', 'A'..'Z', '_', '0'..'9']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
  cp := 0;
  desc := Font.FontDesc;
  // find fontface
  NextC;
  NextToken;
  FontSpec.FaceName := token;
  FontSpec.Attributes := [];
  FontSpec.XSize := Font.TextWidth('v');
  FontSpec.YSize := Font.Height;

  // find font size
  if c = '-' then
  begin
    NextC;
    NextToken;
    FontSpec.PointSize := StrToIntDef(token, 10);
  end;

  // find font attributes
  while c = ':' do
  begin
    NextC;
    NextToken;
    prop    := UpperCase(token);
    propval := '';

    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;
    // convert fontdesc attributes to fontspec attributes
    if prop = 'BOLD' then
      include(FontSpec.Attributes, faBold)
    else if prop = 'ITALIC' then
      include(FontSpec.Attributes, faItalic)
    else if prop = 'UNDERLINE' then
      include(FontSpec.Attributes, faUnderScore)
  end;
end;

// Find a font face with the given name
//------------------------------------------------------------------------
function FindFaceName( const name: string ): TFontFace;
Var
  FaceIndex: LongInt;
  Face: TFontFace;
begin
  for FaceIndex := 0 to FontFaces.Count - 1 do
  begin
    Face := TFontFace(FontFaces[ FaceIndex ]);

    if StringsSame( Face.Name, Name ) then
    begin
      Result := Face;
      exit;
    end;
  end;
  Result := nil;
end;

// Return the first font face of type = Outline (scalable)
//------------------------------------------------------------------------
function GetFirstOutlineFace( FixedWidth: boolean ): TFontFace;
Var
  FaceIndex: LongInt;
  Face: TFontFace;
begin
  for FaceIndex := 0 to FontFaces.Count - 1 do
  begin
    Face := TFontFace(FontFaces[ FaceIndex ]);

    if     ( Face.FixedWidth = FixedWidth )
       and ( Face.FontType = ftOutline ) then
    begin
      Result := Face;
      exit;
    end;
  end;
  Result := nil;
end;

// Find the bitmap font which best matches the given pointsize.
//------------------------------------------------------------------------
function GetClosestBitmapFixedFont( const PointSize: longint ): TLogicalFont;
Var
  FaceIndex: Longint;
  FontIndex: longint;
  Face: TFontFace;
  Font: TLogicalFont;
begin
  Result := nil;
  for FaceIndex := 0 to FontFaces.Count - 1 do
  begin
    Face := TFontFace(FontFaces[ FaceIndex ]);

    if Face.FontType = ftBitmap then
    begin
      for FontIndex := 0 to Face.Sizes.Count - 1 do
      begin
        Font := TLogicalFont(Face.Sizes[ FontIndex ]);
        if Font.FixedWidth then
        begin
          if    ( Result = nil )
             or ( Abs( Font.PointSize - PointSize )
                  < Abs( Result.PointSize - PointSize ) ) then
            Result := Font;
        end;
      end;
    end;
  end;
end;

// Pick some nice default fonts.
//------------------------------------------------------------------------
procedure GetDefaultFonts;
begin
  // courier new is common and reasonably nice
  DefaultOutlineFixedFace := FindFaceName( 'Courier New' );
  if DefaultOutlineFixedFace = nil then
  begin
    DefaultOutlineFixedFace := GetFirstOutlineFace( true ); // first fixed outline face
  end;

  DefaultOutlineProportionalFace := FindFaceName( 'Arial' );
  if DefaultOutlineProportionalFace = nil then
  begin
    DefaultOutlineProportionalFace := GetFirstOutlineFace( false ); // first prop outline face
  end;
end;

// Fetch the global list of font faces and sizes
//------------------------------------------------------------------------
procedure GetFontList;
Var
  Count: LongInt;
  T: LongInt;
  Font: TLogicalFont;
  Face: TFontFace;
  FamilyName: string;
  fl: TStringList;
  f: TfpgFont;
begin
  FontFaces := TList.Create;
  fl := fpgApplication.GetFontFaceList;

  // Get font count
  Count := fl.Count;
  If Count > 0 Then
  Begin
    For T := 0 To Count - 1 Do
    Begin
      Font := TLogicalFont.Create( nil );
      Font.FaceName := fl[T];
      f := fpgGetFont(Font.FaceName + '-10');
//      FamilyName := pfm^[ T ].szFamilyName;
      if (pos('Courier', Font.FaceName) > 0) or (pos('Mono', Font.FaceName) > 0) then
        Font.FixedWidth := True;
      Font.lAveCharWidth := f.TextWidth('g');
      Font.lMaxbaselineExt := f.Height;
      //Font.fsSelection := pfm^[ T ].fsSelection;
      //Font.lMaxbaselineExt := pfm^[ T ].lMaxbaselineExt;
      //Font.lAveCharWidth := pfm^[ T ].lAveCharWidth;
      //Font.lMaxCharInc := pfm^[ T ].lMaxCharInc;
      Font.ID := -1; // and always shall be so...
      f.Free;

      Face := FindFaceName( Font.FaceName );
      if Face = nil then
      begin
        // new face found
        Face := TFontFace.Create;
        Face.Name := Font.FaceName; // point to the actual face name string!
        Face.FixedWidth := Font.FixedWidth;
        Face.FontType := Font.FontType;
        FontFaces.Add( Face );
      end;
      Face.Sizes.Add( Font );
    End;
  End;

  // pick some for defaults
  GetDefaultFonts;

  //FontWindow := TFontWindow.Create( Nil );
  //FontWindow.OwnerDraw := True;
  //FontWindow.CreateWnd;
end;

// Add .subscript to font name for attributes
//------------------------------------------------------------------------
Function ModifyFontName( const FontName: string;
                         const Attrs: TFontAttributes ): String;
Begin
  Result := FontName;
  If faItalic in Attrs Then
    Result := Result + '.Italic';
  If faBold in Attrs Then
    Result := Result + '.Bold';
  If faOutline in Attrs Then
    Result := Result + '.Outline';
  If faStrikeOut in Attrs Then
    Result := Result + '.Strikeout';
  If faUnderScore in Attrs Then
    Result := Result + '.Underscore';
End;

// Create a font without attributes
//------------------------------------------------------------------------
function CreateFontBasic( const FaceName: string; const PointSize: integer ): TLogicalFont;
var
  PPString: string;
begin
  Result := TLogicalFont.Create( nil );
  if FindFaceName( FaceName ) = nil then
    Exit;  //==>
  Result.PointSize := PointSize; // will use later if the result was an outline font...
  Result.FaceName := FaceName;

  // OK now we have found the font face...
  PPString := IntToStr( PointSize) + '.' + FaceName;

  PPString := ModifyFontName( PPString, [] );
  //If Not FontWindow.SetPPFontNameSize( PPString ) Then
  //  Exit;
end;

// Provide outline substitutes for some common bitmap fonts
// From Mozilla/2 source.
//------------------------------------------------------------------------
function SubstituteBitmapFontToOutline( const FaceName: string ): string;
begin
  if StringsSame( FaceName, 'Helv' ) then
    result := 'Arial'
  else if StringsSame( FaceName, 'Helvetica' ) then
    result := 'Arial'
  else if StringsSame( FaceName, 'Tms Rmn' ) then
    result := 'Times New Roman'
  else if StringsSame( FaceName, 'System Proportional' ) then
    result := 'Arial'
  else if StringsSame( FaceName, 'System Monospaced' ) then
    result := 'Courier New'
  else if StringsSame( FaceName, 'System VIO' ) then
    result := 'Courier New'
  else
    result := FaceName; // no substitution
end;

// NOTE!!! Not currently used or working...
// Find a font with exact width and height
//------------------------------------------------------------------------
function FindXYSizeFont( const Face: TFontFace;
                         const XSize: longint;
                         const YSize: longint ): TLogicalFont;
var
  SizeIndex: longint;
  F: TLogicalFont;
begin
  for SizeIndex := 0 to Face.Sizes.Count - 1 do
  begin
    F := TLogicalFont(Face.Sizes[ SizeIndex ]);
    if     ( F.lMaxbaselineExt = YSize )
       and ( F.lAveCharWidth = XSize ) then
    begin
      // found exact match
      //FontInfo.lMaxbaselineExt := F.lMaxbaselineExt;
      //FontInfo.lAveCharWidth := F.lAveCharWidth;
      //Result.FontType := ftBitmap;
    end;
  end;
end;

// Ask OS/2 dummy font window to convert a font spec
// into a FONTMETRICS.
//------------------------------------------------------------------------
//procedure AskOS2FontDetails( const FaceName: string;
//                             const PointSize: longint;
//                             const Attributes: TFontAttributes;
//                             var FontInfo: FONTMETRICS );
//var
//  PPString: string;
//  PresSpace: HPS;
//begin
//  // Hack from Sibyl code - we don't know WTF the algorithm is
//  // for selecting between outline/bitmap and doing substitutions
//  // so send it to a dummy window and find out the resulting details
//  PPString := IntToStr( PointSize )
//              + '.'
//              + FaceName;
//
//  PPString := ModifyFontName( PPString, Attributes );
//
//  FontWindow.SetPPFontNameSize( PPString );
//
//  PresSpace := WinGetPS( FontWindow.Handle );
//  GpiQueryFontMetrics( PresSpace,
//                             SizeOf( FontInfo ),
//                             FontInfo );
//  WinReleasePS( PresSpace );
//end;

// Look for the best match for the given face, size and attributes.
// If FixedWidth is set then makes sure that the result is fixed
// (if there is any fixed font on the system at all!)
// This uses the OS/2 GPI and therefore makes some substitutions,
// such as Helv 8 (bitmap) for Helvetica 8 (outline)
//------------------------------------------------------------------------
procedure FindBestFontMatch( const FaceName: string;
                             const PointSize: longint;
                             const Attributes: TFontAttributes;
                             const FixedWidth: boolean;
                             var FontInfo: string );
var
  BestBitmapFontMatch: TLogicalFont;
  fl: TStringList;
  i: integer;
begin
  { TODO -oGraeme -cfonts : This hack is very quick and dirty. Needs to be refined a lot }
  fl := fpgApplication.GetFontFaceList;
  for i := 0 to fl.Count-1 do
  begin
    if Pos(FaceName, fl[i]) > 0 then
      FontInfo := fl[i] + '-' + IntToStr(PointSize);
  end;

  if Fontinfo = '' then
    // nothing found se use default font of fpGUI
    FontInfo := fpgApplication.DefaultFont.FontDesc;

  //// First just ask GPI to give us a font
  //AskOS2FontDetails( FaceName,
  //                   PointSize,
  //                   Attributes,
  //                   FontInfo );
  //
  //if not FixedWidth then
  //  // OK, whatever it gave us.
  //  exit;
  //
  //// we want a fixed width font...
  //if ( FontInfo.fsType and FM_TYPE_FIXED ) <> 0 then
  //  // got a suitable font
  //  exit;
  //
  //// the stoopid freaking OS/2 GPI has given us
  //// a proportional font for that size
  //if DefaultOutlineFixedFace <> nil then
  //  // use the default fixed width outline face
  //  AskOS2FontDetails( DefaultOutlineFixedFace.pName^,
  //                     PointSize,
  //                     Attributes,
  //                     FontInfo );
  //
  //
  //if ( FontInfo.fsType and FM_TYPE_FIXED ) <> 0 then
  //  // got a suitable font
  //  exit;
  //
  //// still got a proportional font,
  //// or we didn't have any fixed width outline face
  //// so see what we can find in the way of a bitmap fixed font
  //
  //BestBitmapFontMatch := GetClosestBitmapFixedFont( PointSize );
  //if BestBitmapFontMatch <> nil then
  //begin
  //  FontInfo.lMaxbaseLineExt := BestBitmapFontMatch.lMaxbaselineExt;
  //  FontInfo.lAveCharWidth := BestBitmapFontMatch.lAveCharWidth;
  //  FontInfo.fsDefn := 0;
  //  FontInfo.szFaceName := BestBitmapFontMatch.pFaceName^;
  //end;
  //// else - there are no fixed fonts of any kind on the system. Oh dear.

end;

//------------------------------------------------------------------------
// Font manager
//------------------------------------------------------------------------

// constructor
//------------------------------------------------------------------------
constructor TCanvasFontManager.Create(Canvas: TfpgCanvas; AllowBitmapFonts: boolean;
  AWidget: TfpgWidget);
begin
  inherited Create;
  if FontFaces = nil then
    GetFontList;
  FCanvas := Canvas;
  FWidget := AWidget;
  FLogicalFonts := TList.Create;
  FCurrentFontSpec.FaceName := 'Arial';
  FCurrentFont := nil;
  FAllowBitmapFonts := AllowBitmapFonts;
  // get system default font spec
  // as default default ;)
  FPGuiFontToFontSpec( fpgApplication.DefaultFont, FDefaultFontSpec );
end;

// Destructor
//------------------------------------------------------------------------
destructor TCanvasFontManager.Destroy;
var
  i: integer;
  Font: TLogicalFont;
begin
  // select default font so none of our logical fonts are in use
  FCanvas.Font := fpgApplication.DefaultFont;

  // delete each logical font and our record of it
  for i := 0 to FLogicalFonts.Count - 1 do
  begin
    Font := TLogicalFont(FLogicalFonts[ i ]);
    //if not GpiDeleteSetID( FCanvas.Handle, Font.ID ) then
    //  rc := WinGetLastError( AppHandle );
    Font.Free;
  end;
  FLogicalFonts.Free;
  inherited Destroy;
end;

// Create a logical font for the given spec
//------------------------------------------------------------------------
function TCanvasFontManager.CreateFont( const FontSpec: TFontSpec ): TLogicalFont;
var
  UseFaceName: string;
  Face: TFontFace;
  RemoveBoldFromSelection: boolean;
  RemoveItalicFromSelection: boolean;
  UseAttributes: TFontAttributes;
  MatchAttributes: TFontAttributes;
  BaseFont: TLogicalFont;
  BaseFontIsBitmapFont: Boolean;
  FontInfo: string;
  FixedWidth: boolean;
begin
  Face := nil;
  RemoveBoldFromSelection := false;
  RemoveItalicFromSelection := false;

  UseAttributes := FontSpec.Attributes;

  // see if the originally specified font is a fixed width one.
  FixedWidth := false;
  Face := FindFaceName( FontSpec.FaceName );
  if Face <> nil then
    FixedWidth := Face.FixedWidth;

  Face := nil;

  if not FAllowBitmapFonts then
    UseFaceName := SubstituteBitmapFontToOutline( FontSpec.FaceName )
  else
    UseFaceName := FontSpec.FaceName;

  if FontSpec.Attributes <> [] then
  begin
    BaseFontIsBitmapFont := false;
    if FAllowBitmapFonts then
    begin
      // First see if the base font (without attributes)
      // would be a bitmap font...
      BaseFont := CreateFontBasic( UseFaceName, FontSpec.PointSize );
      if BaseFont <> nil then
      begin
        BaseFontIsBitmapFont := BaseFont.FontType = ftBitmap;
        BaseFont.Destroy;
      end;
    end;

    If not BaseFontIsBitmapFont Then
    begin
      // Result is an outline font so look for specific bold/italic fonts
      if     ( faBold in FontSpec.Attributes )
         and ( faItalic in FontSpec.Attributes ) then
      begin
        Face := FindFaceName( UseFaceName + ' BOLD ITALIC' );
        if Face <> nil then
        begin
          Exclude( UseAttributes, faBold );
          Exclude( UseAttributes, faItalic );
          RemoveBoldFromSelection := true;
          RemoveItalicFromSelection := true;
        end;
      end;

      if Face = nil then
        if faBold in FontSpec.Attributes then
        begin
          Face := FindFaceName( UseFaceName + ' BOLD' );
          if Face <> nil then
          begin
            Exclude( UseAttributes, faBold );
            RemoveBoldFromSelection := true;
          end;
        end;

      if Face = nil then
        if faItalic in FontSpec.Attributes then
        begin
          Face := FindFaceName( UseFaceName + ' ITALIC' );
          if Face <> nil then
          begin
            Exclude( UseAttributes, faItalic );
            RemoveItalicFromSelection := true;
          end;
        end;
    end;
  end;

  if Face <> nil then
    // found a styled face, does it match fixed width?
    if Face.FixedWidth <> FixedWidth then
      // no so we don't want to use it.
      Face := nil;

  if Face = nil then
    // didn't find a styled face (or no styles set)
    // so find unmodified, we will use simulation bits
    Face := FindFaceName( UseFaceName );

  if not FAllowBitmapFonts then
    if Face.FontType = ftBitmap then
      // we aren't allowed bitmaps, but that's what this
      // face is. So use the default outline face of the
      // appropriate width type
      if FixedWidth then
        Face := DefaultOutlineFixedFace
      else
        Face := DefaultOutlineProportionalFace;

  if Face = nil then
  begin
    // Could not find the specified font name. Bummer.
    Result := nil;
    exit;
  end;

  // OK now we have found the font face...
  Result := TLogicalFont.Create( nil );
  Result.PointSize    := FontSpec.PointSize; // will use later if the result was an outline font...
  Result.FaceName     := FontSpec.FaceName;
  Result.UseFaceName  := Face.Name;
  Result.Attributes   := FontSpec.Attributes;
  Result.fsSelection  := 0;
  Result.FixedWidth   := Face.FixedWidth;

  if FAllowBitmapFonts then
  begin
    if BaseFontIsBitmapFont then
      MatchAttributes := []
    else
      MatchAttributes := UseAttributes;
    FindBestFontMatch( Face.Name,
                       FontSpec.PointSize,
                       MatchAttributes,
                       FixedWidth,
                       FontInfo );

    Result.UseFaceName := FontInfo;
//    AssignStr( Result.UseFaceName, FontInfo.FaceName );

    // We may actually get a bitmap OR an outline font back
    //If ( FontInfo.fsDefn And FM_DEFN_OUTLINE ) <> 0 Then
    //  Result.FontType := ftOutline
    //else
    //  Result.FontType := ftBitmap;
  end
  else
  begin
    // no bitmap fonts please.
    Result.FontType := ftOutline
  end;

  // store the baseline and average char width.
  // For bitmap fonts, these tell GPI which font we really want
  // For outline fonts, we are just storing them for later ref.
  //Result.lMaxbaseLineExt := FontInfo.lMaxbaselineExt;
  //Result.lAveCharWidth := FontInfo.lAveCharWidth;
  //Result.lMaxCharInc := FontInfo.lMaxCharInc;
  Result.lMaxBaseLineExt := FontSpec.YSize;
  Result.lAveCharWidth := FontSpec.XSize;
  Result.lMaxCharInc := FontSpec.YSize;

  // Set style flags
  with Result do
  begin
    //If faBold in UseAttributes Then
    //  fsSelection := fsSelection or FM_SEL_BOLD;
    //If faItalic in UseAttributes Then
    //  fsSelection := fsSelection or FM_SEL_ITALIC;
    //If faUnderScore in UseAttributes Then
    //  fsSelection := fsSelection or FM_SEl_UNDERSCORE;
    //If faStrikeOut in UseAttributes Then
    //  fsSelection := fsSelection or FM_SEl_STRIKEOUT;
    //If faOutline in UseAttributes Then
    //  fsSelection := fsSelection or FM_SEl_OUTlINE;
  end;

  Result.pCharWidthArray := nil;
end;

// Register the given logical font with GPI and store for later use
//------------------------------------------------------------------------
procedure TCanvasFontManager.RegisterFont( Font: TLogicalFont );
var
//  fa: FATTRS;
  rc: LongInt;
begin
  FLogicalFonts.Add( Font );
  Font.ID := FLogicalFonts.Count + 1; // add 1 to stay out of Sibyl's way

  //// Initialise GPI font attributes
  //FillChar( fa, SizeOf( FATTRS ), 0 );
  //fa.usRecordLength := SizeOf( FATTRS );
  //
  //// Copy facename and 'simulation' attributes from what we obtained
  //// earlier
  //fa.szFaceName := Font.pUseFaceName^;
  //fa.fsSelection := Font.fsSelection;
  //
  //fa.lMatch := 0; // please Mr GPI be helpful and do clever stuff for us, we are ignorant
  //
  //fa.idRegistry := 0; // IBM magic number
  //fa.usCodePage := 0; // use current codepage
  //
  //If Font.FontType = ftOutline then
  //  // Outline font wanted
  //  fa.fsFontUse := FATTR_FONTUSE_OUTLINE Or FATTR_FONTUSE_TRANSFORMABLE
  //else
  //  // bitmap font
  //  fa.fsFontUse := 0;
  //
  //// don't need mixing with graphics (for now)
  //fa.fsFontUse := fa.fsFontUse or FATTR_FONTUSE_NOMIX;
  //
  //// copy char cell width/height from the (valid) one we
  //// found earlier in GetFont (will be zero for outline)
  //fa.lMaxbaseLineExt := Font.lMaxbaselineExt;
  //fa.lAveCharWidth := Font.lAveCharWidth;
  //
  //fa.fsType := 0;
  //
  //// create logical font
  //rc := GpiCreateLogFont( FCanvas.Handle,
  //                        nil,
  //                        Font.ID,
  //                        fa );
end;

// Select the given (existing) logical font
//------------------------------------------------------------------------
procedure TCanvasFontManager.SelectFont( Font: TLogicalFont;
                                         Scale: longint );
var
  aHDC: integer;
  //xRes: LongInt;
  //yRes: LongInt;
  //aSizeF: SIZEF;
  f: TfpgFont;
  s: string;
begin
writeln('TCanvasFontManager.SelectFont >>>>>>>>>');
  // Select the logical font
  //GpiSetCharSet( FCanvas.Handle, Font.ID );
  if Font.FontType = ftOutline then
  begin
    s := Font.FaceName + '-' + IntToStr(Font.PointSize);
    if faBold in Font.Attributes then
      s := s + ':bold';
    if faItalic in Font.Attributes then
      s := s + ':italic';
    if faUnderScore in Font.Attributes then
      s := s + ':underline';
    f := fpgGetFont(s);
    writeln('    fontdesc=', s);
    FCanvas.Font := f;
  //  // For outline fonts, also set character Box
  //  aHDC := GpiQueryDevice( FCanvas.Handle );
  //  DevQueryCaps( aHDC,
  //                CAPS_HORIZONTAL_FONT_RES,
  //                1,
  //                xRes );
  //  DevQueryCaps( aHDC,
  //                CAPS_VERTICAL_FONT_RES,
  //                1,
  //                yRes );
  //
  //  aSizeF.CX := 65536 * xRes* Font.PointSize Div 72 * Scale;
  //  aSizeF.CY := 65536 * yRes* Font.PointSize Div 72 * Scale;
  //
  //  GpiSetCharBox( FCanvas.Handle, aSizeF );
  end;
end;

// Get a font to match the given spec, creating or re-using an
// existing font as needed.
//------------------------------------------------------------------------
function TCanvasFontManager.GetFont( const FontSpec: TFontSpec ): TLogicalFont;
var
  AFont: TLogicalFont;
  FontIndex: integer;
begin
  for FontIndex := 0 to FLogicalFonts.Count - 1 do
  begin
    AFont := TLogicalFont(FLogicalFonts[ FontIndex ]);
    if AFont.PointSize = FontSpec.PointSize then
    begin
      if    ( AFont.PointSize > 0 )
         or (     ( AFont.lAveCharWidth = FontSpec.XSize )
              and ( AFont.lMaxbaselineExt = FontSpec.YSize ) ) then
      begin
        if AFont.Attributes = FontSpec.Attributes then
        begin
          // search name last since it's the slowest thing
          if AFont.FaceName = FontSpec.FaceName then
          begin
            // Found a logical font already created
            Result := AFont;
            // done
            exit;
          end;
        end;
      end;
    end;
  end;

  // Need to create new logical font
  Result := CreateFont( FontSpec );
  if Result <> nil then
  begin
    RegisterFont( Result );
  end;
end;

// Set the current font for the canvas to match the given
// spec, creating or re-using fonts as needed.
//------------------------------------------------------------------------
procedure TCanvasFontManager.SetFont( const FontSpec: TFontSpec );
var
  Font: TLogicalFont;
  lDefaultFontSpec: TFontSpec;
begin
  //if FCurrentFontSpec = FontSpec then
  if (FCurrentFontSpec.FaceName = FontSpec.FaceName) and
     (FCurrentFontSpec.PointSize = FontSpec.PointSize) and
     (FCurrentFontSpec.Attributes = FontSpec.Attributes) then
    // same font
    exit;

  Font := GetFont( FontSpec );

  if Font = nil then
  begin
    // ack! Pfffbt! Couldn't find the font.

    // Try to get the default font
    Font := GetFont( FDefaultFontSpec );
    if Font = nil then
    begin
      FPGuiFontToFontSpec( fpgApplication.DefaultFont, lDefaultFontSpec );
      Font := GetFont( lDefaultFontSpec );
      if Font = nil then
        // Jimminy! We can't even get the default system font
        raise Exception.Create( 'Could not access default font '
                                + 'in place of '
                                + FontSpec.FaceName
                                + ' '
                                + IntToStr( FontSpec.PointSize ) );
    end;

  end;

  SelectFont( Font, 1 );
  FCurrentFontSpec := FontSpec;
  FCurrentFont := Font;
end;

// Get the widths of all characters for current font
// and other dimensions
//------------------------------------------------------------------------
procedure TCanvasFontManager.LoadMetrics;
var
  TheChar: Char;
begin
  // Retrieve all character widths
  if FCurrentFont.FontType = ftOutline then
  begin
    SelectFont( FCurrentFont, FontWidthPrecisionFactor );
  end;

  // allocate memory for storing the char widths
  GetMem( FCurrentFont.pCharWidthArray, sizeof( TCharWidthArray ) );
  //if not GpiQueryWidthTable( FCanvas.Handle,
  //                           0, 256,
  //                           FCurrentFont.pCharWidthArray^[ #0 ] ) then
  //begin
  //  raise Exception.Create( 'Error getting character width table: '
  //                          + 'GpiQueryWidthTable error '
  //                          + IntToStr( WinGetLastError( AppHandle ) ) );
  //end;

  // Convert all widths to positive!
  // For unknown reason, sometimes GPI returns negative values...
  for TheChar := #0 to #255 do
  begin
    FCurrentFont.pCharWidthArray^[ TheChar ] := Abs( FCurrentFont.pCharWidthArray^[ TheChar ] );
  end;

  if FCurrentFont.FontType = ftOutline then
  begin
    SelectFont( FCurrentFont, 1 );
  end
  else
  begin
    // For bitmap fonts, multiply by 256 manually
    for TheChar := #0 to #255 do
    begin
      FCurrentFont.pCharWidthArray^[ TheChar ] :=
        FCurrentFont.pCharWidthArray^[ TheChar ]
        * FontWidthPrecisionFactor;
    end;
  end;

  //GpiQueryFontMetrics( FCanvas.Handle,
  //                     sizeof( fm ),
  //                     fm );
  //FCurrentFont.lMaxbaseLineExt := fm.lMaxbaselineExt;
  //FCurrentFont.lAveCharWidth := fm.lAveCharWidth;
  //FCurrentFont.lMaxCharInc := fm.lMaxCharInc;
  //FCurrentFont.lMaxDescender := fm.lMaxDescender;
end;

procedure TCanvasFontManager.EnsureMetricsLoaded;
begin
  if FCurrentFont = nil then
    raise( Exception.Create( 'No font selected before getting font metrics' ) );

  if FCurrentFont.pCharWidthArray = Nil then
    LoadMetrics;
end;

function TCanvasFontManager.CharWidth( const C: Char ): longint;
begin
  EnsureMetricsLoaded;
  Result := FCurrentFont.pCharWidthArray^[ C ];
  { TODO -ograemeg -chard-coded result : This is a temporary hard-code }
  result := fpgApplication.DefaultFont.TextWidth(C);
end;

function TCanvasFontManager.AverageCharWidth: longint;
begin
  EnsureMetricsLoaded;
  Result := FCurrentFont.lAveCharWidth;
end;

function TCanvasFontManager.MaximumCharWidth: longint;
begin
  EnsureMetricsLoaded;
  Result := FCurrentFont.lMaxCharInc;
end;

function TCanvasFontManager.CharHeight: longint;
begin
  EnsureMetricsLoaded;
  Result := FCurrentFont.lMaxBaseLineExt;
end;

function TCanvasFontManager.CharDescender: longint;
begin
  EnsureMetricsLoaded;
  Result := FCurrentFont.lMaxDescender;
end;

function TCanvasFontManager.IsFixed: boolean;
begin
  Result := FCurrentFont.FixedWidth;
end;

procedure TCanvasFontManager.DrawString(var Point: TPoint; const Length: longint; const S: PChar);
var
  t: string;
begin
  t := s;
  FCanvas.DrawString(Point.X, Point.Y, t);
  Point.x := Point.X + Canvas.Font.TextWidth(t);
end;

end.
