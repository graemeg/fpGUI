Unit CanvasFontManager;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,fpg_base
  ,fpg_main
  ,fpg_widget
  ;

var
  // This defines the fraction of a pixel that
  // font character widths will be given in
  DefaultTopicFontName: string;
  DefaultTopicFontSize: integer = 10;
  DefaultTopicFixedFontName: string;
  DefaultTopicFixedFontSize: integer = 10;

var
  DefaultTopicFont: string;
  DefaultTopicFixedFont: string;

type
  {Standard Font Attributes}
  TFontAttributes = set of (faBold, faItalic, faUnderScore, faOutline, faStrikeOut);

  {Standard Font pitches}
  TFontPitch=(fpFixed,fpProportional);


  TCanvasFontManager = class(TObject)
  private
    FWidget: TfpgWidget;
    FCanvas: TfpgCanvasBase;
    function    GetCurrentFont: TfpgFontResourceBase;
    procedure   SetDefaultFont(const AValue: TfpgFontResourceBase);
  protected
    FDefaultFont: TfpgFontResourceBase;
  public
    constructor Create(ACanvas: TfpgCanvasBase; AWidget: TfpgWidget); reintroduce;
    destructor  Destroy; override;
    function    AverageCharWidth: longint;
    function    CharAscender: longint;
    function    CharDescender: longint;
    function    CharHeight: longint;
    function    CharWidth( const C: TfpgChar ): longint;  // Retrieve the width of the given char, in the current font
    function    IsFixed: boolean;
    function    MaximumCharWidth: longint;
    procedure   DrawString(var Point: TPoint; const Length: longint; const S: PChar);
    procedure   SetFont(const AFontDesc: TfpgString);
    property    Canvas: TfpgCanvasBase read FCanvas;
    property    CurrentFont: TfpgFontResourceBase read GetCurrentFont;
    property    DefaultFont: TfpgFontResourceBase read FDefaultFont write SetDefaultFont;
    property    Widget: TfpgWidget read FWidget;
  end;


// Get the font attributes of a fpGUI font
function GetFPGuiFontAttributes(const AFont: TfpgFontResourceBase): TFontAttributes;
function GetFPGuiFont(const AFontNameSize: string; const Attrs: TFontAttributes): TfpgFontResourceBase;
procedure ApplyFontAttributes(var AFontDesc: string; const Attrs: TFontAttributes);


implementation

uses
  SysUtils
  ,ACLStringUtility
  ,nvUtilities
  ,fpg_stringutils
  ,SettingsUnit
  ;


function GetFPGuiFontAttributes(const AFont: TfpgFontResourceBase): TFontAttributes;
var
  s: string;
  facename: string;
  cp: integer;
  c: char;
  token: string;
  prop, propval: string;
  lDesc: string;
  lFontSize: integer;

  function NextC: char;
  begin
    Inc(cp);
    if cp > length(lDesc) then
      c := #0
    else
      c := lDesc[cp];
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
  Result := [];
  cp := 0;
  if Assigned(AFont) then
    lDesc := AFont.FontDesc
  else
    lDesc := '';

  // find fontface
  NextC;
  NextToken;

  // find font size
  if c = '-' then
  begin
    NextC;
    NextToken;
    lFontSize := StrToIntDef(token, DefaultTopicFontSize);
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
    // convert fontdesc attributes to standard font attributes
    if prop = 'BOLD' then
      include(Result, faBold)
    else if prop = 'ITALIC' then
      include(Result, faItalic)
    else if prop = 'UNDERLINE' then
      include(Result, faUnderScore)
    else if prop = 'OUTLINE' then
      include(Result, faOutline)
    else if prop = 'STRIKEOUT' then
      include(Result, faStrikeOut)
  end;
end;

function GetFPGuiFont(const AFontNameSize: string; const Attrs: TFontAttributes): TfpgFontResourceBase;
var
  s: string;
begin
  s := AFontNameSize;
  ApplyFontAttributes(s, Attrs);
  Result := fpgApplication.FontManager.GetFont(s);
end;

// Add attributes to font name
procedure ApplyFontAttributes(var AFontDesc: string; const Attrs: TFontAttributes);
begin
  if faItalic in Attrs then
    if Pos(':Italic', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Italic';

  if faBold in Attrs then
    if Pos(':Bold', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Bold';

  if faOutline in Attrs Then
    if Pos(':Outline', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Outline';

  if faStrikeOut in Attrs Then
    if Pos(':Strikeout', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Strikeout';

  if faUnderScore in Attrs Then
    if Pos(':Underline', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Underline';
end;

// Provide font name substitutes for some common bitmap fonts found in INF files
function SubstituteBitmapFontToOutline( const FaceName: string ): string;
begin
  if StringsSame( FaceName, 'Helv' ) then
    result := DefaultTopicFontName
  else if StringsSame( FaceName, 'Helvetica' ) then
    result := DefaultTopicFontName
  else if StringsSame( FaceName, 'Tms Rmn' ) then
    result := 'Times New Roman'
  else if StringsSame( FaceName, 'System Proportional' ) then
    result := DefaultTopicFontName
  else if StringsSame( FaceName, 'System Monospaced' ) then
    result := DefaultTopicFixedFontName
  else if StringsSame( FaceName, 'System VIO' ) then
    result := DefaultTopicFixedFontName
  else
    result := FaceName; // no substitution
end;

// Look for the best match for the given face, size and attributes.
// If FixedWidth is set then makes sure that the result is fixed
procedure FindBestFontMatch( const FaceName: string; const PointSize: longint;
    const Attributes: TFontAttributes; const FixedWidth: boolean; var FontDesc: string );
var
  sl: TStringList;
  i: integer;
begin
  FontDesc := '';
  sl := fpgApplication.GetFontFaceList;
  for i := 0 to sl.Count-1 do
  begin
    if Pos(FaceName, sl[i]) > 0 then
      FontDesc := sl[i] + '-' + IntToStr(PointSize);
  end;

  ApplyFontAttributes(FontDesc, Attributes);

  // if nothing found, use default font of fpGUI
  if FontDesc = '' then
  begin
    if Assigned(fpgStyle.GetDefaultFont) then
      FontDesc := fpgStyle.GetDefaultFont.FontDesc
    else
      FontDesc := DefaultTopicFont;
  end;
end;


{ TCanvasFontManager }

constructor TCanvasFontManager.Create(ACanvas: TfpgCanvasBase; AWidget: TfpgWidget);
begin
  inherited Create;
  FCanvas := ACanvas;
  FWidget := AWidget;
  FDefaultFont := fpgApplication.FontManager.GetFont(DefaultTopicFont);
  // Use SetFont overload that accepts TfpgFontResourceBase
  FCanvas.SetFont(FDefaultFont);
end;

destructor TCanvasFontManager.Destroy;
begin
  FCanvas.SetFont(fpgStyle.GetDefaultFont);
  FDefaultFont := nil;  // Clear pointer (font persists in cache)
  inherited Destroy;
end;

procedure TCanvasFontManager.SetDefaultFont(const AValue: TfpgFontResourceBase);
begin
  if FDefaultFont = AValue then
    exit;
  FDefaultFont := nil;  // Clear old pointer (font persists in cache)
  FDefaultFont := AValue;
end;

function TCanvasFontManager.GetCurrentFont: TfpgFontResourceBase;
begin
  Result := FCanvas.Font;
end;

// Set the current font for the canvas to match the given
// spec, using centralized font manager for caching.
procedure TCanvasFontManager.SetFont(const AFontDesc: TfpgString);
var
  lFontDesc: string;
  lFont: TfpgFontResourceBase;
  lCurFontDesc: string;
begin
  // Get current font descriptor - FontDesc is in TfpgFontResourceBase
  if Assigned(FCanvas.Font) then
    lCurFontDesc := FCanvas.Font.FontDesc
  else
    lCurFontDesc := '';

  if lCurFontDesc = AFontDesc then
    Exit; // nothing to do so exit

  if Assigned(FDefaultFont) then
    lFontDesc := FDefaultFont.FontDesc
  else
    lFontDesc := '';

  if lFontDesc = AFontDesc then
  begin
    // Use SetFont overload that accepts TfpgFontResourceBase
    FCanvas.SetFont(FDefaultFont);
    Exit;
  end;

  // Use centralized font manager for caching, then SetFont overload
  lFont := fpgApplication.FontManager.GetFont(AFontDesc);
  FCanvas.SetFont(lFont);
end;

function TCanvasFontManager.CharWidth( const C: TfpgChar ): longint;
begin
  Result := FCanvas.Font.GetTextWidth(C);
end;

function TCanvasFontManager.AverageCharWidth: longint;
begin
  Result := FCanvas.Font.GetTextWidth('c');
end;

function TCanvasFontManager.CharAscender: longint;
begin
  Result := FCanvas.Font.GetAscent();
end;

function TCanvasFontManager.MaximumCharWidth: longint;
begin
  Result := FCanvas.Font.GetTextWidth('W');
end;

function TCanvasFontManager.CharHeight: longint;
begin
  Result := FCanvas.Font.GetHeight();
end;

function TCanvasFontManager.CharDescender: longint;
begin
  Result := FCanvas.Font.GetDescent();
end;

function TCanvasFontManager.IsFixed: boolean;
begin
  // Check if font is fixed-width by comparing character widths
  Result := FCanvas.Font.GetTextWidth('i') = FCanvas.Font.GetTextWidth('W');
end;

procedure TCanvasFontManager.DrawString(var Point: TPoint; const Length: longint; const S: PChar);
var
  t: TfpgString;
begin
  t := s;
  FCanvas.DrawString(Point.X, Point.Y, t);
  Point.x := Point.X + Canvas.Font.GetTextWidth(t);
end;


initialization
  DefaultTopicFontName := FPG_DEFAULT_SANS;
  DefaultTopicFixedFontName := FPG_DEFAULT_FIXED;
  DefaultTopicFont := DefaultTopicFontName + '-' + IntToStr(DefaultTopicFontSize);
  DefaultTopicFixedFont := DefaultTopicFixedFontName + '-' + IntToStr(DefaultTopicFixedFontSize);

end.

