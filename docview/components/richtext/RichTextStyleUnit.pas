Unit RichTextStyleUnit;

{$mode objfpc}{$H+}

Interface

uses
  Classes, fpg_base, fpg_main, CanvasFontManager, RichTextDocumentUnit;

type
  TTextDrawStyle = record
    FontNameSize: TfpgString;
    FontAttributes: TFontAttributes;
    Color: TfpgColor;
    BackgroundColor: TfpgColor;
    Alignment: TTextAlignment;
    Wrap: boolean;
    LeftMargin: longint;
    RightMargin: longint;
  end;

  TMarginSizeStyle = ( msAverageCharWidth, msMaximumCharWidth, msSpecifiedChar );

  TRichTextSettings = class( TfpgComponent )
  protected
    FHeading1Font: TfpgFontResourceBase;
    FHeading2Font: TfpgFontResourceBase;
    FHeading3Font: TfpgFontResourceBase;
    FFixedFont: TfpgFontResourceBase;
    FNormalFont: TfpgFontResourceBase;
    FDefaultBackgroundColor: TfpgColor;
    FDefaultColor: TfpgColor;
    FDefaultAlignment: TTextAlignment;
    FDefaultWrap: boolean;
    FAtLeastOneWordBeforeWrap: boolean;
    FMarginSizeStyle: TMarginSizeStyle;
    FMarginChar: longint;
    FOnChange: TNotifyEvent;
    FMargins: TRect;
    FUpdateCount: longint;
    FChangesPending: boolean;
    Procedure Change;
    Procedure SetNormalFont( NewFont: TfpgFontResourceBase );
    Procedure SetFixedFont( NewFont: TfpgFontResourceBase );
    Procedure SetHeading1Font( NewFont: TfpgFontResourceBase );
    Procedure SetHeading2Font( NewFont: TfpgFontResourceBase );
    Procedure SetHeading3Font( NewFont: TfpgFontResourceBase );
    Procedure SetDefaultColor( NewColor: TfpgColor );
    Procedure SetDefaultBackgroundColor( NewColor: TfpgColor );
    Procedure SetDefaultAlignment( Alignment: TTextAlignment );
    Procedure SetDefaultWrap( Wrap: boolean );
    Procedure SetAtLeastOneWordBeforeWrap( NewValue: boolean );
    Procedure SetMarginSizeStyle( NewValue: TMarginSizeStyle );
    Procedure SetMarginChar( NewValue: longint );
    Procedure SetMargins( const NewMargins: TRect );
    function GetMargin_Left: longint;
    Procedure SetMargin_Left( NewValue: longint );
    function GetMargin_Bottom: longint;
    Procedure SetMargin_Bottom( NewValue: longint );
    function GetMargin_Right: longint;
    Procedure SetMargin_Right( NewValue: longint );
    function GetMargin_Top: longint;
    Procedure SetMargin_Top( NewValue: longint );
    Procedure SetupComponent;
    Procedure AssignFont(var AFont: TfpgFontResourceBase; NewFont: TfpgFontResourceBase);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Margins: TRect read FMargins write SetMargins;
    property Heading1Font: TfpgFontResourceBase read FHeading1Font write SetHeading1Font;
    property Heading2Font: TfpgFontResourceBase read FHeading2Font write SetHeading2Font;
    property Heading3Font: TfpgFontResourceBase read FHeading3Font write SetHeading3Font;
    property FixedFont: TfpgFontResourceBase read FFixedFont write SetFixedFont;
    property NormalFont: TfpgFontResourceBase read FNormalFont write SetNormalFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property DefaultBackgroundColor: TfpgColor read FDefaultBackgroundColor write SetDefaultBackgroundColor;
    property DefaultColor: TfpgColor read FDefaultColor write SetDefaultColor;
    property DefaultAlignment: TTextAlignment read FDefaultAlignment write SetDefaultAlignment;
    property DefaultWrap: boolean read FDefaultWrap write SetDefaultWrap default True;
    property AtLeastOneWordBeforeWrap: boolean read FAtLeastOneWordBeforeWrap write SetAtLeastOneWordBeforeWrap;
    property MarginSizeStyle: TMarginSizeStyle read FMarginSizeStyle write SetMarginSizeStyle;
    property MarginChar: longint read FMarginChar write SetMarginChar;
    // margins are exposed as individual properties here
    // since the Sibyl IDE cannot cope with editing a record property
    // within a class property (as in RichTextView)
    property Margin_Left: longint read GetMargin_Left write SetMargin_Left;
    property Margin_Bottom: longint read GetMargin_Bottom write SetMargin_Bottom;
    property Margin_Right: longint read GetMargin_Right write SetMargin_Right;
    property Margin_Top: longint read GetMargin_Top write SetMargin_Top;
  end;


  Procedure ApplyStyle( var Style: TTextDrawStyle;
                        FontManager: TCanvasFontManager );

  Procedure ApplyStyleTag( const Tag: TTag;
                           Var Style: TTextDrawStyle;
                           FontManager: TCanvasFontManager;
                           const ASettings: TRichTextSettings;
                           const X: longint );

  function GetDefaultStyle( const ASettings: TRichTextSettings ): TTextDrawStyle;



Implementation

uses
  SysUtils,
  ACLStringUtility,
  nvUtilities,
  SettingsUnit;


Procedure ApplyStyle(var Style: TTextDrawStyle; FontManager: TCanvasFontManager);
var
  s: string;
begin
ProfileEvent('DEBUG:  ApplyStyle >>>');
  assert(FontManager <> nil, 'FontManager should not have been nil');
  s := Style.FontNameSize;
  ApplyFontAttributes(s, Style.FontAttributes);
  FontManager.SetFont(s);
  FontManager.Canvas.TextColor := Style.Color;
ProfileEvent('DEBUG:  ApplyStyle <<<');
end;

Procedure ApplyStyleTag( Const Tag: TTag;
                         var Style: TTextDrawStyle;
                         FontManager: TCanvasFontManager;
                         const ASettings: TRichTextSettings;
                         const X: longint );
var
  MarginParam1: string;
  MarginParam2: string;
  NewMargin: longint;
  FontFaceName: string;
  FontSizeString: string;
  NewStyle: TTextDrawStyle;
  ParseIndex: longint;
  XSizeStr: string;
  YSizeStr: string;
  tmpFontParts : TStrings;

  MarginSize: longint;
  ParsePoint: longint;
begin
ProfileEvent('DEBUG:  ApplyStyleTag >>>');
  case Tag.TagType of
    ttBold:
      Include( Style.FontAttributes, faBold );

    ttBoldOff:
      Exclude( Style.FontAttributes, faBold );

    ttItalic:
      Include( Style.FontAttributes, faItalic );

    ttItalicOff:
      Exclude( Style.FontAttributes, faItalic );

    ttUnderline:
      Include( Style.FontAttributes, faUnderscore );

    ttUnderlineOff:
      Exclude( Style.FontAttributes, faUnderscore );

    ttFixedWidthOn:
      begin
        Style.FontNameSize := ASettings.FixedFont.FontDesc;
        Style.FontAttributes := [];  // attributes are already in FontDesc
      end;

    ttFixedWidthOff:
      begin
        Style.FontNameSize := ASettings.NormalFont.FontDesc;
        Style.FontAttributes := [];  // attributes are already in FontDesc
      end;

    ttHeading1:
      begin
        Style.FontNameSize := ASettings.Heading1Font.FontDesc;
        Style.FontAttributes := [];  // attributes are already in FontDesc
      end;

    ttHeading2:
      begin
        Style.FontNameSize := ASettings.Heading2Font.FontDesc;
        Style.FontAttributes := [];  // attributes are already in FontDesc
      end;

    ttHeading3:
      begin
        Style.FontNameSize := ASettings.Heading3Font.FontDesc;
        Style.FontAttributes := [];  // attributes are already in FontDesc
      end;

    ttHeading1Off,
    ttHeading2Off,
    ttHeading3Off:
      begin
        Style.FontNameSize := ASettings.NormalFont.FontDesc;
        Style.FontAttributes := [];  // attributes are already in FontDesc
      end;

    ttFont:
      begin
        tmpFontParts := TStringList.Create;
        StrExtractStringsQuoted(tmpFontParts, Tag.Arguments);
        FontFaceName := tmpFontParts[0];
        if tmpFontParts.Count=2 then
          FontSizeString := tmpFontParts[1];
        tmpFontParts.Free;

        NewStyle := Style;
        NewStyle.FontNameSize := FontFaceName;

        if Pos( 'x', FontSizeString ) > 0 then
        begin
          tmpFontParts := TStringList.Create;
          StrExtractStrings(tmpFontParts, FontSizeString, ['x'], #0);
          XSizeStr := tmpFontParts[0];
          YSizeStr := tmpFontParts[1];
          tmpFontParts.Destroy;
          // This probably needs to be enhanced to extract the font name and size first.
          NewStyle.FontNameSize := NewStyle.FontNameSize + '-' + YSizeStr;
        end
        else if (FontSizeString<>'') then
          // Same here
          NewStyle.FontNameSize := NewStyle.FontNameSize + '-' + FontSizeString;

        if ( NewStyle.FontNameSize <> '' ) then
          Style := NewStyle;
      end;

    ttFontOff:
      begin
        { TODO: Restore to previous font, not NormalFont, because previous font could have
           been something different to NormalFont }
        Style.FontNameSize := ASettings.NormalFont.FontDesc;
        Style.FontAttributes := [];  // attributes are already in FontDesc
      end;

    ttColor:
      GetTagColor( Tag.Arguments, Style.Color );

    ttColorOff,
    ttRedOff,
    ttGreenOff,
    ttBlackOff,
    ttBlueOff:
      Style.Color := ASettings.FDefaultColor;

    ttBackgroundColor:
      GetTagColor( Tag.Arguments, Style.BackgroundColor );

    ttBackgroundColorOff:
      Style.BackgroundColor := ASettings.FDefaultBackgroundColor;

    ttRed:
      Style.Color := clRed;

    ttBlue:
      Style.Color := clBlue;

    ttGreen:
      Style.Color := clGreen;

    ttBlack:
      Style.Color := clBlack;

    ttAlign:
      Style.Alignment := GetTagTextAlignment( Tag.Arguments, ASettings.FDefaultAlignment );

    ttNoWrap:
      Style.Wrap := False;

    ttNoWrapOff:
      Style.Wrap := True;

    ttSetLeftMargin,
    ttSetRightMargin:
      begin
        tmpFontParts := TStringList.Create;
        StrExtractStrings(tmpFontParts, Tag.Arguments, [' '], #0);
        MarginParam1 := tmpFontParts[0];

        ParsePoint := 1;
        if     ( Tag.TagType = ttSetLeftMargin )
           and ( MarginParam1 = 'here' ) then
        begin
          Style.LeftMargin := X;
        end
        else
        begin
          try
            MarginSize := StrToInt( MarginParam1 );
            if tmpFontParts.Count > 1 then   // do we have a second parameter
              MarginParam2 := tmpFontParts[1]
            else
              MarginParam2 := '';

            if MarginParam2 = 'pixels' then
              NewMargin := MarginSize
            else if MarginParam2 = 'deffont' then
              NewMargin := MarginSize * ASettings.NormalFont.GetTextWidth('w')  // Use GetTextWidth from IFontEngine
            else
            begin
              case ASettings.MarginSizeStyle of
                msAverageCharWidth:
                  NewMargin := MarginSize * FontManager.AverageCharWidth;

                msMaximumCharWidth:
                  NewMargin := MarginSize * FontManager.MaximumCharWidth;

                msSpecifiedChar:
                  NewMargin := MarginSize * FontManager.CharWidth(Chr(ASettings.MarginChar));
              end;
            end;
          except
            NewMargin := 0;
          end;

          if Tag.TagType = ttSetLeftMargin then
            Style.LeftMargin := ASettings.Margins.Left + NewMargin
          else
            Style.RightMargin := ASettings.Margins.Right + NewMargin;
        end;
        tmpFontParts.Free;
      end;  { teSet[left|right]margin }

  end;  { case Tag.TagType }

  ApplyStyle( Style, FontManager );
ProfileEvent('DEBUG:  ApplyStyleTag <<<');
end;

function GetDefaultStyle( const ASettings: TRichTextSettings ): TTextDrawStyle;
begin
  FillChar(Result, SizeOf(TTextDrawStyle), 0);
  if (ASettings.NormalFont <> nil) then
    Result.FontNameSize := ASettings.NormalFont.FontDesc
  else
    Result.FontNameSize := DefaultTopicFont;  // fallback to default
  Result.FontAttributes := [];
  Result.Alignment := ASettings.FDefaultAlignment;
  Result.Wrap := ASettings.FDefaultWrap;
  Result.Color := ASettings.FDefaultColor;
  Result.BackgroundColor := ASettings.FDefaultBackgroundColor;
  Result.LeftMargin := ASettings.Margins.Left;
  Result.RightMargin := ASettings.Margins.Right;
end;


Procedure TRichTextSettings.SetupComponent;
begin
  Name := 'RichTextSettings';

  FNormalFont   := fpgApplication.FontManager.GetFont(Settings.NormalFontDesc);
  FFixedFont    := fpgApplication.FontManager.GetFont(Settings.FixedFontDesc);
  FHeading1Font := fpgApplication.FontManager.GetFont(DefaultTopicFontName + '-20');
  FHeading2Font := fpgApplication.FontManager.GetFont(DefaultTopicFontName + '-14');
  FHeading3Font := fpgApplication.FontManager.GetFont(DefaultTopicFontName + '-10:bold');

  FDefaultColor := clBlack;
  FDefaultBackgroundColor := clBoxColor;

  FDefaultAlignment := taLeft;
  FDefaultWrap := true;
  FAtLeastOneWordBeforeWrap := false;

  { TODO: Add this property to DocView's Preferences dialog }
  FMarginSizeStyle := msAverageCharWidth;
  FMarginChar := Ord( ' ' );

  FMargins.Left   := 0;
  FMargins.Right  := 0;
  FMargins.Top    := 0;
  FMargins.Bottom := 0;

  FUpdateCount    := 0;
  FChangesPending := false;
end;

constructor TRichTextSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetupComponent;
end;

destructor TRichTextSettings.Destroy;
begin
  FNormalFont := nil;    // Release font (automatic ref count decrement)
  FFixedFont := nil;     // Release font (automatic ref count decrement)
  FHeading1Font := nil;  // Release font (automatic ref count decrement)
  FHeading2Font := nil;  // Release font (automatic ref count decrement)
  FHeading3Font := nil;  // Release font (automatic ref count decrement)
  Inherited Destroy;
end;

Procedure TRichTextSettings.Change;
begin
  if FUpdateCount > 0 then
  begin
     FChangesPending := true;
     exit;
  end;

  if FOnChange <> nil then
    FOnChange( self );
end;

Procedure TRichTextSettings.SetDefaultAlignment( Alignment: TTextAlignment );
begin
  if Alignment = FDefaultAlignment then
    exit; // no change

  FDefaultAlignment := Alignment;
  Change;
end;

Procedure TRichTextSettings.SetDefaultWrap( Wrap: boolean );
begin
  if Wrap = FDefaultWrap then
    exit; // no change

  FDefaultWrap := Wrap;
  Change;
end;

Procedure TRichTextSettings.SetAtLeastOneWordBeforeWrap( NewValue: boolean );
begin
  if NewValue = FAtLeastOneWordBeforeWrap then
    exit; // no change

  FAtLeastOneWordBeforeWrap := NewValue;
  Change;
end;

Procedure TRichTextSettings.SetMarginChar( NewValue: longint );
begin
  if NewValue = FMarginChar then
    exit; // no change

  FMarginChar := NewValue;

  if FMarginSizeStyle <> msSpecifiedChar then
    // doesn't matter, will be ignored
    exit;
  Change;
end;

Procedure TRichTextSettings.SetMarginSizeStyle( NewValue: TMarginSizeStyle );
begin
  if NewValue = FMarginSizeStyle then
    exit; // no change

  FMarginSizeStyle := NewValue;
  Change;
end;

Function FontSame( FontA: TfpgFontResourceBase; FontB: TfpgFontResourceBase ): boolean;
var
  DescA, DescB: string;
begin
  if ( FontA = nil ) or ( FontB = nil ) then
    Result := False
  else
  begin
    if FontA is TfpgFontResource then
      DescA := TfpgFontResource(FontA).FontDesc
    else
      DescA := '';
    if FontB is TfpgFontResource then
      DescB := TfpgFontResource(FontB).FontDesc
    else
      DescB := '';
    Result := DescA = DescB;
  end;
end;

Procedure TRichTextSettings.AssignFont(var AFont: TfpgFontResourceBase; NewFont: TfpgFontResourceBase );
begin
  If NewFont = Nil Then
    NewFont := fpgStyle.GetDefaultFont;

  if FontSame( NewFont, AFont ) then
    Exit; // no change needed

  AFont := nil;  // Release old font (automatic ref count decrement)
  AFont := NewFont;

  Change;
End;

Procedure TRichTextSettings.SetHeading1Font( NewFont: TfpgFontResourceBase );
begin
  AssignFont( FHeading1Font, NewFont );
end;

Procedure TRichTextSettings.SetHeading2Font( NewFont: TfpgFontResourceBase );
begin
  AssignFont( FHeading2Font, NewFont );
End;

Procedure TRichTextSettings.SetHeading3Font( NewFont: TfpgFontResourceBase );
begin
  AssignFont( FHeading3Font, NewFont );
End;

Procedure TRichTextSettings.SetFixedFont( NewFont: TfpgFontResourceBase );
begin
  AssignFont( FFixedFont, NewFont );
end;

Procedure TRichTextSettings.SetNormalFont( NewFont: TfpgFontResourceBase );
begin
  AssignFont( FNormalFont, NewFont );
end;

Procedure TRichTextSettings.SetMargins( const NewMargins: TRect );
begin
  if NewMargins = FMargins then
    exit; // no change
  FMargins := NewMargins;
  Change;
end;

function TRichTextSettings.GetMargin_Left: longint;
begin
  Result := FMargins.Left;
end;

Procedure TRichTextSettings.SetMargin_Left( NewValue: longint );
begin
  FMargins.Left := NewValue;
end;

function TRichTextSettings.GetMargin_Bottom: longint;
begin
  Result := FMargins.Bottom;
end;

Procedure TRichTextSettings.SetMargin_Bottom( NewValue: longint );
begin
  FMargins.Bottom := NewValue;
end;

function TRichTextSettings.GetMargin_Right: longint;
begin
  Result := FMargins.Right;
end;

Procedure TRichTextSettings.SetMargin_Right( NewValue: longint );
begin
  FMargins.Right := NewValue;
end;

function TRichTextSettings.GetMargin_Top: longint;
begin
  Result := FMargins.Top;
end;

Procedure TRichTextSettings.SetMargin_Top( NewValue: longint );
begin
  FMargins.Top := NewValue;
end;

Procedure TRichTextSettings.SetDefaultColor( NewColor: TfpgColor );
begin
  if NewColor = FDefaultColor then
    exit;
  FDefaultColor := NewColor;
  Change;
end;

Procedure TRichTextSettings.SetDefaultBackgroundColor( NewColor: TfpgColor );
begin
  if NewColor = FDefaultBackgroundColor then
    exit;
  FDefaultBackgroundColor := NewColor;
  Change;
end;

procedure TRichTextSettings.BeginUpdate;
begin
  inc( FUpdateCount );
end;

procedure TRichTextSettings.EndUpdate;
begin
  if FUpdateCount = 0 then
    exit;

  dec( FUpdateCount );
  if FUpdateCount = 0 then
  begin
    if FChangesPending then
    begin
      Change;
      FChangesPending := false;
    end;
  end;
end;


initialization
  RegisterClasses([TRichTextSettings]);

end.

