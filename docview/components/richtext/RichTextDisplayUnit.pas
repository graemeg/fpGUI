Unit RichTextDisplayUnit;

{$mode objfpc}{$H+}

Interface

uses
  Classes
  ,CanvasFontManager
  ,RichTextStyleUnit
  ,RichTextLayoutUnit
  ;

// Selection start and end should both be nil if no selection is to be applied
Procedure DrawRichTextLayout( var FontManager: TCanvasFontManager;
                              Layout: TRichTextLayout;
                              const SelectionStart: PChar;
                              const SelectionEnd: PChar;
                              const StartLine: longint;
                              const EndLine: longint;
                              const StartPoint: TPoint );

// Print as much of the given layout as will fit on the page,
// starting at StartY and StartLine
// EndY is set to the final Y output position used + 1.
// EndLine is set to the last line printed + 1
Procedure PrintRichTextLayout( var FontManager: TCanvasFontManager;
                               var Layout: TRichTextLayout;
                               const StartLine: longint;
                               var EndLine: longint;
                               const StartY: longint;
                               var EndY: longint );

Implementation

uses
  SysUtils
  ,RichTextDocumentUnit
  ,fpg_base
  ,fpg_main
  ,nvUtilities
  ;

// For the given point in the text, update selected if the point
// is at start or end of selection
// Returns true if changed
function SelectionChange( P: PChar;
                          SelectionStart: PChar;
                          SelectionEnd: PChar;
                          var NextSelected: boolean ): boolean;
begin
  Result := false;
  if P = SelectionStart then
  begin
    Result := true;
    if SelectionStart < SelectionEnd then
      // reached start of selection
      NextSelected := true
    else
      // reached end
      NextSelected := false;
  end
  else if P = SelectionEnd then
  begin
    Result := true;
    if SelectionStart < SelectionEnd then
      // reached end of selection
      NextSelected := false
    else
      // reached start
      NextSelected := true;
  end;
end;

function InvertRGB( Arg: TfpgColor ): TfpgColor;
begin
  Result := fpgColorToRGB( Arg ); // in case it's a system color e.g. button face
  Result := Result xor $ffffff; // now invert the RGB components
end;

// Draw a string at the given location with given color/selected state
procedure DrawRichTextString( var FontManager: TCanvasFontManager; Layout: TRichTextLayout;
    var X: longint; Y: longint; S: PChar; Len: longint; Selected: Boolean;
    PenColor: TfpgColor; BackColor: TfpgColor );
var
  Point: TPoint;
begin
ProfileEvent('DEBUG:  DrawRichTextString >>>');
  if Len = 0 then
    exit;

  Point.X := X;
  Point.Y := Y;

  if Selected then
  begin
    FontManager.Canvas.Color := InvertRGB( BackColor );
    FontManager.Canvas.TextColor := InvertRGB(PenColor);
  end
  else
  begin
    FontManager.Canvas.Color := BackColor;
    FontManager.Canvas.TextColor := PenColor;
  end;
  if FontManager.Canvas.Color <> Layout.FRichTextSettings.DefaultBackgroundColor then
    FontManager.Canvas.FillRectangle(x, y,
        FontManager.Canvas.Font.TextWidth(s),
        FontManager.Canvas.Font.Height);
  FontManager.DrawString( Point, Len, S );
  X := Point.X;
ProfileEvent('DEBUG:  DrawRichTextString <<<');
end;

var
  // global, so that we don't reallocate every drawline
  StringToDraw: String = '';

// Draw the specified line at the specified
// (physical) location
Procedure DrawRichTextLine( var FontManager: TCanvasFontManager;
    Layout: TRichTextLayout; SelectionStart: PChar; SelectionEnd: PChar;
    Line: TLayoutLine; Start: TPoint );
var
  X, Y: longint;
  Element: TTextElement;
  StartedDrawing: boolean;
  Style: TTextDrawStyle;
  P: PChar;
  NextP: PChar;
  EndP: PChar;
  BitmapIndex: longint;
  Bitmap: TfpgImage;
  BitmapRect: TRect;
  TextBlockStart: PChar;
  Selected: boolean;
  NextSelected: boolean;
  NewMarginX: longint;
  fStyle: string;

  procedure DrawTextBlock;
  begin
    DrawRichTextString( FontManager, Layout,
                        X,         // value gets adjusted by the time it returns
                        Y,         // value gets adjusted by the time it returns
                        PChar(StringToDraw),
                        Length(StringToDraw),
                        Selected,
                        Style.Color,
                        Style.BackgroundColor);
    StringToDraw := '';
  end;


begin
ProfileEvent('DEBUG:  DrawRichTextLine >>>');
  P := Line.Text;
  EndP := Line.Text + Line.Length;

  if P = EndP then
  begin
    // Empty line
    exit;
  end;

  Selected := false;
  if SelectionStart <= Line.Text then
    // selection start is above.
    Selected := true;
  if SelectionEnd <= Line.Text then
    // selection end is above.
    Selected := not Selected;

  StringToDraw := '';

  Style := Line.Style;
  fStyle := Style.FontNameSize;
  ApplyFontAttributes(fStyle, Style.FontAttributes);
  FontManager.SetFont( fStyle );
  StartedDrawing := false;

  TextBlockStart := P;

  Y := Start.Y; // + Line.MaxDescender; // co-ordinates are from top/left, so do we need descender? [Graeme]

  while P < EndP do
  begin
    Element := ExtractNextTextElement( P, NextP );

    if SelectionChange( P,
                        SelectionStart,
                        SelectionEnd,
                        NextSelected ) then
    begin
      DrawTextBlock;
      TextBlockStart := P;
      Selected := NextSelected;
    end;

    case Element.ElementType of
      teWordBreak,
      teText,
      teImage:
      begin
        if not StartedDrawing then
        begin
          // we haven't yet started drawing:
          // so work out alignment
          X := Start.X + Layout.GetStartX( Style, Line );
          StartedDrawing := true;
        end;

        // Now do the drawing
        if Element.ElementType = teImage then
        begin
          DrawTextBlock;
          TextBlockStart := NextP;

          try
            BitmapIndex := StrToInt( Element.Tag.Arguments );
          except
            BitmapIndex := -1;
          end;
          if Layout.IsValidBitmapIndex( BitmapIndex ) then
          begin
            Bitmap := Layout.Images.Items[BitmapIndex].Image;

            BitmapRect.Left := X;
            BitmapRect.Top := Start.Y;
            BitmapRect.Right := Trunc(BitmapRect.Left
                                + Bitmap.Width * Layout.HorizontalImageScale);
            BitmapRect.Bottom := Trunc(BitmapRect.Top
                              + Bitmap.Height * Layout.VerticalImageScale);

            FontManager.Canvas.StretchDraw(BitmapRect.Left, BitMapRect.Top,
                BitmapRect.Right-BitMapRect.Left, BitMapRect.Bottom-BitMapRect.Top, Bitmap);

            inc( X, trunc( Bitmap.Width * Layout.HorizontalImageScale ) );
          end;
        end
        else
        begin
          // character (or word break)
          // build up the successive characters...
          StringToDraw := StringToDraw + Element.Character;
        end;
      end;

      teStyle:
      begin
        DrawTextBlock;
        TextBlockStart := NextP;

        if     ( Element.Tag.TagType = ttItalicOff )
           and ( faItalic in Style.FontAttributes )
           and ( not FontManager.IsFixed )
           then
          // end of italic; add a space
          inc( X, FontManager.CharWidth( ' ' )  );

        Layout.PerformStyleTag( Element.Tag, Style, X );
        NewMarginX := ( Start.X + Style.LeftMargin );
        if NewMarginX > X then
        begin
          //skip across...
          X := NewMarginX;
        end;
      end;
    end;
    P := NextP;
  end;

  DrawTextBlock;
ProfileEvent('DEBUG:  DrawRichTextLine <<<');
end;

Procedure DrawRichTextLayout( var FontManager: TCanvasFontManager;
                              Layout: TRichTextLayout;
                              const SelectionStart: PChar;
                              const SelectionEnd: PChar;
                              const StartLine: longint;
                              const EndLine: longint;
                              const StartPoint: TPoint );
Var
  Line: TLayoutLine;
  LineIndex: longint;
  Y: longint;
  BottomOfLine: longint;
begin
ProfileEvent('DEBUG:  DrawRichTextLayout >>>');
  assert( StartLine >= 0 );
  assert( StartLine <= Layout.FNumLines );
  assert( EndLine >= 0 );
  assert( EndLine <= Layout.FNumLines );
  assert( StartLine <= EndLine );

  if Layout.FNumLines = 0 then
    // no text to draw
    exit;

  Y := StartPoint.Y + Layout.FRichTextSettings.Margins.Top;
  LineIndex := 0;

  // debug only to show Margins.
  //FontManager.Canvas.Color:= clRed;
  //FontManager.Canvas.DrawLine(0, y, 300, y);

  repeat
    Line := Layout.FLines[LineIndex];
    BottomOfLine := Y;

    if // the line is in the range to be drawn
           ( LineIndex >= StartLine )
       and ( LineIndex <= EndLine )

       // and the line is within the cliprect
       and ( BottomOfLine < FontManager.Canvas.GetClipRect.Bottom )        // -> so we can see partial lines at bottom scroll into the screen
       and ( Y >=  FontManager.Canvas.GetClipRect.Top - Line.Height) then  // -> so we can see partial lines at top scroll off the screen
    begin
      // draw it. First decided whether selection is started or not.
      DrawRichTextLine( FontManager,
                        Layout,
                        SelectionStart,
                        SelectionEnd,
                        Line,
                        Point(StartPoint.X, Y) );

    end;
    inc( Y, Line.Height );

    { TODO 99 -oGraeme -cMUST FIX : Must remove this hard-coded value. It's just a test!!! }
    // 4 is the Border Width of 2px times 2 borders.
    if Y > (FontManager.Widget.Height-4) then
      // past bottom of output canvas
      break;

    inc( LineIndex );

    if LineIndex >= Layout.FNumLines then
      // end of text
      break;

  until false;
ProfileEvent('DEBUG:  DrawRichTextLayout <<<');
End;

Procedure PrintRichTextLayout( var FontManager: TCanvasFontManager;
                               var Layout: TRichTextLayout;
                               const StartLine: longint;
                               var EndLine: longint;
                               const StartY: longint;
                               var EndY: longint );
Var
  Selected: boolean;
  Line: TLayoutLine;
  LineIndex: longint;

  Y: longint;

  BottomOfLine: longint;

  LinesPrinted: longint;
begin
  assert( StartLine >= 0 );
  assert( StartLine <= Layout.FNumLines );

  if Layout.FNumLines = 0 then
    // no text to draw
    exit;

  Y := StartY - Layout.FRichTextSettings.Margins.Top;
  Selected := false; // it's not going to change.
  LinesPrinted := 0;
  LineIndex := StartLine;

  repeat
    Line := TLayoutLine(Layout.FLines[ LineIndex ]);
    BottomOfLine := Y - Line.Height + 1; // bottom pixel row is top - height + 1

    if BottomOfLine < Layout.FRichTextSettings.Margins.Bottom then
      // past bottom of page (less margin)
      if LinesPrinted > 0 then
        // stop, as long as we've printed at least 1 line
        break;

    // draw it
    DrawRichTextLine( FontManager,
                      Layout,
                      nil,
                      nil,
                      Line,
                      Point( 0,
                             BottomOfLine ) );

    dec( Y, Line.Height );

    inc( LinesPrinted );

    inc( LineIndex );

    if LineIndex >= Layout.FNumLines then
      // end of text
      break;

  until false;

  EndY := Y;
  EndLine := LineIndex;
end;


end.

