Unit RichTextPrintUnit;

Interface

uses
  Graphics,
  RichTextStyleUnit;

// Prints the specified rich text, starting at page position PageY.
// Starts new pages as needed; when done, PageY is the final position used
// on the final page.
Procedure PrintRichText( Text: PChar;
                         Images: TImageList;
                         Settings: TRichTextSettings;
                         var PageY: longint );

Implementation

uses
  Classes,
  Printers,
  CanvasFontManager,
  RichTextLayoutUnit, RichTextDisplayUnit, Forms
  ;

Procedure PrintRichText( Text: PChar;
                         Images: TImageList;
                         Settings: TRichTextSettings;
                         var PageY: longint );
var
  Layout: TRichTextLayout;
  FontManager: TCanvasFontManager;
  LineIndex: longint;
  Y: longint;
  FinishLine: longint;
  FinishY: longint;
Begin
  FontManager := TCanvasFontManager.Create( Printer.Canvas,
                                            false // don't allow bitmap fonts
                                          );

  Layout := TRichTextLayout.Create( Text,
                                    Images,
                                    Settings,
                                    FontManager,
                                    Printer.PageWidth );

  LineIndex := 0;
  Y := PageY;
  repeat
    PrintRichTextLayout( FontManager,
                         Layout,
                         LineIndex,
                         FinishLine,
                         Y,
                         FinishY );
    LineIndex := FinishLine;
    Y := FinishY;

    if LineIndex < Layout.FNumLines then
    begin
      // didn't all fit on page, so new page
      Printer.NewPage;
      Y := Printer.PageHeight - 1;
    end;

  until LineIndex >= Layout.FNumLines;

  Layout.Destroy;
  FontManager.Destroy;
  PageY := Y;
end;

Initialization
End.
