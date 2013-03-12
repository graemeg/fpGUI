{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit contains the class declaration for the RichView text
      component that is used in DocView to display the help contents.
}

Unit RichTextView;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  fpg_base,
  fpg_main,
  fpg_widget,
  fpg_scrollbar,
  fpg_menu,
  fpg_imagelist,
  RichTextStyleUnit,
  RichTextLayoutUnit,
  CanvasFontManager;

const
  // for dragtext support, primarily.
  RT_QUERYTEXT    = FPGM_USER + 500;
    // Param1: pointer to buffer (may be nil)
    // Param2: buffer size (-1 to ignore)
    // Returns: number of bytes copied

  RT_QUERYSELTEXT = FPGM_USER + 501;
    // Param1: pointer to buffer (may be nil)
    // Param2: buffer size (-1 to ignore)
    // Returns: number of bytes copied

Type
  TFindOrigin = ( foFromStart, foFromCurrent );

  TScrollingDirection = ( sdUp, sdDown );


Type

  TRichTextView = class;

  // reimplement class
  TLinkEvent = procedure( Sender: TRichTextView; Link: string ) of object;


  TRichTextView = class(TfpgWidget)
  private
    FPopupMenu: TfpgPopupMenu;
    FScrollDistance: integer;
    FBorderStyle: TfpgEditBorderStyle;
    procedure   FVScrollbarScroll(Sender: TObject; position: integer);
    procedure   FHScrollbarScroll(Sender: TObject; position: integer);
    procedure   ShowDefaultPopupMenu(const x, y: integer; const shiftstate: TShiftState); virtual;
    Procedure   CreateDefaultMenu;
    Procedure   SelectAllMIClick( Sender: TObject );
    Procedure   CopyMIClick( Sender: TObject );
    Procedure   RefreshMIClick( Sender: TObject );
    Procedure   WordWrapMIClick( Sender: TObject );
    Procedure   SmoothScrollMIClick( Sender: TObject );
    Procedure   DebugMIClick( Sender: TObject );
    Procedure   DefaultMenuPopup( Sender: TObject );
    procedure   SetScrollDistance(const AValue: integer);
    procedure SetBorderStyle(AValue: TfpgEditBorderStyle);
  protected
    FFontManager: TCanvasFontManager;
    FRichTextSettings: TRichTextSettings;

    // Properties
//    FBorderStyle:TfpgBorderStyle;
    FScrollbarWidth: longint;
    FSmoothScroll: boolean;
    FUseDefaultMenu: boolean;
    FDebug: boolean;

    FOnOverLink: TLinkEvent;
    FOnNotOverLink: TLinkEvent;
    FOnClickLink: TLinkEvent;

    FDefaultMenu: TfpgPopupMenu;
    FSelectAllMI: TfpgMenuItem;
    FCopyMI: TfpgMenuItem;
    FRefreshMI: TfpgMenuItem;
    FWordWrapMI: TfpgMenuItem;
    FSmoothScrollMI: TfpgMenuItem;
    FDebugMI: TfpgMenuItem;

    // Internal layout data
    FNeedVScroll, FNeedHScroll: boolean;

    FLayoutRequired: boolean;
    FLayout: TRichTextLayout;

    // Child controls
    FHScrollbar: TfpgScrollbar;
    FVScrollbar: TfpgScrollbar;

    // Text
    FText: PChar;

    FTopCharIndex: longint; // only applies until following flag set.
    FVerticalPositionInitialised: boolean;

    FCursorRow: longint;
    FCursorOffset: longint;
    FSelectionStart: longint;
    FSelectionEnd: longint;
    FImages: TfpgImageList;

    // Selection scrolling
    //FScrollTimer: TfpgTimer;
    FOldMousePoint: TPoint;
    FScrollingDirection: TScrollingDirection;

    // Scroll information
    // we use these rather than the scrollbar positions direct,
    // since those are not updated during tracking
    FXScroll: longint;
    FYScroll: longint;

    FLastXScroll: longint;
    FLastYScroll: longint;

    // Link
    FLastLinkOver: string;
    FClickedLink: string;

    Procedure CreateWnd;
    procedure HandleResize(AWidth, AHeight: TfpgCoord); override;
    procedure UpdateScrollBarCoords;
    procedure HandlePaint; override;
    procedure HandleHide; override;
    procedure HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;

    //procedure ScanEvent( Var KeyCode: TKeyCode;
    //                     RepeatCount: Byte ); override;

    //Procedure MouseDown( Button: TMouseButton;
    //                     ShiftState: TShiftState;
    //                     X, Y: Longint ); override;
    //Procedure MouseUp( Button: TMouseButton;
    //                   ShiftState: TShiftState;
    //                   X, Y: Longint ); override;

    //Procedure MouseDblClick( Button: TMouseButton;
    //                         ShiftState: TShiftState;
    //                         X, Y: Longint ); override;

    //Procedure MouseMove( ShiftState: TShiftState;
    //                     X, Y: Longint ); override;

    //Procedure Scroll( Sender: TScrollbar;
    //                  ScrollCode: TScrollCode;
    //                  Var ScrollPos: Longint ); override;

    //Procedure KillFocus; override;
    //Procedure SetFocus; override;

    // Messages for DragText
    Procedure RTQueryText( Var Msg: TfpgMessageRec ); message RT_QUERYTEXT;
    Procedure RTQuerySelText( Var Msg: TfpgMessageRec ); message RT_QUERYSELTEXT;

    procedure Layout;

    function FindPoint( XToFind: longint;
                        YToFind: longint;
                        Var LineIndex: longint;
                        Var Offset: longint;
                        Var Link: string ): TTextPosition;

    // Scroll functions

    // Scroll display to given positions (does NOT
    // update scrollbars as this may be called during
    // scrolling)
    Procedure DoVerticalScroll( NewY: longint );
    Procedure DoHorizontalScroll( NewX: longint );

    // Set scrollbar position, and update display
    Procedure SetVerticalPosition( NewY: longint );
    Procedure SetHorizontalPosition( NewX: longint );

    procedure OnScrollTimer( Sender: TObject );
    Function GetLineDownPosition: longint;
    Function GetLineUpPosition: longint;
    Function GetSmallDownScrollPosition: longint;
    Function GetSmallUpScrollPosition: longint;
    Function GetSmallRightScrollPosition: longint;
    Function GetSmallLeftScrollPosition: longint;

    // Calculates line down position given the last line and displayed pixels
    Function GetLineDownPositionFrom( LastLine: longint; PixelsDisplayed: longint ): longint;
    Function GetLineUpPositionFrom( FirstVisibleLine: longint; Offset: longint ): longint;

    // Drawing functions
    Procedure DrawBorder;
    Procedure Draw( StartLine, EndLine: longint );

    // Rectangle (GetClientRect) minus scrollbars (if they are enabled)
    Function GetDrawRect: TfpgRect;
    // Rectangle minus scrollbars (GetDrawRect), minus extra 2px border all round
    function GetTextAreaRect: TfpgRect;
    function GetTextAreaHeight: longint;
    function GetTextAreaWidth: longint;

    // Queries
    procedure GetFirstVisibleLine( Var LineIndex: longint; Var Offset: longint );
    procedure GetBottomLine( Var LineIndex: longint; Var PixelsDisplayed: longint );

    // Layout functions
    Procedure SetupScrollbars;
    Procedure SetupCursor;
    procedure RemoveCursor;

    function GetTextEnd: longint;

    // property handlers
//    procedure SetBorder( BorderStyle: TBorderStyle );
    Procedure SetDebug( Debug: boolean );
    Procedure SetScrollBarWidth( NewValue: longint );

    Procedure OnRichTextSettingsChanged( Sender: TObject );

    function GetCursorIndex: longint;

    Function GetTopCharIndex: longint;
    Procedure SetTopCharIndex( NewValue: longint );
    Function GetTopCharIndexPosition( NewValue: longint ): longint;

    // Update the cursor row/column for the selction start/end
    procedure RefreshCursorPosition;

    procedure SetCursorIndex( Index: longint;
                              PreserveSelection: boolean );
    procedure SetCursorPosition( Offset: longint;
                                 Row: longint;
                                 PreserveSelection: boolean );

    procedure MakeRowVisible( Row: longint );
    procedure MakeRowAndColumnVisible(Row: longint; Column: longint);

    // These two methods set selection start and end,
    // and redraw the screen, but do not set up cursor.
    Procedure SetSelectionStartInternal( SelectionStart: longint );
    Procedure SetSelectionEndInternal( SelectionEnd: longint );

    // Property handlers. These are for programmatic access
    // where a complete setup of selection is needed
    Procedure SetSelectionStart( SelectionStart: longint );
    Procedure SetSelectionEnd( SelectionEnd: longint );

    Procedure SetImages( AImages: TfpgImageList );
    Procedure Notification( AComponent: TComponent;
                            Operation: TOperation ); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; Override;
    // rect (of component) minus frame borders
    function    GetClientRect: TfpgRect; override;
    procedure   AddText( Text: PChar; ADelay: boolean = False );
    procedure   AddParagraph( Text: PChar );
    procedure   AddSelectedParagraph( Text: PChar );
    procedure   Clear(const ADestroying: boolean = False);
    procedure   InsertText( CharIndexToInsertAt: longword; TextToInsert: PChar );
    property    Text: PChar read FText;
    property    TextEnd: longint read GetTextEnd;
    property    SelectionStart: longint read FSelectionStart write SetSelectionStart;
    property    SelectionEnd: longint read FSelectionEnd write SetSelectionEnd;
    property    CursorIndex: longint read GetCursorIndex;

    // Copy all text to buffer
    // Buffer can be nil to simply get size.
    // If BufferLength is negative, it is ignored
    Function CopyTextToBuffer( Buffer: PChar; BufferLength: longint ): longint;

    // Clipboard
    Procedure CopySelectionToClipboard;

    // returns number of chars (that would be) copied.
    // Buffer can be nil to simply get size.
    // If BufferLength is negative, it is ignored
    Function CopySelectionToBuffer( Buffer: PChar;
                                    BufferLength: longint ): longint;

    Function GetSelectionAsString: string; // returns up to 255 chars obviously

    // Selection queries
    Function SelectionLength: longint; // Note: includes formatting
    Function SelectionSet: boolean; // returns true if there is a selection

    // Selection actions
    Procedure ClearSelection;
    Procedure SelectAll;

    property CursorRow: longint read FCursorRow;

    // Navigation
    procedure GoToTop;
    procedure GotoBottom;
    Procedure UpLine;
    Procedure DownLine;
    Procedure UpPage;
    Procedure DownPage;

    Procedure SmallScrollUp;
    Procedure SmallScrollDown;
    Procedure SmallScrollLeft;
    Procedure SmallScrollRight;

    Procedure MakeCharVisible( CharIndex: longint );
    Property TopCharIndex: longint read GetTopCharIndex write SetTopCharIndex;

    Procedure CursorLeft( PreserveSelection: boolean );
    Procedure CursorRight( PreserveSelection: boolean );
    Procedure CursorDown( PreserveSelection: boolean );
    Procedure CursorUp( PreserveSelection: boolean );
    Procedure CursorPageDown( PreserveSelection: boolean );
    Procedure CursorPageUp( PreserveSelection: boolean );

    Procedure CursorToLineStart( PreserveSelection: boolean );
    Procedure CursorToLineEnd( PreserveSelection: boolean );

    Procedure CursorWordLeft( PreserveSelection: boolean );
    Procedure CursorWordRight( PreserveSelection: boolean );

    function HighlightNextLink: boolean;
    function HighlightPreviousLink: boolean;

    // Search for the given text
    // if found, returns true, MatchIndex is set to the first match,
    //   and MatchLength returns the length of the match
    //   (which may be greater than the length of Text due to
    //    to skipping tags)
    // if not found, returns false, pMatch is set to -1
    function FindString( Origin: TFindOrigin;
                         const AText: string;
                         var MatchIndex: longint;
                         var MatchLength: longint ): boolean;

    // Searches for text and selects it found
    // returns true if found, false if not
    function Find( Origin: TFindOrigin;
                   const AText: string ): boolean;

    function LinkFromIndex( const CharIndexToFind: longint): string;

  published
    property Align;
    property BackgroundColor default clBoxColor;
    property BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default ebsDefault;
    //property ParentColor;
    //property ParentFont;
    //property ParentPenColor;
    property ParentShowHint;
    property PopupMenu: TfpgPopupMenu read FPopupMenu write FPopupMenu;
    property ShowHint;
    Property TabOrder;
    Property Focusable;
    property Visible;
    property RichTextSettings: TRichTextSettings read FRichTextSettings;
    property ScrollBarWidth: longint read FScrollBarWidth write SetScrollBarWidth default 16;
    property SmoothScroll: boolean read FSmoothScroll write FSmoothScroll;
    property ScrollDistance: integer read FScrollDistance write SetScrollDistance default 75;
    property UseDefaultMenu: boolean read FUseDefaultMenu write FUseDefaultMenu default True;
    property Debug: boolean read FDebug write SetDebug default False;
    property Images: TfpgImageList read FImages write SetImages;

    // ------- EVENTS ----------

    // Called with the name of the link when the mouse first moves over it
    property OnOverLink: TLinkEvent read FOnOverLink write FOnOverLink;

    // Called with the name of the link when the mouse leaves it
    property OnNotOverLink: TLinkEvent read FOnNotOverLink write FOnNotOverLink;

    // Called when the link is clicked.
    property OnClickLink: TLinkEvent read FOnClickLink write FOnClickLink;

    Property OnClick;
    Property OnDoubleClick;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnEndDrag;
    Property OnEnter;
    Property OnExit;
    //Property OnFontChange;
    //Property OnMouseClick;
    //Property OnMouseDblClick;
    //Property OnSetupShow;

    //Property OnScan;
  End;


implementation

uses
  SysUtils
  ,ACLStringUtility
  ,nvUtilities
  ,RichTextDocumentUnit
  ,RichTextDisplayUnit
  ,fpg_stringutils
  ,SettingsUnit   // TODO: We shouldn't have this dependency!!
  ;

Procedure TRichTextView.SetSelectionStart( SelectionStart: longint );
begin
  RemoveCursor;
  SetSelectionStartInternal( SelectionStart );
  RefreshCursorPosition;
  SetupCursor;
end;

Procedure TRichTextView.SetSelectionEnd( SelectionEnd: longint );
begin
  RemoveCursor;
  SetSelectionEndInternal( SelectionEnd );
  RefreshCursorPosition;
  SetupCursor;
end;

Procedure TRichTextView.SetSelectionStartInternal( SelectionStart: longint );
begin
  if SelectionStart = FSelectionStart then
    exit;

  if SelectionSet then
    if SelectionStart = -1 then
      // small side effect here - also sets selectionend to -1
      ClearSelection;

  FSelectionStart := SelectionStart;
  if FSelectionEnd = -1 then
    // still no selection
    exit;
  RePaint;
end;

Procedure TRichTextView.SetSelectionEndInternal( SelectionEnd: longint );
var
  StartRedrawLine: longint;
  EndRedrawLine: longint;
  OldClip: TfpgRect;
begin
  if SelectionEnd = FSelectionEnd then
    exit;

  if FSelectionStart = -1 then
  begin
    FSelectionEnd := SelectionEnd;
    // still not a valid selection, no need to redraw
    exit;
  end;

  if SelectionEnd = FSelectionStart then
    SelectionEnd := -1;

  if ( FSelectionEnd = -1 ) then
  begin
    // there is currently no selection,
    // and we are setting one: need to draw it all
    StartRedrawLine := FLayout.GetLineFromCharIndex( FSelectionStart );
    EndRedrawLine := FLayout.GetLineFromCharIndex( SelectionEnd );
  end
  else
  begin
    // there is already a selection
    if SelectionEnd = -1 then
    begin
      // and we're clearing it
      StartRedrawLine := FLayout.GetLineFromCharIndex( FSelectionStart );
      EndRedrawLine := FLayout.GetLineFromCharIndex( FSelectionEnd );
    end
    else
    begin
      // and we're setting a new one, so draw from the old end to the new
      StartRedrawLine := FLayout.GetLineFromCharIndex( FSelectionEnd );
      EndRedrawLine := FLayout.GetLineFromCharIndex( SelectionEnd );
    end;
  end;

  FSelectionEnd := SelectionEnd;

  OldClip := Canvas.GetClipRect;
  Canvas.SetClipRect(GetTextAreaRect);

  // (re)draw selection
  { TODO -ograeme : Draw must not be called here }
//  Draw( StartRedrawLine, EndRedrawLine );
  Canvas.SetClipRect(OldClip);
end;

Procedure TRichTextView.ClearSelection;
var
  OldClip: TfpgRect;
  StartLine: longint;
  EndLine: longint;
begin
  if SelectionSet then
  begin
    OldClip := Canvas.GetClipRect;
    Canvas.SetClipRect(GetTextAreaRect);

    StartLine := FLayout.GetLineFromCharIndex( FSelectionStart );
    EndLine := FLayout.GetLineFromCharIndex( FSelectionEnd );

    FSelectionEnd := -1;
    FSelectionStart := -1;
    Canvas.SetClipRect(OldClip);
 end;

  FSelectionEnd := -1;
  FSelectionStart := -1;
  Repaint;
end;

Function TRichTextView.GetTextEnd: longint;
begin
  Result := StrLen( FText );
end;

Procedure TRichTextView.CreateDefaultMenu;
begin
  FDefaultMenu := TfpgPopupMenu.Create(nil);
  FDefaultMenu.OnShow := @DefaultMenuPopup;

  with FDefaultMenu do
  begin
    FSelectAllMI    := AddMenuItem('Select &All', '', @SelectAllMIClick);
    FCopyMI         := AddMenuItem('&Copy', '', @CopyMIClick);
    AddMenuItem('-', '', nil);
    FRefreshMI      := AddMenuItem('&Refresh', '', @RefreshMIClick);
    AddMenuItem('-', '', nil);
    FSmoothScrollMI := AddMenuItem('&Smooth Scrolling', '', @SmoothScrollMIClick);
    FWordWrapMI     := AddMenuItem('&Word Wrap', '', @WordWrapMIClick);
    FDebugMI        := AddMenuItem('&Debug', '', @DebugMIClick);
  end;

  FSelectAllMI.Enabled := False;  // TODO: implement me
  FCopyMI.Enabled := False;  // TODO: implement me
end;

Procedure TRichTextView.SelectAllMIClick( Sender: TObject );
begin
  SelectAll;
end;

Procedure TRichTextView.CopyMIClick( Sender: TObject );
begin
  CopySelectionToClipBoard;
end;

Procedure TRichTextView.RefreshMIClick( Sender: TObject );
begin
  RePaint;
end;

Procedure TRichTextView.WordWrapMIClick( Sender: TObject );
begin
  FRichTextSettings.DefaultWrap := not FRichTextSettings.DefaultWrap;
end;

Procedure TRichTextView.SmoothScrollMIClick( Sender: TObject );
begin
  SmoothScroll := not SmoothScroll;
end;

Procedure TRichTextView.DebugMIClick( Sender: TObject );
begin
  Debug := not Debug;
//  writeln('VScrollbar.Position=', FVScrollbar.Position, '  min/max=', FVScrollbar.Min, '/', FVScrollbar.Max);
//  writeln('FNeedHScroll=', FNeedHScroll, '   FNeedVScroll=', FNeedVScroll);
  RePaint;
end;

Procedure TRichTextView.DefaultMenuPopup( Sender: TObject );
begin
  FWordWrapMI.Checked := FRichTextSettings.DefaultWrap;
  FSmoothScrollMI.Checked := SmoothScroll;
  FDebugMI.Checked := Debug;
end;

procedure TRichTextView.SetScrollDistance(const AValue: integer);
begin
  if FScrollDistance = AValue then
    exit;
  FScrollDistance := AValue;
  if Assigned(FVScrollBar) then
    FVScrollBar.ScrollStep := FScrollDistance;
  if Assigned(FHScrollBar) then
    FHScrollBar.ScrollStep := FScrollDistance;
end;

procedure TRichTextView.SetBorderStyle(AValue: TfpgEditBorderStyle);
begin
  if FBorderStyle = AValue then
    Exit;
  FBorderStyle := AValue;
  Repaint;
end;

constructor TRichTextView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'RichTextView';
  FWidth := 150;
  FHeight := 70;
  FFocusable := True;
  FBorderStyle := ebsDefault;

  FNeedVScroll := False;
  FNeedHScroll := False;
  FSmoothScroll := True;
  FScrollbarWidth := 16;
  FUseDefaultMenu := True;
  FDebug          := False;
  FLayoutRequired := True;

  FTextColor        := Parent.TextColor;
  FBackgroundColor  := clBoxColor;

  FRichTextSettings := TRichTextSettings.Create( self );
  FRichTextSettings.Margins := Rect( 5, 5, 5, 5 );
  FRichTextSettings.OnChange := @OnRichTextSettingsChanged;

  FImages := nil;

  if not InDesigner then
  begin
    FFontManager := nil;

    FText := StrAlloc( 100 );
    FText[ 0 ] := #0;

    FTopCharIndex := 0;
    FVerticalPositionInitialised := false;
  end;

  CreateWnd;
end;

procedure TRichTextView.HandlePaint;
Var
  CornerRect: TfpgRect;
  TextRect: TfpgRect;
  DrawRect: TfpgRect;
  rect: TRect;
  x, y: integer;

  // Just for fun! :-)
  procedure DesignerPainting(const AText: string; AColor: TfpgColor; AFontDesc: TfpgString = '');
  var
    oldf: TfpgString;
  begin
    oldf := '';
    if AFontDesc <> '' then
    begin
      oldf := Canvas.Font.FontDesc; // save original font
      Canvas.Font := fpgGetFont(AFontDesc); // set new font
    end;
    Canvas.TextColor := AColor; // set new color
    Canvas.DrawString(x, 10, AText);
    x := x + Canvas.Font.TextWidth(AText);  // calc x offset for next text
    if oldf <> '' then
      Canvas.Font := fpgGetFont(oldf);  // restore original font
  end;

begin
  ProfileEvent('TRichTextView.HandlePaint >>>');
  Canvas.ClearClipRect;
  DrawBorder;
  DrawRect := GetDrawRect;
  Canvas.Color := RichTextSettings.DefaultBackgroundColor;
  Canvas.FillRectangle(DrawRect);

  TextRect := GetTextAreaRect;
  Canvas.SetClipRect(TextRect);

  if InDesigner then
  begin
    Canvas.TextColor := clInactiveWgFrame;
    x := 10;
    DesignerPainting('<', clInactiveWgFrame);
    DesignerPainting('rich', clBlack, 'Sans-10:bold');
    DesignerPainting(' text', clRed, 'Sans-10:italic');
    DesignerPainting(' ', clInactiveWgFrame);
    DesignerPainting('will', clBlue, 'Sans-10:underline');
    DesignerPainting(' appear here>', clInactiveWgFrame);
//    Canvas.DrawString(10, 10, '<rich text will appear here>');
    Canvas.ClearClipRect;
    Exit; //==>
  end;

  if Length(FText) = 0 then
    exit;   // no need to paint anything further.

  if FLayoutRequired then
    // we haven't yet done a layout
    Layout;
  if not Debug then
    Draw( 0, FLayout.FNumLines )
  else
    Canvas.DrawText(8, 8, GetTextAreaWidth, GetTextAreaHeight{1000}, FText, [txtLeft, txtTop, txtWrap]);
  Canvas.ClearClipRect;

  if FHScrollbar.Visible and FVScrollbar.Visible then
  begin
    case BorderStyle of
      ebsNone:
        begin
          x := 0;
          y := 0;
        end;
      ebsDefault:
        begin
          rect := fpgStyle.GetControlFrameBorders;
          x := rect.Right;
          y := rect.Bottom;
        end;
      ebsSingle:
        begin
          x := 1;
          y := 1;
        end;
    end;
    // blank out corner between scrollbars
    CornerRect.Left := Width - x - FScrollBarWidth;
    CornerRect.Top := Height - y - FScrollBarWidth;
    CornerRect.Width := FScrollBarWidth;
    CornerRect.Height := FScrollBarWidth;
    Canvas.Color := clWindowBackground;
    Canvas.FillRectangle(CornerRect);
  end;
ProfileEvent('DEBUG:  TRichTextView.HandlePaint <<<');
end;

procedure TRichTextView.HandleHide;
begin
//  fpgCaret.UnSetCaret (Canvas);
  inherited HandleHide;
end;

procedure TRichTextView.HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
  var consumed: boolean);
begin
ProfileEvent('HandleKeyPress');
  case keycode of
    keyPageDown:
        begin
          consumed := True;
          UpPage;
        end;
    keyPageUp:
        begin
          consumed := True;
          DownPage;
        end;

  end;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TRichTextView.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(PopupMenu) then
    PopupMenu.ShowAt(self, x, y)
  else
    ShowDefaultPopupMenu(x, y, ShiftState);
end;

procedure TRichTextView.HandleMouseScroll(x, y: integer; shiftstate: TShiftState;
  delta: smallint);
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  if delta < 0 then
    // scroll up
    SetVerticalPosition(FVScrollbar.Position - FVScrollbar.ScrollStep)
  else
    // scroll down
    SetVerticalPosition(FVScrollbar.Position + FVScrollbar.ScrollStep);
end;

procedure TRichTextView.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  Line: longint;
  Offset: longint;
  Link: TfpgString;
  Position: TTextPosition;
  Shift: boolean;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if FText = '' then
    Exit; //==>   no need to do anything further

  Offset := 0;
  Position := FindPoint( X, Y, Line, Offset, Link );
  FClickedLink := Link;
  //writeln('  link=', Link, '  line=', Line, ' offset=', offset);

  if Position in [tpAboveTextArea, tpBelowTextArea] then
    // not on the control (this probably won't happen)
    exit;

  // if shift is pressed then keep the same selection start.
  Shift := ssShift in ShiftState;
  RemoveCursor;

  if not Shift then
    ClearSelection;

  SetCursorPosition(Offset, Line, Shift);
end;

procedure TRichTextView.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  if FClickedLink <> '' then
  begin
    if Assigned( FOnClickLink ) then
      FOnClickLink( Self, FClickedLink );
  end;
  FClickedLink := ''; // reset link
end;

procedure TRichTextView.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  Line: longint;
  Offset: longint;
  Link: TfpgString;
  Position: TTextPosition;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  if FText = '' then
    exit;
  Position := FindPoint(X, Y, Line, Offset, Link);

  if Link <> FLastLinkOver then
  begin
    if Link <> '' then
    begin
      if Assigned(FOnOverLink) then
        FOnOverLink(Self, Link)
    end
    else
    begin
      if Assigned(FOnNotOverLink) then
        FOnNotOverLink(Self, FLastLinkOver);
    end;
    FLastLinkOver := Link;
  end;

  if Link <> '' then
    MouseCursor := mcHand
  else
    MouseCursor := mcDefault;   // TODO: later this should be IBeam when RichView supports editing
end;

Destructor TRichTextView.Destroy;
Begin
  FDefaultMenu.Free;
  // destroy the font manager NOW
  // while the canvas is still valid
  // (it will be freed in TControl.DisposeWnd)
  // in order to release logical fonts
  FFontManager.Free;
  if Assigned(FLayout) then
    FreeAndNil(FLayout);

  //FScrollTimer.Free;
  if not InDesigner then
  begin
    RemoveCursor;
    StrDispose( FText );
  end;
  Inherited Destroy;
End;

//Procedure TRichTextView.KillFocus;
//begin
//  RemoveCursor;
//  inherited KillFocus;
//end;

//Procedure TRichTextView.SetFocus;
//begin
//  inherited SetFocus;
//  SetupCursor;
//end;

// Custom window messages for DragText support
Procedure TRichTextView.RTQueryText( Var Msg: TfpgMessageRec );
begin
  //Msg.Handled := true;
  //Msg.Result :=
  //  CopyPlainTextToBuffer( FText,
  //                         FText + strlen( FText ),
  //                         PChar( Msg.Param1 ),
  //                         Msg.Param2 );
end;

Procedure TRichTextView.RTQuerySelText( Var Msg: TfpgMessageRec );
begin
  //Msg.Handled := true;
  //Msg.Result :=
  //  CopySelectionToBuffer( PChar( Msg.Param1 ),
  //                         Msg.Param2 );
end;

Procedure TRichTextView.SetDebug( Debug: boolean );
begin
  if Debug = FDebug then
    exit;
  FDebug := Debug;
  RePaint;
end;

Procedure TRichTextView.SetScrollBarWidth( NewValue: longint );
begin
  if    ( NewValue < 0 )
     or ( NewValue = FScrollBarWidth ) then
    exit;
  FScrollBarWidth := NewValue;
  Layout;
  RePaint;
end;

procedure TRichTextView.FVScrollbarScroll(Sender: TObject; position: integer);
begin
  SetVerticalPosition(position);
end;

procedure TRichTextView.FHScrollbarScroll(Sender: TObject; position: integer);
begin
  SetHorizontalPosition(position);
end;

procedure TRichTextView.ShowDefaultPopupMenu(const x, y: integer;
  const shiftstate: TShiftState);
begin
  if not Assigned(FDefaultMenu) then
    CreateDefaultMenu;
  FDefaultMenu.ShowAt(x, y);
end;

Procedure TRichTextView.CreateWnd;
begin
  if InDesigner then
    exit;

  FFontManager := TCanvasFontManager.Create(Canvas, Self);
  FLastLinkOver := '';
  FSelectionStart := -1;
  FSelectionEnd := -1;

  if FUseDefaultMenu then
  begin
    CreateDefaultMenu;
    FPopupMenu := FDefaultMenu;
  end;

  FHScrollbar := TfpgScrollBar.Create( self );
  FHScrollbar.Visible := False;
  FHScrollbar.Orientation := orHorizontal;
  FHScrollBar.SetPosition(2, Height-2-FScrollbarWidth, Width-4-FScrollbarWidth, FScrollbarWidth);

  FVScrollbar := TfpgScrollBar.Create( self );
  FVScrollBar.Visible := False;
  FVScrollBar.Orientation := orVertical;
  FVScrollbar.SetPosition(Width-2-FScrollbarWidth, 2, FScrollbarWidth, Height-4-FScrollbarWidth);

//  FScrollTimer := TfpgTimer.Create( 100 );
//  FScrollTimer.OnTimer := @OnScrollTimer;

//  FLinkCursor := GetLinkCursor;

  if FLayoutRequired then
    // we haven't yet done a layout
    Layout;
end;

procedure TRichTextView.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  if InDesigner then
    exit;

  if WinHandle = 0 then
    exit;

  RemoveCursor;

  if FVerticalPositionInitialised then
  begin
    // Preserve current position
    if FLayout.FNumLines > 0 then
      FTopCharIndex := GetTopCharIndex
    else
      FTopCharIndex := 0;
  end;

  Layout;

  // This is the point at which vertical position
  // is initialised during first window show
  FVScrollBar.Position := GetTopCharIndexPosition( FTopCharIndex );

  FYScroll := FVScrollBar.Position;
  FLastYScroll := FYScroll;
  FVerticalPositionInitialised := true;

  UpdateScrollbarCoords;
  SetupCursor;
end;

procedure TRichTextView.UpdateScrollBarCoords;
var
  r: TfpgRect;
begin
  r := GetDrawRect; // this includes borders and scrollbars

  FHScrollBar.Top     := r.Bottom+1;
  FHScrollBar.Left    := r.Left;
  FHScrollBar.Width   := r.Width;

  FVScrollBar.Top     := r.Top;
  FVScrollBar.Left    := r.Right+1;
  FVScrollBar.Height  := r.Height;

  FVScrollBar.UpdateWindowPosition;
  FHScrollBar.UpdateWindowPosition;
end;


// Main procedure: reads through the whole text currently stored
// and breaks up into lines - each represented as a TLayoutLine in
// the array FLines[ 0.. FNumLines ]
Procedure TRichTextView.Layout;
Var
  DrawWidth: longint;
begin
ProfileEvent('DEBUG:  TRichTextView.Layout >>>>');
  FLayoutRequired := true;

  if InDesigner then
    exit;
  if WinHandle = 0 then
    exit;
ProfileEvent('DEBUG:  TRichTextView.Layout    1 of 6');
  FSelectionEnd := -1;
  FSelectionStart := -1;
  RemoveCursor;

ProfileEvent('DEBUG:  TRichTextView.Layout    2');
  DrawWidth := GetTextAreaRect.Width;

  try
    if Assigned(FLayout) then
    begin
ProfileEvent('DEBUG:  TRichTextView.Layout    3');
      FLayout.Free;
      FLayout := nil;
    end;
  except
    // this is only every a issue under 64bit. FLayout can suddenly not be referenced anymore
    on E: Exception do
      ProfileEvent('ERROR:  Failed to free FLayout.  Error Msg: ' + E.Message);
//      raise Exception.Create('Failed to free FLayout. Error msg: ' + E.Message);
  end;

ProfileEvent('DEBUG:  TRichTextView.Layout    4');
  FLayout := TRichTextLayout.Create( FText,
                                     FImages,
                                     FRichTextSettings,
                                     FFontManager,
                                     DrawWidth-(FScrollbarWidth{*6}) );

ProfileEvent('DEBUG:  TRichTextView.Layout    5');

  SetupScrollBars;
ProfileEvent('DEBUG:  TRichTextView.Layout    6');
  RefreshCursorPosition;

  FLayoutRequired := false;
ProfileEvent('DEBUG:  TRichTextView.Layout <<<<');
End;

procedure TRichTextView.GetFirstVisibleLine( Var LineIndex: longint;
                                             Var Offset: longint );
begin
  FLayout.GetLineFromPosition( FYScroll,
                               LineIndex,
                               Offset );
end;

procedure TRichTextView.GetBottomLine( Var LineIndex: longint;
                                       Var PixelsDisplayed: longint );
begin
  FLayout.GetLineFromPosition( FYScroll + GetTextAreaHeight,
                               LineIndex,
                               PixelsDisplayed );
end;

function TRichTextView.FindPoint( XToFind: longint;
                                  YToFind: longint;
                                  Var LineIndex: longint;
                                  Var Offset: longint;
                                  Var Link: string ): TTextPosition;
var
  TextHeight: longint;
begin
  LineIndex := 0;
  Offset := 0;
  Link := '';

  TextHeight := GetTextAreaHeight;

  // Should we take into account Border Styles?
  XToFind := XToFind - FRichTextSettings.Margins.Left;
  YToFind := YToFind - FRichTextSettings.Margins.Top;

  if YToFind < 3 then
  begin
    // above the top
    Result := tpAboveTextArea;
    exit;
  end;

  if YToFind >= TextHeight then
  begin
    // below the bottom
    Result := tpBelowTextArea;
    LineIndex := FLayout.FNumLines;
    Offset := FLayout.FLines[FLayout.FNumLines-1].Length - 1;
    exit;
  end;

  Result := FLayout.FindPoint( XToFind + FXScroll,     // horizontal scrolls into positive
                               YToFind + (-FYScroll),  // vertical scrolls into negative
                               LineIndex,
                               Offset,
                               Link );
end;

procedure TRichTextView.DrawBorder;
var
  r: TfpgRect;
begin
//  Canvas.GetWinRect(Rect);
  r.SetRect(0, 0, Width, Height);
  case BorderStyle of
    ebsNone:
        begin
          // do nothing
        end;
    ebsDefault:
        begin
          Canvas.DrawControlFrame(r);
        end;
    ebsSingle:
        begin
          Canvas.SetColor(clShadow2);
          Canvas.DrawRectangle(r);
        end;
  end;
end;

Procedure TRichTextView.Draw( StartLine, EndLine: longint );
Var
  DrawRect: TfpgRect;
  X: longint;
  Y: longint;
  SelectionStartP: PChar;
  SelectionEndP: PChar;
  Temp: longint;
begin
ProfileEvent('DEBUG:  TRichTextView.Draw >>>');
  DrawRect := GetTextAreaRect;
  if StartLine > EndLine then
  begin
    // swap
    Temp := EndLine;
    EndLine := StartLine;
    StartLine := Temp;
  end;
  // calculate selection pointers
  if SelectionSet then
  begin
    SelectionStartP := FText + FSelectionStart;
    SelectionEndP := FText + FSelectionEnd;
  end
  else
  begin
    SelectionStartP := nil;
    SelectionEndP := nil;
  end;
  // calculate destination point
  Y := DrawRect.Top + FYScroll;
  X := DrawRect.Left - FXScroll;
  DrawRichTextLayout( FFontManager,
                      FLayout,
                      SelectionStartP,
                      SelectionEndP,
                      StartLine,
                      EndLine,
                      Point(X, Y)
                      );
ProfileEvent('DEBUG:  TRichTextView.Draw <<<');
End;

// This gets the area of the control that we can draw on
// (not taken up by vertical scroll bar)
Function TRichTextView.GetDrawRect: TfpgRect;
begin
  Result := GetClientRect;
  if InDesigner then
    exit;

  if FNeedHScroll then
    dec( Result.Height, FScrollbarWidth );

  if FNeedVScroll then
    dec( Result.Width, FScrollbarWidth );
end;

// Gets the area that we are drawing text on, which is the
// draw rect minus borders
Function TRichTextView.GetTextAreaRect: TfpgRect;
begin
  Result := GetDrawRect;
//  InflateRect(Result, -2, -2);
end;

function TRichTextView.GetTextAreaHeight: longint;
begin
  Result := GetTextAreaRect.Height;
end;

function TRichTextView.GetTextAreaWidth: longint;
begin
  Result := GetTextAreaRect.Width;
end;

Procedure TRichTextView.SetupScrollbars;
var
  AvailableWidth: longint;
  MaxDisplayWidth: longint;
  AvailableHeight: longint;
begin
  // Reset to defaults
  FNeedVScroll := false;
  FNeedHScroll := false;

  // Calculate used and available width
  AvailableWidth := GetTextAreaWidth;
  MaxDisplayWidth := FLayout.Width;

  { This might seem redundant, but the HScrollBar and VScrollBar are a bit
    of "chicken vs egg" situation. This checked and the same check done a bit
    later just makes the scrollbar calculation a bit more accurate }
  AvailableHeight := GetTextAreaHeight; // this includes borders and scrollbars and small margin
  if FLayout.Height > AvailableHeight then
    FNeedVScroll := true;

  // Horizontal scroll setup
  if FNeedVScroll then
  begin
    if MaxDisplayWidth > (AvailableWidth - FScrollbarWidth) then
      FNeedHScroll := true;
  end
  else
  begin
    if MaxDisplayWidth > AvailableWidth then
      FNeedHScroll := true;
  end;

//  FHScrollbar.SliderSize := AvailableWidth div 2;
  FHScrollbar.Min := 0;
  if FNeedHScroll then
    FHScrollbar.Max := (MaxDisplayWidth - AvailableWidth) + FScrollbarWidth + FRichTextSettings.Margins.Right
  else
  begin
    FHScrollBar.Position := 0;
    FHScrollbar.Max := 0;
  end;

  // Calculate available height.
  // Note: this depends on whether a h scroll bar is needed.
  AvailableHeight := GetTextAreaHeight; // this includes borders and scrollbars and small margin
  if FLayout.Height > AvailableHeight then
    FNeedVScroll := true;
  FVScrollBar.Min := 0;
  if FNeedVScroll then
    FVScrollBar.Max := (FLayout.Height - AvailableHeight) + FScrollbarWidth
  else
  begin
    FVScrollBar.Position := 0;
    FVScrollBar.Max := 0;
  end;

  FHScrollBar.ScrollStep  := FScrollDistance; // pixels
  FHScrollBar.PageSize    := AvailableWidth - FHScrollbar.ScrollStep; // slightly less than width
  FHScrollBar.SliderSize  := AvailableWidth / MaxDisplayWidth;
  FVScrollBar.ScrollStep  := FScrollDistance; // pixels
  FVScrollBar.PageSize    := AvailableHeight - FVScrollBar.ScrollStep;
  FVScrollBar.SliderSize  := AvailableHeight / FLayout.Height;

  // Physical horizontal scroll setup
  FHScrollbar.Visible := FNeedHScroll;
  FHScrollbar.Enabled := FNeedHScroll;

  // Physical vertical scroll setup
  FVScrollbar.Visible := FNeedVScroll;
  FVScrollbar.Enabled := FNeedVScroll;

  // Initialise scroll
  FYScroll := FVScrollBar.Position;
  FLastYScroll := FYScroll;
  FXScroll := FHScrollBar.Position;
  FLastXScroll := FXScroll;

  FVScrollbar.OnScroll := @FVScrollbarScroll;
  FHScrollbar.OnScroll := @FHScrollbarScroll;

  UpdateScrollBarCoords;
End;

Procedure TRichTextView.SetupCursor;
var
  Line: TLayoutLine;
  X, Y: longint;
  TextRect: TfpgRect;
  DrawHeight: longint;
  DrawWidth: longint;
  CursorHeight: longint;
  TextHeight: longint;
  LineHeight: longint;
  Descender: longint;
  MaxDescender: longint;
begin
  RemoveCursor;
  if FSelectionStart = -1 then
    exit;

  TextRect := GetTextAreaRect;
  DrawHeight := TextRect.Top - TextRect.Bottom;
  DrawWidth := TextRect.Right - TextRect.Left;

  Line := FLayout.FLines[CursorRow];
  LineHeight := Line.Height;

  Y := DrawHeight - (FLayout.GetLinePosition(CursorRow) - FVScrollbar.Position);
  // Now Y is the top of the line
  if Y < 0 then
    // off bottom
    exit;
  if ( Y - LineHeight ) > DrawHeight then
    // off top
    exit;

  FLayout.GetXFromOffset( FCursorOffset, CursorRow, X );

  X := X - FHScrollBar.Position;

  if X < 0 then
    // offscreen to left
    exit;

  if X > DrawWidth then
    // offscreen to right
    exit;

  TextHeight := FFontManager.CharHeight;
  Descender := FFontManager.CharDescender;
  MaxDescender := FLayout.FLines[CursorRow].MaxDescender;
  CursorHeight := TextHeight;

  dec( Y, LineHeight - 1 );
  // now Y is the BOTTOM of the line

  // move Y up to the bottom of the cursor;
  // since the current text may be smaller than the highest in the line
  inc( Y, MaxDescender - Descender );

  if Y < 0 then
  begin
    // bottom of line will be below bottom of display.
    dec( CursorHeight, 1 - Y );
    Y := 0;
  end;

  if Y + CursorHeight - 1 > DrawHeight then
  begin
    // top of cursor will be above top of display
    CursorHeight := DrawHeight - Y + 1;
  end;

//  fpgCaret.SetCaret(Canvas, TextRect.Left + X, TextRect.Bottom + Y, 2, CursorHeight);
end;

procedure TRichTextView.RemoveCursor;
begin
//  fpgCaret.UnSetCaret(Canvas);
end;

Function TRichTextView.GetLineDownPosition: longint;
var
  LastLine: longint;
  PixelsDisplayed: longint;
begin
  GetBottomLine( LastLine,
                 PixelsDisplayed );

  Result := GetLineDownPositionFrom( LastLine, PixelsDisplayed );
end;

Function TRichTextView.GetLineDownPositionFrom( LastLine: longint;
                                                PixelsDisplayed: longint ): longint;
var
  LineHeight: longint;
begin
  if LastLine = -1 then
    exit;

  LineHeight := FLayout.FLines[LastLine].Height;

  if LastLine = FLayout.FNumLines - 1 then
  begin
    // last line
    if PixelsDisplayed >= LineHeight then
    begin
      // and it's fully displayed, so scroll to show margin
      Result := FLayout.Height - GetTextAreaHeight;
      exit;
    end;
  end;

  // Scroll to make last line fully visible...
  Result := FVScrollBar.Position
            + LineHeight
            - PixelsDisplayed;
  if PixelsDisplayed > LineHeight div 2 then
    // more than half line already displayed so
    if LastLine < FLayout.FNumLines - 1 then
      // AND to make next line fully visible
      inc( Result, FLayout.FLines[LastLine+1].Height );
end;

Function TRichTextView.GetSmallDownScrollPosition: longint;
var
  LastLine: longint;
  PixelsDisplayed: longint;
  LineTextHeight: longint;
  Diff: longint;
begin
  GetBottomLine( LastLine,
                 PixelsDisplayed );

  Result := GetLineDownPositionFrom( LastLine, PixelsDisplayed );

  // Now limit the scrolling to max text height for the bottom line
  Diff := Result - FVScrollBar.Position;

  LineTextHeight := FLayout.FLines[LastLine].MaxTextHeight;
  if Diff > LineTextHeight then
    Diff := LineTextHeight;
  Result := FVScrollBar.Position + Diff;
end;

Function TRichTextView.GetSmallUpScrollPosition: longint;
var
  FirstVisibleLine: longint;
  Offset: longint;
  LineTextHeight: longint;
  Diff: longint;
begin
  GetFirstVisibleLine( FirstVisibleLine,
                       Offset );
  Result := GetLineUpPositionFrom( FirstVisibleLine,
                                   Offset );
  // Now limit the scrolling to max text height for the bottom line
  Diff := FVScrollBar.Position - Result;

  LineTextHeight := FLayout.FLines[FirstVisibleLine].MaxTextHeight;
  if Diff > LineTextHeight then
    Diff := LineTextHeight;
  Result := FVScrollBar.Position - Diff;
end;

Function TRichTextView.GetSmallRightScrollPosition: longint;
begin
  Result := FHScrollBar.Position + FHScrollBar.ScrollStep;
  if Result > FHScrollBar.Max then
    Result := FHScrollBar.Max;
end;

Function TRichTextView.GetSmallLeftScrollPosition: longint;
begin
  Result := FHScrollBar.Position - FHScrollBar.ScrollStep;
  if Result < 0 then
    Result := 0;
end;

function TRichTextView.GetLineUpPosition: longint;
var
  FirstVisibleLine: longint;
  Offset: longint;
begin
  GetFirstVisibleLine( FirstVisibleLine, Offset );
  Result := GetLineUpPositionFrom( FirstVisibleLine, Offset );
end;

function TRichTextView.GetLineUpPositionFrom( FirstVisibleLine: longint; Offset: longint ): longint;
begin
  // we should never have scrolled all lines off the top!!
  assert( FirstVisibleLine <> -1 );

  if FirstVisibleLine = 0 then
  begin
    // first line
    if Offset = 0 then
    begin
      // and it's already fully visible, so scroll to show margin
      Result := 0;
      exit;
    end;
  end;

  // scroll so that top line is fully visible...
  Result := FVScrollBar.Position - Offset;

  if Offset < (FLayout.FLines[FirstVisibleLine].Height div 2) then
    // more than half the line was already displayed so
    if FirstVisibleLine > 0 then
      // AND to make next line up visible
      dec( Result, FLayout.FLines[FirstVisibleLine-1].Height );
end;

Function Sign( arg: longint ): longint;
begin
  if arg>0 then
    Result := 1
  else if arg<0 then
    Result := -1
  else
    Result := 0;
end;

Function FSign( arg: double ): double;
begin
  if arg>0 then
    Result := 1
  else if arg<0 then
    Result := -1
  else
    Result := 0;
end;

Procedure ExactDelay( MS: Cardinal );
begin
  Sleep(MS);
end;

(*
Procedure TRichTextView.Scroll( Sender: TScrollbar;
                                ScrollCode: TScrollCode;
                                Var ScrollPos: Longint );

begin
  case ScrollCode of
//    scVertEndScroll,
//    scVertPosition,
    scPageUp,
    scPageDown,
    scVertTrack:
      DoVerticalScroll( ScrollPos );

    // Line up and down positions are calculated for each case
    scLineDown:
    begin
      ScrollPos := GetSmallDownScrollPosition;
      DoVerticalScroll( ScrollPos );
    end;

    scLineUp:
    begin
      ScrollPos := GetSmallUpScrollPosition;
      DoVerticalScroll( ScrollPos );
    end;

    scHorzPosition,
    scPageRight,
    scPageLeft,
    scHorzTrack,
    scColumnRight,
    scColumnLeft:
    begin
      DoHorizontalScroll( ScrollPos );
    end;
  end;
end;
*)

Procedure TRichTextView.DoVerticalScroll( NewY: longint );
begin
  FYScroll := 0 - NewY;
  if not Visible then
  begin
    FLastYScroll := FYScroll;
    exit;
  end;
  FLastYScroll := FYScroll;
  RePaint;
  SetupCursor;
end;

Procedure TRichTextView.DoHorizontalScroll( NewX: longint );
begin
  FXScroll := NewX;
  if not Visible then
  begin
    FLastXScroll := FXScroll;
    exit;
  end;
  FLastXScroll := FXScroll;
  RePaint;
  SetupCursor;
end;

Procedure TRichTextView.SetVerticalPosition( NewY: longint );
begin
  FVScrollbar.Position := NewY;
  FVScrollbar.RepaintSlider;
  DoVerticalScroll( FVScrollbar.Position );
end;

Procedure TRichTextView.SetHorizontalPosition( NewX: longint );
begin
  FHScrollbar.Position := NewX;
  FHScrollbar.RepaintSlider;
  DoHorizontalScroll( FHScrollbar.Position );
end;

Procedure TRichTextView.AddParagraph( Text: PChar );
begin
  if GetTextEnd > 0 then
  begin
    AddText( #13, True );
    AddText( #10, True );
  end;
  AddText( Text );
end;

Procedure TRichTextView.AddSelectedParagraph( Text: PChar );
begin
  if GetTextEnd > 0 then
  begin
    AddText( #13, True);
    AddText( #10, True);
  end;
  SelectionStart := GetTextEnd;
  AddText( Text );
  SelectionEnd := GetTextEnd;
  MakeCharVisible( SelectionStart );
end;

// ADelay = True means that we hold off on redoing the Layout and Painting.
Procedure TRichTextView.AddText( Text: PChar; ADelay: boolean );
begin
  AddAndResize( FText, Text);
  if not ADelay then
  begin
    Layout;
    RePaint;
  end;
end;

// Insert at current point
Procedure TRichTextView.InsertText( CharIndexToInsertAt: longword;
                                    TextToInsert: PChar );
var
  NewText: PChar;
begin
  NewText := StrAlloc( StrLen( FText ) + StrLen( TextToInsert ) + 1 );
  StrLCopy( NewText, FText, CharIndexToInsertAt );
  StrCat( NewText, TextToInsert );
  StrCat( NewText, FText + CharIndexToInsertAt );

  Clear;
  AddText( NewText );
  StrDispose( NewText );
end;

Procedure TRichTextView.Clear(const ADestroying: boolean = False);
begin
  ClearSelection;
  FText[ 0 ] := #0;
  FTopCharIndex := 0;
  if not ADestroying then
  begin
    Layout;
    if FLayout.FNumLines > 1 then
      raise Exception.Create('FLayout.FNumLines should have been 0 but it was ' + IntToStr(FLayout.FNumLines));
    RePaint;
  end;
end;

//procedure TRichTextView.SetBorder( BorderStyle: TBorderStyle );
//begin
//  FBorderStyle := BorderStyle;
//  Refresh;
//end;

Procedure TRichTextView.SetImages( AImages: TfpgImageList );
begin
  if AImages = FImages then
    exit; // no change

  { TODO -oGraeme : TfpgImageList is not a TComponent descendant. Maybe it should be? }
  //if FImages <> nil then
  //  // Tell the old imagelist not to inform us any more
  //  FImages.Notification( Self, opRemove );

  FImages := AImages;
  //if FImages <> nil then
  //  // request notification when other is freed
  //  FImages.FreeNotification( Self );

  if GetTextEnd = 0 then
    // no text - can't be any image references - no need to layout
    exit;

  Layout;
  RePaint;
end;

Procedure TRichTextView.OnRichTextSettingsChanged( Sender: TObject );
begin
  if not InDesigner then
  begin
    Layout;
    RePaint;
  end;
end;

Procedure TRichTextView.Notification( AComponent: TComponent;
                                      Operation: TOperation );
begin
  inherited Notification( AComponent, Operation );
  { TODO -oGraeme : TfpgImageList is not a TComponent descendant. Maybe it should be? }
  //if AComponent = FImages then
  //  if Operation = opRemove then
  //    FImages := nil;
end;

(*
Procedure TRichTextView.MouseDown( Button: TMouseButton;
                                   ShiftState: TShiftState;
                                   X, Y: Longint );
var
  Line: longint;
  Offset: longint;
  Link: string;
  Position: TTextPosition;
  Shift: boolean;
begin
  Focus;

  inherited MouseDown( Button, ShiftState, X, Y );

  if Button <> mbLeft then
  begin
    if Button = mbRight then
    begin
      if MouseCapture then
      begin
        // this is a shortcut - left mouse drag to select, right mouse to copy
        CopySelectionToClipboard;
      end;
    end;
    exit;
  end;

//  if FText[ 0 ] = #0 then
//    exit;

  Position := FindPoint( X, Y, Line, Offset, Link );
  FClickedLink := Link;

  if Position in [ tpAboveTextArea,
                   tpBelowTextArea ] then
    // not on the control (this probably won't happen)
    exit;

  // if shift is pressed then keep the same selection start.

  Shift := ssShift in ShiftState;
  RemoveCursor;

  if not Shift then
    ClearSelection;

  SetCursorPosition( Offset, Line, Shift );
  MouseCapture := true;

end;
*)

(*
Procedure TRichTextView.MouseUp( Button: TMouseButton;
                                 ShiftState: TShiftState;
                                 X, Y: Longint );
begin
  if Button = mbRight then
    if MouseCapture then
      // don't popup menu for shortcut - left mouse drag to select, right mouse to copy
      exit;

  inherited MouseUp( Button, ShiftState, X, Y );

  if Button <> mbLeft then
    exit;

  if not MouseCapture then
    // not a mouse up from a link click
    exit;

  if FScrollTimer.Running then
    FScrollTimer.Stop;

  MouseCapture := false;

  SetupCursor;

  if FClickedLink <> '' then
    if Assigned( FOnClickLink ) then
      FOnClickLink( Self, FClickedLink );

end;
*)

(*
Procedure TRichTextView.MouseDblClick( Button: TMouseButton;
                                       ShiftState: TShiftState;
                                       X, Y: Longint );
var
  Row: longint;
  Offset: longint;
  Link: string;
  Position: TTextPosition;
  P: PChar;
  pWordStart: PChar;
  WordLength: longint;
begin
  inherited MouseDblClick( Button, ShiftState, X, Y );

  if Button <> mbLeft then
    exit;

//  if FText[ 0 ] = #0 then
//    exit;

  Position := FindPoint( X, Y, Row, Offset, Link );

  if Position in [ tpAboveTextArea,
                   tpBelowTextArea ] then
    // not on the control (this probably won't happen)
    exit;

  Assert( Row >= 0 );
  Assert( Row < FLayout.FNumLines );

  P := FLayout.FLines[ Row ].Text + Offset;

  RemoveCursor;

  if not RichTextWordAt( FText,
                         P,
                         pWordStart,
                         WordLength ) then
  begin
    // not in a word
    SetCursorPosition( Offset, Row, false );
    SetupCursor;
    exit;
  end;

  SetSelectionStartInternal( FLayout.GetCharIndex( pWordStart ) );
  SetSelectionEndInternal( FLayout.GetCharIndex( pWordStart )
                           + WordLength );
  RefreshCursorPosition;
  SetupCursor;
end;
*)

(*
Procedure TRichTextView.MouseMove( ShiftState: TShiftState;
                                   X, Y: Longint );
var
  Line: longint;
  Offset: longint;
  Link: string;
  Position: TTextPosition;
begin
  inherited MouseMove( ShiftState, X, Y );

  Position := FindPoint( X, Y, Line, Offset, Link );

  if not MouseCapture then
  begin
    if Link <> FLastLinkOver then
    begin
      if Link <> '' then
      begin
        if Assigned( FOnOverLink ) then
          FOnOverLink( Self, Link )
      end
      else
      begin
        if Assigned( FOnNotOverLink ) then
          FOnNotOverLink( Self, FLastLinkOver );
      end;

      FLastLinkOver := Link;
    end;

    if Link <> '' then
      Cursor := FLinkCursor
    else
      Cursor := crIBeam;
    exit;
  end;

  // We are holding mouse down and dragging to set a selection:

  if Position in [ tpAboveTextArea,
                   tpBelowTextArea ] then
  begin
    // above top or below bottom of control
    FOldMousePoint := Point( X, Y );

    if Position = tpAboveTextArea then
      FScrollingDirection := sdUp
    else
      FScrollingDirection := sdDown;

    if not FScrollTimer.Running then
    begin
      FScrollTimer.Start;
      OnScrollTimer( self );
    end;
    exit;
  end;

  // Normal selection, cursor within text rect
  if FScrollTimer.Running then
    FScrollTimer.Stop;

  SetCursorPosition( Offset,
                     Line,
                     true );

  if SelectionSet then
  begin
    FClickedLink := ''; // if they move while on a link we don't want to follow it.
    Cursor := crIBeam;
  end;

end;
*)

procedure TRichTextView.OnScrollTimer( Sender: TObject );
var
  Line, Offset: longint;
  MousePoint: TPoint;
  TextRect: TRect;
begin
  exit;
  //MousePoint := Screen.MousePos;
  //MousePoint := ScreenToClient( MousePoint );
  //TextRect := GetTextAreaRect;
  //
  //if FScrollingDirection = sdDown then
  //  // scrolling down
  //  if FVScrollbar.Position = FVScrollbar.Max then
  //    exit
  //  else
  //  begin
  //    if ( TextRect.Bottom - MousePoint.Y ) < 20 then
  //      DownLine
  //    else
  //      DownPage;
  //
  //    GetBottomLine( Line, Offset );
  //    SetSelectionEndInternal( FLayout.GetCharIndex( FLayout.Flines[ Line ].Text )
  //                             + FLayout.FLines[ Line ].Length );
  //  end
  //else
  //  // scrolling up
  //  if FVScrollbar.Position = FVScrollbar.Min then
  //    exit
  //  else
  //  begin
  //    if ( MousePoint.Y - TextRect.Top ) < 20 then
  //      UpLine
  //    else
  //      UpPage;
  //    GetFirstVisibleLine( Line, Offset );
  //    SetSelectionEndInternal( FLayout.GetCharIndex( FLayout.FLines[ Line ].Text ) );
  //  end;

end;

Procedure TRichTextView.UpLine;
begin
  SetVerticalPosition( GetLineUpPosition );
end;

Procedure TRichTextView.DownLine;
begin
  SetVerticalPosition( GetLineDownPosition );
end;

Procedure TRichTextView.UpPage;
begin
  SetVerticalPosition( FVScrollbar.Position + FVScrollbar.PageSize );
end;

Procedure TRichTextView.DownPage;
begin
  SetVerticalPosition( FVScrollbar.Position - FVScrollbar.PageSize );
end;

Procedure TRichTextView.SmallScrollUp;
begin
  SetVerticalPosition( GetSmallUpScrollPosition );
end;

Procedure TRichTextView.SmallScrollDown;
begin
  SetVerticalPosition( GetSmallDownScrollPosition );
end;

Procedure TRichTextView.SmallScrollRight;
begin
  SetHorizontalPosition( GetSmallRightScrollPosition );
end;

Procedure TRichTextView.SmallScrollLeft;
begin
  SetHorizontalPosition( GetSmallLeftScrollPosition );
end;

function TRichTextView.GetCursorIndex: longint;
begin
  if FCursorRow = -1 then
  begin
    Result := -1;
    exit;
  end;
  Result := FLayout.GetCharIndex( FLayout.FLines[FCursorRow].Text ) + FCursorOffset;
end;

procedure TRichTextView.RefreshCursorPosition;
var
  Index: longint;
  Row: longint;
begin
  if SelectionSet then
  begin
    Index := FSelectionEnd
  end
  else
  begin
    Index := FSelectionStart;
  end;

  if Index = -1 then
  begin
    FCursorRow := -1;
    FCursorOffset := 0;
    RemoveCursor;
    exit;
  end;

  Row := FLayout.GetLineFromCharIndex( Index );
  SetCursorPosition( Index - FLayout.GetCharIndex( FLayout.FLines[Row].Text ), Row, true );
end;

procedure TRichTextView.SetCursorIndex( Index: longint; PreserveSelection: boolean );
var
  Row: longint;
begin
  Row := FLayout.GetLineFromCharIndex( Index );
  SetCursorPosition( Index - FLayout.GetCharIndex( FLayout.FLines[Row].Text ), Row, PreserveSelection );
  SetupCursor;
end;

procedure TRichTextView.SetCursorPosition( Offset: longint;
                                           Row: longint;
                                           PreserveSelection: boolean );
var
  Index: longint;
begin
  RemoveCursor;
  FCursorOffset := Offset;
  FCursorRow := Row;
  Index := FLayout.GetCharIndex( FLayout.FLines[Row].Text ) + Offset;

  //writeln('  SetCursorPosition: offset=', FCursorOffset, ' row=', FCursorRow, ' index=', Index);
  exit;    { TODO:  Complete this selection of text code - currently gives AV's }
  if PreserveSelection then
  begin
    SetSelectionEndInternal( Index )
  end
  else
  begin
    SetSelectionEndInternal( -1 );
    SetSelectionStartInternal( Index );
  end;
  MakeRowAndColumnVisible( FCursorRow, Offset );
end;

Procedure TRichTextView.CursorRight( PreserveSelection: boolean );
Var
  P: PChar;
  NextP: PChar;
  Element: TTextElement;
  NewOffset: longint;
  Line: TLayoutLine;
begin
  P := FText + CursorIndex;

  Element := ExtractNextTextElement( P, NextP );
  P := NextP;
  while Element.ElementType = teStyle do
  begin
    Element := ExtractNextTextElement( P, NextP );
    P := NextP;
  end;

//  if Element.ElementType = teTextEnd then
//    exit;

//  SetCursorIndex( GetCharIndex( P ), PreserveSelection );
  Line := FLayout.FLines[CursorRow];
  NewOffset := PCharDiff( P, Line.Text );
  if NewOffset < Line.Length then
  begin
    SetCursorPosition( NewOffset, FCursorRow, PreserveSelection )
  end
  else if ( NewOffset = Line.Length )
          and not Line.Wrapped then
  begin
    SetCursorPosition( NewOffset, FCursorRow, PreserveSelection )
  end
  else
  begin
    if FCursorRow >= FLayout.FNumLines - 1 then
      exit;
    SetCursorPosition( 0, FCursorRow + 1, PreserveSelection );
  end;
  SetupCursor;
end;

Procedure TRichTextView.CursorLeft( PreserveSelection: boolean );
Var
  P: PChar;
  NextP: PChar;
  Element: TTextElement;
  Line: TLayoutLine;
  NewOffset: longint;
begin
  P := FText + CursorIndex;

  Element := ExtractPreviousTextElement( FText, P, NextP );
  P := NextP;
  while Element.ElementType = teStyle do
  begin
    Element := ExtractPreviousTextElement( FText, P, NextP );
    P := NextP;
  end;

//  if Element.ElementType = teTextEnd then
//    exit;
  Line := FLayout.FLines[CursorRow];
  NewOffset := PCharDiff( P, Line.Text );
  if NewOffset >= 0 then
  begin
    SetCursorPosition( NewOffset, FCursorRow, PreserveSelection )
  end
  else
  begin
    if FCursorRow <= 0 then
      exit;
    Line := FLayout.FLines[CursorRow-1];
    if Line.Wrapped then
      SetCursorPosition( Line.Length - 1, FCursorRow - 1, PreserveSelection )
    else
      SetCursorPosition( Line.Length, FCursorRow - 1, PreserveSelection )
  end;
  SetupCursor;

end;

Procedure TRichTextView.CursorWordLeft( PreserveSelection: boolean );
Var
  P: PChar;
begin
  P := FText + CursorIndex;

  P := RichTextWordLeft( FText, P );

  SetCursorIndex( FLayout.GetCharIndex( P ),
                  PreserveSelection );
end;

Procedure TRichTextView.CursorWordRight( PreserveSelection: boolean );
Var
  P: PChar;
begin
  P := FText + CursorIndex;

  P := RichTextWordRight( P );

  SetCursorIndex( FLayout.GetCharIndex( P ),
                  PreserveSelection );
end;

Procedure TRichTextView.CursorToLineStart( PreserveSelection: boolean );
Var
  Line: TLayoutLine;
begin
  Line := FLayout.FLines[FCursorRow];
  SetCursorPosition( 0, FCursorRow, PreserveSelection );
  SetupCursor;
end;

Procedure TRichTextView.CursorToLineEnd( PreserveSelection: boolean );
Var
  Line: TLayoutLine;
begin
  Line := FLayout.FLines[FCursorRow];
  SetCursorPosition( Line.Length, FCursorRow, PreserveSelection );
  SetupCursor;
end;

Procedure TRichTextView.CursorDown( PreserveSelection: boolean );
var
  X: longint;
  Link: string;
  Offset: longint;
begin
  if CursorRow >= FLayout.FNumLines - 1 then
    exit;

  FLayout.GetXFromOffset( FCursorOffset, FCursorRow, X );
  FLayout.GetOffsetFromX( X,
                          FCursorRow + 1,
                          Offset,
                          Link );

  SetCursorPosition( Offset, FCursorRow + 1, PreserveSelection );
  SetupCursor;
end;

Procedure TRichTextView.CursorUp( PreserveSelection: boolean );
var
  X: longint;
  Link: string;
  Offset: longint;
begin
  if CursorRow <= 0 then
    exit;

  FLayout.GetXFromOffset( FCursorOffset,
                          FCursorRow,
                          X );
  FLayout.GetOffsetFromX( X,
                          FCursorRow - 1,
                          Offset,
                          Link );

  SetCursorPosition( Offset, FCursorRow - 1, PreserveSelection );
  SetupCursor;

end;

Procedure TRichTextView.CursorPageDown( PreserveSelection: boolean );
var
  X: longint;
  Link: string;
  Offset: longint;
  Distance: longint;
  NewRow: longint;
begin
  NewRow := CursorRow;
  Distance := 0;
  while ( Distance < GetTextAreaHeight ) do
  begin
    if NewRow >= FLayout.FNumLines - 1 then
      break;

    Distance := Distance + FLayout.FLines[NewRow].Height;
    inc( NewRow );
  end;

  FLayout.GetXFromOffset( FCursorOffset, FCursorRow, X );
  FLayout.GetOffsetFromX( X, NewRow, Offset, Link );
  SetCursorPosition( Offset, NewRow, PreserveSelection );
  SetupCursor;
end;

Procedure TRichTextView.CursorPageUp( PreserveSelection: boolean );
var
  X: longint;
  Link: string;
  Offset: longint;
  Distance: longint;
  NewRow: longint;
begin
  NewRow := CursorRow;
  Distance := 0;
  while ( Distance < GetTextAreaHeight ) do
  begin
    if NewRow <= 0 then
      break;
    dec( NewRow );
    Distance := Distance + FLayout.FLines[NewRow].Height;
  end;

  FLayout.GetXFromOffset( FCursorOffset, FCursorRow, X );
  FLayout.GetOffsetFromX( X, NewRow, Offset, Link );
  SetCursorPosition( Offset, NewRow, PreserveSelection );
  SetupCursor;
end;

Function TRichTextView.GetSelectionAsString: string; // returns up to 255 chars obviously
var
  Buffer: array[ 0..255 ] of char;
  Length: longint;
begin
  Length := CopySelectionToBuffer( Addr( Buffer ), 255 );

  Result := StrNPas( Buffer, Length );
end;

Procedure TRichTextView.CopySelectionToClipboard;
var
  SelLength: Longint;
  Buffer: PChar;
begin
  SelLength := SelectionLength;
  if SelectionLength = 0 then
    exit;

  Buffer := StrAlloc( SelLength + 1 );

  CopySelectionToBuffer( Buffer, SelLength + 1 );

  fpgClipboard.Text := Buffer;

  StrDispose( Buffer );
end;

function TRichTextView.CopySelectionToBuffer( Buffer: PChar;
                                              BufferLength: longint ): longint;
var
  P, EndP: PChar;
begin
  Result := 0;
  if    ( FSelectionStart = -1 )
     or ( FSelectionEnd = -1 ) then
    exit;

  if FSelectionStart < FSelectionEnd then
  begin
    P := FText + FSelectionStart;
    EndP := FText + FSelectionEnd;
  end
  else
  begin
    P := FText + FSelectionEnd;
    EndP := FText + FSelectionStart;
  end;

  //Result := CopyPlainTextToBuffer( P,
  //                                 EndP,
  //                                 Buffer,
  //                                 BufferLength );
end;

function TRichTextView.CopyTextToBuffer( Buffer: PChar;
                                         BufferLength: longint ): longint;
begin
  //Result := CopyPlainTextToBuffer( FText,
  //                                 FText + strlen( FText ),
  //                                 Buffer,
  //                                 BufferLength );
end;

Function TRichTextView.SelectionLength: longint;
begin
  Result := 0;
  if    ( FSelectionStart = -1 )
     or ( FSelectionEnd = -1 ) then
    exit;

  Result := FSelectionEnd - FSelectionStart;
  if Result < 0 then
    Result := FSelectionStart - FSelectionEnd;
end;

Function TRichTextView.SelectionSet: boolean;
begin
  Result :=     ( FSelectionStart <> -1 )
            and ( FSelectionEnd <> - 1 )
            and ( FSelectionStart <> FSelectionEnd );
end;

Procedure TRichTextView.SelectAll;
begin
  ClearSelection;
  FSelectionStart := FLayout.GetCharIndex( FText );
  FSelectionEnd := FLayout.GetTextEnd;
  Repaint;
end;

(*
procedure TRichTextView.ScanEvent( Var KeyCode: TKeyCode;
                                   RepeatCount: Byte );
var
  CursorVisible: boolean;
  Shift: boolean;
  Key: TKeyCode;
begin
  CursorVisible := FSelectionStart <> -1;

  Case KeyCode of
    kbTab:
    begin
      if HighlightNextLink then
      begin
        KeyCode := kbNull;
        exit;
      end;
    end;

    kbShiftTab:
    begin
      if HighlightPreviousLink then
      begin
        KeyCode := kbNull;
        exit;
      end;
    end;

    kbEnter:
    begin

    end;
  end;

  Shift := KeyCode and kb_Shift > 0 ;
  Key := KeyCode and ( not kb_Shift );

  // Keys which work the same regardless of whether
  // cursor is present or not
  case Key of
    kbCtrlC, kbCtrlIns:
      CopySelectionToClipboard;
    kbCtrlA:
      SelectAll;

    kbAltCUp:
      SmallScrollUp;
    kbAltCDown:
      SmallScrollDown;
    kbAltCLeft:
      SmallScrollLeft;
    kbAltCRight:
      SmallScrollRight;
  end;

  // Keys which change behaviour if cursor is present
  if CursorVisible then
  begin
    case Key of
      kbCUp:
        CursorUp( Shift );
      kbCDown:
        CursorDown( Shift );

      // these next two are not exactly orthogonal or required,
      // but better match other text editors.
      kbCtrlCUp:
        if Shift then
          CursorUp( Shift )
        else
          SmallScrollUp;
      kbCtrlCDown:
        if Shift then
          CursorDown( Shift )
        else
          SmallScrollDown;

      kbCRight:
        CursorRight( Shift );
      kbCLeft:
        CursorLeft( Shift );

      kbCtrlCLeft:
        CursorWordLeft( Shift );
      kbCtrlCRight:
        CursorWordRight( Shift );

      kbCtrlHome, kbCtrlPageUp:
        SetCursorIndex( 0, Shift );
      kbCtrlEnd, kbCtrlPageDown:
        SetCursorIndex( GetTextEnd, Shift );

      kbPageUp:
        CursorPageUp( Shift );
      kbPageDown:
        CursorPageDown( Shift );

      kbHome:
        CursorToLineStart( Shift );
      kbEnd:
        CursorToLineEnd( Shift );
    end
  end
  else // no cursor visible
  begin
    case Key of
      kbCUp, kbCtrlCUp:
        SmallScrollUp;
      kbCDown, kbCtrlCDown:
        SmallScrollDown;

      kbCLeft, kbCtrlCLeft:
        SmallScrollLeft;
      kbCRight, kbCtrlCRight:
        SmallScrollRight;

      kbPageUp:
        UpPage;
      kbPageDown:
        DownPage;

      kbHome, kbCtrlHome, kbCtrlPageUp:
        GotoTop;
      kbEnd, kbCtrlEnd, kbCtrlPageDown:
        GotoBottom;
    end;
  end;

  inherited ScanEvent( KeyCode, RepeatCount );

end;
*)

function TRichTextView.HighlightNextLink: boolean;
Var
  P: PChar;
  NextP: PChar;
  T: TTextElement;
  StartP: PChar;
begin
  if CursorIndex = -1 then
    P := FText // no cursor yet
  else
    P := FText + CursorIndex;

  result := false;

  // if we're sitting on a begin-link, skip it...
  T := ExtractNextTextElement( P, NextP );
  if T.ElementType = teStyle then
    if T.Tag.TagType = ttBeginLink then
      P := NextP;

  while true do
  begin
    T := ExtractNextTextElement( P, NextP );
    if T.ElementType = teTextEnd then
      // no link found
      exit;

    if T.ElementType = teStyle then
      if T.Tag.TagType = ttBeginLink then
        break;

    p := NextP;

  end;

  StartP := P;
  p := NextP; // skip begin link

  while true do
  begin
    T := ExtractNextTextElement( P, NextP );
    if T.ElementType = teTextEnd then
      break; // no explicit link end...

    if T.ElementType = teStyle then
      if T.Tag.TagType = ttEndLink then
        break;

    p := NextP;
  end;

  SetSelectionStart( FLayout.GetCharIndex( StartP ) );
  SetSelectionEnd( FLayout.GetCharIndex( NextP ) );

  result := true;
end;

function TRichTextView.HighlightPreviousLink: boolean;
Var
  P: PChar;
  PreviousP: PChar;
  T: TTextElement;
  EndP: PChar;
begin
  result := false;
  if CursorIndex = -1 then
    exit; // no cursor yet

  P := FText + CursorIndex;

  // if we're sitting on an end-of-link, skip it...
  T := ExtractPreviousTextElement( FText, P, PreviousP );
  if T.ElementType = teStyle then
    if T.Tag.TagType = ttEndLink then
      P := PreviousP;

  while true do
  begin
    T := ExtractPreviousTextElement( FText, P, PreviousP );
    if T.ElementType = teTextEnd then
      // no link found
      exit;

    if T.ElementType = teStyle then
      if T.Tag.TagType = ttEndLink then
        break;

    p := PreviousP;

  end;

  EndP := P;
  p := PreviousP; // skip end link

  while true do
  begin
    T := ExtractPreviousTextElement( FText, P, PreviousP );
    if T.ElementType = teTextEnd then
      break; // no explicit link end...

    if T.ElementType = teStyle then
      if T.Tag.TagType = ttBeginLink then
        break;

    p := PreviousP;
  end;

  SetSelectionStart( FLayout.GetCharIndex( EndP ) );
  SetSelectionEnd( FLayout.GetCharIndex( PreviousP ) );

  result := true;
end;

procedure TRichTextView.GoToTop;
begin
  SetVerticalPosition( 0 );
end;

procedure TRichTextView.GotoBottom;
begin
  SetVerticalPosition( FVScrollBar.Max );
end;

Function TRichTextView.GetTopCharIndex: longint;
var
  LineIndex: longint;
  Y: longint;
begin
  if not FVerticalPositionInitialised then
  begin
    Result := FTopCharIndex;
    exit;
  end;
  GetFirstVisibleLine( LineIndex,
                       Y );
  if LineIndex >= 0 then
    Result := FLayout.GetCharIndex( FLayout.FLines[LineIndex].Text )
  else
    Result := 0;
end;

Function TRichTextView.GetTopCharIndexPosition( NewValue: longint ): longint;
var
  Line: longint;
  lHeight: longint;
begin
  if NewValue > GetTextEnd then
  begin
    Result := FVScrollBar.Max;
    exit;
  end;
  Line := FLayout.GetLineFromCharIndex( NewValue );
  if Line = 0 then
  begin
    Result := 0; // include top margin
    exit;
  end;

  if Line < 0 then
  begin
    Result := FVScrollBar.Position;
    exit;
  end;
  lHeight := FLayout.GetLinePosition( Line );
  Result := lHeight;
end;

Procedure TRichTextView.SetTopCharIndex( NewValue: longint );
var
  NewPosition: longint;
begin
  if not FVerticalPositionInitialised then
  begin
    if ( NewValue >= 0 )
       and ( NewValue < GetTextEnd ) then
    begin
      FTopCharIndex := NewValue;
    end;
    exit;
  end;
  NewPosition := GetTopCharIndexPosition( NewValue );
  SetVerticalPosition( NewPosition );
end;

procedure TRichTextView.MakeCharVisible( CharIndex: longint );
var
  Line: longint;
begin
  Line := FLayout.GetLineFromCharIndex( CharIndex );

  MakeRowAndColumnVisible( Line,
                           FLayout.GetOffsetFromCharIndex( CharIndex, Line ) );
end;

procedure TRichTextView.MakeRowVisible( Row: longint );
var
  TopLine: longint;
  BottomLine: longint;
  Offset: longint;
  NewPosition: longint;
begin
  GetFirstVisibleLine( TopLine, Offset );
  GetBottomLine( BottomLine, Offset );

  if     ( Row > TopLine )
     and ( Row < BottomLine ) then
    // already visible
    exit;

  if     ( Row = BottomLine )
     and ( Offset >= FLayout.FLines[BottomLine].Height - 1 ) then
    // bottom row already entirely visible
    exit;

  if Row <= TopLine then
  begin
    // need to scroll up, desird row above top line
    if Row = 0 then
      NewPosition := 0 // include margins
    else
      NewPosition := FLayout.GetLinePosition( Row );

    if NewPosition > FVScrollbar.Position then
      // no need to scroll
      exit;
    SetVerticalPosition( NewPosition );
  end
  else
  begin
    //writeln('need to scroll down, desired row below bottom line');
    //writeln('BottomLine = ', BottomLine, '  Row = ', Row);
    //writeln('new pos = ', FLayout.GetLinePosition( Row )
    //                       + FLayout.FLines^[ Row ].Height
    //                       - GetTextAreaHeight);
    // need to scroll down, desired row below bottom line
    if     ( BottomLine <> -1 )
       and ( Row >= BottomLine ) then
      SetVerticalPosition( FLayout.GetLinePosition( Row )
                           + FLayout.FLines[Row].Height
                           - GetTextAreaHeight );
  end;
end;

procedure TRichTextView.MakeRowAndColumnVisible(Row: longint; Column: longint);
var
  X: Longint;
begin
  MakeRowVisible( Row );
  FLayout.GetXFromOffset( Column, Row, X );

  if X > (FXScroll + GetTextAreaWidth) then
    // off the right
    SetHorizontalPosition( X - GetTextAreaWidth + 5 )
  else if X < FXScroll then
    // off to left
    SetHorizontalPosition( X );
end;

function TRichTextView.LinkFromIndex( const CharIndexToFind: longint): string;
begin
  Result := FLayout.LinkFromIndex( CharIndexToFind );
end;

function TRichTextView.FindString( Origin: TFindOrigin;
                                   const AText: string;
                                   var MatchIndex: longint;
                                   var MatchLength: longint ): boolean;
var
  P: PChar;
  pMatch: pchar;
begin
  if     ( Origin = foFromCurrent )
     and ( FSelectionStart <> -1 ) then
  begin
    // start at current cursor position
    P := FText + GetCursorIndex;
  end
  else
  begin
    P := FText;
  end;

  Result := RichTextFindString( P, AText, pMatch, MatchLength );

  if Result then
    // found
    MatchIndex := FLayout.GetCharIndex( pMatch )
  else
    MatchIndex := -1;

end;

function TRichTextView.Find( Origin: TFindOrigin;
                             const AText: string ): boolean;
var
  MatchIndex: longint;
  MatchLength: longint;
begin
  Result := FindString( Origin,
                        AText,
                        MatchIndex,
                        MatchLength );
  if Result then
  begin
    MakeCharVisible( MatchIndex );
    FSelectionStart := MatchIndex;
    SelectionEnd := MatchIndex + MatchLength;
  end;
end;

function TRichTextView.GetClientRect: TfpgRect;
var
  r: TRect;
begin
  Result.SetRect(0, 0, Width, Height);
  case BorderStyle of
    ebsNone:
        begin
          // do nothing
        end;
    ebsDefault:
        begin
          r := fpgStyle.GetControlFrameBorders;
          InflateRect(Result, -r.Left, -r.Top);  { assuming borders are even on opposite sides }
        end;
    ebsSingle:
        begin
          InflateRect(Result, -1, -1);
        end;
  end;
end;


end.

