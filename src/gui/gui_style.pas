{
  This is where all style related types should be define. The base Style
  class should also be defined here.
}
unit gui_style;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget;
  
  
type
  TfpgPrimitiveElement = (
      peFocusRectangle,                // The focus rectangle
      pePanel,                         // Generic bevel of a panel
      pePanelButtonBevel,              // The bevel of a button
      pePanelEditBox,                  // Frame around a text edit box
      peFrameMenu,                     // Frame for popup windows and menus
      pePanelMenuBar,                  // The menu bar panel
      pePanelScrollAreaCorner,         // Panel at the bottom right corner of the scroll area
      peFrameDefaultButton,            // Frame around a default button like in a dialog
      peFramePageControl,              // Frame for a Page Control
      peIndicatorArrowUp,              // Generic up arrow
      peIndicatorArrowDown,            // Generic down arrow
      peIndicatorArrowRight,           // Generic right arrow
      peIndicatorArrowLeft,            // Generic left arrow
      peIndicatorCheckBox,             // On/off indicator used in a CheckBox
      peIndicatorRadioButton,          // Exclusive on indicator used in a Radio Button
      peIndicatorHeaderArrow,          // Indicator used in List or Tabel header to show sorting
      peIndicatorMenuCheckMark,        // Check mark used in menus
      peIndicatorProgressBar           // Body section of a Progress Bar
      );
      
      
  TfpgControlElement = (
      cePushButton,                    // The Bevel, Label and FocusRect
      cePushButtonBevel,
      cePushButtonLabel,
      ceRadioButton,                   // Indicator, FocusRect and Label
      ceRadioButtonLabel,
      ceCheckBox,                      // Indicator, FocusRect and Label
      ceCheckBoxLabel,
      ceMenuItem,
      ceMenuBarItem,
      ceMenuBarEmptyArea,
      ceMenuTearOff,
      ceMenuHMargin,
      ceMenuVMargin,
      ceProgressBar,
      cePageControlTab,                // Both the Shape and Label
      cePageControlShape,
      cePageControlLabel
      );
      
      
  TfpgStyleOptionEnum = (
      soDefault,
      soFocusRect,
      soButton,
      soComboBox,
      soCheckBox,
      soMenuItem,
      soTrackBar,
      soPanel,
      soComplex
      );
      
      
  TfpgStateItem = (
      stNone,
      stActive,
      stReadOnly,
      stSelected,
      stRaised,
      stLowered,
      stHasFocus,
      stEnabled
      );
  TfpgState = set of TfpgStateItem;
  
      
  // Just a data class
  TfpgStyleOption = class(TObject)
  private
    FRect: TfpgRect;
    FState: TfpgState;
    FStyleOption: TfpgStyleOptionEnum;
  public
    property  StyleOption: TfpgStyleOptionEnum read FStyleOption write FStyleOption;
    property  Rect: TfpgRect read FRect write FRect;
    property  State: TfpgState read FState write FState;
  end;
  
  
  TfpgButtonFeatures = set of (bfNone, bfFlat, bfDefault);

  // Button specific options
  TfpgButtonStyleOption = class(TfpgStyleOption)
  private
    FButtonFeatures: TfpgButtonFeatures;
  public
    property  ButtonFeatures: TfpgButtonFeatures read FButtonFeatures write FButtonFeatures;
  end;
  
  
  TfpgBaseStyle = class(TObject)
  public
    procedure   DrawControl(element: TfpgControlElement; const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget = nil); virtual; abstract;
    procedure   DrawPrimitive(element: TfpgPrimitiveElement; const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget = nil); virtual; abstract;
  end;
  
  
  
  //-----------------------------------------
  // The classes below will be better placed in their own units!
  

  // This class encapsulates the common look and feel of the GUI
  TfpgCommonStyle = class(TfpgBaseStyle)
  public
    procedure DrawControl(element: TfpgControlElement; const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget=nil); override;
    procedure DrawPrimitive(element: TfpgPrimitiveElement; const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget=nil); override;
  end;
  
  
  // The Windows 2000 look
  TfpgWin2000Style = class(TfpgCommonStyle)
  end;
  
  
  TfpgWinXPStyle = class(TfpgCommonStyle)
  end;
  
  
  // This class provides a widgte style similar to the classic BlueCurve theme
  // originally created by Red Hat.
  TfpgBluecurveStyle = class(TfpgCommonStyle)
  end;
  
  
  // This class provides a widget style similar to GNOME
  TfpgClearLookStyle = class(TfpgCommonStyle)
  end;


  // For the die-hard unix fans!
  TfpgMotifStyle = class(TfpgCommonStyle)
  end;




implementation



{ TfpgCommonStyle }

procedure TfpgCommonStyle.DrawControl(element: TfpgControlElement;
  const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget);
begin
  //  Do common things here
end;

procedure TfpgCommonStyle.DrawPrimitive(element: TfpgPrimitiveElement;
  const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget);
begin
  // Do common things here. It's going to be a huge case statement. This design
  // allows us to add new controls or elements without having to instantly
  // implement them in all descendant classes!
  case element of
    peFocusRectangle:
        begin
          canvas.DrawFocusRect(option.Rect);
        end;
    peIndicatorRadioButton:
        begin      // just an example!!!!!!!!
          canvas.SetColor(clShadow1);
          canvas.DrawArc(option.Rect.Left, option.Rect.Top, option.Rect.Width, option.Rect.Height, 0, 180);
          canvas.SetColor(clHilite1);
          canvas.DrawArc(option.Rect.Left, option.Rect.Top, option.Rect.Width, option.Rect.Height, 180, 0);
        end;
  end;
end;

end.

