{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This is where all style related types should be defined. The base
      Style class should also be defined here.


   *********************************************************
                           WARNING
    
                 THIS IS AN EXPERIMENTAL UNIT
   *********************************************************
}

unit fpg_styler;

{$mode objfpc}{$H+}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;
  
  
type
  TfpgPrimitiveElement = (
      peFocusRectangle,                // The focus rectangle
      pePanel,                         // Generic bevel of a panel
      pePanelButton,                   // Panel are of standard button
      pePanelButtonBevel,              // The bevel of a button
      pePanelEditBox,                  // Frame around a text edit box
      pePanelToolbarButton,            // Panel area of a toolbar button
      pePanelMenuBar,                  // The menu bar panel
      pePanelScrollAreaCorner,         // Panel at the bottom right corner of the scroll area
      peFrameMenu,                     // Frame for popup windows and menus
      peFrameDefaultButton,            // Frame around a default button like in a dialog
      peFrameToolbarButton,            // Frame around a toolbar button
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
  
  
  TfpgStandardPixmap = (
      spMessageBoxInformation,
      spMessageBoxCritical,
      spMessageBoxError,
      spMessageBoxWarning,
      spMessageBoxQuestion,
      spDirOpenIcon,
      spDirCloseIcon,
      spDirIcon,
      spDirLinkIcon,
      spFileIcon,
      spFileLinkIcon,
      spFileDialogToParent,           // Icon of back to parent dir
      spFileDialogNewFolder,
      spDialogOkButton,
      spDialogCancelButton,
      spDialogHelpButton,
      spDialogSaveButton,
      spDialogOpenButton,
      spDialogCloseButton,
      spDialogApplyButton,
      spDialogResetButton,
      spDialogDiscardButton,
      spDialogYesButton,
      spDialogNoButton
      );
  
      
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
  
  
  TfpgButtonFeatures = set of (bfNone, bfFlat, bfDefault, bfEmbedded);

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
  TfpgBlueCurveStyle = class(TfpgCommonStyle)
  end;
  
  
  // This class provides a widget style similar to GNOME
  TfpgClearLookStyle = class(TfpgCommonStyle)
  end;


  // For the die-hard unix fans!
  TfpgMotifStyle = class(TfpgCommonStyle)
  end;


implementation

uses
  fpg_button
  ;

{ TfpgCommonStyle }

procedure TfpgCommonStyle.DrawControl(element: TfpgControlElement;
    const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget);
var
  r: TfpgRect;
  dx, dy: integer;
  offset: integer;
begin
  //  Do common things here
  case element of
    cePushButtonBevel:
        begin
          {$IFDEF DEBUG}
          writeln('TfpgCommonStyle.DrawControl: cePushButtonBevel');
          {$ENDIF}
          r.SetRect(option.Rect.Left, option.Rect.Top, option.Rect.Width, option.Rect.Height);
          
          if bfDefault in TfpgButtonStyleOption(option).ButtonFeatures then
          begin
            Canvas.SetColor(clBlack);
            Canvas.SetLineStyle(1, lsSolid);
            Canvas.DrawRectangle(r);
            InflateRect(r, -1, -1);
          end;

//          Canvas.SetColor(clButtonFace);
//          Canvas.SetLineStyle(1, lsSolid);
 //         Canvas.FillRectangle(r.Left, r.Top, r.Width, r.Height);

          // Left and Top (outer)
          if stLowered in option.State then
          begin
            if bfEmbedded in TfpgButtonStyleOption(option).ButtonFeatures then
              Canvas.SetColor(clHilite1)
            else
              Canvas.SetColor(clShadow2);
          end
          else
            Canvas.SetColor(clHilite1);
          Canvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top);  // left
          Canvas.DrawLine(r.Left, r.Top, r.Right, r.Top);    // top

          // Right and Bottom (outer)
          if stLowered in option.State then
          begin
            if bfEmbedded in TfpgButtonStyleOption(option).ButtonFeatures then
              Canvas.SetColor(clHilite1)
            else
              Canvas.SetColor(clShadow2);
          end
          else
            Canvas.SetColor(clShadow2);
          Canvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
          Canvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);   // bottom

          // Right and Bottom (inner)
          if stLowered in option.State then
          begin
            if bfEmbedded in TfpgButtonStyleOption(option).ButtonFeatures then
              Canvas.SetColor(clButtonFace)
            else
              Canvas.SetColor(clHilite1);
          end
          else
            Canvas.SetColor(clShadow1);
          Canvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right
          Canvas.DrawLine(r.Right-1, r.Bottom-1, r.Left, r.Bottom-1);   // bottom
        end;  { cePushButtonBevel }

    cePushButtonLabel:
        begin
          {$IFDEF DEBUG}
          writeln('TfpgCommonStyle.DrawControl: cePushButtonLabel');
          {$ENDIF}
          r.SetRect(option.Rect.Left, option.Rect.Top, option.Rect.Width, option.Rect.Height);
//          InflateRect(r, -3, -3); { same size as used in the focus rectangle }

          Canvas.SetTextColor(TfpgButton(widget).TextColor);
          Canvas.SetFont(TfpgButton(widget).Font);
          Canvas.SetClipRect(r);

//          if stLowered in TfpgButtonStyleOption(option).State then
//            offset := 1
//          else
//            offset := 0;

          Canvas.DrawText(r, TfpgButton(widget).Text);
//          Canvas.DrawString(tx+offset, ty+offset, Text, Enabled);
        end;  { cePushButtonLabel }
  end;
end;

procedure TfpgCommonStyle.DrawPrimitive(element: TfpgPrimitiveElement;
    const option: TfpgStyleOption; canvas: TfpgCanvas; widget: TfpgWidget);
var
  r: TfpgRect;
  oldColor: TfpgColor;
  oldLineWidth: integer;
  oldLineStyle: TfpgLineStyle;
begin
  // Do common things here. It's going to be a huge case statement. This design
  // allows us to add new controls or elements without having to instantly
  // implement them in all descendant classes!
  case element of
    peFocusRectangle:
        begin
          {$IFDEF DEBUG}
          writeln('TfpgCommonStyle.DrawPrimitive: peFocusRectangle');
          {$ENDIF}
          if stHasFocus in option.State then
          begin
            r.SetRect(option.Rect.Left, option.Rect.Top, option.Rect.Width, option.Rect.Height);
            InflateRect(r, -3, -3);

            oldColor      := Canvas.Color;
            oldLineWidth  := Canvas.LineWidth;
            oldLineStyle  := Canvas.LineStyle;

            Canvas.SetColor(clText1);
            Canvas.SetLineStyle(1, lsDot);
            Canvas.DrawRectangle(r);

            // restore previous settings
            Canvas.SetColor(oldColor);
            Canvas.SetLineStyle(oldLineWidth, oldLineStyle);
          end;
        end;  { peFocusRectangle }
        
    peIndicatorRadioButton:
        begin      // just an example!!!!!!!!
          {$IFDEF DEBUG}
          writeln('TfpgCommonStyle.DrawPrimitive: peIndicatorRadioButton');
          {$ENDIF}
          Canvas.SetColor(clShadow1);
          Canvas.DrawArc(option.Rect.Left, option.Rect.Top, option.Rect.Width, option.Rect.Height, 0, 180);
          Canvas.SetColor(clHilite1);
          Canvas.DrawArc(option.Rect.Left, option.Rect.Top, option.Rect.Width, option.Rect.Height, 180, 0);
        end; { peIndicatorRadioButton }
  end;
end;

end.

