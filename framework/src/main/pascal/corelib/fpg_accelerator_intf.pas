{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2026 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Accelerator (mnemonic) support. A widget that wants to respond to an
      Alt+<letter> key combination - as marked by an '&' in its text, eg:
      '&Scan' - implements IfpgAcceleratorTarget.

      Dispatch is driven by TfpgWidget.DoKeyShortcut, which already walks the
      whole widget tree. It only offers the key to targets when Alt is the
      exclusive modifier, so Ctrl+Alt+S never fires an Alt+S accelerator.
}
unit fpg_accelerator_intf;

{$I fpg_defines.inc}

interface

uses
  fpg_base;

type
  { Implemented by widgets that can be activated by an Alt+<letter>
    accelerator derived from their text. }
  IfpgAcceleratorTarget = interface(IInterface)
  ['{364A0B5F-CB57-4F0C-B02F-0D824FEC2A75}']
    { The accelerator character, or '' when the text has no '&' marker.
      Returned verbatim - matching is done case-insensitively by the caller. }
    function    GetAcceleratorChar: TfpgString;
    { Can this target currently accept an accelerator? Implementations should
      check at least Visible and Enabled. }
    function    CanAcceptAccelerator: boolean;
    { Fire the accelerator - focus the widget and/or perform its action. }
    procedure   ExecuteAccelerator;
  end;


implementation


end.
