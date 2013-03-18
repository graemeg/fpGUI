{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

unit ideconst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  cProjectExt     = '.project';

  // Project Options
  cProjectOptions  = 'ProjectOptions';
  cOpenedUnits     = 'OpenedUnits';
  cUnits           = 'Units';

  // INI Environment
  cEnvironment    = 'Environment';
  cEditor         = 'Editor';
  cShortcuts      = 'Shortcuts';
  cINIMakeOption  = 'MakeOption';
  cINIMakeOptionGrid = 'MakeOptionEnabled';
  cINIUnitDir     = 'UnitDir';
  cINIUnitDirGrid = 'UnitDirEnabled';


  // Predefined Macros
  cMacroPrefix          = '${';
  cMacroSuffix          = '}';
  cMacro_FPCSrcDir      = '${FPCSRCDIR}';
  cMacro_FPGuiDir       = '${FPGUIDIR}';
  cMacro_FPGuiLibDir    = '${FPGUILIBDIR}';
  cMacro_SyntaxDefDir   = '${SYNTAXDEFDIR}';
  cMacro_TemplateDir    = '${TEMPLATEDIR}';
  cMacro_Compiler       = '${COMPILER}';
  cMacro_Debugger       = '${DEBUGGER}';
  cMacro_ExeExt         = '${EXEEXT}';
  cMacro_Target         = '${TARGET}';
  cMacro_ProjectDir     = '${PROJDIR}';


  OSTarget: String    = {$I %FPCTARGETOS%};
  CPUTarget: String   = {$I %FPCTARGETCPU%};
  FPCVersion: String  = {$I %FPCVERSION%};
  FPCDate: String     = {$I %FPCDATE%};


  // Unicode character used as grid check mark
  cMultiplicationX = #$E2#$9C#$95;
  cNormCheck = #$E2#$9C#$93;
  cHeavyCheck = #$E2#$9C#$94;
  cHeavyX = #$E2#$9C#$96;
  cMedCircle = #$E2#$9A#$AB;
  cCheck = cHeavyCheck;
{
  U+2715 MULTIPLICATION X
    UTF-8: 0xE2 0x9C 0x95
    UTF-16: 0x2715

  U+2713 CHECK MARK
    UTF-8: 0xE2 0x9C 0x93
    UTF-16: 0x2713

  U+2714 HEAVY CHECK MARK
    UTF-8: 0xE2 0x9C 0x94
    UTF-16: 0x2714

  U+2716 HEAVY MULTIPLICATION X
    UTF-8: 0xE2 0x9C 0x96
    UTF-16: 0x2716

  U+26AB MEDIUM BLACK CIRCLE
    UTF-8: 0xE2 0x9A 0xAB
    UTF-16: 0x26AB
}


implementation

initialization
  OSTarget := Lowercase(OSTarget);

end.

