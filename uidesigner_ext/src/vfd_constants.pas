{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
  distribution, for details of the copyright.

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    This unit contains all the language resource strings used by the
    UI Designer project. Thus making it possible to translate the UI Designer
    to other languages. It also contains all project constants.
}

unit vfd_constants;

{$mode objfpc}{$H+}

interface

uses
  fpg_constants;


const
  cFileFilter = '%s (%s)|%s';
  cPascalSourceFiles = '*.pp;*.pas;*.inc;*.dpr;*.lpr';
  cAppName = 'fpGUI UI Designer';
  cAppVersion = FPGUI_VERSION;
  cAppNameAndVersion = cAppName + ' v' + cAppVersion;
  cDesignerINIVersion = 1;

resourcestring
  rsOpenFormFile = 'Open form file';
  rsPascalSourceFiles = 'Pascal source filess';
  rsSaveFormFile = 'Save form source';
  rsVersion = 'Version: %s';
  rsWrittenBy = 'Written by %s';
  rsCompiledOn = 'Compiled on:  %s';
  rsNewUnnamedForm = 'new';
  rsDesignerHelp1 = 'F11: switch to Properties';
  rsDesignerHelp2 = 'TAB, SHIFT+TAB: select next widget';
  rsDesignerHelp3 = 'F2: edit widget order';
  rsDesignerQuickHelp = 'Quick Help';
  rsLeft = 'Left';
  rsTop = 'Top';
  rsWidth = 'Width';
  rsHeight = 'Height';
  rsNewClassName = 'Class name';
  rsNewFormName = 'Form name';
  rsPosition = 'Position';
  rsFormTitle = 'Form %s';
  rsUp = 'Up';
  rsDown = 'Down';
  rsGridResolution = 'Grid resolution';
  rsRecentFilesCount = 'Recent files count';
  rsShowFullPathName = 'Show the full file path';
  rsFormDesigner = 'Form designer';
  rsOpenRecentSettings = 'Open Recent menu settings';
  rsVarious = 'Various';
  rsUndoOnPropertyExit = 'Undo on property editor exit';
  rsOneClickSelectAndMove = 'One click select and move';
  rsDefaultFileExt = 'Default file extension';
  rsUseCodeRegions = 'Use code-folding regions in auto-generated code';
  rsIndentType = 'Indent Type for generated code';

  rsDlgProductInfo = 'Product Information';
  rsDlgSetup = 'General Settings';
  rsDlgInsertCustomWidget = 'Insert Custom Widget';
  rsDlgNewForm = 'New Form';
  rsDlgEditFormPosition = 'Form Position';
  rsDlgWidgetOrder = 'Widget Order';
  rsDlgTabOrder = 'Tab Order';

  rsErrUnitNotFound = 'The unit <%s> was not found.';
  rsErrLoadingForm = 'Error loading form';
  rsErrFailedToFindDesignerForm = 'Failed to find Designer Form';
  rsErrFormSaveIOError = 'Form save I/O failure in <%s>.';
  rsErrNameConflict = 'Name Conflict';
  rsErrNoStringsProperty = 'Failed to find TStrings type property.';
  rsErrFailedToParseBoolean = 'Failed to parse Boolean value <%s>';

implementation

end.

