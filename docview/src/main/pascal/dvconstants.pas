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
      Constants used by DocView.
}

unit dvConstants;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,fpg_base
  ;

const
  { DO NOT LOCALIZE }
  HelpPathEnvironmentVar = 'HELP';
  BookshelfEnvironmentVar = 'BOOKSHELF';
  HELP_FILE_DELIMITER = '+';
  HELP_FILE_EXTENSION = ExtensionSeparator + 'hlp';
  INF_FILE_EXTENSION = ExtensionSeparator + 'inf';
  NOTES_FILE_EXTENSION = ExtensionSeparator + 'notes';
  BOOKMARK_FILE_EXTENSION = ExtensionSeparator + 'bookmark';
  BOOKMARK_SECTION = '[BOOKMARK]';
  OWN_HELP_MARKER = '[DOCVIEWHELP]';
  cDocViewHelpFile = 'docview.inf';


resourcestring
  rsDVTitle = 'Documentation Viewer';
  rsDVSearchingMsg = 'Searching...';
  rsDVDisplaying = 'Displaying...';
  rsDVLoadingNotes = 'Loading notes...';
  rsDVDisplayContents = 'Display contents...';
  rsDVDisplayingFirstTopic = 'Display first topic...';
  rsDVDone = 'Done';
  rsDVNoFile = 'No file';
  rsDVOpenHelpFile = 'Open Help File';
  rsDVHelpFiles = 'Help Files';
  rsDVNoMatchesFound = '(No matches found for <%s>)';
  rsDVSearchSyntaxError = 'Error in search syntax: ';
  rsDVSearchFoundMsg = 'Found %d matches for ';
  rsDVCouldNotOpen = 'Could not open <%s>';
  rsDVUntitled = '(Untitled)';

const
  hcConfigGeneralTab               = 510;
  hcConfigFontsColorTab            = 520;

const
  PARAM_LINK_NOTE = 'note';
  PARAM_LINK_PROGRAM = 'program';
  PARAM_LINK_URL = 'url';
  PARAM_LINK_EXTERNAL = 'external';

  PRGM_EXPLORER = 'explore'; // web explorer
  PRGM_NETSCAPE = 'netscape';
  PRGM_MOZILLA = 'mozilla';
  PRGM_FIREFOX = 'firefox';



implementation

end.

