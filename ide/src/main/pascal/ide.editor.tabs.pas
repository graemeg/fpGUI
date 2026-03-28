{
    fpGUI IDE - Editor Tab Helpers

    Copyright (C) 2006 - 2026 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Extracted from ide.form.main.pas — separates editor tab helper
      logic from the main form.

      HighlightKindForExtension is a pure function that maps file
      extensions to syntax highlight kinds.
}
unit ide.editor.tabs;

{$mode objfpc}{$H+}

interface

type
  THighlightKind = (hkNone, hkPascal, hkPatch, hkINI, hkXML);

{ Pure function: given a file extension (including the dot), return
  the appropriate highlight kind. }
function HighlightKindForExtension(const AExt: string): THighlightKind;

implementation

function HighlightKindForExtension(const AExt: string): THighlightKind;
begin
  if (AExt = '.pas') or (AExt = '.pp') or (AExt = '.inc') or
     (AExt = '.lpr') or (AExt = '.dpr') then
    Result := hkPascal
  else if (AExt = '.patch') or (AExt = '.diff') then
    Result := hkPatch
  else if (AExt = '.ini') or (AExt = '.cfg') or (AExt = '.conf') then
    Result := hkINI
  else if (AExt = '.xml') or (AExt = '.html') or (AExt = '.htm') or
          (AExt = '.xhtml') or (AExt = '.svg') or (AExt = '.xsd') or
          (AExt = '.xsl') or (AExt = '.xslt') or (AExt = '.lpi') or
          (AExt = '.lpk') then
    Result := hkXML
  else
    Result := hkNone;
end;

end.
