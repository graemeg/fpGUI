{
  Constants used throughout fpGUI will be defined here. This includes
  language constants for localization.
  
  You only need to changes these defines if you want the default fpGUI language
  to be something other than English.
  
  Soon the lang_*.inc files will be auto generated from the actual *.po files.
  At which point only the .po files need to be maintained.
}
unit gfx_constants;

{$mode objfpc}{$H+}

interface

uses
  gfxbase;

resourcestring

{ Define Compiler language symbol (eg: de for German) to include the correct
  language resource file otherwise the Default (English) resource file will
  be used. }

{.$DEFINE de}     // German
{.$DEFINE ru}     // Russian
{.$DEFINE fr}     // French
{.$DEFINE pt}     // Portuguese (Brazil)
{.$DEFINE af}     // Afrikaans
{.$DEFINE it}     // Italian
{.$DEFINE es}     // Spanish



{$IF defined(de)}
  {$I lang_german.inc}
  
{$ELSEIF defined(ru)}
  {$I lang_russian.inc}
  
{$ELSEIF defined(fr)}
  {$I lang_french.inc}
  
{$ELSEIF defined(pt)}
  {$I lang_portuguese.inc}
  
{$ELSEIF defined(af)}
  {$I lang_afrikaans.inc}

{$ELSEIF defined(it)}
  {$I lang_italian.inc}

{$ELSEIF defined(es)}
  {$I lang_spanish.inc}

{$ELSE}
  {$I lang_english.inc}
{$IFEND}


{ This is so that when we support LTR and RTL languages, the colon will be
  added at the correct place. }
function fpgAddColon(const AText: TfpgString): TfpgString;


implementation


function fpgAddColon(const AText: TfpgString): TfpgString;
begin
  { TODO : Check language direction and add colon at appropriate end. }
  result := AText + ':';
end;

end.

