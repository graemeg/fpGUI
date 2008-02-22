{
  Constants used throughout fpGUI will be defined here. This includes
  language constants for localization. For now, localization is very simplistic
  and done at compile time. At a later date we can autodetect and autoselect
  the correct language resource at runtime.
  
  GetText might also be a better option.
}
unit gfx_constants;

{$mode objfpc}{$H+}

interface

resourcestring

{ Define Compiler language symbol (eg: de for German) to include the correct
  language resource file otherwise the Default (English) resource file will
  be used. }

{.$DEFINE de}     // German
{.$DEFINE ru}     // Russian
{.$DEFINE fr}     // French
{.$DEFINE pt}     // Portuguese
{.$DEFINE za}     // Afrikaans
{.$DEFINE it}     // Italian



{$IF defined(de)}
  {$I lang_german.inc}
  
{$ELSEIF defined(ru)}
  {$I lang_russian.inc}
  
{$ELSEIF defined(fr)}
  {$I lang_french.inc}
  
{$ELSEIF defined(pt)}
  {$I lang_portuguese.inc}
  
{$ELSEIF defined(za)}
  {$I lang_afrikaans.inc}

{$ELSEIF defined(it)}
  {$I lang_italian.inc}

{$ELSE}
  {$I lang_english.inc}
{$IFEND}


implementation

end.

