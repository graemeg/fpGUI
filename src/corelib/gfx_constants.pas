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

{ Define Compiler language symbol (eg: gr for German) to include the correct
  language resource file otherwise the Default (English) resource file will
  be used. }

{.$DEFINE gr}     // German
{.$DEFINE ru}     // Russian
{.$DEFINE fr}     // French
{.$DEFINE pt}     // Portuguese


{$IF defined(gr)}
  {$I lang_german.inc}
{$ELSEIF defined(ru)}
  {$I lang_russian.inc}
{$ELSEIF defined(fr)}
  {$I lang_french.inc}
{$ELSEIF defined(pt)}
  {$I lang_portuguese.inc}
{$ELSE}
  {$I lang_english.inc}
{$IFEND}


implementation

end.

