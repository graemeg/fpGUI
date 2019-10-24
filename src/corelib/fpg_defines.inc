// common compiler options
{$mode objfpc}{$H+}
{$interfaces corba}
{$if defined(darwin)}
  {$define AggCanvas}
  {$modeswitch objectivec2}
{$endif}
{$WARN 5024 off : Parameter "$1" not used}


// To enable the AggPas powered Canvas class
{.$define AGGCanvas}


// **** Feature specific defines ****
{$ifdef WINDOWS}
  // enable or disable DND support. Disabled by default while implementing AlienWindows.
  {$define HAS_DND}
  // enable or disaple window opacity support
  {$define HAS_OPACITY}
{$endif}


// *************  DEBUG use only - [start]  *************
{.$define CStackDebug}      // call stack output
{.$define GDEBUG}           // various verbose output to console
{.$Define DNDDEBUG}         // drag-n-drop specific debugging
{.$Define DND_DEBUG}        // drag-n-drop specific debugging
{.$Define X11CanvasDEBUG}   // As the name suggests
{.$Define DEBUG_NETWMSTATE} // Debug _NET_WM_STATE property messages
{.$Define DEBUGKEYS}        // Debug key input - currently GDI testing
// *************  DEBUG use only - [end]    *************


