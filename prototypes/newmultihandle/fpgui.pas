unit fpgui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gfxbase, fpgfx;

{ Style includes }
{$include gui_style.inc}

{ TFWidget includes }
{$include gui_widget.inc}

{ Primary widgets includes }
{$include gui_button.inc}

var
  { Default Styles }
  GFDefaultStyle: TFStyle;

implementation

{$define READ_IMPLEMENTATION}

{ Style includes }
{$include gui_style.inc}

{ TFWidget includes }
{$include gui_widget.inc}

{ Primary widgets includes }
{$include gui_button.inc}

initialization
  { Default Styles }
  GFDefaultStyle := TFStyle.Create;

end.

