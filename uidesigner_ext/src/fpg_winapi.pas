{ Win32 header for loading system palette

  Copyright (C) 2013 Krzysztof Dibowski dibowski_at_interia.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


unit fpg_WinAPI;

{$mode objfpc}

interface

uses
  dynlibs;

const
  LIB_USER32 = 'user32.dll';

// Colors description on http://msdn.microsoft.com/en-us/library/windows/desktop/ms724371(v=vs.85).aspx
const
  COLOR_BACKGROUND    = 1;
  COLOR_BTNFACE       = 15;
  COLOR_WINDOW        = 5;
  COLOR_APPWORKSPACE  = 12;
  COLOR_GRAYTEXT      = 17;
  COLOR_BTNSHADOW     = 16;
  COLOR_HIGHLIGHT     = 13;
  COLOR_HIGHLIGHTTEXT = 14;
  COLOR_CAPTIONTEXT   = 9;
  COLOR_BTNHIGHLIGHT  = 20;
  COLOR_BTNHILIGHT    = 20;
  COLOR_SCROLLBAR     = 0;
  COLOR_INFOBK        = 24;

var
  LibUser32Handle: TLibHandle=0;
  GetSysColor: function (nIndex:longint):DWORD; stdcall;

  function LoadUser32Lib: Boolean;
  procedure UnloadUser32Lib;

implementation

function LoadUser32Lib: Boolean;
begin
  if (LibUser32Handle<>0) then Exit(True);

  LibUser32Handle := LoadLibrary(LIB_USER32);

  Result := LibUser32Handle<>0;

  if Result then
    Pointer(GetSysColor) := GetProcedureAddress(LibUser32Handle, 'GetSysColor');
end;

procedure UnloadUser32Lib;
begin
  if LibUser32Handle<>0 then
    UnloadLibrary(LibUser32Handle);
end;

end.
