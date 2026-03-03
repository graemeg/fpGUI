{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2021 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      XRandR interface functions
}

unit fpg_xrandr_ext;

{$I fpg_defines.inc}

interface

uses
  Classes
  ,SysUtils
  ,ctypes
  ,X
  ,XLib
  ,Xutil
  ,XRandr
  ;

const
  {$IF Defined(DARWIN)}
    libXrandr = 'libXrandr.dylib';
    {$LINKLIB libXrandr}
  {$ELSE}
    libXrandr = 'libXrandr.so';
  {$IFEND}


type
  PRROutput = ^TRROutput;
  TRROutput = culong;

  PXRRMonitorInfo = ^TXRRMonitorInfo;

  TXRRMonitorInfo = record
    name: TAtom;
    primary: TBool;
    automatic: TBool;
    noutput: cint;
    x: cint;
    y: cint;
    width: cint;
    height: cint;
    mwidth: cint;
    mheight: cint;
    outputs: PRROutput;
  end;

function XRRGetMonitors(
  dpy: PDisplay;
  window: TWindow;
  get_active: TBool;
  nmonitors: Pcint
): PXRRMonitorInfo; cdecl; external libXrandr;

procedure XRRFreeMonitors(
  monitors: PXRRMonitorInfo
); cdecl; external libXrandr;


implementation

end.

