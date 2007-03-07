{
    fpGUI  -  Free Pascal GUI Library
    
    fpGFX  -  Main unit for the core drawing engine of fpGUI
    
    Copyright (C) 2006 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit fpgfx;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  gfxinterface;

type

  { TFFont }

  TFFont = class(TDefFont)
  end;

  { TFCanvas }

  TFCanvas = class(TDefCanvas)
  end;

  { TFBitmap }

  TFBitmap = class(TDefBitmap)
  end;

  { TFScreen }

  TFScreen = class(TDefScreen)
  end;

  { TFWindow }

  TFWindow = class(TDefWindow)
  end;

  { TFApplication }

  TFApplication = class(TDefApplication)
  public
//    procedure   CreateForm(InstanceClass: TComponentClass; var Reference);
  end;

{ Using the singleton pattern to hide instance variables and
  only instantiate them when they are referred to for the first time. }
function GFScreen: TFScreen;
function GFApplication: TFApplication;


implementation


var
  uScreen: TFScreen;
  uApplication: TFApplication;


function GFScreen: TFScreen;
begin
  if uScreen = nil then
    uScreen := TFScreen.Create;
  result := uScreen;
end;

function GFApplication: TFApplication;
begin
  if uApplication = nil then
    uApplication := TFApplication.Create;
  result := uApplication;
end;


initialization
  uScreen := nil;
  uApplication := nil;

finalization
  uApplication.Free;
  uScreen.Free;

end.

