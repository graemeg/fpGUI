{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2015 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Implements the Windows 2000 look. Simple and functional without
      distractions.
}
unit fpg_style_win2k;

{$mode objfpc}{$H+}

interface

uses
  fpg_main
  ,fpg_style
  ;

type

  TfpgWin2000Style = class(TfpgStyle)

  end;

implementation

uses
  fpg_stylemanager
  ;


initialization
  fpgStyleManager.RegisterClass(cDefaultStyle, TfpgWin2000Style);   // TODO: This will change later
  fpgStyleManager.RegisterClass('Win2000', TfpgWin2000Style);


end.

