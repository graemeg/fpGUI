unit gui_mig_unitvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphash;
  
type

  TUnitValue = class(TObject)
  private
    FPixels: integer;
  public
    property    Pixels: integer read FPixels;
  end;
  
  TUnitValueArray = array[0..3] of TUnitValue;

implementation

const
    cSTATIC = 100;
    cADD = 101; // Must have "sub-unit values"
    cSUB = 102; // Must have "sub-unit values"
    cMUL = 103; // Must have "sub-unit values"
    cDIV = 104; // Must have "sub-unit values"
    cMIN = 105; // Must have "sub-unit values"
    cMAX = 106; // Must have "sub-unit values"
    cMID = 107; // Must have "sub-unit values"

    cPIXEL = 0;
    cLPX = 1;
    cLPY = 2;
    cMM = 3;
    cCM = 4;
    cINCH = 5;
    cPERCENT = 6;
    cPT = 7;
    cSPX = 8;
    cSPY = 9;
    cALIGN = 12;
    cMIN_SIZE = 13;
    cPREF_SIZE = 14;
    cMAX_SIZE = 15;
    cBUTTON = 16;
    cLINK_X = 18;   // First link
    cLINK_Y = 19;
    cLINK_W = 20;
    cLINK_H = 21;
    cLINK_X2 = 22;
    cLINK_Y2 = 23;
    cLINK_XPOS = 24;
    cLINK_YPOS = 25;    // Last link
    cLOOKUP = 26;
    cLABEL_ALIGN = 27;
    cIDENTITY = -1;


end.

