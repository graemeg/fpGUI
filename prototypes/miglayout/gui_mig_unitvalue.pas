unit gui_mig_unitvalue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils{, fphash,}, contnrs;
  
type

  TUnitValue = class(TObject)
  private
    FPixels: integer;
  public
    const
      STATIC = 100;
      ADD = 101; // Must have "sub-unit values"
      SUB = 102; // Must have "sub-unit values"
      MUL = 103; // Must have "sub-unit values"
      DIV_ = 104; // Must have "sub-unit values"
      MIN = 105; // Must have "sub-unit values"
      MAX = 106; // Must have "sub-unit values"
      MID = 107; // Must have "sub-unit values"

      PIXEL = 0;
      LPX = 1;
      LPY = 2;
      MM = 3;
      CM = 4;
      INCH = 5;
      PERCENT = 6;
      PT = 7;
      SPX = 8;
      SPY = 9;
      ALIGN = 12;
      MIN_SIZE = 13;
      PREF_SIZE = 14;
      MAX_SIZE = 15;
      BUTTON = 16;
      LINK_X = 18;   // First link
      LINK_Y = 19;
      LINK_W = 20;
      LINK_H = 21;
      LINK_X2 = 22;
      LINK_Y2 = 23;
      LINK_XPOS = 24;
      LINK_YPOS = 25;    // Last link
      LOOKUP = 26;
      LABEL_ALIGN = 27;
      IDENTITY = -1;
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

