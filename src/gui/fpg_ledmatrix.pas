{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2016 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit defines a classic 5x7 LED Matrix display widget. The
      layout of the matrix is as follows. Each column is represented by a Byte
      data type. In the figure below we are representing a '?' symbol. The o
      denotes the bit is not set. The x denotes the bit is set.

            |  COLUMN
            | 0 1 2 3 4
       -----+----------
          0 | o x x x o
          1 | x o o o x
       R  2 | o o o o x
       O  3 | o o o x o
       W  4 | o o x o o
          5 | o o o o o
          6 | o o x o o
          7 | -unused-


    TODO:
      * Support multiple font sets
      * Ability for developer to add custom characters
      * Scrolling text support
      * Custom borders
      * Various matrix size. eg: 5x8, 8x8
      * Multi-line support
      * Limit the matrix processing only to the characters that are actually
        visible in the widget.

}
unit fpg_ledmatrix;

{$mode objfpc}{$h+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  fpg_widget;

type

  TfpgLEDMatrix = class(TfpgWidget)
  private
    FLEDOnColor: TfpgColor;
    FLEDOffColor: TfpgColor;
    FLEDSize: integer;
    FLEDGap: integer;
    FText: TfpgString;
    FTimer: TfpgTimer;
    FScrolling: boolean;
    FCurrentStep: smallint;
    FScrollLengthPixels: integer;
    procedure   SetLEDOnColor(const avalue: TfpgColor);
    procedure   SetLEDOffColor(const avalue: TfpgColor);
    procedure   SetLEDSize(const avalue: integer);
    procedure   SetLEDGap(const avalue: integer);
    procedure   SetScrolling(const avalue: boolean);
    procedure   SetText(const avalue: TfpgString);
    procedure   TimerFired(Sender: TObject);
    procedure   CalcScrollLength;
  protected
    procedure   HandlePaint; override;
    procedure   HandleAlignments(const dwidth, dheight: TfpgCoord); override;
    procedure   PaintBackgroundLEDs(const AX, AY: TfpgCoord); virtual;
    procedure   DrawLEDChar(const AX, AY: TfpgCoord; const AChar: TfpgChar); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Anchors;
    property    Align;
    property    BackgroundColor default clBlack;
//    property    BorderStyle: TfpgEditBorderStyle read FBorderStyle write SetBorderStyle default ebsDefault;
    property    Height default 30;
    property    Hint;
    property    LEDGap: integer read FLEDGap write SetLEDGap default 1;
    property    LEDSize: integer read FLEDSize write SetLEDSize default 2;
    property    LEDOnColor: TfpgColor read FLEDOnColor write SetLEDOnColor default TfpgColor($FFFFB539);
    property    LEDOffColor: TfpgColor read FLEDOffColor write SetLEDOffColor default TfpgColor($FF634210);
    property    TabOrder;
    property    Text: TfpgString read FText write SetText;
    property    Width default 150;
    property    Scrolling: boolean read FScrolling write SetScrolling default False;
    property    ShowHint;
    property    OnClick;
    property    OnDoubleClick;
    property    OnEnter;
    property    OnExit;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnShowHint;
  end;


implementation

uses
  fpg_stringutils;

type

  TLEDCharMask = object
    Col0: byte;
    Col1: byte;
    Col2: byte;
    Col3: byte;
    Col4: byte;
    function GetMaskForColumn(const ACol: integer): Byte;
    function IsBitSet(const ACol, ARow: integer): boolean;
  end;

const
  cLEDFont: array[32..255] of TLEDCharMask = (
      (Col0: $00; Col1: $00; Col2: $00; Col3: $00; Col4: $00 ),  // space
      (Col0: $00; Col1: $00; Col2: $F6; Col3: $00; Col4: $00 ),  // !
      (Col0: $00; Col1: $C0; Col2: $00; Col3: $C0; Col4: $00 ),  // "
      (Col0: $28; Col1: $7C; Col2: $28; Col3: $7C; Col4: $28 ),  // #
      (Col0: $24; Col1: $54; Col2: $FE; Col3: $54; Col4: $48 ),  //
      (Col0: $64; Col1: $68; Col2: $10; Col3: $2C; Col4: $4C ),  // %
      (Col0: $6C; Col1: $92; Col2: $AA; Col3: $44; Col4: $0A ),  // &
      (Col0: $00; Col1: $20; Col2: $C0; Col3: $00; Col4: $00 ),  // '
      (Col0: $00; Col1: $00; Col2: $7C; Col3: $82; Col4: $00 ),  // (
      (Col0: $00; Col1: $82; Col2: $7C; Col3: $00; Col4: $00 ),  // )
      (Col0: $54; Col1: $38; Col2: $FE; Col3: $38; Col4: $54 ),  // *
      (Col0: $10; Col1: $10; Col2: $7C; Col3: $10; Col4: $10 ),  // +
      (Col0: $00; Col1: $00; Col2: $1A; Col3: $1C; Col4: $00 ),  // ,
      (Col0: $10; Col1: $10; Col2: $10; Col3: $10; Col4: $10 ),  // -
      (Col0: $00; Col1: $06; Col2: $06; Col3: $00; Col4: $00 ),  // .
      (Col0: $04; Col1: $08; Col2: $10; Col3: $20; Col4: $40 ),  // /
      (Col0: $7C; Col1: $8A; Col2: $92; Col3: $A2; Col4: $7C ),  // 0
      (Col0: $00; Col1: $42; Col2: $FE; Col3: $02; Col4: $00 ),  // 1
      (Col0: $46; Col1: $8A; Col2: $92; Col3: $92; Col4: $62 ),  // 2
      (Col0: $44; Col1: $92; Col2: $92; Col3: $92; Col4: $6C ),  // 3
      (Col0: $18; Col1: $28; Col2: $48; Col3: $FE; Col4: $08 ),  // 4
      (Col0: $F4; Col1: $92; Col2: $92; Col3: $92; Col4: $8C ),  // 5
      (Col0: $7C; Col1: $92; Col2: $92; Col3: $92; Col4: $4C ),  // 6
      (Col0: $C0; Col1: $80; Col2: $8E; Col3: $90; Col4: $E0 ),  // 7
      (Col0: $6C; Col1: $92; Col2: $92; Col3: $92; Col4: $6C ),  // 8
      (Col0: $64; Col1: $92; Col2: $92; Col3: $92; Col4: $7C ),  // 9
      (Col0: $00; Col1: $6C; Col2: $6C; Col3: $00; Col4: $00 ),  // :
      (Col0: $00; Col1: $DA; Col2: $DC; Col3: $00; Col4: $00 ),  // ;
      (Col0: $10; Col1: $28; Col2: $44; Col3: $82; Col4: $00 ),  // <
      (Col0: $28; Col1: $28; Col2: $28; Col3: $28; Col4: $28 ),  // =
      (Col0: $00; Col1: $82; Col2: $44; Col3: $28; Col4: $10 ),  // >
      (Col0: $40; Col1: $80; Col2: $9A; Col3: $90; Col4: $60 ),  // ?
      (Col0: $7C; Col1: $92; Col2: $AA; Col3: $BA; Col4: $70 ),  // @
      (Col0: $7E; Col1: $90; Col2: $90; Col3: $90; Col4: $7E ),  // A
      (Col0: $FE; Col1: $92; Col2: $92; Col3: $92; Col4: $6C ),  // B
      (Col0: $7C; Col1: $82; Col2: $82; Col3: $82; Col4: $44 ),  // C
      (Col0: $FE; Col1: $82; Col2: $82; Col3: $82; Col4: $7C ),  // D
      (Col0: $FE; Col1: $92; Col2: $92; Col3: $92; Col4: $82 ),  // E
      (Col0: $FE; Col1: $90; Col2: $90; Col3: $90; Col4: $80 ),  // F
      (Col0: $7C; Col1: $82; Col2: $92; Col3: $92; Col4: $5C ),  // G
      (Col0: $FE; Col1: $10; Col2: $10; Col3: $10; Col4: $FE ),  // H
      (Col0: $82; Col1: $82; Col2: $FE; Col3: $82; Col4: $82 ),  // I
      (Col0: $04; Col1: $02; Col2: $02; Col3: $02; Col4: $FC ),  // J
      (Col0: $FE; Col1: $10; Col2: $28; Col3: $44; Col4: $82 ),  // K
      (Col0: $FE; Col1: $02; Col2: $02; Col3: $02; Col4: $02 ),  // L
      (Col0: $FE; Col1: $40; Col2: $20; Col3: $40; Col4: $FE ),  // M
      (Col0: $FE; Col1: $20; Col2: $10; Col3: $08; Col4: $FE ),  // N
      (Col0: $7C; Col1: $82; Col2: $82; Col3: $82; Col4: $7C ),  // O
      (Col0: $FE; Col1: $90; Col2: $90; Col3: $90; Col4: $60 ),  // P
      (Col0: $7C; Col1: $82; Col2: $82; Col3: $86; Col4: $7E ),  // Q
      (Col0: $FE; Col1: $90; Col2: $90; Col3: $90; Col4: $6E ),  // R
      (Col0: $64; Col1: $92; Col2: $92; Col3: $92; Col4: $4C ),  // S
      (Col0: $80; Col1: $80; Col2: $FE; Col3: $80; Col4: $80 ),  // T
      (Col0: $FC; Col1: $02; Col2: $02; Col3: $02; Col4: $FC ),  // U
      (Col0: $E0; Col1: $18; Col2: $06; Col3: $18; Col4: $E0 ),  // V
      (Col0: $FC; Col1: $02; Col2: $0C; Col3: $02; Col4: $FC ),  // W
      (Col0: $C6; Col1: $28; Col2: $10; Col3: $28; Col4: $C6 ),  // X
      (Col0: $C0; Col1: $20; Col2: $1E; Col3: $20; Col4: $C0 ),  // Y
      (Col0: $86; Col1: $8A; Col2: $92; Col3: $A2; Col4: $C2 ),  // Z
      (Col0: $00; Col1: $FE; Col2: $82; Col3: $82; Col4: $00 ),  // [
      (Col0: $40; Col1: $20; Col2: $10; Col3: $08; Col4: $04 ),  // \
      (Col0: $00; Col1: $82; Col2: $82; Col3: $FE; Col4: $00 ),  // ]
      (Col0: $20; Col1: $40; Col2: $80; Col3: $40; Col4: $20 ),  // ^
      (Col0: $02; Col1: $02; Col2: $02; Col3: $02; Col4: $02 ),  // _
      (Col0: $00; Col1: $00; Col2: $C0; Col3: $20; Col4: $00 ),  // `
      (Col0: $04; Col1: $2A; Col2: $2A; Col3: $2A; Col4: $1E ),  // a
      (Col0: $FE; Col1: $22; Col2: $22; Col3: $22; Col4: $1C ),  // b
      (Col0: $1C; Col1: $22; Col2: $22; Col3: $22; Col4: $14 ),  // c
      (Col0: $1C; Col1: $22; Col2: $22; Col3: $22; Col4: $FE ),  // d
      (Col0: $1C; Col1: $2A; Col2: $2A; Col3: $2A; Col4: $18 ),  // e
      (Col0: $10; Col1: $7E; Col2: $90; Col3: $90; Col4: $40 ),  // f
      (Col0: $10; Col1: $2A; Col2: $2A; Col3: $2A; Col4: $1C ),  // g
      (Col0: $FE; Col1: $20; Col2: $20; Col3: $20; Col4: $1E ),  // h
      (Col0: $00; Col1: $22; Col2: $BE; Col3: $02; Col4: $00 ),  // i
      (Col0: $00; Col1: $02; Col2: $BC; Col3: $00; Col4: $00 ),  // j
      (Col0: $FE; Col1: $08; Col2: $08; Col3: $14; Col4: $22 ),  // k
      (Col0: $00; Col1: $82; Col2: $FE; Col3: $02; Col4: $00 ),  // l
      (Col0: $3E; Col1: $20; Col2: $3E; Col3: $20; Col4: $1E ),  // m
      (Col0: $3E; Col1: $20; Col2: $20; Col3: $20; Col4: $1E ),  // n
      (Col0: $1C; Col1: $22; Col2: $22; Col3: $22; Col4: $1C ),  // o
      (Col0: $3E; Col1: $28; Col2: $28; Col3: $28; Col4: $10 ),  // p
      (Col0: $10; Col1: $28; Col2: $28; Col3: $28; Col4: $3E ),  // q
      (Col0: $3E; Col1: $20; Col2: $20; Col3: $20; Col4: $10 ),  // r
      (Col0: $12; Col1: $2A; Col2: $2A; Col3: $2A; Col4: $24 ),  // s
      (Col0: $20; Col1: $FC; Col2: $22; Col3: $22; Col4: $00 ),  // t
      (Col0: $3C; Col1: $02; Col2: $02; Col3: $02; Col4: $3E ),  // u
      (Col0: $38; Col1: $04; Col2: $02; Col3: $04; Col4: $38 ),  // v
      (Col0: $3C; Col1: $02; Col2: $0C; Col3: $02; Col4: $3C ),  // w
      (Col0: $22; Col1: $14; Col2: $08; Col3: $14; Col4: $22 ),  // x
      (Col0: $22; Col1: $14; Col2: $08; Col3: $10; Col4: $20 ),  // y
      (Col0: $22; Col1: $26; Col2: $2A; Col3: $32; Col4: $22 ),  // z
      (Col0: $00; Col1: $10; Col2: $6C; Col3: $82; Col4: $00 ),  // {
      (Col0: $00; Col1: $00; Col2: $FE; Col3: $00; Col4: $00 ),  // |
      (Col0: $00; Col1: $82; Col2: $6C; Col3: $10; Col4: $00 ),  // }
      (Col0: $40; Col1: $80; Col2: $40; Col3: $20; Col4: $40 ),  // ~  end of Basic-Latin
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //   U+007F
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $7C; Col2: $7C; Col3: $7C; Col4: $00 ),  //    ???
      (Col0: $00; Col1: $00; Col2: $00; Col3: $00; Col4: $00 ),  //    U+00A0  No-break space
      (Col0: $00; Col1: $00; Col2: $DE; Col3: $00; Col4: $00 ),  // ¡  U+00A1  inverted exclamation - start of Latin-1 Supplement
      (Col0: $38; Col1: $44; Col2: $FE; Col3: $44; Col4: $28 ),  // ¢
      (Col0: $06; Col1: $7A; Col2: $92; Col3: $92; Col4: $42 ),  // £
      (Col0: $44; Col1: $38; Col2: $28; Col3: $38; Col4: $44 ),  // ¤
      (Col0: $D0; Col1: $30; Col2: $1E; Col3: $30; Col4: $D0 ),  // ¥
      (Col0: $00; Col1: $00; Col2: $EE; Col3: $00; Col4: $00 ),  // ¦
      (Col0: $52; Col1: $AA; Col2: $AA; Col3: $AA; Col4: $94 ),  // §
      (Col0: $00; Col1: $80; Col2: $00; Col3: $80; Col4: $00 ),  // ¨
      (Col0: $7C; Col1: $92; Col2: $AA; Col3: $AA; Col4: $7C ),  // ©
      (Col0: $00; Col1: $B4; Col2: $B4; Col3: $74; Col4: $00 ),  // ª
      (Col0: $08; Col1: $14; Col2: $2A; Col3: $14; Col4: $22 ),  // «
      (Col0: $10; Col1: $10; Col2: $10; Col3: $10; Col4: $18 ),  // ¬
      (Col0: $00; Col1: $10; Col2: $10; Col3: $10; Col4: $00 ),  // ­
      (Col0: $7C; Col1: $82; Col2: $BA; Col3: $A2; Col4: $7C ),  // ®
      (Col0: $80; Col1: $80; Col2: $80; Col3: $80; Col4: $80 ),  // ¯
      (Col0: $00; Col1: $40; Col2: $A0; Col3: $40; Col4: $00 ),  // °
      (Col0: $00; Col1: $24; Col2: $74; Col3: $24; Col4: $00 ),  // ±
      (Col0: $00; Col1: $90; Col2: $B0; Col3: $50; Col4: $00 ),  // ²
      (Col0: $00; Col1: $88; Col2: $A8; Col3: $70; Col4: $00 ),  // ³
      (Col0: $00; Col1: $00; Col2: $40; Col3: $80; Col4: $00 ),  // ´
      (Col0: $3E; Col1: $04; Col2: $04; Col3: $38; Col4: $04 ),  // µ
      (Col0: $60; Col1: $F0; Col2: $FE; Col3: $80; Col4: $FE ),  // ¶
      (Col0: $00; Col1: $00; Col2: $10; Col3: $00; Col4: $00 ),  // ·
      (Col0: $00; Col1: $02; Col2: $0A; Col3: $04; Col4: $00 ),  // ¸
      (Col0: $00; Col1: $50; Col2: $F0; Col3: $10; Col4: $00 ),  // ¹
      (Col0: $00; Col1: $50; Col2: $B0; Col3: $50; Col4: $00 ),  // º
      (Col0: $22; Col1: $14; Col2: $2A; Col3: $14; Col4: $08 ),  // »
      (Col0: $E8; Col1: $10; Col2: $2C; Col3: $44; Col4: $9E ),  // ¼
      (Col0: $E8; Col1: $10; Col2: $20; Col3: $56; Col4: $8A ),  // ½
      (Col0: $E8; Col1: $F0; Col2: $2C; Col3: $44; Col4: $9E ),  // ¾
      (Col0: $0C; Col1: $12; Col2: $B2; Col3: $02; Col4: $05 ),  // ¿
      (Col0: $1E; Col1: $A8; Col2: $68; Col3: $28; Col4: $1E ),  // À
      (Col0: $1E; Col1: $28; Col2: $68; Col3: $A8; Col4: $1E ),  // Á
      (Col0: $1E; Col1: $68; Col2: $A8; Col3: $68; Col4: $1E ),  // Â
      (Col0: $5E; Col1: $A8; Col2: $A8; Col3: $68; Col4: $9E ),  // Ã
      (Col0: $1E; Col1: $A8; Col2: $28; Col3: $A8; Col4: $1E ),  // Ä
      (Col0: $1E; Col1: $28; Col2: $A8; Col3: $28; Col4: $1E ),  // Å
      (Col0: $7E; Col1: $90; Col2: $FE; Col3: $92; Col4: $92 ),  // Æ
      (Col0: $78; Col1: $84; Col2: $86; Col3: $84; Col4: $48 ),  // Ç
      (Col0: $78; Col1: $84; Col2: $86; Col3: $84; Col4: $48 ),  // È
      (Col0: $3E; Col1: $2A; Col2: $6A; Col3: $AA; Col4: $22 ),  // É
      (Col0: $3E; Col1: $6A; Col2: $AA; Col3: $6A; Col4: $22 ),  // Ê
      (Col0: $3E; Col1: $AA; Col2: $2A; Col3: $AA; Col4: $22 ),  // Ë
      (Col0: $22; Col1: $A2; Col2: $7E; Col3: $22; Col4: $22 ),  // Ì
      (Col0: $22; Col1: $22; Col2: $7E; Col3: $A2; Col4: $22 ),  // Í
      (Col0: $22; Col1: $62; Col2: $BE; Col3: $62; Col4: $22 ),  // Î
      (Col0: $22; Col1: $A2; Col2: $3E; Col3: $A2; Col4: $22 ),  // Ï
      (Col0: $10; Col1: $FE; Col2: $92; Col3: $44; Col4: $38 ),  // Ð
      (Col0: $7E; Col1: $90; Col2: $88; Col3: $44; Col4: $BE ),  // Ñ
      (Col0: $1C; Col1: $A2; Col2: $62; Col3: $22; Col4: $14 ),  // Ò
      (Col0: $1C; Col1: $22; Col2: $62; Col3: $A2; Col4: $14 ),  // Ó
      (Col0: $1C; Col1: $62; Col2: $A2; Col3: $62; Col4: $14 ),  // Ô
      (Col0: $5C; Col1: $A2; Col2: $A2; Col3: $62; Col4: $94 ),  // Õ
      (Col0: $1C; Col1: $A2; Col2: $22; Col3: $A2; Col4: $14 ),  // Ö
      (Col0: $00; Col1: $14; Col2: $08; Col3: $14; Col4: $00 ),  // ×
      (Col0: $7C; Col1: $8A; Col2: $92; Col3: $A2; Col4: $7C ),  // Ø
      (Col0: $7C; Col1: $82; Col2: $42; Col3: $02; Col4: $7C ),  // Ù
      (Col0: $7C; Col1: $02; Col2: $42; Col3: $82; Col4: $7C ),  // Ú
      (Col0: $7C; Col1: $42; Col2: $82; Col3: $42; Col4: $7C ),  // Û
      (Col0: $7C; Col1: $82; Col2: $02; Col3: $82; Col4: $7C ),  // Ü
      (Col0: $60; Col1: $10; Col2: $4E; Col3: $90; Col4: $60 ),  // Ý
      (Col0: $00; Col1: $FE; Col2: $28; Col3: $28; Col4: $10 ),  // Þ
      (Col0: $7E; Col1: $90; Col2: $92; Col3: $72; Col4: $0C ),  // ß
      (Col0: $04; Col1: $AA; Col2: $6A; Col3: $2A; Col4: $1E ),  // à
      (Col0: $04; Col1: $2A; Col2: $6A; Col3: $AA; Col4: $1E ),  // á
      (Col0: $04; Col1: $6A; Col2: $AA; Col3: $6A; Col4: $1E ),  // â
      (Col0: $44; Col1: $AA; Col2: $AA; Col3: $6A; Col4: $9E ),  // ã
      (Col0: $04; Col1: $AA; Col2: $2A; Col3: $AA; Col4: $1E ),  // ä
      (Col0: $04; Col1: $2A; Col2: $AA; Col3: $2A; Col4: $1E ),  // å
      (Col0: $04; Col1: $2A; Col2: $3E; Col3: $2A; Col4: $1A ),  // æ
      (Col0: $18; Col1: $24; Col2: $26; Col3: $24; Col4: $00 ),  // ç
      (Col0: $1C; Col1: $AA; Col2: $6A; Col3: $2A; Col4: $18 ),  // è
      (Col0: $1C; Col1: $2A; Col2: $6A; Col3: $AA; Col4: $18 ),  // é
      (Col0: $1C; Col1: $6A; Col2: $AA; Col3: $6A; Col4: $18 ),  // ê
      (Col0: $1C; Col1: $AA; Col2: $2A; Col3: $AA; Col4: $18 ),  // ë
      (Col0: $00; Col1: $A2; Col2: $7E; Col3: $02; Col4: $00 ),  // ì
      (Col0: $00; Col1: $22; Col2: $7E; Col3: $82; Col4: $00 ),  // í
      (Col0: $00; Col1: $62; Col2: $BE; Col3: $42; Col4: $00 ),  // î
      (Col0: $00; Col1: $A2; Col2: $3E; Col3: $82; Col4: $00 ),  // ï
      (Col0: $0C; Col1: $B2; Col2: $52; Col3: $B2; Col4: $1C ),  // ð
      (Col0: $7E; Col1: $A0; Col2: $A0; Col3: $60; Col4: $9E ),  // ñ
      (Col0: $1C; Col1: $A2; Col2: $62; Col3: $22; Col4: $1C ),  // ò
      (Col0: $1C; Col1: $22; Col2: $62; Col3: $A2; Col4: $1C ),  // ó
      (Col0: $1C; Col1: $62; Col2: $A2; Col3: $62; Col4: $1C ),  // ô
      (Col0: $5C; Col1: $A2; Col2: $A2; Col3: $62; Col4: $9C ),  // õ
      (Col0: $1C; Col1: $A2; Col2: $22; Col3: $A2; Col4: $1C ),  // ö
      (Col0: $10; Col1: $10; Col2: $54; Col3: $10; Col4: $10 ),  // ÷
      (Col0: $3A; Col1: $4C; Col2: $54; Col3: $64; Col4: $B8 ),  // ø
      (Col0: $3C; Col1: $82; Col2: $42; Col3: $02; Col4: $3E ),  // ù
      (Col0: $3C; Col1: $02; Col2: $42; Col3: $82; Col4: $3E ),  // ú
      (Col0: $3C; Col1: $42; Col2: $82; Col3: $42; Col4: $3E ),  // û
      (Col0: $3C; Col1: $82; Col2: $02; Col3: $82; Col4: $3E ),  // ü
      (Col0: $22; Col1: $14; Col2: $48; Col3: $90; Col4: $20 ),  // ý
      (Col0: $00; Col1: $7E; Col2: $14; Col3: $08; Col4: $00 ),  // þ
      (Col0: $22; Col1: $94; Col2: $08; Col3: $90; Col4: $20 )   // ÿ
    );


{ TLEDCharMask }

function TLEDCharMask.GetMaskForColumn(const ACol: integer): Byte;
begin
  case ACol of
    0: Result := Col0;
    1: Result := Col1;
    2: Result := Col2;
    3: Result := Col3;
    4: Result := Col4;
  end;
end;

function TLEDCharMask.IsBitSet(const ACol, ARow: integer): boolean;
begin
  Result := (GetMaskForColumn(ACol) and ($80 shr ARow)) <> 0;
end;


{ TfpgLEDMatrix }

procedure TfpgLEDMatrix.SetLEDOnColor(const avalue: TfpgColor);
begin
  if FLEDOnColor = AValue then
    Exit;
  FLEDOnColor := AValue;
  Repaint;
end;

procedure TfpgLEDMatrix.SetLEDOffColor(const avalue: TfpgColor);
begin
  if FLEDOffColor = AValue then
    Exit;
  FLEDOffColor := AValue;
  Repaint;
end;

procedure TfpgLEDMatrix.PaintBackgroundLEDs(const AX, AY: TfpgCoord);
var
  c, r: integer;
  dx, dy: TfpgCoord;
begin
  Canvas.Color := LEDOffColor;
  dx := AX;
  while dx < Width do
  begin
    for c := 0 to 4 do  // 5 columns
    begin
      dy := AY;
      for r := 0 to 6 do  // 7 rows
      begin
        Canvas.FillRectangle(dx, dy, LEDSize, LEDSize);
        inc(dy, LEDSize + LEDGap);
      end;
      inc(dx, LEDSize + LEDGap);
    end;
  end;  { while }
end;

constructor TfpgLEDMatrix.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 150;
  FHeight := 30;
  FBackgroundColor := clBlack;
  FLEDOnColor := TfpgColor($FFFFB539);
  FLEDOffColor := TfpgColor($FF634210);
  FLEDGap := 1;
  FLEDSize := 2;
  FScrolling := False;
  FCurrentStep := 0;
end;

procedure TfpgLEDMatrix.SetLEDSize(const avalue: integer);
begin
  if FLEDSize = AValue then
    Exit;
  FLEDSize := AValue;
  if FScrolling then
    CalcScrollLength;
  Repaint;
end;

procedure TfpgLEDMatrix.SetLEDGap(const avalue: integer);
begin
  if FLEDGap = AValue then
    Exit;
  FLEDGap := AValue;
  if FScrolling then
    CalcScrollLength;
  Repaint;
end;

procedure TfpgLEDMatrix.SetScrolling(const avalue: boolean);
begin
  if FScrolling = AValue then
    Exit;
  FScrolling := AValue;
  if FScrolling then
  begin
    CalcScrollLength;
    FTimer := TfpgTimer.Create(100);
    FTimer.OnTimer := @TimerFired;
    FTimer.Enabled := True;
  end
  else
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;
end;

procedure TfpgLEDMatrix.HandlePaint;
var
  i: integer;
  dx, dy: TfpgCoord;
  lLen: integer;
begin
  inherited HandlePaint;
  dx := LEDGap;
  dy := (GetClientRect.Height - (LEDSize * 7) - (LEDGap * 6)) div 2;
  Canvas.Color := LEDOffColor;
  PaintBackgroundLEDs(dx, dy);
  { When scrolling we want the text to start on the right of the widget }
  if FScrolling and not (csDesigning in ComponentState) then
    dx := GetClientRect.Right - LEDGap - FCurrentStep;
  lLen := UTF8Length(Text);
  for i := 1 to lLen do
  begin
    { TODO: optimise to only paint visible characters }
    DrawLEDChar(dx, dy, UTF8Copy(Text, i, 1));
    inc(dx, (LEDSize+LEDGap) * 6);  // 6 is used because we want one empty column between characters
  end;
end;

{ when the widget gets resized, this is called }
procedure TfpgLEDMatrix.HandleAlignments(const dwidth, dheight: TfpgCoord);
begin
  inherited HandleAlignments(dwidth, dheight);
  if FScrolling then
    CalcScrollLength;
end;

procedure TfpgLEDMatrix.SetText(const avalue: TfpgString);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  if FScrolling then
    CalcScrollLength;
  RePaint;
end;

procedure TfpgLEDMatrix.TimerFired(Sender: TObject);
begin
  Inc(FCurrentStep, LEDSize + LEDGap);
  RePaint;
  if FCurrentStep > FScrollLengthPixels then
    FCurrentStep := 0;
end;

{ This calculate by how much we must scroll all the text off the "screen". }
procedure TfpgLEDMatrix.CalcScrollLength;
begin
                         { text characters ;   LED char size         ;  widget width }
  FScrollLengthPixels := (UTF8Length(Text) * ((LEDSize+LEDGap) * 6)) + GetClientRect.Width;
end;

procedure TfpgLEDMatrix.DrawLEDChar(const AX, AY: TfpgCoord; const AChar: TfpgChar);
var
  c, r: integer;
  dx, dy: TfpgCoord;
  m: TLEDCharMask;
  ansi: AnsiString;
begin
  ansi := Utf8ToAnsi(AChar);
  m := cLEDFont[Ord(ansi[1])];  // retrieve mask record from array
  dx := AX;
  for c := 0 to 4 do  // 5 columns
  begin
    dy := AY;
    for r := 0 to 6 do  // 7 rows
    begin
      if m.IsBitSet(c, r) then
        Canvas.Color := LEDOnColor
      else
        Canvas.Color := LEDOffColor;
      Canvas.FillRectangle(dx, dy, LEDSize, LEDSize);
      inc(dy, LEDSize + LEDGap);
    end;
    inc(dx, LEDSize + LEDGap);
  end;  { for }
end;

end.
