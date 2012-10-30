{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
  distribution, for details of the copyright.

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    This unit forms part of the PDF Reporting Engine. This unit builds
    the objects in memory to produce either the preview or pdf files.

    The PDF Reporting Engine was originally written by
    Jean-Marc Levecque <jean-marc.levecque@jmlesite.fr>
}

unit U_Command;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_base,
  fpg_main,
  U_Pdf;

type
  TZone        = (zHeader, zFooter, zPage, zMargins);
  TSectPageNum = (PageNum, SectNum, PSectNum);
  TBorderFlags = set of (bfLeft, bfRight, bfTop, bfBottom);

  TDimensions = record
    T: single;
    L: single;
    R: single;
    B: single;
  end;

  TPaper = record
    H: integer;
    W: integer;
    Printable: TDimensions;
  end;

  // document classes

  T_Section = class(TObject)
  private
    FNumSect: integer;
    FNbPages: integer;
    FPaper: TPaper;
    FMargins: TDimensions;
    FBotHead: single;
    FTopFoot: single;
    FPages: TList;
    FHeader: TList;
    FFooter: TList;
    FFrames: TList;
    FDefCol: integer;
    FTitle: string;
    function GetFirstPage: integer;
    function GetTotalPages: integer;
  public
    constructor Create(APaper: TPaper; AMargins: TDimensions; ANum: integer); virtual;
    destructor Destroy; override;
    procedure LoadPage(APageNum: integer);
    procedure LoadCmdHeader;
    procedure LoadCmdPage;
    procedure LoadCmdFooter;
    procedure LoadCmdGroup;
    procedure LoadCmdGroupToPage;
    procedure LoadSpaceHeader(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer);
    procedure LoadSpacePage(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer);
    procedure LoadSpaceFooter(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer);
    procedure LoadSpaceGroup(AHeight: single);
    procedure LoadFrame(AStyle, ALeft, ARight, ATop, ABottom: integer; AZone: TZone);
    procedure LoadLine(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
    procedure LoadLineHorizHeader(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
    procedure LoadLineHorizPage(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
    procedure LoadLineHorizFooter(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
    procedure LoadLineHorizGroupe(AHeight: single);
    procedure LoadSurf(APos: T_Points; AColor: TfpgColor);
    procedure LoadImgHeader(APosX, APosY: single; AColumn, AImgNum: integer);
    procedure LoadImgPage(APosX, APosY: single; AColumn, AImgNum: integer);
    procedure LoadImgFooter(APosX, APosY: single; AColumn, AImgNum: integer);
    function GetCmdPage(NumPage: integer): TList;
    property CmdHeader: TList read FHeader;
    property CmdFooter: TList read FFooter;
    property NbPages: integer read FNbPages;
    property FirstPage: integer read GetFirstPage;
    property Pages: TList read FPages;
    property TotPages: integer read GetTotalPages;
    property Paper: TPaper read FPaper;
    property Margins: TDimensions read FMargins;
    property CmdFrames: TList read FFrames;
    property DefaultCol: integer read FDefCol write FDefCol;
    property Title: string read FTitle write FTitle;
  end;

  T_Page = class(TObject)
  private
    FNumPageTot: integer;
    FNumPageSect: integer;
    FCommands: TList;
  public
    constructor Create(ANumSec, ANumTot: integer); virtual;
    destructor Destroy; override;
    property Commands: TList read FCommands write FCommands;
    property PagesTot: integer read FNumPageTot;
    property PagesSect: integer read FNumPageSect;
  end;

  T_Group = class(TObject)
  private
    FLineHeight: single;
    FGroupHeight: single;
    FCommands: TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Commands: TList read FCommands write FCommands;
    property LineHeight: single read FLineHeight;
    property GroupeHeight: single read FGroupHeight;
  end;

  T_WriteLine = class(TObject)
  private
    FHeight: integer;
    FCommands: TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadText(APosX, APosY: single; AColumn, AText, AFont, AHeight, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags);
    procedure LoadNumber(APosX, APosY: single; AColumn, ATextNum, ATextTot, AFont, AHeight, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal, AAlpha: Boolean; ATypeNum: TSectPageNum);
    property Commands: TList read FCommands;
    property LineHeight: integer read FHeight;
  end;

  // command classes

  T_Command = class
  end;

  PSection  = ^T_Section;
  PPage     = ^T_Page;
  PLigne    = ^T_WriteLine;
  PCommande = ^T_Command;
  PFont     = ^TfpgFont;

  T_WriteText = class(T_Command)
  private
    FPosX: single;
    FPosY: single;
    FColumn: integer;
    FText: integer;
    FFont: integer;
    FBackColor: integer;
    FBorder: integer;
    FLineSpace: integer;
    FCurFont: Boolean;
    FFlags: TfpgTextFlags;
  public
    constructor Create(APosX, APosY: single; AColumn, AText, AFont, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags); virtual;
    procedure SetPosY(const AValue: single);
    property GetPosX: single read FPosX;
    property GetPosY: single read FPosY;
    property GetColumn: integer read FColumn;
    property GetText: integer read FText;
    property GetFont: integer read FFont;
    property GetBackColor: integer read FBackColor;
    property GetBorder: integer read FBorder;
    property GetLineSpace: integer read FLineSpace;
    property GetCurFont: Boolean read FCurFont;
    property GetFlags: TfpgTextFlags read FFlags;
  end;

  T_Number = class(T_Command)
  private
    FPosX: single;
    FPosY: single;
    FColumn: integer;
    FTextNum: integer;
    FTextTot: integer;
    FFont: integer;
    FBackColor: integer;
    FBorder: integer;
    FLineSpace: integer;
    FCurFont: Boolean;
    FFlags: TfpgTextFlags;
    FTotal: Boolean;
    FAlpha: Boolean;
    FTypeNum: TSectPageNum;
  public
    constructor Create(APosX, APosY: single; AColumn, ATextNum, ATextTot, AFont, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal, AAlpha: Boolean; ATypeNum: TSectPageNum); virtual;
    procedure SetPosY(const AValue: single);
    property GetPosX: single read FPosX;
    property GetPosY: single read FPosY;
    property GetColumn: integer read FColumn;
    property GetTextNum: integer read FTextNum;
    property GetTextTot: integer read FTextTot;
    property GetFont: integer read FFont;
    property GetBackColor: integer read FBackColor;
    property GetBorder: integer read FBorder;
    property GetLineSpace: integer read FLineSpace;
    property GetCurFont: Boolean read FCurFont;
    property GetFlags: TfpgTextFlags read FFlags;
    property GetTotal: Boolean read FTotal;
    property GetAlpha: Boolean read FAlpha;
    property GetTypeNum: TSectPageNum read FTypeNum;
  end;

  T_Line = class(T_Command)
  private
    FPosX: single;
    FPosY: single;
    FColumn: integer;
    FStyle: integer;
    FEndX: single;
    FEndY: single;
  public
    constructor Create(APosX, APosY: single; AColumn, AStyle: integer; AEndX, AEndY: single); virtual;
    property GetPosX: single read FPosX;
    property GetPosY: single read FPosY;
    property GetColumn: integer read FColumn;
    property GetStyle: integer read FStyle;
    property GetEndX: single read FEndX;
    property GetEndY: single read FEndY;
  end;

  T_Column = class(T_Command)
  private
    FPos: single;
    FWidth: single;
    FMargin: single;
    FColor: TfpgColor;
  public
    constructor Create(APos, AWidth, AMargin: single; AColor: TfpgColor); virtual;
    function GetTextPos: single;
    function GetTextWidth: single;
    procedure SetColColor(AColor: TfpgColor);
    property ColPos: single read FPos write FPos;
    property ColWidth: single read FWidth write FWidth;
    property ColMargin: single read FMargin write FMargin;
    property GetColor: TfpgColor read FColor;
  end;

  T_Font = class(T_Command)
  private
    FFont: TfpgFont;
    FColor: TfpgColor;
    FSize: string;
  public
    constructor Create(AFont: string; AColor: TfpgColor); virtual;
    destructor Destroy; override;
    function GetHeight: integer;
    property GetFont: TfpgFont read FFont;
    property GetColor: TfpgColor read FColor;
    property GetSize: string read FSize;
  end;

  T_LineSpace = class(T_Command)
  private
    FSup: single;
    FInt: single;
    FInf: single;
  public
    constructor Create(ASup, AInt, AInf: single); virtual;
    property GetSup: single read FSup;
    property GetInt: single read FInt;
    property GetInf: single read FInf;
  end;

  T_Space = class(T_Command)
  private
    FPosY: single;
    FColumn: integer;
    FHeight: single;
    FBackColor: integer;
  public
    constructor Create(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer); virtual;
    procedure SetPosY(const AValue: single);
    property GetPosY: single read FPosY;
    property GetColumn: integer read FColumn;
    property GetHeight: single read FHeight;
    property GetBackColor: integer read FBackColor;
  end;

  T_BackColor = class(T_Command)
  private
    FColor: TfpgColor;
  public
    constructor Create(AColor: TfpgColor); virtual;
    property GetColor: TfpgColor read FColor;
  end;

  T_LineStyle = class(T_Command)
  private
    FThick: single;
    FColor: TfpgColor;
    FStyle: TfpgLineStyle;
  public
    constructor Create(AThick: single; AColor: TfpgColor; AStyle: TfpgLineStyle); virtual;
    property GetThick: single read FThick;
    property GetColor: TfpgColor read FColor;
    property GetStyle: TfpgLineStyle read FStyle;
  end;

  T_Border = class(T_Command)
  private
    FFlags: TBorderFlags;
    FStyle: integer;
  public
    constructor Create(AFlags: TBorderFlags; AStyle: integer);
    property GetFlags: TBorderFlags read FFlags;
    property GetStyle: integer read FStyle;
  end;

  T_Frame = class(T_Command)
  private
    FStyle: integer;
    FZone: TZone;
    FLeft: integer;
    FRight: integer;
    FTop: integer;
    FBottom: integer;
  public
    constructor Create(AStyle, ALeft, ARight, ATop, ABottom: integer; AZone: TZone);
    property GetStyle: integer read FStyle;
    property GetZone: TZone read FZone;
    property GetLeft: integer read FLeft;
    property GetRight: integer read FRight;
    property GetTop: integer read FTop;
    property GetBottom: integer read FBottom;
  end;

  T_Surface = class(T_Command)
  private
    FPoints: T_Points;
    FColor: TfpgColor;
  public
    constructor Create(APoints: array of TRefPos; AColor: TfpgColor);
    property GetPoints: T_Points read FPoints;
    property GetColor: TfpgColor read FColor;
  end;

  T_Image = class(T_Command)
  private
    FImage: integer;
    FColumn: integer;
    FPosX: single;
    FPosY: single;
  public
    constructor Create(APosX, APosY: single; AColumn, AImageNum: integer);
    property GetImage: integer read FImage;
    property GetColumn: integer read FColumn;
    property GetPosX: single read FPosX;
    property GetPosY: single read FPosY;
  end;

var
  Sections: TList;
  Columns: TList;
  Texts: TStringList;
  ImageNames: TStringList;
  Fonts: TList;
  LineSpaces: TList;
  BackColors: TList;
  LineStyles: TList;
  Borders: TList;
  Images: TList;
  VSection: T_Section;
  VPage: T_Page;
  VGroup: T_Group;
  VWriteLine: T_WriteLine;
  VCommand: T_Command;
  VColumn: T_Column;
  VBackColor: T_BackColor;
  VFont: T_Font;
  VLineSpace: T_LineSpace;
  VLineStyle: T_LineStyle;
  VBorder: T_Border;

implementation

 // utility functions

 // extracts the font size from the fontdesc
function ExtractFontSize(const AValue: string): string;
begin
  if Pos(':', AValue) > 0 then
    Result := Copy(AValue, Succ(Pos('-', AValue)), Pred(Pos(':', Avalue) - Pos('-', AValue)))
  else
    Result := Copy(AValue, Succ(Pos('-', AValue)), Length(AValue) - Pos('-', AValue));
end;

// document class methods
function T_Section.GetFirstPage: integer;
begin
  Result := T_Page(Pages[0]).PagesTot;
end;

function T_Section.GetTotalPages: integer;
begin
  if Pages.Count > 0 then
    Result := T_Page(Pages[Pred(Pages.Count)]).PagesTot
  else
    Result := 0;
end;

constructor T_Section.Create(APaper: TPaper; AMargins: TDimensions; ANum: integer);
begin
  FNumSect := ANum;
  FNbPages := 0;
  FPaper   := APaper;
  FMargins := AMargins;
  FBotHead := FMargins.T;
  FTopFoot := FMargins.B;
  FPages   := TList.Create;
  FHeader  := TList.Create;
  FFooter  := TList.Create;
  FFrames  := TList.Create;
end;

destructor T_Section.Destroy;
var
  Cpt: integer;
begin
  if FPages.Count > 0 then
    for Cpt := 0 to Pred(FPages.Count) do
      T_Page(FPages[Cpt]).Free;
  FPages.Free;
  if FHeader.Count > 0 then
    for Cpt := 0 to Pred(FHeader.Count) do
      T_Command(FHeader[Cpt]).Free;
  FHeader.Free;
  if FFooter.Count > 0 then
    for Cpt := 0 to Pred(FFooter.Count) do
      T_Command(FFooter[Cpt]).Free;
  FFooter.Free;
  if FFrames.Count > 0 then
    for Cpt := 0 to Pred(FFrames.Count) do
      T_Command(FFrames[Cpt]).Free;
  FFrames.Free;
  inherited Destroy;
end;

procedure T_Section.LoadPage(APageNum: integer);
begin
  Inc(FNbPages);
  VPage := T_Page.Create(FNbPages, APageNum);
  FPages.Add(VPage);
end;

procedure T_Section.LoadCmdHeader;
var
  Cpt: integer;
begin
  for Cpt := 0 to Pred(VWriteLine.Commands.Count) do
    FHeader.Add(VWriteLine.Commands.Items[Cpt]);
  VWriteLine.FHeight := 0;
  VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdPage;
var
  Cpt: integer;
begin
  for Cpt := 0 to Pred(VWriteLine.Commands.Count) do
    T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VWriteLine.Commands.Items[Cpt]);
  VWriteLine.FHeight := 0;
  VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdFooter;
var
  Cpt: integer;
begin
  for Cpt := 0 to Pred(VWriteLine.Commands.Count) do
    FFooter.Add(VWriteLine.Commands.Items[Cpt]);
  VWriteLine.FHeight := 0;
  VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdGroup;
var
  Cpt: integer;
begin
  for Cpt := 0 to Pred(VWriteLine.Commands.Count) do
    VGroup.Commands.Add(VWriteLine.Commands.Items[Cpt]);
  with VGroup do
  begin
    FLineHeight  := VWriteLine.FHeight;
    FGroupHeight := FGroupHeight + FLineHeight;
  end;
  VWriteLine.FHeight := 0;
  VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdGroupToPage;
var
  Cpt: integer;
begin
  for Cpt := 0 to Pred(VGroup.Commands.Count) do
    T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VGroup.Commands.Items[Cpt]);
  VGroup.FGroupHeight := 0;
  VGroup.Commands.Clear;
end;

procedure T_Section.LoadSpaceHeader(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer);
begin
  VCommand := T_Space.Create(APosY, AColumn, AHeight, ABackColor);
  FHeader.Add(VCommand);
end;

procedure T_Section.LoadSpacePage(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer);
begin
  VCommand := T_Space.Create(APosY, AColumn, AHeight, ABackColor);
  T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadSpaceFooter(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer);
begin
  VCommand := T_Space.Create(APosY, AColumn, AHeight, ABackColor);
  FFooter.Add(VCommand);
end;

procedure T_Section.LoadSpaceGroup(AHeight: single);
begin
  VGroup.FGroupHeight := VGroup.FGroupHeight + AHeight;
end;

procedure T_Section.LoadFrame(AStyle, ALeft, ARight, ATop, ABottom: integer; AZone: TZone);
begin
  VCommand := T_Frame.Create(AStyle, ALeft, ARight,ATop, ABottom, AZone);
  FFrames.Add(VCommand);
end;

procedure T_Section.LoadLine(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
begin
  VCommand := T_Line.Create(APosXBegin, APosYBegin, AColumn, AStyle, APosXEnd, APosYEnd);
  T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadLineHorizHeader(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
begin
  VCommand := T_Line.Create(APosXBegin, APosYBegin, AColumn, AStyle, APosXEnd, APosYEnd);
  FHeader.Add(VCommand);
end;

procedure T_Section.LoadLineHorizPage(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
begin
  VCommand := T_Line.Create(APosXBegin, APosYBegin, AColumn, AStyle, APosXEnd, APosYEnd);
  T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadLineHorizFooter(APosXBegin, APosYBegin: single; AColumn: integer; APosXEnd, APosYEnd: single; AStyle: integer);
begin
  VCommand := T_Line.Create(APosXBegin, APosYBegin, AColumn, AStyle, APosXEnd, APosYEnd);
  FFooter.Add(VCommand);
end;

procedure T_Section.LoadLineHorizGroupe(AHeight: single);
begin
  VGroup.FGroupHeight := VGroup.FGroupHeight + AHeight;
end;

procedure T_Section.LoadSurf(APos: T_Points; AColor: TfpgColor);
begin
  VCommand := T_Surface.Create(APos, AColor);
  T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadImgHeader(APosX, APosY: single; AColumn, AImgNum: integer);
begin
  VCommand := T_Image.Create(APosX, APosY, AColumn, AImgNum);
  FHeader.Add(VCommand);
end;

procedure T_Section.LoadImgPage(APosX, APosY: single; AColumn, AImgNum: integer);
begin
  VCommand := T_Image.Create(APosX, APosY, AColumn, AImgNum);
  T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadImgFooter(APosX, APosY: single; AColumn, AImgNum: integer);
begin
  VCommand := T_Image.Create(APosX, APosY, AColumn, AImgNum);
  FFooter.Add(VCommand);
end;

function T_Section.GetCmdPage(NumPage: integer): TList;
begin
  Result := T_Page(Pages[Pred(NumPage)]).Commands;
end;

constructor T_Page.Create(ANumSec, ANumTot: integer);
begin
  FNumPageTot  := ANumTot;
  FNumPageSect := ANumSec;
  FCommands    := TList.Create;
end;

destructor T_Page.Destroy;
var
  Cpt: integer;
begin
  if FCommands.Count > 0 then
    for Cpt := 0 to Pred(FCommands.Count) do
      T_Command(FCommands[Cpt]).Free;
  FCommands.Free;
  inherited Destroy;
end;

constructor T_Group.Create;
begin
  FLineHeight  := 0;
  FGroupHeight := 0;
  FCommands    := TList.Create;
end;

destructor T_Group.Destroy;
var
  Cpt: integer;
begin
  if FCommands.Count > 0 then
    for Cpt := 0 to Pred(FCommands.Count) do
      T_Command(FCommands[Cpt]).Free;
  FCommands.Free;
  inherited Destroy;
end;

constructor T_WriteLine.Create;
begin
  FHeight   := 0;
  FCommands := TList.Create;
end;

destructor T_WriteLine.Destroy;
var
  Cpt: integer;
begin
  if FCommands.Count > 0 then
    for Cpt := 0 to Pred(FCommands.Count) do
      T_Command(FCommands[Cpt]).Free;
  FCommands.Free;
  inherited Destroy;
end;

procedure T_WriteLine.LoadText(APosX, APosY: single; AColumn, AText, AFont, AHeight, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags);
begin
  if FHeight < AHeight then
    FHeight := AHeight;
  VCommand := T_WriteText.Create(APosX, APosY, AColumn, AText, AFont, ABackColor, ABorder, ALineSpace, ACurFont, AFlags);
  Commands.Add(VCommand);
end;

procedure T_WriteLine.LoadNumber(APosX, APosY: single; AColumn, ATextNum, ATextTot, AFont, AHeight, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal, AAlpha: Boolean; ATypeNum: TSectPageNum);
begin
  if FHeight < AHeight then
    FHeight := AHeight;
  VCommand := T_Number.Create(APosX, APosY, AColumn, ATextNum, ATextTot, AFont, ABackColor, ABorder, ALineSpace, ACurFont, AFlags, ATotal, AAlpha, ATypeNum);
  Commands.Add(VCommand);
end;

// command class methods

procedure T_WriteText.SetPosY(const AValue: single);
begin
  if FPosY <> AValue then
    FPosY := AValue;
end;

constructor T_WriteText.Create(APosX, APosY: single; AColumn, AText, AFont, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags);
begin
  inherited Create;
  FPosX      := APosX;
  FPosY      := APosY;
  FColumn    := AColumn;
  FText      := AText;
  FFont      := AFont;
  FBackColor := ABackColor;
  FBorder    := ABorder;
  FLineSpace := ALineSpace;
  FCurFont   := ACurFont;
  FFlags     := AFlags;
end;

procedure T_Number.SetPosY(const AValue: single);
begin
  if FPosY <> AValue then
    FPosY := AValue;
end;

constructor T_Number.Create(APosX, APosY: single; AColumn, ATextNum, ATextTot, AFont, ABackColor, ABorder, ALineSpace: integer; ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal, AAlpha: Boolean; ATypeNum: TSectPageNum);
begin
  inherited Create;
  FPosX      := APosX;
  FPosY      := APosY;
  FColumn    := AColumn;
  FTextNum   := ATextNum;
  FTextTot   := ATextTot;
  FFont      := AFont;
  FBackColor := ABackColor;
  FBorder    := ABorder;
  FLineSpace := ALineSpace;
  FCurFont   := ACurFont;
  FFlags     := AFlags;
  FTotal     := ATotal;
  FAlpha     := AAlpha;
  FTypeNum   := ATypeNum;
end;

constructor T_Line.Create(APosX, APosY: single; AColumn, AStyle: integer; AEndX, AEndY: single);
begin
  FPosX   := APosX;
  FPosY   := APosY;
  FColumn := AColumn;
  FStyle  := AStyle;
  FEndX   := AEndX;
  FEndY   := AEndY;
end;

constructor T_Column.Create(APos, AWidth, AMargin: single; AColor: TfpgColor);
begin
  inherited Create;
  FPos    := APos;
  FWidth  := AWidth;
  FMargin := AMargin;
  FColor  := AColor;
end;

function T_Column.GetTextPos: single;
begin
  Result := FPos + FMargin;
end;

function T_Column.GetTextWidth: single;
begin
  Result := FWidth - (FMargin * 2);
end;

procedure T_Column.SetColColor(AColor: TfpgColor);
begin
  if FColor <> AColor then
    FColor := AColor;
end;

constructor T_Font.Create(AFont: string; AColor: TfpgColor);
begin
  inherited Create;
  FFont  := fpgApplication.GetFont(AFont);
  FColor := AColor;
  FSize  := ExtractFontSize(AFont);
end;

destructor T_Font.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

function T_Font.GetHeight: integer;
begin
  Result := TfpgFont(FFont).Height;
end;

constructor T_LineSpace.Create(ASup, AInt, AInf: single);
begin
  inherited Create;
  FSup := ASup;
  FInt := AInt;
  FInf := AInf;
end;

constructor T_Space.Create(APosY: single; AColumn: integer; AHeight: single; ABackColor: integer);
begin
  inherited Create;
  FPosY      := APosY;
  FColumn    := AColumn;
  FHeight    := AHeight;
  FBackColor := ABackColor;
end;

constructor T_Surface.Create(APoints: array of TRefPos; AColor: TfpgColor);
var
  Cpt: integer;
begin
  inherited Create;
  SetLength(FPoints, Length(APoints));
  for Cpt := 0 to Pred(Length(FPoints)) do
    FPoints[Cpt] := APoints[Cpt];
  FColor := AColor;
end;

procedure T_Space.SetPosY(const AValue: single);
begin
  if FPosY <> AValue then
    FPosY := AValue;
end;

constructor T_BackColor.Create(AColor: TfpgColor);
begin
  FColor := AColor;
end;

constructor T_LineStyle.Create(AThick: single; AColor: TfpgColor; AStyle: TfpgLineStyle);
begin
  inherited Create;
  FThick := AThick;
  FColor := AColor;
  FStyle := AStyle;
end;

constructor T_Border.Create(AFlags: TBorderFlags; AStyle: integer);
begin
  inherited Create;
  FFlags := AFlags;
  FStyle := AStyle;
end;

constructor T_Frame.Create(AStyle, ALeft, ARight, ATop, ABottom: integer; AZone: TZone);
begin
  inherited Create;
  FStyle := AStyle;
  FZone:= AZone;
  FLeft:= ALeft;
  FRight:= ARight;
  FTop:= ATop;
  FBottom:= ABottom;;
end;

constructor T_Image.Create(APosX, APosY: single; AColumn, AImageNum: integer);
begin
  inherited Create;
  FImage  := AImageNum;
  FColumn := AColumn;
  FPosX   := APosX;
  FPosY   := APosY;
end;

end.

