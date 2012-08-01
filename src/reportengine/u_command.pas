{
    << Impressions >>  U_Commande.pas

    Copyright (C) 2010 - Jean-Marc Levecque <jean-marc.levecque@jmlesite.fr>

   This library is a free software coming as a add-on to fpGUI toolkit
   See the copyright included in the fpGUI distribution for details about redistribution

   This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit builds the objects in memory to produce either the preview or pdf file
}

unit U_Command;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, //fpg_imgfmt_bmp, fpg_imgfmt_jpg,
  U_Pdf;

type
  TZone = (zHeader,zFooter,zPage,zMargins);
  TSectPageNum = (PageNum,SectNum,PSectNum);
  TBorderFlags= set of (bfLeft,bfRight,bfTop,bfBottom);

  TDimensions= record
    T: Single;
    L: Single;
    R: Single;
    B: Single;
    end;

  TPaper= record
    H: Integer;
    W: Integer;
    Printable: TDimensions;
    end;

  // document classes

  T_Section = class
    private
      FNumSect: Integer;
      FNbPages: Integer;
      FPaper: TPaper;
      FMargins: TDimensions;
      FBotHead: Single;
      FTopFoot: Single;
      FPages: TList;
      FHeader: TList;
      FFooter: TList;
      FFrames: TList;
      FDefCol: Integer;
      FTitle: string;
      function GetFirstPage: Integer;
      function GetTotalPages: Integer;
    public
      constructor Create(APaper: TPaper; AMargins: TDimensions; ANum: Integer); virtual;
      destructor Destroy; override;
      procedure LoadPage(APageNum: Integer);
      procedure LoadCmdHeader;
      procedure LoadCmdPage;
      procedure LoadCmdFooter;
      procedure LoadCmdGroup;
      procedure LoadCmdGroupToPage;
      procedure LoadSpaceHeader(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer);
      procedure LoadSpacePage(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer);
      procedure LoadSpaceFooter(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer);
      procedure LoadSpaceGroup(AHeight: Single);
      procedure LoadFrame(AStyle: Integer; AZone: TZone);
      procedure LoadLine(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single; AStyle: Integer);
      procedure LoadLineHorizHeader(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single; AStyle: Integer);
      procedure LoadLineHorizPage(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single; AStyle: Integer);
      procedure LoadLineHorizFooter(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single; AStyle: Integer);
      procedure LoadLineHorizGroupe(AHeight: Single);
      procedure LoadSurf(APos: T_Points; AColor: TfpgColor);
      procedure LoadImgHeader(APosX,APosY: Single; AColumn,AImgNum: Integer);
      procedure LoadImgPage(APosX,APosY: Single; AColumn,AImgNum: Integer);
      procedure LoadImgFooter(APosX,APosY: Single; AColumn,AImgNum: Integer);
      function GetCmdPage(NumPage: Integer): TList;
      property CmdHeader: TList read FHeader;
      property CmdFooter: TList read FFooter;
      property NbPages: Integer read FNbPages;
      property FirstPage: Integer read GetFirstPage;
      property Pages: TList read FPages;
      property TotPages: Integer read GetTotalPages;
      property Paper: TPaper read FPaper;
      property Margins: TDimensions read FMargins;
      property CmdFrames: TList read FFrames;
      property DefaultCol: Integer read FDefCol write FDefCol;
      property Title: string read FTitle write FTitle;
    end;

  T_Page = class
    private
      FNumPageTot: Integer;
      FNumPageSect: Integer;
      FCommands: TList;
    public
      constructor Create(ANumSec,ANumTot: Integer); virtual;
      destructor Destroy; override;
      property Commands: TList read FCommands write FCommands;
      property PagesTot: Integer read FNumPageTot;
      property PagesSect: Integer read FNumPageSect;
    end;

  T_Group = class
    private
      FLineHeight: Single;
      FGroupHeight: Single;
      FCommands: TList;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      property Commands: TList read FCommands write FCommands;
      property LineHeight: Single read FLineHeight;
      property GroupeHeight: Single read FGroupHeight;
    end;

  T_WriteLine = class
    private
      FHeight: Integer;
      FCommands: TList;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure LoadText(APosX,APosY: Single; AColumn,AText,AFont,AHeight,ABackColor,ABorder,ALineSpace: Integer;
                ACurFont: Boolean; AFlags: TfpgTextFlags);
      procedure LoadNumber(APosX,APosY: Single; AColumn,ATextNum,ATextTot,AFont,AHeight,ABackColor,ABorder,ALineSpace: Integer;
                ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
      property Commands: TList read FCommands;
      property LineHeight: Integer read FHeight;
    end;

  // command classes

  T_Command = class
    end;

  PSection = ^T_Section;
  PPage = ^T_Page;
  PLigne = ^T_WriteLine;
  PCommande = ^T_Command;
  PFont = ^TfpgFont;

  T_WriteText = class(T_Command)
    private
      FPosX: Single;
      FPosY: Single;
      FColumn: Integer;
      FText: Integer;
      FFont: Integer;
      FBackColor: Integer;
      FBorder: Integer;
      FLineSpace: Integer;
      FCurFont: Boolean;
      FFlags: TfpgTextFlags;
    public
      constructor Create(APosX,APosY: Single; AColumn,AText,AFont,ABackColor,ABorder,ALineSpace: Integer; ACurFont: Boolean; AFlags: TfpgTextFlags); virtual;
      procedure SetPosY(const AValue: Single);
      property GetPosX: Single read FPosX;
      property GetPosY: Single read FPosY;
      property GetColumn: Integer read FColumn;
      property GetText: Integer read FText;
      property GetFont: Integer read FFont;
      property GetBackColor: Integer read FBackColor;
      property GetBorder: Integer read FBorder;
      property GetLineSpace: Integer read FLineSpace;
      property GetCurFont: Boolean read FCurFont;
      property GetFlags: TfpgTextFlags read FFlags;
    end;

  T_Number = class(T_Command)
    private
      FPosX: Single;
      FPosY: Single;
      FColumn: Integer;
      FTextNum: Integer;
      FTextTot: Integer;
      FFont: Integer;
      FBackColor: Integer;
      FBorder: Integer;
      FLineSpace: Integer;
      FCurFont: Boolean;
      FFlags: TfpgTextFlags;
      FTotal: Boolean;
      FAlpha: Boolean;
      FTypeNum: TSectPageNum;
    public
      constructor Create(APosX,APosY: Single; AColumn,ATextNum,ATextTot,AFont,ABackColor,ABorder,ALineSpace: Integer;
                  ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum); virtual;
      procedure SetPosY(const AValue: Single);
      property GetPosX: Single read FPosX;
      property GetPosY: Single read FPosY;
      property GetColumn: Integer read FColumn;
      property GetTextNum: Integer read FTextNum;
      property GetTextTot: Integer read FTextTot;
      property GetFont: Integer read FFont;
      property GetBackColor: Integer read FBackColor;
      property GetBorder: Integer read FBorder;
      property GetLineSpace: Integer read FLineSpace;
      property GetCurFont: Boolean read FCurFont;
      property GetFlags: TfpgTextFlags read FFlags;
      property GetTotal: Boolean read FTotal;
      property GetAlpha: Boolean read FAlpha;
      property GetTypeNum: TSectPageNum read FTypeNum;
    end;

  T_Line = class(T_Command)
    private
      FPosX: Single;
      FPosY: Single;
      FColumn: Integer;
      FStyle: Integer;
      FEndX: Single;
      FEndY: Single;
    public
      constructor Create(APosX,APosY: Single; AColumn,AStyle: Integer; AEndX,AEndY: Single); virtual;
      property GetPosX: Single read FPosX;
      property GetPosY: Single read FPosY;
      property GetColumn: Integer read FColumn;
      property GetStyle: Integer read FStyle;
      property GetEndX: Single read FEndX;
      property GetEndY: Single read FEndY;
    end;

  T_Column = class(T_Command)
    private
      FPos: Single;
      FWidth: Single;
      FMargin: Single;
      FColor: TfpgColor;
    public
      constructor Create(APos,AWidth,AMargin: Single; AColor: TfpgColor); virtual;
      function GetTextPos: Single;
      function GetTextWidth: Single;
      procedure SetColColor(AColor: TfpgColor);
      property ColPos: Single read FPos write FPos;
      property ColWidth: Single read FWidth write FWidth;
      property ColMargin: Single read FMargin write FMargin;
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
      function GetHeight: Integer;
      property GetFont: TfpgFont read FFont;
      property GetColor: TfpgColor read FColor;
      property GetSize: string read FSize;
    end;

  T_LineSpace = class(T_Command)
    private
      FSup: Single;
      FInt: Single;
      FInf: Single;
    public
      constructor Create(ASup,AInt,AInf: Single); virtual;
      property GetSup: Single read FSup;
      property GetInt: Single read FInt;
      property GetInf: Single read FInf;
    end;

  T_Space = class(T_Command)
    private
      FPosY: Single;
      FColumn: Integer;
      FHeight: Single;
      FBackColor: Integer;
    public
      constructor Create(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer); virtual;
      procedure SetPosY(const AValue: Single);
      property GetPosY: Single read FPosY;
      property GetColumn: Integer read FColumn;
      property GetHeight: Single read FHeight;
      property GetBackColor: Integer read FBackColor;
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
      FThick: Single;
      FColor: TfpgColor;
      FStyle: TfpgLineStyle;
    public
      constructor Create(AThick: Single; AColor: Tfpgcolor; AStyle: TfpgLineStyle); virtual;
      property GetThick: Single read FThick;
      property GetColor: TfpgColor read FColor;
      property GetStyle: TfpgLineStyle read FStyle;
    end;

  T_Border = class(T_Command)
    private
      FFlags: TBorderFlags;
      FStyle: Integer;
    public
      constructor Create(AFlags: TBorderFlags; AStyle: Integer);
      property GetFlags: TBorderFlags read FFlags;
      property GetStyle: Integer read FStyle;
    end;

  T_Frame = class(T_Command)
    private
      FStyle: Integer;
      FZone: TZone;
    public
      constructor Create(AStyle: Integer; AZone: TZone);
      property GetStyle: Integer read FStyle;
      property GetZone: TZone read FZone;
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
      FImage: Integer;
      FColumn: Integer;
      FPosX: Single;
      FPosY: Single;
    public
      constructor Create(APosX,APosY: Single; AColumn,AImageNum: Integer);
      property GetImage: Integer read FImage;
      property GetColumn: Integer read FColumn;
      property GetPosX: Single read FPosX;
      property GetPosY: Single read FPosY;
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
if Pos(':',AValue)> 0
then
  Result:= Copy(AValue,Succ(Pos('-',AValue)),Pred(Pos(':',Avalue)-Pos('-',AValue)))
else
  Result:= Copy(AValue,Succ(Pos('-',AValue)),Length(AValue)-Pos('-',AValue));
end;

// document class methods

function T_Section.GetFirstPage: Integer;
begin
Result:= T_Page(Pages[0]).PagesTot;
end;

function T_Section.GetTotalPages: Integer;
begin
if Pages.Count> 0
then
  Result:= T_Page(Pages[Pred(Pages.Count)]).PagesTot
else
  Result:= 0;
end;

constructor T_Section.Create(APaper: TPaper; AMargins: TDimensions; ANum: Integer);
begin
FNumSect:= ANum;
FNbPages:= 0;
FPaper:= APaper;
FMargins:= AMargins;
FBotHead:= FMargins.T;
FTopFoot:= FMargins.B;
FPages:= TList.Create;
FHeader:= TList.Create;
FFooter:= TList.Create;
FFrames:= TList.Create;
end;

destructor T_Section.Destroy;
var
  Cpt: Integer;
begin
if FPages.Count> 0
then
  for Cpt:= 0 to Pred(FPages.Count) do
    T_Page(FPages[Cpt]).Free;
FPages.Free;
if FHeader.Count> 0
then
  for Cpt:= 0 to Pred(FHeader.Count) do
    T_Command(FHeader[Cpt]).Free;
FHeader.Free;
if FFooter.Count> 0
then
  for Cpt:= 0 to Pred(FFooter.Count) do
    T_Command(FFooter[Cpt]).Free;
FFooter.Free;
if FFrames.Count> 0
then
  for Cpt:= 0 to Pred(FFrames.Count) do
    T_Command(FFrames[Cpt]).Free;
FFrames.Free;
inherited Destroy;
end;

procedure T_Section.LoadPage(APageNum: Integer);
begin
Inc(FNbPages);
VPage:= T_Page.Create(FNbPages,APageNum);
FPages.Add(VPage);
end;

procedure T_Section.LoadCmdHeader;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(VWriteLine.Commands.Count) do
  FHeader.Add(VWriteLine.Commands.Items[Cpt]);
VWriteLine.FHeight:= 0;
VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdPage;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(VWriteLine.Commands.Count) do
  T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VWriteLine.Commands.Items[Cpt]);
VWriteLine.FHeight:= 0;
VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdFooter;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(VWriteLine.Commands.Count) do
  FFooter.Add(VWriteLine.Commands.Items[Cpt]);
VWriteLine.FHeight:= 0;
VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdGroup;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(VWriteLine.Commands.Count) do
  VGroup.Commands.Add(VWriteLine.Commands.Items[Cpt]);
with VGroup do
  begin
  FLineHeight:= VWriteLine.FHeight;
  FGroupHeight:= FGroupHeight+FLineHeight;
  end;
VWriteLine.FHeight:= 0;
VWriteLine.Commands.Clear;
end;

procedure T_Section.LoadCmdGroupToPage;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(VGroup.Commands.Count) do
  T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VGroup.Commands.Items[Cpt]);
VGroup.FGroupHeight:= 0;
VGroup.Commands.Clear;
end;

procedure T_Section.LoadSpaceHeader(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer);
begin
VCommand:= T_Space.Create(APosY,AColumn,AHeight,ABackColor);
FHeader.Add(VCommand);
end;

procedure T_Section.LoadSpacePage(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer);
begin
VCommand:= T_Space.Create(APosY,AColumn,AHeight,ABackColor);
T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadSpaceFooter(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer);
begin
VCommand:= T_Space.Create(APosY,AColumn,AHeight,ABackColor);
FFooter.Add(VCommand);
end;

procedure T_Section.LoadSpaceGroup(AHeight: Single);
begin
VGroup.FGroupHeight:= VGroup.FGroupHeight+AHeight;
end;

procedure T_Section.LoadFrame(AStyle: Integer; AZone: TZone);
begin
VCommand:= T_Frame.Create(AStyle,AZone);
FFrames.Add(VCommand);
end;

procedure T_Section.LoadLine(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single; AStyle: Integer);
begin
VCommand:= T_Line.Create(APosXBegin,APosYBegin,AColumn,AStyle,APosXEnd,APosYEnd);
T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadLineHorizHeader(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single;
          AStyle: Integer);
begin
VCommand:= T_Line.Create(APosXBegin,APosYBegin,AColumn,AStyle,APosXEnd,APosYEnd);
FHeader.Add(VCommand);
end;

procedure T_Section.LoadLineHorizPage(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single; AStyle: Integer);
begin
VCommand:= T_Line.Create(APosXBegin,APosYBegin,AColumn,AStyle,APosXEnd,APosYEnd);
T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadLineHorizFooter(APosXBegin,APosYBegin: Single; AColumn: Integer; APosXEnd,APosYEnd: Single; AStyle: Integer);
begin
VCommand:= T_Line.Create(APosXBegin,APosYBegin,AColumn,AStyle,APosXEnd,APosYEnd);
FFooter.Add(VCommand);
end;

procedure T_Section.LoadLineHorizGroupe(AHeight: Single);
begin
VGroup.FGroupHeight:= VGroup.FGroupHeight+AHeight;
end;

procedure T_Section.LoadSurf(APos: T_Points; AColor: TfpgColor);
begin
VCommand:= T_Surface.Create(APos,AColor);
T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadImgHeader(APosX,APosY: Single; AColumn,AImgNum: Integer);
begin
VCommand:= T_Image.Create(APosX,APosY,AColumn,AImgNum);
FHeader.Add(VCommand);
end;

procedure T_Section.LoadImgPage(APosX,APosY: Single; AColumn,AImgNum: Integer);
begin
VCommand:= T_Image.Create(APosX,APosY,AColumn,AImgNum);
T_Page(Pages[Pred(FPages.Count)]).Commands.Add(VCommand);
end;

procedure T_Section.LoadImgFooter(APosX,APosY: Single; AColumn,AImgNum: Integer);
begin
VCommand:= T_Image.Create(APosX,APosY,AColumn,AImgNum);
FFooter.Add(VCommand);
end;

function T_Section.GetCmdPage(NumPage: Integer): TList;
begin
Result:= T_Page(Pages[Pred(NumPage)]).Commands;
end;

constructor T_Page.Create(ANumSec,ANumTot: Integer);
begin
FNumPageTot:= ANumTot;
FNumPageSect:= ANumSec;
FCommands:= TList.Create;
end;

destructor T_Page.Destroy;
var
  Cpt: Integer;
begin
if FCommands.Count> 0
then
  for Cpt:= 0 to Pred(FCommands.Count) do
    T_Command(FCommands[Cpt]).Free;
FCommands.Free;
inherited Destroy;
end;

constructor T_Group.Create;
begin
FLineHeight:= 0;
FGroupHeight:= 0;
FCommands:= TList.Create;
end;

destructor T_Group.Destroy;
var
  Cpt: Integer;
begin
if FCommands.Count> 0
then
  for Cpt:= 0 to Pred(FCommands.Count) do
    T_Command(FCommands[Cpt]).Free;
FCommands.Free;
inherited Destroy;
end;

constructor T_WriteLine.Create;
begin
FHeight:= 0;
FCommands:= TList.Create;
end;

destructor T_WriteLine.Destroy;
var
  Cpt: Integer;
begin
if FCommands.Count> 0
then
  for Cpt:= 0 to Pred(FCommands.Count) do
    T_Command(FCommands[Cpt]).Free;
FCommands.Free;
inherited Destroy;
end;

procedure T_WriteLine.LoadText(APosX,APosY: Single; AColumn,AText,AFont,AHeight,ABackColor,ABorder,ALineSpace: Integer;
          ACurFont: Boolean; AFlags: TfpgTextFlags);
begin
if FHeight< AHeight
then
  FHeight:= AHeight;
VCommand:= T_WriteText.Create(APosX,APosY,AColumn,AText,AFont,ABackColor,ABorder,ALineSpace,ACurFont,AFlags);
Commands.Add(VCommand);
end;

procedure T_WriteLine.LoadNumber(APosX,APosY: Single; AColumn,ATextNum,ATextTot,AFont,AHeight,ABackColor,ABorder,ALineSpace: Integer;
          ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
begin
if FHeight< AHeight
then
  FHeight:= AHeight;
VCommand:= T_Number.Create(APosX,APosY,AColumn,ATextNum,ATextTot,AFont,ABackColor,ABorder,ALineSpace,ACurFont,AFlags,ATotal,AAlpha,ATypeNum);
Commands.Add(VCommand);
end;

// command class methods

procedure T_WriteText.SetPosY(const AValue: Single);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_WriteText.Create(APosX,APosY: Single; AColumn,AText,AFont,ABackColor,ABorder,ALineSpace: Integer; ACurFont: Boolean; AFlags: TfpgTextFlags);
begin
inherited Create;
FPosX:= APosX;
FPosY:= APosY;
FColumn:= AColumn;
FText:= AText;
FFont:= AFont;
FBackColor:= ABackColor;
FBorder:= ABorder;
FLineSpace:= ALineSpace;
FCurFont:= ACurFont;
FFlags:= AFlags;
end;

procedure T_Number.SetPosY(const AValue: Single);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_Number.Create(APosX,APosY: Single; AColumn,ATextNum,ATextTot,AFont,ABackColor,ABorder,ALineSpace: Integer;
            ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
begin
inherited Create;
FPosX:= APosX;
FPosY:= APosY;
FColumn:= AColumn;
FTextNum:= ATextNum;
FTextTot:= ATextTot;
FFont:= AFont;
FBackColor:= ABackColor;
FBorder:= ABorder;
FLineSpace:= ALineSpace;
FCurFont:= ACurFont;
FFlags:= AFlags;
FTotal:= ATotal;
FAlpha:= AAlpha;
FTypeNum:= ATypeNum;
end;

constructor T_Line.Create(APosX,APosY: Single; AColumn,AStyle: Integer; AEndX,AEndY: Single);
begin
FPosX:= APosX;
FPosY:= APosY;
FColumn:= AColumn;
FStyle:= AStyle;
FEndX:= AEndX;
FEndY:= AEndY;
end;

constructor T_Column.Create(APos,AWidth,AMargin: Single; AColor: TfpgColor);
begin
inherited Create;
FPos:= APos;
FWidth:= AWidth;
FMargin:= AMargin;
FColor:= AColor;
end;

function T_Column.GetTextPos: Single;
begin
Result:= FPos+FMargin;
end;

function T_Column.GetTextWidth: Single;
begin
Result:= FWidth-(FMargin*2);
end;

procedure T_Column.SetColColor(AColor: TfpgColor);
begin
if FColor<> AColor
then
  FColor:= AColor;
end;

constructor T_Font.Create(AFont: string; AColor: TfpgColor);
begin
inherited Create;
FFont:= fpgApplication.GetFont(AFont);
FColor:= AColor;
FSize:= ExtractFontSize(AFont);
end;

destructor T_Font.Destroy;
begin
FFont.Free;
inherited Destroy;
end;

function T_Font.GetHeight: Integer;
begin
Result:= TfpgFont(FFont).Height;
end;

constructor T_LineSpace.Create(ASup,AInt,AInf: Single);
begin
inherited Create;
FSup:= ASup;
FInt:= AInt;
FInf:= AInf;
end;

constructor T_Space.Create(APosY: Single; AColumn: Integer; AHeight: Single; ABackColor: Integer);
begin
inherited Create;
FPosY:= APosY;
FColumn:= AColumn;
FHeight:= AHeight;
FBackColor:= ABackColor;
end;

constructor T_Surface.Create(APoints: array of TRefPos; AColor: TfpgColor);
var
  Cpt: Integer;
begin
inherited Create;
SetLength(FPoints,Length(APoints));
for Cpt:= 0 to Pred(Length(FPoints)) do
  FPoints[Cpt]:= APoints[Cpt];
FColor:= AColor;
end;

procedure T_Space.SetPosY(const AValue: Single);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_BackColor.Create(AColor: TfpgColor);
begin
FColor:= AColor;
end;

constructor T_LineStyle.Create(AThick: Single; AColor: Tfpgcolor; AStyle: TfpgLineStyle);
begin
inherited Create;
FThick:= AThick;
FColor:= AColor;
FStyle:= AStyle;
end;

constructor T_Border.Create(AFlags: TBorderFlags; AStyle: Integer);
begin
inherited Create;
FFlags:= AFlags;
FStyle:= AStyle;
end;

constructor T_Frame.Create(AStyle: Integer; AZone: TZone);
begin
inherited Create;
FStyle:= AStyle;
FZone:= AZone;
end;

constructor T_Image.Create(APosX,APosY: Single; AColumn,AImageNum: Integer);
begin
inherited Create;
FImage:= AImageNum;
FColumn:= AColumn;
FPosX:= APosX;
FPosY:= APosY;
end;

end.

