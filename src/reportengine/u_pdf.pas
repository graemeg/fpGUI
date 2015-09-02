{
  fpGUI  -  Free Pascal GUI Toolkit

  Copyright (C) 2006 - 2015 See the file AUTHORS.txt, included in this
  distribution, for details of the copyright.

  See the file COPYING.modifiedLGPL, included in this distribution,
  for details about redistributing fpGUI.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  Description:
    This unit forms part of the PDF Reporting Engine. This unit
    produces the PDF file.

    The PDF Reporting Engine was originally written by
    Jean-Marc Levecque <jmarc.levecque@jmlesite.web4me.fr>
}

unit U_Pdf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  fpg_base;

type
  TPdfObject = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TPdfBoolean = class(TPdfObject)
  private
    FValue: Boolean;
  protected
    procedure WriteBoolean(const AStream: TStream);
  public
    constructor CreateBoolean(const AValue: Boolean);
  end;

  TPdfInteger = class(TPdfObject)
  private
    FValue: integer;
  protected
    procedure WriteInteger(const AStream: TStream);
    procedure IncrementeInteger;
    property Value: integer read FValue write FValue;
  public
    constructor CreateInteger(const AValue: integer);
  end;

  TPdfReference = class(TPdfObject)
  private
    FValue: integer;
  protected
    procedure WriteReference(const AStream: TStream);
  public
    constructor CreateReference(const AValue: integer);
  end;

  TPdfName = class(TPdfObject)
  private
    FValue: string;
  protected
    procedure WriteName(const AStream: TStream);
  public
    constructor CreateName(const AValue: string);
  end;

  TPdfString = class(TPdfObject)
  private
    FValue: string;
  protected
    procedure Write(const AStream: TStream);
  public
    constructor CreateString(const AValue: string);
  end;

  TPdfArray = class(TPdfObject)
  private
    FArray: TList;
  protected
    procedure WriteArray(const AStream: TStream);
    procedure AddItem(const AValue: TPdfObject);
  public
    constructor CreateArray;
    destructor Destroy; override;
  end;

  TPdfStream = class(TPdfObject)
  private
    FStream: TList;
  protected
    procedure WriteStream(const AStream: TStream);
    procedure AddItem(const AValue: TPdfObject);
  public
    constructor CreateStream;
    destructor Destroy; override;
  end;

  TPdfEmbeddedFont = class(TPdfObject)
  private
    FTxtFont: integer;
    FTxtSize: string;
  protected
    procedure WriteFont(const AStream: TStream);
    function WriteEmbeddedFont(const ASrcStream: TMemoryStream; const AStream: TStream): int64;
  public
    constructor CreateFont(const AFont: integer; const ASize: string);
  end;

  TPdfText = class(TPdfObject)
  private
    FTxtPosX: single;
    FTxtPosY: single;
    FTxtText: TPdfString;
  protected
    procedure WriteText(const AStream: TStream);
  public
    constructor CreateText(const APosX, APosY: single; const AText: string);
    destructor Destroy; override;
  end;

  TPdfLineSegment = class(TPdfObject)
  private
    FWidth: single;
    FX1: single;
    FY1: single;
    FX2: single;
    FY2: single;
  protected
    procedure WriteLineSegment(const AStream: TStream);
  public
    constructor CreateLineSegment(const AWidth, AX1, AY1, AX2, AY2: single);
  end;

  TPdfRectangle = class(TPdfObject)
  private
    FLineWidth: single;
    FRecX: single;
    FRecY: single;
    FRecW: single;
    FRecH: single;
    FFill: Boolean;
    FStroke: Boolean;
  protected
    procedure WriteRectangle(const AStream: TStream);
  public
    constructor CreateRectangle(const ALineWidth, APosX, APosY, AWidth, AHeight: single; const AFill, AStroke: Boolean);
  end;

  TRefPos = record
    X: single;
    Y: single;
  end;

  T_Points = array of TRefPos;

  TPdfSurface = class(TPdfObject)
  private
    FPoints: T_Points;
  protected
    procedure WriteSurface(const AStream: TStream);
  public
    constructor CreateSurface(const APoints: T_Points);
  end;

  TPdfImage = class(TPdfObject)
  private
    FNumber: integer;
    FLeft: single;
    FBottom: single;
    FWidth: integer;
    FHeight: integer;
  protected
    function WriteImageStream(const ANumber: integer; AStream: TStream): int64;
    procedure WriteImage(const AStream: TStream);
  public
    constructor CreateImage(const ALeft, ABottom: single; AWidth, AHeight, ANumber: integer);
  end;

  TPdfLineStyle = class(TPdfObject)
  private
    FDash: TfpgLineStyle;
    FPhase: integer;
  protected
    procedure WriteLineStyle(const AStream: TStream);
  public
    constructor CreateLineStyle(ADash: TfpgLineStyle; APhase: integer);
  end;

  TPdfColor = class(TPdfObject)
  private
    FRed: string;
    FGreen: string;
    FBlue: string;
    FStroke: Boolean;
  protected
    procedure WriteColor(const AStream: TStream);
  public
    constructor CreateColor(const AStroke: Boolean; AColor: TfpgColor);
  end;

  TPdfDicElement = class(TObject)
  private
    FKey: TPdfName;
    FValue: TPdfObject;
  protected
    procedure WriteDicElement(const AStream: TStream);
  public
    constructor CreateDicElement(const AKey: string; const AValue: TPdfObject);
    destructor Destroy; override;
  end;

  TPdfDictionary = class(TPdfObject)
  private
    FElement: TList; // list of TPdfDicElement
  protected
    procedure AddElement(const AKey: string; const AValue: TPdfObject);
    function ElementParCle(const AValue: string): integer;
    procedure WriteDictionary(const AObjet: integer; const AStream: TStream);
  public
    constructor CreateDictionary;
    destructor Destroy; override;
  end;

  TPdfXRef = class(TObject)
  private
    FOffset: integer;
    FDict: TPdfDictionary;
    FStream: TPdfStream;
  protected
    procedure WriteXRef(const AStream: TStream);
  public
    constructor CreateXRef;
    destructor Destroy; override;
    property Offset: integer read FOffset write FOffset;
    Property Dict: TPdfDictionary read FDict;
  end;

  TPageLayout = (lSingle, lTwo, lContinuous);

  TPdfDocument = class(TObject)
  private
    FPreferences: Boolean;
    FPageLayout: TPageLayout;
    FZoomValue: string;
    FGlobalXRefs: TList; // list of TPdfXRef
  protected
    function ElementParNom(const AValue: string): integer;
    procedure WriteXRefTable(const AStream: TStream);
    procedure WriteObject(const AObject: integer; const AStream: TStream);
    procedure CreateRefTable;
    procedure CreateTrailer;
    function CreateCatalog: integer;
    procedure CreateInfo;
    procedure CreatePreferences;
    function CreatePages(Parent: integer): integer;
    function CreatePage(Parent, Haut, Larg, PageNum: integer): integer;
    function CreateOutlines: integer;
    function CreateOutline(Parent, SectNo, PageNo: integer; SectTitre: string): integer;
    procedure CreateStdFont(NomFonte: string; NumFonte: integer);
    function LoadFont(AFontName: string): string;
    procedure CreateTtfFont(const NumFonte: integer);
    procedure CreateTp1Font(const NumFonte: integer);
    procedure CreateFontDescriptor(const NumFonte: integer);
    procedure CreateFontWidth;
    procedure CreateFontFile(const NumFonte: integer);
    procedure CreateImage(ImgWidth, ImgHeight, ImgNumber: integer);
    function CreateContents: integer;
    procedure CreateStream(NumeroPage, PageNum: integer);
  public
    constructor CreateDocument(const ALayout: TPageLayout = lSingle; const AZoom: string = '100'; const APreferences: Boolean = True);
    destructor Destroy; override;
    procedure WriteDocument(const AStream: TStream);
    property PageLayout: TPageLayout read FPageLayout write FPageLayout default lSingle;
  end;

  TFontDef = record
    FType: string;
    FName: string;
    FAscent: string;
    FDescent: string;
    FCapHeight: string;
    FFlags: string;
    FFontBBox: string;
    FItalicAngle: string;
    FStemV: string;
    FMissingWidth: string;
    FEncoding: string;
    FFile: string;
    FOriginalSize: string;
    FDiffs: WideString;
    FCharWidth: WideString;
  end;

const
  CRLF         = #13#10;
  PDF_VERSION  = '%PDF-1.3';
  PDF_FILE_END = '%%EOF';
  PDF_MAX_GEN_NUM = 65535;
  PDF_UNICODE_HEADER = 'FEFF001B%s001B';
  PDF_LANG_STRING = 'en';

var
  Document: TPdfDocument;
  OldDecSeparator: char;
  Outline: Boolean;
  FontDirectory: string;

implementation

uses
  fpg_main,
  fpg_dialogs,
  fpg_utils,
  fpg_constants,
  U_Report,
  U_Command;

var
  uDictionary: TPdfDictionary;
  uCurrentColor: string;
  uCurrentWidth: string;
  uCatalogue: integer;
  uFontDef: TFontDef;
  uStream: TMemoryStream;
  uFontFiles: array of string;

// utility functions

function InsertEscape(const AValue: string): string;
var
  S: string;
begin
  Result := '';
  S := AValue;
  if Pos('\', S) > 0 then
    S := AnsiReplaceStr(S, '\', '\\');
  if Pos('(', S) > 0 then
    S := AnsiReplaceStr(S, '(', '\(');
  if Pos(')', S) > 0 then
    S := AnsiReplaceStr(S, ')', '\)');
  Result := S;
end;

procedure WriteString(const AValue: string; AStream: TStream);
begin
  AStream.Write(PChar(AValue)^, Length(AValue));
end;

function IntToString(const AValue: integer; const ALength: integer): string;
var
  S: string;
  Cpt: integer;
begin
  Result := '';
  S := IntToStr(AValue);
  if Length(S) < ALength then
    for Cpt := Succ(Length(S)) to ALength do
      Result := Result + '0';
  Result := Result + S;
end;

function DateToPdfDate(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('"D:"yyyymmddhhnnss', ADate);
end;

function ExtractBaseFontName(const AValue: string): string;
var
  FontName, Chaine1, Chaine2: string;
begin
  FontName := Copy(AValue, 1, Pred(Pos('-', AValue)));
  if Pos(':', AValue) > 0 then
  begin
    Chaine1 := Copy(AValue, Succ(Pos(':', AValue)), Length(AValue) - Pos(':', AValue));
    Chaine1 := Uppercase(Chaine1[1]) + Copy(Chaine1, 2, Pred(Length(Chaine1)));
    if Pos(':', Chaine1) > 0 then
    begin
      Chaine2 := Copy(Chaine1, Succ(Pos(':', Chaine1)), Length(Chaine1) - Pos(':', Chaine1));
      Chaine2 := Uppercase(Chaine2[1]) + Copy(Chaine2, 2, Pred(Length(Chaine2)));
      Chaine1 := Copy(Chaine1, 1, Pred(Pos(':', Chaine1)));
      Chaine1 := Uppercase(Chaine1[1]) + Copy(Chaine1, 2, Pred(Length(Chaine1)));
      Chaine1 := Chaine1 + Chaine2;
    end;
    Chaine1 := '-' + Chaine1;
  end;
  Result := FontName + Chaine1;
end;

// object methods

constructor TPdfObject.Create;
begin
  // to be implemented by descendants
end;

destructor TPdfObject.Destroy;
begin
  inherited;
end;

procedure TPdfBoolean.WriteBoolean(const AStream: TStream);
begin
  if FValue then
    WriteString('true', AStream)
  else
    WriteString('false', AStream);
end;

constructor TPdfBoolean.CreateBoolean(const AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TPdfInteger.WriteInteger(const AStream: TStream);
begin
  WriteString(IntToStr(FValue), AStream);
end;

procedure TPdfInteger.IncrementeInteger;
begin
  FValue := FValue + 1;
end;

constructor TPdfInteger.CreateInteger(const AValue: integer);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TPdfReference.WriteReference(const AStream: TStream);
begin
  WriteString(IntToStr(FValue) + ' 0 R', AStream);
end;

constructor TPdfReference.CreateReference(const AValue: integer);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TPdfName.WriteName(const AStream: TStream);
begin
  if FValue <> '' then
    if Pos('Length1', FValue) > 0 then
      WriteString('/Length1', AStream)
    else
      WriteString('/' + FValue, AStream);
end;

constructor TPdfName.CreateName(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TPdfString.Write(const AStream: TStream);
begin
  WriteString('(' + Utf8ToAnsi(FValue) + ')', AStream);
end;

constructor TPdfString.CreateString(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
  if (Pos('(', FValue) > 0) or (Pos(')', FValue) > 0) or (Pos('\', FValue) > 0) then
    FValue := InsertEscape(FValue);
end;

procedure TPdfArray.WriteArray(const AStream: TStream);
var
  Cpt: integer;
begin
  WriteString('[', AStream);
  for Cpt := 0 to Pred(FArray.Count) do
  begin
    if Cpt > 0 then
      WriteString(' ', AStream);
    if TPdfObject(FArray[Cpt]) is TPdfInteger then
      TPdfInteger(FArray[Cpt]).WriteInteger(AStream);
    if TPdfObject(FArray[Cpt]) is TPdfReference then
      TPdfReference(FArray[Cpt]).WriteReference(AStream);
    if TPdfObject(FArray[Cpt]) is TPdfName then
      TPdfName(FArray[Cpt]).WriteName(AStream);
  end;
  WriteString(']', AStream);
end;

procedure TPdfArray.AddItem(const AValue: TPdfObject);
begin
  FArray.Add(AValue);
end;

constructor TPdfArray.CreateArray;
begin
  inherited Create;
  FArray := TList.Create;
end;

destructor TPdfArray.Destroy;
var
  Cpt: integer;
begin
  if FArray.Count > 0 then
    for Cpt := 0 to Pred(FArray.Count) do
      if TPdfObject(FArray[Cpt]) is TPdfInteger then
        TPdfInteger(FArray[Cpt]).Free
      else if TPdfObject(FArray[Cpt]) is TPdfReference then
        TPdfReference(FArray[Cpt]).Free
      else if TPdfObject(FArray[Cpt]) is TPdfName then
        TPdfName(FArray[Cpt]).Free;
  FArray.Free;
  inherited;
end;

procedure TPdfStream.WriteStream(const AStream: TStream);
var
  Cpt: integer;
begin
  for Cpt := 0 to Pred(FStream.Count) do
  begin
    if TPdfObject(FStream[Cpt]) is TPdfEmbeddedFont then
      TPdfEmbeddedFont(FStream[Cpt]).WriteFont(AStream);
    if TPdfObject(FStream[Cpt]) is TPdfColor then
      TPdfColor(FStream[Cpt]).WriteColor(AStream);
    if TPdfObject(FStream[Cpt]) is TPdfText then
      TPdfText(FStream[Cpt]).WriteText(AStream);
    if TPdfObject(FStream[Cpt]) is TPdfRectangle then
      TPdfRectangle(FStream[Cpt]).WriteRectangle(AStream);
    if TPdfObject(FStream[Cpt]) is TPdfLineSegment then
      TPdfLineSegment(FStream[Cpt]).WriteLineSegment(AStream);
    if TPdfObject(FStream[Cpt]) is TPdfLineStyle then
      TPdfLineStyle(FStream[Cpt]).WriteLineStyle(AStream);
    if TPdfObject(FStream[Cpt]) is TPdfSurface then
      TPdfSurface(FStream[Cpt]).WriteSurface(AStream);
    if TPdfObject(FStream[Cpt]) is TPdfImage then
      TPdfImage(FStream[Cpt]).WriteImage(AStream);
  end;
end;

procedure TPdfStream.AddItem(const AValue: TPdfObject);
begin
  FStream.Add(AValue);
end;

constructor TPdfStream.CreateStream;
begin
  inherited Create;
  FStream := TList.Create;
end;

destructor TPdfStream.Destroy;
var
  Cpt: integer;
begin
  if FStream.Count > 0 then
  begin
    for Cpt := 0 to Pred(FStream.Count) do
    begin
      if TPdfObject(FStream[Cpt]) is TPdfEmbeddedFont then
        TPdfEmbeddedFont(FStream[Cpt]).Free
      else if TPdfObject(FStream[Cpt]) is TPdfColor then
        TPdfColor(FStream[Cpt]).Free
      else if TPdfObject(FStream[Cpt]) is TPdfText then
        TPdfText(FStream[Cpt]).Free
      else if TPdfObject(FStream[Cpt]) is TPdfRectangle then
        TPdfRectangle(FStream[Cpt]).Free
      else if TPdfObject(FStream[Cpt]) is TPdfLineSegment then
        TPdfLineSegment(FStream[Cpt]).Free
      else if TPdfObject(FStream[Cpt]) is TPdfLineStyle then
        TPdfLineStyle(FStream[Cpt]).Free
      else if TPdfObject(FStream[Cpt]) is TPdfSurface then
        TPdfSurface(FStream[Cpt]).Free
      else if TPdfObject(FStream[Cpt]) is TPdfImage then
        TPdfImage(FStream[Cpt]).Free;
    end;
  end;
  FStream.Free;
  inherited;
end;

procedure TPdfEmbeddedFont.WriteFont(const AStream: TStream);
begin
  WriteString('/F' + IntToStr(FTxtFont) + ' ' + FTxtSize + ' Tf' + CRLF, AStream);
end;

function TPdfEmbeddedFont.WriteEmbeddedFont(const ASrcStream: TMemoryStream; const AStream: TStream): int64;
var
  BeginFlux, EndFlux: int64;
begin
  WriteString(CRLF + 'stream' + CRLF, AStream);
  BeginFlux := AStream.Position;
  ASrcStream.SaveToStream(AStream);
  EndFlux   := AStream.Position;
  Result    := EndFlux - BeginFlux;
  WriteString(CRLF, AStream);
  WriteString('endstream', AStream);
end;

constructor TPdfEmbeddedFont.CreateFont(const AFont: integer; const ASize: string);
begin
  inherited Create;
  FTxtFont := AFont;
  FTxtSize := ASize;
end;

procedure TPdfText.WriteText(const AStream: TStream);
begin
  WriteString('BT' + CRLF, AStream);
  WriteString(FormatFloat('0.##', FTxtPosX) + ' ' + FormatFloat('0.##', FTxtPosY) + ' Td' + CRLF, AStream);
  TPdfString(FTxtText).Write(AStream);
  WriteString(' Tj' + CRLF, AStream);
  WriteString('ET' + CRLF, AStream);
end;

constructor TPdfText.CreateText(const APosX, APosY: single; const AText: string);
begin
  inherited Create;
  FTxtPosX := APosX;
  FTxtPosY := APosY;
  FTxtText := TPdfString.CreateString(AText);
end;

destructor TPdfText.Destroy;
begin
  FTxtText.Free;
  inherited;
end;

procedure TPdfLineSegment.WriteLineSegment(const AStream: TStream);
begin
  if (FormatFloat('0.##', FWidth) + ' w') <> uCurrentWidth then
  begin
    WriteString('1 J' + CRLF, AStream);
    WriteString(FormatFloat('0.##', FWidth) + ' w' + CRLF, AStream);
    uCurrentWidth := FormatFloat('0.##', FWidth) + ' w';
  end;
  WriteString(FormatFloat('0.##', FX1) + ' ' + FormatFloat('0.##', FY1) + ' m' + CRLF, AStream);
  WriteString(FormatFloat('0.##', FX2) + ' ' + FormatFloat('0.##', FY2) + ' l' + CRLF, AStream);
  WriteString('S' + CRLF, AStream);
end;

constructor TPdfLineSegment.CreateLineSegment(const AWidth, AX1, AY1, AX2, AY2: single);
begin
  inherited Create;
  FWidth := AWidth;
  FX1  := AX1;
  FY1  := AY1;
  FX2  := AX2;
  FY2  := AY2;
end;

procedure TPdfRectangle.WriteRectangle(const AStream: TStream);
begin
  if FStroke then
  begin
    if (FormatFloat('0.##', FLineWidth) + ' w') <> uCurrentWidth then
    begin
      WriteString('1 J' + CRLF, AStream);
      WriteString(FormatFloat('0.##', FLineWidth) + ' w' + CRLF, AStream);
      uCurrentWidth := FormatFloat('0.##', FLineWidth) + ' w';
    end;
  end;
  WriteString(FormatFloat('0.##', FRecX) + ' ' + FormatFloat('0.##', FRecY) + ' ' + FormatFloat('0.##', FRecW) + ' ' + FormatFloat('0.##', FRecH) + ' re' + CRLF, AStream);
  if FStroke then
    WriteString('S' + CRLF, AStream);
  if FFill then
    WriteString('f' + CRLF, AStream);
end;

constructor TPdfRectangle.CreateRectangle(const ALineWidth, APosX, APosY, AWidth, AHeight: single; const AFill, AStroke: Boolean);
begin
  inherited Create;
  FLineWidth  := ALineWidth;
  FRecX   := APosX;
  FRecY   := APosY;
  FRecW   := AWidth;
  FRecH   := AHeight;
  FFill   := AFill;
  FStroke := AStroke;
end;

procedure TPdfSurface.WriteSurface(const AStream: TStream);
var
  Cpt: integer;
begin
  WriteString(FormatFloat('0.##', FPoints[0].X) + ' ' + FormatFloat('0.##', FPoints[0].Y) + ' m' + CRLF, AStream);
  for Cpt := 1 to Pred(Length(FPoints)) do
    WriteString(FormatFloat('0.##', FPoints[Cpt].X) + ' ' + FormatFloat('0.##', FPoints[Cpt].Y) + ' l' + CRLF, AStream);
  WriteString('h' + CRLF, AStream);
  WriteString('f' + CRLF, AStream);
end;

constructor TPdfSurface.CreateSurface(const APoints: T_Points);
begin
  inherited Create;
  FPoints := APoints;
end;

function TPdfImage.WriteImageStream(const ANumber: integer; AStream: TStream): int64;
var
  CptW, CptH: integer;
  BeginFlux, EndFlux: int64;
begin
  WriteString(CRLF + 'stream' + CRLF, AStream);
  BeginFlux := AStream.Position;
  for CptH := 0 to Pred(TfpgImage(Images[ANumber]).Height) do
  begin
    for CptW := 0 to Pred(TfpgImage(Images[ANumber]).Width) do
    begin
      AStream.WriteByte(fpgGetRed(TfpgImage(Images[ANumber]).Colors[CptW, CptH]));
      AStream.WriteByte(fpgGetGreen(TfpgImage(Images[ANumber]).Colors[CptW, CptH]));
      AStream.WriteByte(fpgGetBlue(TfpgImage(Images[ANumber]).Colors[CptW, CptH]));
    end;
  end;
  EndFlux := AStream.Position;
  Result  := EndFlux - BeginFlux;
  WriteString(CRLF, AStream);
  WriteString('endstream', AStream);
end;

procedure TPdfImage.WriteImage(const AStream: TStream);
begin
  WriteString('q' + CRLF, AStream);
  WriteString(IntToStr(FWidth) + ' 0 0 ' + IntToStr(FHeight) + ' ' + FormatFloat('0.##', FLeft) + ' ' + FormatFloat('0.##', FBottom) + ' cm' + CRLF, AStream);
  WriteString('/I' + IntToStr(FNumber) + ' Do ' + CRLF, AStream);
  WriteString('Q' + CRLF, AStream);
end;

constructor TPdfImage.CreateImage(const ALeft, ABottom: single; AWidth, AHeight, ANumber: integer);
begin
  inherited Create;
  FNumber := ANumber;
  FLeft   := ALeft;
  FBottom := ABottom;
  FWidth  := AWidth;
  FHeight := AHeight;
end;

procedure TPdfLineStyle.WriteLineStyle(const AStream: TStream);
begin
  WriteString('[', AStream);
  case FDash of
    lsDash:
      WriteString('5 5', AStream);
    lsDot:
      WriteString('2 2', AStream);
    lsDashDot:
      WriteString('5 2 2 2', AStream);
    lsDashDotDot:
      WriteString('5 2 2 2 2 2', AStream);
  end;
  WriteString('] ' + IntToStr(FPhase) + ' d' + CRLF, AStream);
end;

constructor TPdfLineStyle.CreateLineStyle(ADash: TfpgLineStyle; APhase: integer);
begin
  inherited Create;
  FDash  := ADash;
  FPhase := APhase;
end;

procedure TPdfColor.WriteColor(const AStream: TStream);
begin
  if FStroke then
  begin
    if (FRed + ' ' + FGreen + ' ' + FBlue + ' rg') <> uCurrentColor then
    begin
      WriteString(FRed + ' ' + FGreen + ' ' + FBlue + ' rg' + CRLF, AStream);
      uCurrentColor := FRed + ' ' + FGreen + ' ' + FBlue + ' rg';
    end;
  end
  else if (FRed + ' ' + FGreen + ' ' + FBlue + ' RG') <> uCurrentColor then
  begin
    WriteString(FRed + ' ' + FGreen + ' ' + FBlue + ' RG' + CRLF, AStream);
    uCurrentColor := FRed + ' ' + FGreen + ' ' + FBlue + ' RG';
  end;
end;

constructor TPdfColor.CreateColor(const AStroke: Boolean; AColor: TfpgColor);
begin
  inherited Create;
  FBlue   := FormatFloat('0.##', fpgGetBlue(AColor) / 256);
  FGreen  := FormatFloat('0.##', fpgGetGreen(AColor) / 256);
  FRed    := FormatFloat('0.##', fpgGetRed(AColor) / 256);
  FStroke := AStroke;
end;

procedure TPdfDicElement.WriteDicElement(const AStream: TStream);
begin
  FKey.WriteName(AStream);
  WriteString(' ', AStream);
  if FValue is TPdfBoolean then
    TPdfBoolean(FValue).WriteBoolean(AStream);
  if FValue is TPdfInteger then
    TPdfInteger(FValue).WriteInteger(AStream);
  if FValue is TPdfReference then
    TPdfReference(FValue).WriteReference(AStream);
  if FValue is TPdfName then
    TPdfName(FValue).WriteName(AStream);
  if FValue is TPdfString then
    TPdfString(FValue).Write(AStream);
  if FValue is TPdfArray then
    TPdfArray(FValue).WriteArray(AStream);
  if FValue is TPdfDictionary then
    TPdfDictionary(FValue).WriteDictionary(-1, AStream);
  WriteString(CRLF, AStream);
end;

constructor TPdfDicElement.CreateDicElement(const AKey: string; const AValue: TPdfObject);
begin
  inherited Create;
  FKey   := TPdfName.CreateName(AKey);
  FValue := AValue;
end;

destructor TPdfDicElement.Destroy;
begin
  FKey.Free;
  if FValue is TPdfBoolean then
    TPdfBoolean(FValue).Free
  else if FValue is TPdfDictionary then
    TPdfDictionary(FValue).Free
  else if FValue is TPdfInteger then
    TPdfInteger(FValue).Free
  else if FValue is TPdfName then
    TPdfName(FValue).Free
  else if FValue is TPdfReference then
    TPdfReference(FValue).Free
  else if FValue is TPdfString then
    TPdfString(FValue).Free
  else if FValue is TPdfArray then
    TPdfArray(FValue).Free;
  inherited;
end;

procedure TPdfDictionary.AddElement(const AKey: string; const AValue: TPdfObject);
var
  DicElement: TPdfDicElement;
begin
  DicElement := TPdfDicElement.CreateDicElement(AKey, AValue);
  FElement.Add(DicElement);
end;

function TPdfDictionary.ElementParCle(const AValue: string): integer;
var
  Cpt: integer;
begin
  Result := -1;
  for Cpt := 0 to Pred(FElement.Count) do
    if TPdfDicElement(FElement[Cpt]).FKey.FValue = AValue then
    begin
      Result := Cpt;
      Exit;
    end;
end;

procedure TPdfDictionary.WriteDictionary(const AObjet: integer; const AStream: TStream);
var
  Long: TPdfInteger;
  Cpt, NumImg, NumFnt: integer;
  Value: string;
begin
  if TPdfName(TPdfDicElement(FElement[0]).FKey).FValue = '' then
    TPdfDicElement(FElement[0]).WriteDicElement(AStream)  // write a charwidth array of a font
  else
  begin
    WriteString('<<' + CRLF, AStream);
    if FElement.Count > 0 then
      for Cpt := 0 to Pred(FElement.Count) do
        TPdfDicElement(FElement[Cpt]).WriteDicElement(AStream);
    NumImg := -1;
    NumFnt := -1;
    if FElement.Count > 0 then
    begin
      for Cpt := 0 to Pred(FElement.Count) do
      begin
        if AObjet > -1 then
        begin
          if (TPdfName(TPdfDicElement(FElement[Cpt]).FKey).FValue = 'Name') then
          begin
            if (TPdfObject(TPdfDicElement(FElement[Cpt]).FValue) is TPdfName) and (TPdfName(TPdfDicElement(FElement[Cpt]).FValue).FValue[1] = 'I') then
            begin
              NumImg        := StrToInt(Copy(TPdfName(TPdfDicElement(FElement[Cpt]).FValue).FValue, 2, Length(TPdfName(TPdfDicElement(FElement[Cpt]).FValue).FValue) - 1));
              uStream          := TMemoryStream.Create;
              uStream.Position := 0;
              // write image stream length in xobject dictionary
              Long          := TPdfInteger.CreateInteger(TPdfImage(TPdfXRef(Document.FGlobalXRefs[AObjet]).FDict).WriteImageStream(NumImg, uStream));
              TPdfDictionary(TPdfXRef(Document.FGlobalXRefs[AObjet]).FDict).AddElement('Length', Long);
              TPdfDicElement(FElement[Pred(FElement.Count)]).WriteDicElement(AStream);
              uStream.Free;
              WriteString('>>', AStream);
              // write image stream in xobject dictionary
              TPdfImage(TPdfXRef(Document.FGlobalXRefs[AObjet]).FDict).WriteImageStream(NumImg, AStream);
            end;
          end;
          if Pos('Length1', TPdfName(TPdfDicElement(FElement[Cpt]).FKey).FValue) > 0 then
          begin
            uStream   := TMemoryStream.Create;
            Value  := TPdfName(TPdfDicElement(FElement[Cpt]).FKey).FValue;
            NumFnt := StrToInt(Copy(Value, Succ(Pos(' ', Value)), Length(Value) - Pos(' ', Value)));
            uStream.LoadFromFile(uFontFiles[NumFnt]);
            // write fontfile stream length in xobject dictionary
            Long   := TPdfInteger.CreateInteger(uStream.Size);
            TPdfDictionary(TPdfXRef(Document.FGlobalXRefs[AObjet]).FDict).AddElement('Length', Long);
            TPdfDicElement(FElement[Pred(FElement.Count)]).WriteDicElement(AStream);
            WriteString('>>', AStream);
            // write fontfile stream in xobject dictionary
            TPdfEmbeddedFont(TPdfXRef(Document.FGlobalXRefs[NumFnt]).FDict).WriteEmbeddedFont(uStream, AStream);
            uStream.Free;
          end;
        end;
      end; { for Cpt... }
    end; { if FElement.Count... }
    if (NumImg = -1) and (NumFnt = -1) then
      WriteString('>>', AStream);
  end; { if/else }
end;

constructor TPdfDictionary.CreateDictionary;
begin
  inherited Create;
  FElement := TList.Create;
end;

destructor TPdfDictionary.Destroy;
var
  Cpt: integer;
begin
  if FElement.Count > 0 then
    for Cpt := 0 to Pred(FElement.Count) do
      TPdfDicElement(FElement[Cpt]).Free;
  FElement.Free;
  inherited;
end;

procedure TPdfXRef.WriteXRef(const AStream: TStream);
begin
  WriteString(IntToString(FOffset, 10) + ' ' + IntToString(0, 5) + ' n' + CRLF, AStream);
end;

constructor TPdfXRef.CreateXRef;
begin
  inherited Create;
  FOffset := 0;
  FDict  := TpdfDictionary.CreateDictionary;
  FStream := nil;
end;

destructor TPdfXRef.Destroy;
begin
  FDict.Free;
  FStream.Free;
  inherited;
end;

function TPdfDocument.ElementParNom(const AValue: string): integer;
var
  Cpt: integer;
begin
  for Cpt := 1 to Pred(FGlobalXRefs.Count) do
    if TPdfName(TPdfDicElement(TPdfDictionary(TPdfXRef(FGlobalXRefs[Cpt]).FDict).FElement[0]).FValue).FValue = AValue then
      Result := Cpt;
end;

procedure TPdfDocument.WriteXRefTable(const AStream: TStream);
var
  Cpt: integer;
begin
  if FGlobalXRefs.Count > 1 then
    for Cpt := 1 to Pred(FGlobalXRefs.Count) do
      TPdfXRef(FGlobalXRefs[Cpt]).WriteXRef(AStream);
end;

procedure TPdfDocument.WriteObject(const AObject: integer; const AStream: TStream);
var
  Long: TPdfInteger;
  Flux: TMemoryStream;
begin
  WriteString(IntToStr(AObject) + ' 0 obj' + CRLF, AStream);
  if TPdfXRef(FGlobalXRefs[AObject]).FStream = nil then
    TPdfDictionary(TPdfXRef(FGlobalXRefs[AObject]).FDict).WriteDictionary(AObject, AStream)
  else
  begin
    Flux          := TMemoryStream.Create;
    Flux.Position := 0;
    uCurrentColor  := '';
    uCurrentWidth  := '';
    TPdfXRef(FGlobalXRefs[AObject]).FStream.WriteStream(Flux);
    // write stream length element in contents dictionary
    Long          := TPdfInteger.CreateInteger(Flux.Size);
    TPdfDictionary(TPdfXRef(FGlobalXRefs[AObject]).FDict).AddElement('Length', Long);
    Flux.Free;
    TPdfXRef(FGlobalXRefs[AObject]).FDict.WriteDictionary(-1, AStream);
    // write stream in contents dictionary
    uCurrentColor := '';
    uCurrentWidth := '';
    WriteString(CRLF + 'stream' + CRLF, AStream);
    TPdfXRef(FGlobalXRefs[AObject]).FStream.WriteStream(AStream);
    WriteString('endstream', AStream);
  end;
  WriteString(CRLF + 'endobj' + CRLF + CRLF, AStream);
end;

procedure TPdfDocument.CreateRefTable;
var
  XRefObjet: TPdfXRef;
begin
  FGlobalXRefs := TList.Create;
  // add first xref entry
  XRefObjet   := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(XRefObjet);
end;

procedure TPdfDocument.CreateTrailer;
var
  XRefObjets: TPdfInteger;
begin
  uDictionary    := TPdfDictionary.CreateDictionary;
  // add size uDictionary element
  XRefObjets := TPdfInteger.CreateInteger(FGlobalXRefs.Count);
  uDictionary.AddElement('Size', XRefObjets);
end;

function TPdfDocument.CreateCatalog: integer;
var
  Catalog: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Table: TPdfArray;
begin
  // add xref entry
  Catalog    := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Catalog);
  // add root uDictionary element
  XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
  uDictionary.AddElement('Root', XRefObjets);
  // add type element to catalog dictionary
  Nom        := TPdfName.CreateName('Catalog');
  Catalog.FDict.AddElement('Type', Nom);
  // add pagelayout element to catalog dictionary
  case FPageLayout of
    lSingle:
      Nom := TPdfName.CreateName('SinglePage');
    lTwo:
      Nom := TPdfName.CreateName('TwoColumnLeft');
    lContinuous:
      Nom := TPdfName.CreateName('OneColumn');
  end;
  Catalog.FDict.AddElement('PageLayout', Nom);
  // add openaction element to catalog dictionary
  Table  := TPdfArray.CreateArray;
  Catalog.FDict.AddElement('OpenAction', Table);
  Result := Pred(FGlobalXRefs.Count);
end;

procedure TPdfDocument.CreateInfo;
var
  Info: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfString;
begin
  // add xref entry
  Info       := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Info);
  // add info uDictionary element
  XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
  uDictionary.AddElement('Info', XRefObjets);
  TPdfInteger(TPdfDicElement(uDictionary.FElement[uDictionary.ElementParCle('Size')]).FValue).FValue := FGlobalXRefs.Count;
  // add title element to info dictionary
  lName        := TPdfString.CreateString(Infos.Title);
  Info.FDict.AddElement('Title', lName);
  // add author element to info dictionary
  lName        := TPdfString.CreateString(Infos.Author);
  Info.FDict.AddElement('Author', lName);
  // add creator element to info dictionary
  lName        := TPdfString.CreateString(ApplicationName);
  Info.FDict.AddElement('Creator', lName);
  // add producer element to info dictionary
  lName        := TPdfString.CreateString(fpGUIName + ' ' + FPGUI_VERSION);
  Info.FDict.AddElement('Producer', lName);
  // add creationdate element to info dictionary
  lName        := TPdfString.CreateString(DateToPdfDate(Now));
  Info.FDict.AddElement('CreationDate', lName);
end;

procedure TPdfDocument.CreatePreferences;
var
  Viewer: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfName;
  Preference: TPdfBoolean;
begin
  // add xref entry
  Viewer     := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Viewer);
  // add type element to preferences dictionary
  lName        := TPdfName.CreateName('ViewerPreferences');
  Viewer.FDict.AddElement('Type', lName);
  // add preference element to preferences dictionary
  Preference := TPdfBoolean.CreateBoolean(True);
  Viewer.FDict.AddElement('FitWindow', Preference);
  // add preferences reference to catalog dictionary
  XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
  TPdfDictionary(TPdfXRef(FGlobalXRefs[ElementParNom('Catalog')]).FDict).AddElement('ViewerPreferences', XRefObjets);
end;

function TPdfDocument.CreatePages(Parent: integer): integer;
var
  Pages: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfName;
  Dictionaire: TPdfDictionary;
  Table: TPdfArray;
  Count: TPdfInteger;
begin
  // add xref entry
  Pages := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Pages);
  // add type element to pages dictionary
  lName   := TPdfName.CreateName('Pages');
  Pages.FDict.AddElement('Type', lName);
  // add parent reference to pages dictionary if pages is not the root of the page tree
  if Parent > 0 then
  begin
    XRefObjets  := TPdfReference.CreateReference(Parent);
    Pages.FDict.AddElement('Parent', XRefObjets);
    // increment count in parent pages dictionary
    Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[Parent]).FDict);
    TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
    // add kid reference in parent pages dictionary
    XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
    TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Kids')]).FValue).AddItem(XRefObjets);
  end
  else
  begin
    // add pages reference to catalog dictionary
    XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
    TPdfDictionary(TPdfXRef(FGlobalXRefs[ElementParNom('Catalog')]).FDict).AddElement('Pages', XRefObjets);
  end;
  // add kids element to pages dictionary
  Table  := TPdfArray.CreateArray;
  Pages.FDict.AddElement('Kids', Table);
  // add count element to pages dictionary
  Count  := TPdfInteger.CreateInteger(0);
  Pages.FDict.AddElement('Count', Count);
  Result := Pred(FGlobalXRefs.Count);
end;

function TPdfDocument.CreatePage(Parent, Haut, Larg, PageNum: integer): integer;
var
  Page: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfName;
  Dictionaire: TPdfDictionary;
  Table: TPdfArray;
  Coord: TPdfInteger;
  Cpt: integer;
begin
  // add xref entry
  Page        := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Page);
  // add type element to page dictionary
  lName         := TPdfName.CreateName('Page');
  Page.FDict.AddElement('Type', lName);
  // add parent reference to page dictionary
  XRefObjets  := TPdfReference.CreateReference(Parent);
  Page.FDict.AddElement('Parent', XRefObjets);
  // increment count in parent pages dictionary
  Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[Parent]).FDict);
  TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
  // add kid reference in parent pages dictionary
  XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Kids')]).FValue).AddItem(XRefObjets);
  // add mediabox element to page dictionary
  Table       := TPdfArray.CreateArray;
  Page.FDict.AddElement('MediaBox', Table);
  // add coordinates in page mediabox
  Dictionaire := TPdfDictionary(TPdfXRef(Page).FDict);
  Coord       := TPdfInteger.CreateInteger(0);
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
  Coord       := TPdfInteger.CreateInteger(0);
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
  Coord       := TPdfInteger.CreateInteger(Larg);
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
  Coord       := TPdfInteger.CreateInteger(Haut);
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
  // add resources element to page dictionary
  Dictionaire := TPdfDictionary.CreateDictionary;
  Page.FDict.AddElement('Resources', Dictionaire);
  // add procset element in resources element to page dictionary
  Table       := TPdfArray.CreateArray;
  TPdfDictionary(TPdfDicElement(Page.FDict.FElement[Pred(Page.FDict.FElement.Count)]).FValue).AddElement('ProcSet', Table);
  // add font element in resources element to page dictionary
  if Fonts.Count > 0 then
  begin
    Dictionaire := TPdfDictionary.CreateDictionary;
    TPdfDictionary(TPdfDicElement(Page.FDict.FElement[Pred(Page.FDict.FElement.Count)]).FValue).AddElement('Font', Dictionaire);
  end;
  for Cpt := 0 to Pred(PdfPage.Count) do
  begin
    if TPdfElement(PdfPage[Cpt]) is TPdfImg then
    begin
      if TPdfImg(PdfPage[Cpt]).PageId = PageNum then
      begin
        // add xobject element in resources element to page dictionary
        Dictionaire := TPdfDictionary.CreateDictionary;
        TPdfDictionary(TPdfDicElement(Page.FDict.FElement[Pred(Page.FDict.FElement.Count)]).FValue).AddElement('XObject', Dictionaire);
        Break;
      end;
    end;
  end;
  // add pdf element in procset array to page dictionary
  Dictionaire := TPdfDictionary(TPdfDicElement(Page.FDict.FElement[Pred(Page.FDict.FElement.Count)]).FValue);
  lName         := TPdfName.CreateName('PDF');
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(lName);
  // add text element in procset array to page dictionary
  lName         := TPdfName.CreateName('Text');
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(lName);
  // add image element in procset array to page dictionary
  lName         := TPdfName.CreateName('ImageC');
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(lName);
  Result      := Pred(FGlobalXRefs.Count);
end;

function TPdfDocument.CreateOutlines: integer;
var
  Outlines: TPdfXRef;
  lName: TPdfName;
  Count: TPdfInteger;
begin
  // add xref entry
  Outlines := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Outlines);
  // add type element to outlines dictionary
  lName      := TPdfName.CreateName('Outlines');
  Outlines.FDict.AddElement('Type', lName);
  // add count element to outlines dictionary
  Count    := TPdfInteger.CreateInteger(0);
  Outlines.FDict.AddElement('Count', Count);
  Result   := Pred(FGlobalXRefs.Count);
end;

function TPdfDocument.CreateOutline(Parent, SectNo, PageNo: integer; SectTitre: string): integer;
var
  Outline: TPdfXRef;
  XRefObjets: TPdfReference;
  Titre: TPdfString;
  Count: TPdfInteger;
  Table: TPdfArray;
begin
  // add xref entry
  Outline := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Outline);
  // add title element to outline dictionary
  if PageNo > -1 then
  begin
    if SectTitre <> '' then
      Titre := TPdfString.CreateString(SectTitre + ' Page ' + IntToStr(PageNo))
    else
      Titre := TPdfString.CreateString('Section ' + IntToStr(SectNo) + ' Page ' + IntToStr(PageNo))
  end
  else
  begin
    if SectTitre <> '' then
      Titre := TPdfString.CreateString(SectTitre)
    else
      Titre := TPdfString.CreateString('Section ' + IntToStr(SectNo));
  end;
  Outline.FDict.AddElement('Title', Titre);
  // add parent reference to outline dictionary
  XRefObjets := TPdfReference.CreateReference(Parent);
  Outline.FDict.AddElement('Parent', XRefObjets);
  // add count element to outline dictionary
  Count  := TPdfInteger.CreateInteger(0);
  Outline.FDict.AddElement('Count', Count);
  // add dest element to outline dictionary
  Table  := TPdfArray.CreateArray;
  Outline.FDict.AddElement('Dest', Table);
  Result := Pred(FGlobalXRefs.Count);
end;

procedure TPdfDocument.CreateStdFont(NomFonte: string; NumFonte: integer);
var
  Fontes: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfName;
  Dictionaire: TPdfDictionary;
  Cpt: integer;
begin
  if Pos('Italic', NomFonte) > 0 then
    NomFonte := Copy(NomFonte, 1, Pred(Pos('Italic', NomFonte))) + 'Oblique';
  //  AnsiReplaceText(NomFonte,'Italic','Oblique');
  // add xref entry
  Fontes := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Fontes);
  // add type element to font dictionary
  lName := TPdfName.CreateName('Font');
  Fontes.FDict.AddElement('Type', lName);
  // add subtype element to font dictionary
  lName := TPdfName.CreateName('Type1');
  Fontes.FDict.AddElement('Subtype', lName);
  // add encoding element to font dictionary
  lName := TPdfName.CreateName('WinAnsiEncoding');
  Fontes.FDict.AddElement('Encoding', lName);
  // add firstchar element to font dictionary
  lName := TPdfName.CreateName('32');
  //lName:= TPdfName.CreateName('0');
  Fontes.FDict.AddElement('FirstChar', lName);
  // add lastchar element to font dictionary
  lName := TPdfName.CreateName('255');
  Fontes.FDict.AddElement('LastChar', lName);
  // add basefont element to font dictionary
  lName := TPdfName.CreateName(NomFonte);
  Fontes.FDict.AddElement('BaseFont', lName);
  // add name element to font dictionary
  lName := TPdfName.CreateName('F' + IntToStr(NumFonte));
  Fontes.FDict.AddElement('Name', lName);
  // add font reference to all page dictionary
  for Cpt := 1 to Pred(FGlobalXRefs.Count) do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[Cpt]).FDict);
    if Dictionaire.FElement.Count > 0 then
      if TPdfName(TPdfDicElement(Dictionaire.FElement[0]).FValue).FValue = 'Page' then
      begin
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Resources')]).FValue);
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Font')]).FValue);
        XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
        Dictionaire.AddElement(TPdfName(lName).FValue, XRefObjets);
      end;
  end;
  SetLength(uFontFiles, Succ(Length(uFontFiles)));
  uFontFiles[NumFonte] := '';
end;

function TPdfDocument.LoadFont(AFontName: string): string;
var
  FileTxt: TextFile;
  lLine: WideString;
begin
  if fpgFileExists(FontDirectory + AFontName + '.fnt') then
  begin
    AssignFile(FileTxt, FontDirectory + AFontName + '.fnt');
    Reset(FileTxt);
    while not EOF(FileTxt) do
    begin
      Readln(FileTxt, lLine);
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'FontType' then
        uFontDef.FType         := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'FontName' then
        uFontDef.FName         := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'Ascent' then
        uFontDef.FAscent       := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'Descent' then
        uFontDef.FDescent      := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'CapHeight' then
        uFontDef.FCapHeight    := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'Flags' then
        uFontDef.FFlags        := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'FontBBox' then
        uFontDef.FFontBBox     := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'ItalicAngle' then
        uFontDef.FItalicAngle  := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'StemV' then
        uFontDef.FStemV        := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'MissingWidth' then
        uFontDef.FMissingWidth := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'Encoding' then
        uFontDef.FEncoding     := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'FontFile' then
        uFontDef.FFile         := FontDirectory + Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'OriginalSize' then
        uFontDef.FOriginalSize := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'Diffs' then
        uFontDef.FDiffs        := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
      if Copy(lLine, 1, Pred(Pos('=', lLine))) = 'CharWidth' then
        uFontDef.FCharWidth    := Copy(lLine, Succ(Pos('=', lLine)), Length(lLine) - Pos('=', lLine));
    end;
    Result := uFontDef.FType;
  end
  else
    ShowMessage(Format(rsErrReportFontFileMissing, [AFontName]));
end;

procedure TPdfDocument.CreateTtfFont(const NumFonte: integer);
var
  Fontes: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfName;
  Dictionaire: TPdfDictionary;
  Value: TPdfInteger;
  Cpt: integer;
begin
  // add xref entry
  Fontes := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Fontes);
  // add type element to font dictionary
  lName    := TPdfName.CreateName('Font');
  Fontes.FDict.AddElement('Type', lName);
  // add subtype element to font dictionary
  lName    := TPdfName.CreateName(uFontDef.FType);
  Fontes.FDict.AddElement('Subtype', lName);
  // add encoding element to font dictionary
  lName    := TPdfName.CreateName('WinAnsiEncoding');
  Fontes.FDict.AddElement('Encoding', lName);
  // add firstchar element to font dictionary
  Value  := TPdfInteger.CreateInteger(32);
  Fontes.FDict.AddElement('FirstChar', Value);
  // add lastchar element to font dictionary
  Value  := TPdfInteger.CreateInteger(255);
  Fontes.FDict.AddElement('LastChar', Value);
  // add basefont element to font dictionary
  lName    := TPdfName.CreateName(uFontDef.FName);
  Fontes.FDict.AddElement('BaseFont', lName);
  // add name element to font dictionary
  lName    := TPdfName.CreateName('F' + IntToStr(NumFonte));
  Fontes.FDict.AddElement('Name', lName);
  // add font reference to all page dictionary
  for Cpt := 1 to Pred(FGlobalXRefs.Count) do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[Cpt]).FDict);
    if Dictionaire.FElement.Count > 0 then
      if TPdfName(TPdfDicElement(Dictionaire.FElement[0]).FValue).FValue = 'Page' then
      begin
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Resources')]).FValue);
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Font')]).FValue);
        XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
        Dictionaire.AddElement(TPdfName(lName).FValue, XRefObjets);
      end;
  end;
  CreateFontDescriptor(NumFonte);
  // add fontdescriptor reference to font dictionary
  XRefObjets          := TPdfReference.CreateReference(FGlobalXRefs.Count - 2);
  Fontes.FDict.AddElement('FontDescriptor', XRefObjets);
  CreateFontWidth;
  // add fontwidth reference to font dictionary
  XRefObjets          := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
  Fontes.FDict.AddElement('Widths', XRefObjets);
  SetLength(uFontFiles, Succ(Length(uFontFiles)));
  uFontFiles[NumFonte] := uFontDef.FFile;
end;

procedure TPdfDocument.CreateTp1Font(const NumFonte: integer);
begin

end;

procedure TPdfDocument.CreateFontDescriptor(const NumFonte: integer);
var
  FtDesc: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfName;
  Value: TPdfInteger;
  Table: TPdfArray;
  Dictionaire: TPdfDictionary;
begin
  // add xref entry
  FtDesc := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(FtDesc);
  // add type element to fontdescriptor dictionary
  lName    := TPdfName.CreateName('FontDescriptor');
  FtDesc.FDict.AddElement('Type', lName);
  // add fontname element to fontdescriptor dictionary
  lName    := TPdfName.CreateName(uFontDef.FName);
  FtDesc.FDict.AddElement('FontName', lName);
  // add ascent element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(uFontDef.FAscent));
  FtDesc.FDict.AddElement('Ascent', Value);
  // add descent element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(uFontDef.FDescent));
  FtDesc.FDict.AddElement('Descent', Value);
  // add capheight element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(uFontDef.FCapHeight));
  FtDesc.FDict.AddElement('CapHeight', Value);
  // add flags element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(uFontDef.FFlags));
  FtDesc.FDict.AddElement('Flags', Value);
  // add fontbbox element to fontdescriptor dictionary
  Table  := TPdfArray.CreateArray;
  FtDesc.FDict.AddElement('FontBBox', Table);
  // add coordinates in page fontbbox
  while Pos(' ', uFontDef.FFontBBox) > 0 do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FtDesc).FDict);
    Value       := TPdfInteger.CreateInteger(StrToInt(Copy(uFontDef.FFontBBox, 1, Pred(Pos(' ', uFontDef.FFontBBox)))));
    TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('FontBBox')]).FValue).AddItem(Value);
    uFontDef.FFontBBox := Copy(uFontDef.FFontBBox, Succ(Pos(' ', uFontDef.FFontBBox)), Length(uFontDef.FFontBBox) - Pos(' ', uFontDef.FFontBBox));
  end;
  // add italicangle element to fontdescriptor dictionary
  Value      := TPdfInteger.CreateInteger(StrToInt(uFontDef.FItalicAngle));
  FtDesc.FDict.AddElement('ItalicAngle', Value);
  // add stemv element to fontdescriptor dictionary
  Value      := TPdfInteger.CreateInteger(StrToInt(uFontDef.FStemV));
  FtDesc.FDict.AddElement('StemV', Value);
  // add missingwidth element to fontdescriptor dictionary
  Value      := TPdfInteger.CreateInteger(StrToInt(uFontDef.FMissingWidth));
  FtDesc.FDict.AddElement('MissingWidth', Value);
  CreateFontFile(NumFonte);
  // add fontfilereference to fontdescriptor dictionary
  XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
  FtDesc.FDict.AddElement('FontFile2', XRefObjets);
end;

procedure TPdfDocument.CreateFontWidth;
var
  FtDesc: TPdfXRef;
  Value: TPdfInteger;
  Table: TPdfArray;
  Dictionaire: TPdfDictionary;
begin
  // add xref entry
  FtDesc := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(FtDesc);
  // add element to fontwidth dictionary
  Table  := TPdfArray.CreateArray;
  FtDesc.FDict.AddElement('', Table);
  // add width values in fontwidth array
  while Pos(' ', uFontDef.FCharWidth) > 0 do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FtDesc).FDict);
    Value       := TPdfInteger.CreateInteger(StrToInt(Copy(uFontDef.FCharWidth, 1, Pred(Pos(' ', uFontDef.FCharWidth)))));
    TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('')]).FValue).AddItem(Value);
    uFontDef.FCharWidth := Copy(uFontDef.FCharWidth, Succ(Pos(' ', uFontDef.FCharWidth)), Length(uFontDef.FCharWidth) - Pos(' ', uFontDef.FCharWidth));
  end;
end;

procedure TPdfDocument.CreateFontFile(const NumFonte: integer);
var
  FtDesc: TPdfXRef;
  lName: TPdfName;
  Value: TPdfInteger;
begin
  // add xref entry
  FtDesc := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(FtDesc);
  // add filter element to fontfile dictionary
  lName    := TPdfName.CreateName('FlateDecode');
  FtDesc.FDict.AddElement('Filter', lName);
  // add length1 element to fontfile dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(uFontDef.FOriginalSize));
  FtDesc.FDict.AddElement('Length1 ' + IntToStr(NumFonte), Value);
end;

procedure TPdfDocument.CreateImage(ImgWidth, ImgHeight, ImgNumber: integer);
var
  Images: TPdfXRef;
  XRefObjets: TPdfReference;
  lName: TPdfName;
  lDictionary: TPdfDictionary;
  Long: TPdfInteger;
  i: integer;
begin
  // add xref entry
  Images := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Images);
  // add type element to image dictionary
  lName    := TPdfName.CreateName('XObject');
  Images.FDict.AddElement('Type', lName);
  // add subtype element to image dictionary
  lName    := TPdfName.CreateName('Image');
  Images.FDict.AddElement('Subtype', lName);
  // add width element to image dictionary
  Long   := TPdfInteger.CreateInteger(ImgWidth);
  Images.FDict.AddElement('Width', Long);
  // add height element to image dictionary
  Long   := TPdfInteger.CreateInteger(ImgHeight);
  Images.FDict.AddElement('Height', Long);
  // add color space element to image dictionary
  lName    := TPdfName.CreateName('DeviceRGB');
  Images.FDict.AddElement('ColorSpace', lName);
  // add bits per component element to image dictionary
  Long   := TPdfInteger.CreateInteger(8);
  Images.FDict.AddElement('BitsPerComponent', Long);
  // add name element to image dictionary
  lName    := TPdfName.CreateName('I' + IntToStr(ImgNumber));
  Images.FDict.AddElement('Name', lName);
  // add image reference to page dictionary
  for i := 1 to Pred(FGlobalXRefs.Count) do
  begin
    lDictionary := TPdfDictionary(TPdfXRef(FGlobalXRefs[i]).FDict);
    if lDictionary.FElement.Count > 0 then
    begin
      if TPdfName(TPdfDicElement(lDictionary.FElement[0]).FValue).FValue = 'Page' then
      begin
        lDictionary := TPdfDictionary(TPdfDicElement(lDictionary.FElement[lDictionary.ElementParCle('Resources')]).FValue);
        if lDictionary.ElementParCle('XObject') > -1 then
        begin
          lDictionary := TPdfDictionary(TPdfDicElement(lDictionary.FElement[lDictionary.ElementParCle('XObject')]).FValue);
          XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
          lDictionary.AddElement(TPdfName(lName).FValue, XRefObjets);
        end;
      end;
    end;
  end;
end;

function TPdfDocument.CreateContents: integer;
var
  Contents: TPdfXRef;
  XRefObjets: TPdfReference;
  Stream: TPdfStream;
begin
  // add xref entry
  Contents   := TPdfXRef.CreateXRef;
  FGlobalXRefs.Add(Contents);
  Stream     := TPdfStream.CreateStream;
  TPdfXRef(FGlobalXRefs[Pred(FGlobalXRefs.Count)]).FStream := Stream;
  // add contents reference to page dictionary
  XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
  TPdfDictionary(TPdfXRef(FGlobalXRefs[Pred(Pred(FGlobalXRefs.Count))]).FDict).AddElement('Contents', XRefObjets);
  Result     := Pred(FGlobalXRefs.Count);
end;

procedure TPdfDocument.CreateStream(NumeroPage, PageNum: integer);
var
  i: integer;
  Txt: TPdfText;
  Clr: TPdfColor;
  Fnt: TPdfEmbeddedFont;
  Rct: TPdfRectangle;
  Lin: TPdfLineSegment;
  Srf: TPdfSurface;
  Sty: TPdfLineStyle;
  Img: TPdfImage;
begin
  for i := 0 to Pred(PdfPage.Count) do
  begin
    if TPdfElement(PdfPage[i]) is TPdfTexte then
    begin
      if TPdfTexte(PdfPage[i]).PageId = NumeroPage then
      begin
        with TPdfTexte(PdfPage[i]) do
        begin
          if FontName > -1 then
          begin
            Fnt          := TPdfEmbeddedFont.CreateFont(FontName, FontSize);
            // adjust font size to display device
            Fnt.FTxtSize := IntToStr(Round((StrToInt(FontSize) * fpgApplication.Screen_dpi_y) div 72));
            TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Fnt);
            Clr          := TPdfColor.CreateColor(True, Couleur);
            TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Clr);
          end;
          Txt := TPdfText.CreateText(TextPosX, TextPosY, Writting);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Txt);
        end;
      end;
    end;
    if TPdfElement(PdfPage[i]) is TPdfRect then
    begin
      if TPdfRect(PdfPage[i]).PageId = NumeroPage then
      begin
        with TPdfRect(PdfPage[i]) do
        begin
          Clr := TPdfColor.CreateColor(True, RectColor);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Clr);
          if RectStroke then
          begin
            Sty := TPdfLineStyle.CreateLineStyle(RectLineStyle, 0);
            TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Sty);
          end;
          Rct := TPdfRectangle.CreateRectangle(RectThickness, RectLeft, RectBottom, RectWidth, RectHeight, RectFill, RectStroke);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Rct);
        end;
      end;
    end;
    if TPdfElement(PdfPage[i]) is TPdfLine then
    begin
      if TPdfLine(PdfPage[i]).PageId = NumeroPage then
      begin
        with TPdfLine(PdfPage[i]) do
        begin
          Clr := TPdfColor.CreateColor(False, LineColor);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Clr);
          Sty := TPdfLineStyle.CreateLineStyle(LineStyle, 0);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Sty);
          Lin := TPdfLineSegment.CreateLineSegment(LineThikness, LineBeginX, LineBeginY, LineEndX, LineEndY);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Lin);
        end;
      end;
    end;
    if TPdfElement(PdfPage[i]) is TPdfSurf then
    begin
      if TPdfSurf(PdfPage[i]).PageId = NumeroPage then
      begin
        with TPdfSurf(PdfPage[i]) do
        begin
          Clr := TPdfColor.CreateColor(True, SurfColor);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Clr);
          Srf := TPdfSurface.CreateSurface(Points);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Srf);
        end;
      end;
    end;
    if TPdfElement(PdfPage[i]) is TPdfImg then
    begin
      if TPdfImg(PdfPage[i]).PageId = NumeroPage then
      begin
        with TPdfImg(PdfPage[i]) do
        begin
          Img := TPdfImage.CreateImage(ImgLeft, ImgBottom, ImgWidth, ImgHeight, ImgNumber);
          TPdfStream(TPdfXRef(FGlobalXRefs[PageNum]).FStream).AddItem(Img);
        end;
      end;
    end;
  end; { for i... }
end;

constructor TPdfDocument.CreateDocument(const ALayout: TPageLayout; const AZoom: string; const APreferences: Boolean);
var
  Cpt, CptSect, CptPage, NumFont, TreeRoot, ParentPage, PageNum, NumPage: integer;
  OutlineRoot, ParentOutline, PageOutline, NextOutline, NextSect, NewPage, PrevOutline, PrevSect: integer;
  Dictionaire: TPdfDictionary;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  FontName, FtName: string;
begin
  inherited Create;
  FPreferences  := APreferences;
  FPageLayout   := ALayout;
  FZoomValue    := AZoom;
  CreateRefTable;
  CreateTrailer;
  uCatalogue     := CreateCatalog;
  CreateInfo;
  CreatePreferences;
  ParentPage    := 0;
  ParentOutline := 0;
  if Sections.Count > 1 then
  begin
    if Outline then
    begin
      OutlineRoot := CreateOutlines;
      // add outline reference to catalog dictionary
      XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
      TPdfDictionary(TPdfXRef(FGlobalXRefs[uCatalogue]).FDict).AddElement('Outlines', XRefObjets);
      // add useoutline element to catalog dictionary
      Nom         := TPdfName.CreateName('UseOutlines');
      TPdfDictionary(TPdfXRef(FGlobalXRefs[uCatalogue]).FDict).AddElement('PageMode', Nom);
    end;
    TreeRoot := CreatePages(ParentPage);
  end;
  NumPage := 0; // page number identical to the call to PrintPage
  for CptSect := 0 to Pred(Sections.Count) do
  begin
    if Sections.Count > 1 then
    begin
      if Outline then
      begin
        ParentOutline := CreateOutline(OutlineRoot, Succ(CptSect), -1, T_Section(Sections[CptSect]).Title);
        Dictionaire   := TPdfDictionary(TPdfXRef(FGlobalXRefs[OutlineRoot]).FDict);
        TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
        if CptSect = 0 then
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
          TPdfDictionary(TPdfXRef(FGlobalXRefs[OutlineRoot]).FDict).AddElement('First', XRefObjets);
          NextSect   := ParentOutline;
          PrevSect   := Pred(FGlobalXRefs.Count);
        end
        else
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
          TPdfDictionary(TPdfXRef(FGlobalXRefs[NextSect]).FDict).AddElement('Next', XRefObjets);
          XRefObjets := TPdfReference.CreateReference(PrevSect);
          TPdfDictionary(TPdfXRef(FGlobalXRefs[ParentOutline]).FDict).AddElement('Prev', XRefObjets);
          NextSect   := ParentOutline;
          if CptSect < Pred(Sections.Count) then
            PrevSect := Pred(FGlobalXRefs.Count);
        end;
        if CptSect = Pred(Sections.Count) then
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
          TPdfDictionary(TPdfXRef(FGlobalXRefs[OutlineRoot]).FDict).AddElement('Last', XRefObjets);
        end;
      end;
      ParentPage := CreatePages(TreeRoot);
    end
    else
      ParentPage := CreatePages(ParentPage);
    for CptPage  := 0 to Pred(T_Section(Sections[CptSect]).Pages.Count) do
    begin
      with T_Section(Sections[CptSect]) do
        NewPage := CreatePage(ParentPage, Paper.H, Paper.W, Succ(NumPage));
      // add zoom factor to catalog dictionary
      if (CptSect = 0) and (CptPage = 0) then
      begin
        XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
        Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[ElementParNom('Catalog')]).FDict);
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('OpenAction')]).FValue).AddItem(XRefObjets);
        Nom         := TPdfName.CreateName('XYZ null null ' + FormatFloat('0.##', StrToInt(FZoomValue) / 100));
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('OpenAction')]).FValue).AddItem(Nom);
      end;
      Inc(NumPage);
      PageNum := CreateContents; // pagenum = object number in the pdf file
      CreateStream(NumPage, PageNum);
      if (Sections.Count > 1) and Outline then
      begin
        PageOutline := CreateOutline(ParentOutline, Succ(CptSect), Succ(Cptpage), T_Section(Sections[CptSect]).Title);
        Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[ParentOutline]).FDict);
        TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
        // add page reference to outline destination
        Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[PageOutline]).FDict);
        XRefObjets  := TPdfReference.CreateReference(NewPage);
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(XRefObjets);
        // add display control name to outline destination
        Nom         := TPdfName.CreateName('Fit');
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(Nom);
        if CptPage = 0 then
        begin
          XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
          TPdfDictionary(TPdfXRef(FGlobalXRefs[ParentOutline]).FDict).AddElement('First', XRefObjets);
          NextOutline := PageOutline;
          PrevOutline := Pred(FGlobalXRefs.Count);
          // add page reference to parent outline destination
          Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[ParentOutline]).FDict);
          XRefObjets  := TPdfReference.CreateReference(NewPage);
          TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(XRefObjets);
          // add display control name to outline destination
          Nom         := TPdfName.CreateName('Fit');
          TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(Nom);
        end
        else
        begin
          XRefObjets  := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
          TPdfDictionary(TPdfXRef(FGlobalXRefs[NextOutline]).FDict).AddElement('Next', XRefObjets);
          XRefObjets  := TPdfReference.CreateReference(PrevOutline);
          TPdfDictionary(TPdfXRef(FGlobalXRefs[PageOutline]).FDict).AddElement('Prev', XRefObjets);
          NextOutline := PageOutline;
          if CptPage < Pred(T_Section(Sections[CptSect]).Pages.Count) then
            PrevOutline := Pred(FGlobalXRefs.Count);
        end;
        if CptPage = Pred(T_Section(Sections[CptSect]).Pages.Count) then
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FGlobalXRefs.Count));
          TPdfDictionary(TPdfXRef(FGlobalXRefs[ParentOutline]).FDict).AddElement('Last', XRefObjets);
        end;
      end;
    end;
  end;
  if Sections.Count > 1 then
  begin
    // update count in root parent pages dictionary
    Dictionaire := TPdfDictionary(TPdfXRef(FGlobalXRefs[TreeRoot]).FDict);
    TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).Value := T_Section(Sections[CptSect]).TotPages;
  end;
  if FontDirectory = '' then
    FontDirectory := ExtractFilePath(ParamStr(0));
  // select the font type
  NumFont := 0;
  if Fonts.Count > 0 then
  begin
    for Cpt := 0 to Pred(Fonts.Count) do
    begin
      FontName := ExtractBaseFontName(T_Font(Fonts[Cpt]).GetFont.FontDesc);
      if Pos('-', FontName) > 0 then
        FtName := Copy(FontName, 1, Pred(Pos('-', FontName)))
      else
        FtName := FontName;
      if (Lowercase(FtName) = 'courier') or (Lowercase(FtName) = 'helvetica') or (Lowercase(FtName) = 'times') then
      begin
        FontName := Uppercase(FontName[1]) + Copy(FontName, 2, Pred(Length(FontName)));
        CreateStdFont(FontName, NumFont);
      end
      else if LoadFont(FontName) = 'TrueType' then
        CreateTtfFont(NumFont)
      else
        CreateTp1Font(NumFont);  // not implemented yet
      Inc(NumFont);
    end;
  end;
  if Images.Count > 0 then
    for Cpt := 0 to Pred(Images.Count) do
      CreateImage(TfpgImage(Images[Cpt]).Width, TfpgImage(Images[Cpt]).Height, Cpt);
  TPdfInteger(TPdfDicElement(uDictionary.FElement[uDictionary.ElementParCle('Size')]).FValue).FValue := FGlobalXRefs.Count;
end;

destructor TPdfDocument.Destroy;
var
  Cpt: integer;
begin
  uDictionary.Free;
  if FGlobalXRefs.Count > 0 then
    for Cpt := 0 to Pred(FGlobalXRefs.Count) do
      TPdfXRef(FGlobalXRefs[Cpt]).Free;
  FGlobalXRefs.Free;
  inherited;
end;

procedure TPdfDocument.WriteDocument(const AStream: TStream);
var
  Cpt, XRefPos: integer;
begin
  AStream.Position := 0;
  WriteString(PDF_VERSION + CRLF, AStream);
  // write numbered indirect objects
  for Cpt := 1 to Pred(FGlobalXRefs.Count) do
  begin
    XRefPos := AStream.Position;
    WriteObject(Cpt, AStream);
    TPdfXRef(FGlobalXRefs[Cpt]).Offset := XRefPos;
  end;
  XRefPos := AStream.Position;
  // write xref table
  WriteString('xref' + CRLF + '0 ' + IntToStr(FGlobalXRefs.Count) + CRLF, AStream);
  with TPdfXRef(FGlobalXRefs[0]) do
    WriteString(IntToString(Offset, 10) + ' ' + IntToString(PDF_MAX_GEN_NUM, 5) + ' f' + CRLF, AStream);
  WriteXRefTable(AStream);
  // write uDictionary
  WriteString('trailer' + CRLF, AStream);
  uDictionary.WriteDictionary(-1, AStream);
  // write offset of last xref table
  WriteString(CRLF + 'startxref' + CRLF + IntToStr(XRefPos) + CRLF, AStream);
  WriteString(PDF_FILE_END, AStream);
end;

end.

