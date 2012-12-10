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
    This unit forms part of the PDF Reporting Engine. This unit
    produces the PDF file.

    The PDF Reporting Engine was originally written by
    Jean-Marc Levecque <jean-marc.levecque@jmlesite.fr>
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
  TPdfObjet = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TPdfBoolean = class(TPdfObjet)
  private
    FValue: Boolean;
  protected
    procedure WriteBoolean(const AFlux: TStream);
  public
    constructor CreateBoolean(const AValue: Boolean);
    destructor Destroy; override;
  end;

  TPdfInteger = class(TPdfObjet)
  private
    FValue: integer;
  protected
    procedure WriteInteger(const AFlux: TStream);
    procedure IncrementeInteger;
    property Value: integer read FValue write FValue;
  public
    constructor CreateInteger(const AValue: integer);
    destructor Destroy; override;
  end;

  TPdfReference = class(TPdfObjet)
  private
    FValue: integer;
  protected
    procedure WriteReference(const AFlux: TStream);
  public
    constructor CreateReference(const AValue: integer);
    destructor Destroy; override;
  end;

  TPdfName = class(TPdfObjet)
  private
    FValue: string;
  protected
    procedure WriteName(const AFlux: TStream);
  public
    constructor CreateName(const AValue: string);
    destructor Destroy; override;
  end;

  TPdfString = class(TPdfObjet)
  private
    FValue: string;
  protected
    procedure WriteString(const AFlux: TStream);
  public
    constructor CreateString(const AValue: string);
    destructor Destroy; override;
  end;

  TPdfArray = class(TPdfObjet)
  private
    FArray: TList;
  protected
    procedure WriteArray(const AFlux: TStream);
    procedure AddItem(const AValue: TPdfObjet);
  public
    constructor CreateArray;
    destructor Destroy; override;
  end;

  TPdfStream = class(TPdfObjet)
  private
    FStream: TList;
  protected
    procedure WriteStream(const AFlux: TStream);
    procedure AddItem(const AValue: TPdfObjet);
  public
    constructor CreateStream;
    destructor Destroy; override;
  end;

  TPdfFonte = class(TPdfObjet)
  private
    FTxtFont: integer;
    FTxtSize: string;
  protected
    procedure WriteFonte(const AFlux: TStream);
    function WriteFonteStream(const FFlux: TMemoryStream; const AFlux: TStream): int64;
  public
    constructor CreateFonte(const AFont: integer; const ASize: string);
    destructor Destroy; override;
  end;

  TPdfText = class(TPdfObjet)
  private
    FTxtPosX: single;
    FTxtPosY: single;
    FTxtText: TPdfString;
  protected
    procedure WriteText(const AFlux: TStream);
  public
    constructor CreateText(const APosX, APosY: single; const AText: string);
    destructor Destroy; override;
  end;

  TPdfLigne = class(TPdfObjet)
  private
    FEpais: single;
    FStaX: single;
    FStaY: single;
    FEndX: single;
    FEndY: single;
  protected
    procedure WriteLigne(const AFlux: TStream);
  public
    constructor CreateLigne(const AEpais, AStaX, AStaY, AEndX, AEndY: single);
    destructor Destroy; override;
  end;

  TPdfRectangle = class(TPdfObjet)
  private
    FEpais: single;
    FRecX: single;
    FRecY: single;
    FRecW: single;
    FRecH: single;
    FFill: Boolean;
    FStroke: Boolean;
  protected
    procedure WriteRectangle(const AFlux: TStream);
  public
    constructor CreateRectangle(const AEpais, APosX, APosY, AWidth, AHeight: single; const AFill, AStroke: Boolean);
    destructor Destroy; override;
  end;

  TRefPos = record
    X: single;
    Y: single;
  end;

  T_Points = array of TRefPos;

  TPdfSurface = class(TPdfObjet)
  private
    FPoints: T_Points;
  protected
    procedure WriteSurface(const AFlux: TStream);
  public
    constructor CreateSurface(const APoints: T_Points);
    destructor Destroy; override;
  end;

  TPdfImage = class(TPdfObjet)
  private
    FNumber: integer;
    FLeft: single;
    FBottom: single;
    FWidth: integer;
    FHeight: integer;
  protected
    function WriteImageStream(const ANumber: integer; AFlux: TStream): int64;
    procedure WriteImage(const AFlux: TStream);
  public
    constructor CreateImage(const ALeft, ABottom: single; AWidth, AHeight, ANumber: integer);
    destructor Destroy; override;
  end;

  TPdfLineStyle = class(TPdfObjet)
  private
    FDash: TfpgLineStyle;
    FPhase: integer;
  protected
    procedure WriteLineStyle(const AFlux: TStream);
  public
    constructor CreateLineStyle(ADash: TfpgLineStyle; APhase: integer);
    destructor Destroy; override;
  end;

  TPdfColor = class(TPdfObjet)
  private
    FRed: string;
    FGreen: string;
    FBlue: string;
    FStroke: Boolean;
  protected
    procedure WriteColor(const AFlux: TStream);
  public
    constructor CreateColor(const AStroke: Boolean; AColor: TfpgColor);
    destructor Destroy; override;
  end;

  TPdfDicElement = class(TObject)
  private
    FKey: TPdfName;
    FValue: TPdfObjet;
  protected
    procedure WriteDicElement(const AFlux: TStream);
  public
    constructor CreateDicElement(const AKey: string; const AValue: TPdfObjet);
    destructor Destroy; override;
  end;

  TPdfDictionary = class(TPdfObjet)
  private
    FElement: TList; // list of TPdfDicElement
  protected
    procedure AddElement(const AKey: string; const AValue: TPdfObjet);
    function ElementParCle(const AValue: string): integer;
    procedure WriteDictionary(const AObjet: integer; const AFlux: TStream);
  public
    constructor CreateDictionary;
    destructor Destroy; override;
  end;

  TPdfXRef = class(TObject)
  private
    FOffset: integer;
    FObjet: TPdfDictionary;
    FStream: TPdfStream;
  protected
    procedure WriteXRef(const AFlux: TStream);
  public
    constructor CreateXRef;
    destructor Destroy; override;
    property Offset: integer read FOffset write FOffset;
  end;

  TPageLayout = (lSingle, lTwo, lContinuous);

  TPdfDocument = class(TObject)
  private
    FPreferences: Boolean;
    FPageLayout: TPageLayout;
    FZoomValue: string;
    FXRefObjets: TList; // list of TPdfXRef
  protected
    function ElementParNom(const AValue: string): integer;
    procedure WriteXRefTable(const AFlux: TStream);
    procedure WriteObjet(const AObjet: integer; const AFlux: TStream);
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
    function LoadFont(NomFonte: string): string;
    procedure CreateTtfFont(const NumFonte: integer);
    procedure CreateTp1Font(const NumFonte: integer);
    procedure CreateFontDescriptor(const NumFonte: integer);
    procedure CreateFontWidth;
    procedure CreateFontFile(const NumFonte: integer);
    procedure CreateImage(ImgWidth, ImgHeight, NumImg: integer);
    function CreateContents: integer;
    procedure CreateStream(NumeroPage, PageNum: integer);
  public
    constructor CreateDocument(const ALayout: TPageLayout = lSingle; const AZoom: string = '100'; const APreferences: Boolean = True);
    destructor Destroy; override;
    procedure WriteDocument(const AFlux: TStream);
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
  PDF_LANG_STRING = 'fr';

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
  Trailer: TPdfDictionary;
  CurrentColor: string;
  CurrentWidth: string;
  Catalogue: integer;
  FontDef: TFontDef;
  Flux: TMemoryStream;
  FontFiles: array of string;

// utility functions

function InsertEscape(const AValue: string): string;
var
  Chaine: string;
begin
  Result := '';
  Chaine := AValue;
  if Pos('\', Chaine) > 0 then
    Chaine := AnsiReplaceStr(Chaine, '\', '\\');
  if Pos('(', Chaine) > 0 then
    Chaine := AnsiReplaceStr(Chaine, '(', '\(');
  if Pos(')', Chaine) > 0 then
    Chaine := AnsiReplaceStr(Chaine, ')', '\)');
  Result := Chaine;
  //while Pos('\',Chaine)> 0 do
  //  begin
  //  Result:= Result+Copy(Chaine,1,Pred(Pos('\',Chaine)))+'\\';
  //  Chaine:= Copy(Chaine,Succ(Pos('\',Chaine)),Length(Chaine)-Pos('\',Chaine));
  //  end;
  //Chaine:= Result+Chaine;
  //Result:= '';
  //while Pos('(',Chaine)> 0 do
  //  begin
  //  Result:= Result+Copy(Chaine,1,Pred(Pos('(',Chaine)))+'\(';
  //  Chaine:= Copy(Chaine,Succ(Pos('(',Chaine)),Length(Chaine)-Pos('(',Chaine));
  //  end;
  //Chaine:= Result+Chaine;
  //Result:= '';
  //while Pos(')',Chaine)> 0 do
  //  begin
  //  Result:= Result+Copy(Chaine,1,Pred(Pos(')',Chaine)))+'\)';
  //  Chaine:= Copy(Chaine,Succ(Pos(')',Chaine)),Length(Chaine)-Pos(')',Chaine));
  //  end;
  //Result:= Result+Chaine;
end;

procedure WriteChaine(const Valeur: string; AFlux: TStream);
begin
  AFlux.Write(PChar(Valeur)^, Length(Valeur));
end;

function IntToChaine(const Valeur: integer; const Long: integer): string;
var
  Chaine: string;
  Cpt: integer;
begin
  Result := '';
  Chaine := IntToStr(Valeur);
  if Length(Chaine) < Long then
    for Cpt := Succ(Length(Chaine)) to Long do
      Result := Result + '0';
  Result := Result + Chaine;
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

constructor TPdfObjet.Create;
begin
  // to be implemented by descendents
end;

destructor TPdfObjet.Destroy;
begin
  inherited;
end;

procedure TPdfBoolean.WriteBoolean(const AFlux: TStream);
begin
  if FValue then
    WriteChaine('true', AFlux)
  else
    WriteChaine('false', AFlux);
end;

constructor TPdfBoolean.CreateBoolean(const AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TPdfBoolean.Destroy;
begin
  inherited;
end;

procedure TPdfInteger.WriteInteger(const AFlux: TStream);
begin
  WriteChaine(IntToStr(FValue), AFlux);
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

destructor TPdfInteger.Destroy;
begin
  inherited;
end;

procedure TPdfReference.WriteReference(const AFlux: TStream);
begin
  WriteChaine(IntToStr(FValue) + ' 0 R', AFlux);
end;

constructor TPdfReference.CreateReference(const AValue: integer);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TPdfReference.Destroy;
begin
  inherited;
end;

procedure TPdfName.WriteName(const AFlux: TStream);
begin
  if FValue <> '' then
    if Pos('Length1', FValue) > 0 then
      WriteChaine('/Length1', AFlux)
    else
      WriteChaine('/' + FValue, AFlux);
end;

constructor TPdfName.CreateName(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

destructor TPdfName.Destroy;
begin
  inherited;
end;

procedure TPdfString.WriteString(const AFlux: TStream);
begin
  WriteChaine('(' + Utf8ToAnsi(FValue) + ')', AFlux);
end;

constructor TPdfString.CreateString(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
  if (Pos('(', FValue) > 0) or (Pos(')', FValue) > 0) or (Pos('\', FValue) > 0) then
    FValue := InsertEscape(FValue);
end;

destructor TPdfString.Destroy;
begin
  inherited;
end;

procedure TPdfArray.WriteArray(const AFlux: TStream);
var
  Cpt: integer;
begin
  WriteChaine('[', AFlux);
  for Cpt := 0 to Pred(FArray.Count) do
  begin
    if Cpt > 0 then
      WriteChaine(' ', AFlux);
    if TPdfObjet(FArray[Cpt]) is TPdfInteger then
      TPdfInteger(FArray[Cpt]).WriteInteger(AFlux);
    if TPdfObjet(FArray[Cpt]) is TPdfReference then
      TPdfReference(FArray[Cpt]).WriteReference(AFlux);
    if TPdfObjet(FArray[Cpt]) is TPdfName then
      TPdfName(FArray[Cpt]).WriteName(AFlux);
  end;
  WriteChaine(']', AFlux);
end;

procedure TPdfArray.AddItem(const AValue: TPdfObjet);
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
      if TPdfObjet(FArray[Cpt]) is TPdfInteger then
        TPdfInteger(FArray[Cpt]).Free
      else if TPdfObjet(FArray[Cpt]) is TPdfReference then
        TPdfReference(FArray[Cpt]).Free
      else if TPdfObjet(FArray[Cpt]) is TPdfName then
        TPdfName(FArray[Cpt]).Free;
  FArray.Free;
  inherited;
end;

procedure TPdfStream.WriteStream(const AFlux: TStream);
var
  Cpt: integer;
begin
  for Cpt := 0 to Pred(FStream.Count) do
  begin
    if TPdfObjet(FStream[Cpt]) is TPdfFonte then
      TPdfFonte(FStream[Cpt]).WriteFonte(AFlux);
    if TPdfObjet(FStream[Cpt]) is TPdfColor then
      TPdfColor(FStream[Cpt]).WriteColor(AFlux);
    if TPdfObjet(FStream[Cpt]) is TPdfText then
      TPdfText(FStream[Cpt]).WriteText(AFlux);
    if TPdfObjet(FStream[Cpt]) is TPdfRectangle then
      TPdfRectangle(FStream[Cpt]).WriteRectangle(AFlux);
    if TPdfObjet(FStream[Cpt]) is TPdfLigne then
      TPdfLigne(FStream[Cpt]).WriteLigne(AFlux);
    if TPdfObjet(FStream[Cpt]) is TPdfLineStyle then
      TPdfLineStyle(FStream[Cpt]).WriteLineStyle(AFlux);
    if TPdfObjet(FStream[Cpt]) is TPdfSurface then
      TPdfSurface(FStream[Cpt]).WriteSurface(AFlux);
    if TPdfObjet(FStream[Cpt]) is TPdfImage then
      TPdfImage(FStream[Cpt]).WriteImage(AFlux);
  end;
end;

procedure TPdfStream.AddItem(const AValue: TPdfObjet);
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
      if TPdfObjet(FStream[Cpt]) is TPdfFonte then
        TPdfFonte(FStream[Cpt]).Free
      else if TPdfObjet(FStream[Cpt]) is TPdfColor then
        TPdfColor(FStream[Cpt]).Free
      else if TPdfObjet(FStream[Cpt]) is TPdfText then
        TPdfText(FStream[Cpt]).Free
      else if TPdfObjet(FStream[Cpt]) is TPdfRectangle then
        TPdfRectangle(FStream[Cpt]).Free
      else if TPdfObjet(FStream[Cpt]) is TPdfLigne then
        TPdfLigne(FStream[Cpt]).Free
      else if TPdfObjet(FStream[Cpt]) is TPdfLineStyle then
        TPdfLineStyle(FStream[Cpt]).Free
      else if TPdfObjet(FStream[Cpt]) is TPdfSurface then
        TPdfSurface(FStream[Cpt]).Free
      else if TPdfObjet(FStream[Cpt]) is TPdfImage then
        TPdfImage(FStream[Cpt]).Free;
    end;
  end;
  FStream.Free;
  inherited;
end;

procedure TPdfFonte.WriteFonte(const AFlux: TStream);
begin
  WriteChaine('/F' + IntToStr(FTxtFont) + ' ' + FTxtSize + ' Tf' + CRLF, AFlux);
end;

function TPdfFonte.WriteFonteStream(const FFlux: TMemoryStream; const AFlux: TStream): int64;
var
  BeginFlux, EndFlux: int64;
begin
  WriteChaine(CRLF + 'stream' + CRLF, AFlux);
  BeginFlux := AFlux.Position;
  FFlux.SaveToStream(AFlux);
  EndFlux   := AFlux.Position;
  Result    := EndFlux - BeginFlux;
  WriteChaine(CRLF, AFlux);
  WriteChaine('endstream', AFlux);
end;

constructor TPdfFonte.CreateFonte(const AFont: integer; const ASize: string);
begin
  inherited Create;
  FTxtFont := AFont;
  FTxtSize := ASize;
end;

destructor TPdfFonte.Destroy;
begin
  inherited;
end;

procedure TPdfText.WriteText(const AFlux: TStream);
begin
  WriteChaine('BT' + CRLF, AFlux);
  WriteChaine(FormatFloat('0.##', FTxtPosX) + ' ' + FormatFloat('0.##', FTxtPosY) + ' Td' + CRLF, AFlux);
  TPdfString(FTxtText).WriteString(AFlux);
  WriteChaine(' Tj' + CRLF, AFlux);
  WriteChaine('ET' + CRLF, AFlux);
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

procedure TPdfLigne.WriteLigne(const AFlux: TStream);
begin
  if (FormatFloat('0.##', FEpais) + ' w') <> CurrentWidth then
  begin
    WriteChaine('1 J' + CRLF, AFlux);
    WriteChaine(FormatFloat('0.##', FEpais) + ' w' + CRLF, AFlux);
    CurrentWidth := FormatFloat('0.##', FEpais) + ' w';
  end;
  WriteChaine(FormatFloat('0.##', FStaX) + ' ' + FormatFloat('0.##', FStaY) + ' m' + CRLF, AFlux);
  WriteChaine(FormatFloat('0.##', FEndX) + ' ' + FormatFloat('0.##', FEndY) + ' l' + CRLF, AFlux);
  WriteChaine('S' + CRLF, AFlux);
end;

constructor TPdfLigne.CreateLigne(const AEpais, AStaX, AStaY, AEndX, AEndY: single);
begin
  inherited Create;
  FEpais := AEpais;
  FStaX  := AStaX;
  FStaY  := AStaY;
  FEndX  := AEndX;
  FEndY  := AEndY;
end;

destructor TPdfLigne.Destroy;
begin
  inherited;
end;

procedure TPdfRectangle.WriteRectangle(const AFlux: TStream);
begin
  if FStroke then
  begin
    if (FormatFloat('0.##', FEpais) + ' w') <> CurrentWidth then
    begin
      WriteChaine('1 J' + CRLF, AFlux);
      WriteChaine(FormatFloat('0.##', FEpais) + ' w' + CRLF, AFlux);
      CurrentWidth := FormatFloat('0.##', FEpais) + ' w';
    end;
  end;
  WriteChaine(FormatFloat('0.##', FRecX) + ' ' + FormatFloat('0.##', FRecY) + ' ' + FormatFloat('0.##', FRecW) + ' ' + FormatFloat('0.##', FRecH) + ' re' + CRLF, AFlux);
  if FStroke then
    WriteChaine('S' + CRLF, AFlux);
  if FFill then
    WriteChaine('f' + CRLF, AFlux);
end;

constructor TPdfRectangle.CreateRectangle(const AEpais, APosX, APosY, AWidth, AHeight: single; const AFill, AStroke: Boolean);
begin
  inherited Create;
  FEpais  := AEpais;
  FRecX   := APosX;
  FRecY   := APosY;
  FRecW   := AWidth;
  FRecH   := AHeight;
  FFill   := AFill;
  FStroke := AStroke;
end;

destructor TPdfRectangle.Destroy;
begin
  inherited;
end;

procedure TPdfSurface.WriteSurface(const AFlux: TStream);
var
  Cpt: integer;
begin
  WriteChaine(FormatFloat('0.##', FPoints[0].X) + ' ' + FormatFloat('0.##', FPoints[0].Y) + ' m' + CRLF, AFlux);
  for Cpt := 1 to Pred(Length(FPoints)) do
    WriteChaine(FormatFloat('0.##', FPoints[Cpt].X) + ' ' + FormatFloat('0.##', FPoints[Cpt].Y) + ' l' + CRLF, AFlux);
  WriteChaine('h' + CRLF, AFlux);
  WriteChaine('f' + CRLF, AFlux);
end;

constructor TPdfSurface.CreateSurface(const APoints: T_Points);
begin
  inherited Create;
  FPoints := APoints;
end;

destructor TPdfSurface.Destroy;
begin
  inherited;
end;

function TPdfImage.WriteImageStream(const ANumber: integer; AFlux: TStream): int64;
var
  CptW, CptH: integer;
  BeginFlux, EndFlux: int64;
begin
  WriteChaine(CRLF + 'stream' + CRLF, AFlux);
  BeginFlux := AFlux.Position;
  for CptH := 0 to Pred(TfpgImage(Images[ANumber]).Height) do
  begin
    for CptW := 0 to Pred(TfpgImage(Images[ANumber]).Width) do
    begin
      AFlux.WriteByte(fpgGetRed(TfpgImage(Images[ANumber]).Colors[CptW, CptH]));
      AFlux.WriteByte(fpgGetGreen(TfpgImage(Images[ANumber]).Colors[CptW, CptH]));
      AFlux.WriteByte(fpgGetBlue(TfpgImage(Images[ANumber]).Colors[CptW, CptH]));
    end;
  end;
  EndFlux := AFlux.Position;
  Result  := EndFlux - BeginFlux;
  WriteChaine(CRLF, AFlux);
  WriteChaine('endstream', AFlux);
end;

procedure TPdfImage.WriteImage(const AFlux: TStream);
begin
  WriteChaine('q' + CRLF, AFlux);
  WriteChaine(IntToStr(FWidth) + ' 0 0 ' + IntToStr(FHeight) + ' ' + FormatFloat('0.##', FLeft) + ' ' + FormatFloat('0.##', FBottom) + ' cm' + CRLF, AFlux);
  WriteChaine('/I' + IntToStr(FNumber) + ' Do ' + CRLF, AFlux);
  WriteChaine('Q' + CRLF, AFlux);
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

destructor TPdfImage.Destroy;
begin
  inherited;
end;

procedure TPdfLineStyle.WriteLineStyle(const AFlux: TStream);
begin
  WriteChaine('[', AFlux);
  case FDash of
    lsDash:
      WriteChaine('5 5', AFlux);
    lsDot:
      WriteChaine('2 2', AFlux);
    lsDashDot:
      WriteChaine('5 2 2 2', AFlux);
    lsDashDotDot:
      WriteChaine('5 2 2 2 2 2', AFlux);
  end;
  WriteChaine('] ' + IntToStr(FPhase) + ' d' + CRLF, AFlux);
end;

constructor TPdfLineStyle.CreateLineStyle(ADash: TfpgLineStyle; APhase: integer);
begin
  inherited Create;
  FDash  := ADash;
  FPhase := APhase;
end;

destructor TPdfLineStyle.Destroy;
begin
  inherited;
end;

procedure TPdfColor.WriteColor(const AFlux: TStream);
begin
  if FStroke then
  begin
    if (FRed + ' ' + FGreen + ' ' + FBlue + ' rg') <> CurrentColor then
    begin
      WriteChaine(FRed + ' ' + FGreen + ' ' + FBlue + ' rg' + CRLF, AFlux);
      CurrentColor := FRed + ' ' + FGreen + ' ' + FBlue + ' rg';
    end;
  end
  else if (FRed + ' ' + FGreen + ' ' + FBlue + ' RG') <> CurrentColor then
  begin
    WriteChaine(FRed + ' ' + FGreen + ' ' + FBlue + ' RG' + CRLF, AFlux);
    CurrentColor := FRed + ' ' + FGreen + ' ' + FBlue + ' RG';
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

destructor TPdfColor.Destroy;
begin
  inherited;
end;

procedure TPdfDicElement.WriteDicElement(const AFlux: TStream);
begin
  FKey.WriteName(AFlux);
  WriteChaine(' ', AFlux);
  if FValue is TPdfBoolean then
    TPdfBoolean(FValue).WriteBoolean(AFlux);
  if FValue is TPdfInteger then
    TPdfInteger(FValue).WriteInteger(AFlux);
  if FValue is TPdfReference then
    TPdfReference(FValue).WriteReference(AFlux);
  if FValue is TPdfName then
    TPdfName(FValue).WriteName(AFlux);
  if FValue is TPdfString then
    TPdfString(FValue).WriteString(AFlux);
  if FValue is TPdfArray then
    TPdfArray(FValue).WriteArray(AFlux);
  if FValue is TPdfDictionary then
    TPdfDictionary(FValue).WriteDictionary(-1, AFlux);
  WriteChaine(CRLF, AFlux);
end;

constructor TPdfDicElement.CreateDicElement(const AKey: string; const AValue: TPdfObjet);
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

procedure TPdfDictionary.AddElement(const AKey: string; const AValue: TPdfObjet);
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

procedure TPdfDictionary.WriteDictionary(const AObjet: integer; const AFlux: TStream);
var
  Long: TPdfInteger;
  Cpt, NumImg, NumFnt: integer;
  Value: string;
begin
  if TPdfName(TPdfDicElement(FElement[0]).FKey).FValue = '' then
    TPdfDicElement(FElement[0]).WriteDicElement(AFlux)  // write a charwidth array of a font
  else
  begin
    WriteChaine('<<' + CRLF, AFlux);
    if FElement.Count > 0 then
      for Cpt := 0 to Pred(FElement.Count) do
        TPdfDicElement(FElement[Cpt]).WriteDicElement(AFlux);
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
            if (TPdfObjet(TPdfDicElement(FElement[Cpt]).FValue) is TPdfName) and (TPdfName(TPdfDicElement(FElement[Cpt]).FValue).FValue[1] = 'I') then
            begin
              NumImg        := StrToInt(Copy(TPdfName(TPdfDicElement(FElement[Cpt]).FValue).FValue, 2, Length(TPdfName(TPdfDicElement(FElement[Cpt]).FValue).FValue) - 1));
              Flux          := TMemoryStream.Create;
              Flux.Position := 0;
              // write image stream length in xobject dictionary
              Long          := TPdfInteger.CreateInteger(TPdfImage(TPdfXRef(Document.FXRefObjets[AObjet]).FObjet).WriteImageStream(NumImg, Flux));
              TPdfDictionary(TPdfXRef(Document.FXRefObjets[AObjet]).FObjet).AddElement('Length', Long);
              TPdfDicElement(FElement[Pred(FElement.Count)]).WriteDicElement(AFlux);
              Flux.Free;
              WriteChaine('>>', AFlux);
              // write image stream in xobject dictionary
              TPdfImage(TPdfXRef(Document.FXRefObjets[AObjet]).FObjet).WriteImageStream(NumImg, AFlux);
            end;
          end;
          if Pos('Length1', TPdfName(TPdfDicElement(FElement[Cpt]).FKey).FValue) > 0 then
          begin
            Flux   := TMemoryStream.Create;
            Value  := TPdfName(TPdfDicElement(FElement[Cpt]).FKey).FValue;
            NumFnt := StrToInt(Copy(Value, Succ(Pos(' ', Value)), Length(Value) - Pos(' ', Value)));
            Flux.LoadFromFile(FontFiles[NumFnt]);
            // write fontfile stream length in xobject dictionary
            Long   := TPdfInteger.CreateInteger(Flux.Size);
            TPdfDictionary(TPdfXRef(Document.FXRefObjets[AObjet]).FObjet).AddElement('Length', Long);
            TPdfDicElement(FElement[Pred(FElement.Count)]).WriteDicElement(AFlux);
            WriteChaine('>>', AFlux);
            // write fontfile stream in xobject dictionary
            TPdfFonte(TPdfXRef(Document.FXRefObjets[NumFnt]).FObjet).WriteFonteStream(Flux, AFlux);
            Flux.Free;
          end;
        end;
      end; { for Cpt... }
    end; { if FElement.Count... }
    if (NumImg = -1) and (NumFnt = -1) then
      WriteChaine('>>', AFlux);
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

procedure TPdfXRef.WriteXRef(const AFlux: TStream);
begin
  WriteChaine(IntToChaine(FOffset, 10) + ' ' + IntToChaine(0, 5) + ' n' + CRLF, AFlux);
end;

constructor TPdfXRef.CreateXRef;
begin
  inherited Create;
  FOffset := 0;
  FObjet  := TpdfDictionary.CreateDictionary;
  FStream := nil;
end;

destructor TPdfXRef.Destroy;
begin
  FObjet.Free;
  FStream.Free;
  inherited;
end;

function TPdfDocument.ElementParNom(const AValue: string): integer;
var
  Cpt: integer;
begin
  for Cpt := 1 to Pred(FXRefObjets.Count) do
    if TPdfName(TPdfDicElement(TPdfDictionary(TPdfXRef(FXRefObjets[Cpt]).FObjet).FElement[0]).FValue).FValue = AValue then
      Result := Cpt;
end;

procedure TPdfDocument.WriteXRefTable(const AFlux: TStream);
var
  Cpt: integer;
begin
  if FXRefObjets.Count > 1 then
    for Cpt := 1 to Pred(FXRefObjets.Count) do
      TPdfXRef(FXRefObjets[Cpt]).WriteXRef(AFlux);
end;

procedure TPdfDocument.WriteObjet(const AObjet: integer; const AFlux: TStream);
var
  Long: TPdfInteger;
  Flux: TMemoryStream;
begin
  WriteChaine(IntToStr(AObjet) + ' 0 obj' + CRLF, AFlux);
  if TPdfXRef(FXRefObjets[AObjet]).FStream = nil then
    TPdfDictionary(TPdfXRef(FXRefObjets[AObjet]).FObjet).WriteDictionary(AObjet, AFlux)
  else
  begin
    Flux          := TMemoryStream.Create;
    Flux.Position := 0;
    CurrentColor  := '';
    CurrentWidth  := '';
    TPdfXRef(FXRefObjets[AObjet]).FStream.WriteStream(Flux);
    // write stream length element in contents dictionary
    Long          := TPdfInteger.CreateInteger(Flux.Size);
    TPdfDictionary(TPdfXRef(FXRefObjets[AObjet]).FObjet).AddElement('Length', Long);
    Flux.Free;
    TPdfXRef(FXRefObjets[AObjet]).FObjet.WriteDictionary(-1, AFlux);
    // write stream in contents dictionary
    CurrentColor := '';
    CurrentWidth := '';
    WriteChaine(CRLF + 'stream' + CRLF, AFlux);
    TPdfXRef(FXRefObjets[AObjet]).FStream.WriteStream(AFlux);
    WriteChaine('endstream', AFlux);
  end;
  WriteChaine(CRLF + 'endobj' + CRLF + CRLF, AFlux);
end;

procedure TPdfDocument.CreateRefTable;
var
  XRefObjet: TPdfXRef;
begin
  FXRefObjets := TList.Create;
  // add first xref entry
  XRefObjet   := TPdfXRef.CreateXRef;
  FXRefObjets.Add(XRefObjet);
end;

procedure TPdfDocument.CreateTrailer;
var
  XRefObjets: TPdfInteger;
begin
  Trailer    := TPdfDictionary.CreateDictionary;
  // add size trailer element
  XRefObjets := TPdfInteger.CreateInteger(FXRefObjets.Count);
  Trailer.AddElement('Size', XRefObjets);
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
  FXRefObjets.Add(Catalog);
  // add root trailer element
  XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  Trailer.AddElement('Root', XRefObjets);
  // add type element to catalog dictionary
  Nom        := TPdfName.CreateName('Catalog');
  Catalog.FObjet.AddElement('Type', Nom);
  // add pagelayout element to catalog dictionary
  case FPageLayout of
    lSingle:
      Nom := TPdfName.CreateName('SinglePage');
    lTwo:
      Nom := TPdfName.CreateName('TwoColumnLeft');
    lContinuous:
      Nom := TPdfName.CreateName('OneColumn');
  end;
  Catalog.FObjet.AddElement('PageLayout', Nom);
  // add openaction element to catalog dictionary
  Table  := TPdfArray.CreateArray;
  Catalog.FObjet.AddElement('OpenAction', Table);
  Result := Pred(FXRefObjets.Count);
end;

procedure TPdfDocument.CreateInfo;
var
  Info: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfString;
begin
  // add xref entry
  Info       := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Info);
  // add info trailer element
  XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  Trailer.AddElement('Info', XRefObjets);
  TPdfInteger(TPdfDicElement(Trailer.FElement[Trailer.ElementParCle('Size')]).FValue).FValue := FXRefObjets.Count;
  // add title element to info dictionary
  Nom        := TPdfString.CreateString(Infos.Titre);
  Info.FObjet.AddElement('Title', Nom);
  // add author element to info dictionary
  Nom        := TPdfString.CreateString(Infos.Auteur);
  Info.FObjet.AddElement('Author', Nom);
  // add creator element to info dictionary
  Nom        := TPdfString.CreateString(ApplicationName);
  Info.FObjet.AddElement('Creator', Nom);
  // add producer element to info dictionary
  Nom        := TPdfString.CreateString(fpGUIName + ' ' + FPGUI_VERSION);
  Info.FObjet.AddElement('Producer', Nom);
  // add creationdate element to info dictionary
  Nom        := TPdfString.CreateString(DateToPdfDate(Now));
  Info.FObjet.AddElement('CreationDate', Nom);
end;

procedure TPdfDocument.CreatePreferences;
var
  Viewer: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Preference: TPdfBoolean;
begin
  // add xref entry
  Viewer     := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Viewer);
  // add type element to preferences dictionary
  Nom        := TPdfName.CreateName('ViewerPreferences');
  Viewer.FObjet.AddElement('Type', Nom);
  // add preference element to preferences dictionary
  Preference := TPdfBoolean.CreateBoolean(True);
  Viewer.FObjet.AddElement('FitWindow', Preference);
  // add preferences reference to catalog dictionary
  XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  TPdfDictionary(TPdfXRef(FXRefObjets[ElementParNom('Catalog')]).FObjet).AddElement('ViewerPreferences', XRefObjets);
end;

function TPdfDocument.CreatePages(Parent: integer): integer;
var
  Pages: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Table: TPdfArray;
  Count: TPdfInteger;
begin
  // add xref entry
  Pages := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Pages);
  // add type element to pages dictionary
  Nom   := TPdfName.CreateName('Pages');
  Pages.FObjet.AddElement('Type', Nom);
  // add parent reference to pages dictionary if pages is not the root of the page tree
  if Parent > 0 then
  begin
    XRefObjets  := TPdfReference.CreateReference(Parent);
    Pages.FObjet.AddElement('Parent', XRefObjets);
    // increment count in parent pages dictionary
    Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[Parent]).FObjet);
    TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
    // add kid reference in parent pages dictionary
    XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
    TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Kids')]).FValue).AddItem(XRefObjets);
  end
  else
  begin
    // add pages reference to catalog dictionary
    XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
    TPdfDictionary(TPdfXRef(FXRefObjets[ElementParNom('Catalog')]).FObjet).AddElement('Pages', XRefObjets);
  end;
  // add kids element to pages dictionary
  Table  := TPdfArray.CreateArray;
  Pages.FObjet.AddElement('Kids', Table);
  // add count element to pages dictionary
  Count  := TPdfInteger.CreateInteger(0);
  Pages.FObjet.AddElement('Count', Count);
  Result := Pred(FXRefObjets.Count);
end;

function TPdfDocument.CreatePage(Parent, Haut, Larg, PageNum: integer): integer;
var
  Page: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Table: TPdfArray;
  Coord: TPdfInteger;
  Cpt: integer;
begin
  // add xref entry
  Page        := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Page);
  // add type element to page dictionary
  Nom         := TPdfName.CreateName('Page');
  Page.FObjet.AddElement('Type', Nom);
  // add parent reference to page dictionary
  XRefObjets  := TPdfReference.CreateReference(Parent);
  Page.FObjet.AddElement('Parent', XRefObjets);
  // increment count in parent pages dictionary
  Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[Parent]).FObjet);
  TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
  // add kid reference in parent pages dictionary
  XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Kids')]).FValue).AddItem(XRefObjets);
  // add mediabox element to page dictionary
  Table       := TPdfArray.CreateArray;
  Page.FObjet.AddElement('MediaBox', Table);
  // add coordinates in page mediabox
  Dictionaire := TPdfDictionary(TPdfXRef(Page).FObjet);
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
  Page.FObjet.AddElement('Resources', Dictionaire);
  // add procset element in resources element to page dictionary
  Table       := TPdfArray.CreateArray;
  TPdfDictionary(TPdfDicElement(Page.FObjet.FElement[Pred(Page.FObjet.FElement.Count)]).FValue).AddElement('ProcSet', Table);
  // add font element in resources element to page dictionary
  if Fonts.Count > 0 then
  begin
    Dictionaire := TPdfDictionary.CreateDictionary;
    TPdfDictionary(TPdfDicElement(Page.FObjet.FElement[Pred(Page.FObjet.FElement.Count)]).FValue).AddElement('Font', Dictionaire);
  end;
  for Cpt := 0 to Pred(PdfPage.Count) do
  begin
    if TPdfElement(PdfPage[Cpt]) is TPdfImg then
    begin
      if TPdfImg(PdfPage[Cpt]).PageId = PageNum then
      begin
        // add xobject element in resources element to page dictionary
        Dictionaire := TPdfDictionary.CreateDictionary;
        TPdfDictionary(TPdfDicElement(Page.FObjet.FElement[Pred(Page.FObjet.FElement.Count)]).FValue).AddElement('XObject', Dictionaire);
        Break;
      end;
    end;
  end;
  // add pdf element in procset array to page dictionary
  Dictionaire := TPdfDictionary(TPdfDicElement(Page.FObjet.FElement[Pred(Page.FObjet.FElement.Count)]).FValue);
  Nom         := TPdfName.CreateName('PDF');
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(Nom);
  // add text element in procset array to page dictionary
  Nom         := TPdfName.CreateName('Text');
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(Nom);
  // add image element in procset array to page dictionary
  Nom         := TPdfName.CreateName('ImageC');
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(Nom);
  Result      := Pred(FXRefObjets.Count);
end;

function TPdfDocument.CreateOutlines: integer;
var
  Outlines: TPdfXRef;
  Nom: TPdfName;
  Count: TPdfInteger;
begin
  // add xref entry
  Outlines := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Outlines);
  // add type element to outlines dictionary
  Nom      := TPdfName.CreateName('Outlines');
  Outlines.FObjet.AddElement('Type', Nom);
  // add count element to outlines dictionary
  Count    := TPdfInteger.CreateInteger(0);
  Outlines.FObjet.AddElement('Count', Count);
  Result   := Pred(FXRefObjets.Count);
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
  FXRefObjets.Add(Outline);
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
  Outline.FObjet.AddElement('Title', Titre);
  // add parent reference to outline dictionary
  XRefObjets := TPdfReference.CreateReference(Parent);
  Outline.FObjet.AddElement('Parent', XRefObjets);
  // add count element to outline dictionary
  Count  := TPdfInteger.CreateInteger(0);
  Outline.FObjet.AddElement('Count', Count);
  // add dest element to outline dictionary
  Table  := TPdfArray.CreateArray;
  Outline.FObjet.AddElement('Dest', Table);
  Result := Pred(FXRefObjets.Count);
end;

procedure TPdfDocument.CreateStdFont(NomFonte: string; NumFonte: integer);
var
  Fontes: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Cpt: integer;
begin
  if Pos('Italic', NomFonte) > 0 then
    NomFonte := Copy(NomFonte, 1, Pred(Pos('Italic', NomFonte))) + 'Oblique';
  //  AnsiReplaceText(NomFonte,'Italic','Oblique');
  // add xref entry
  Fontes := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Fontes);
  // add type element to font dictionary
  Nom := TPdfName.CreateName('Font');
  Fontes.FObjet.AddElement('Type', Nom);
  // add subtype element to font dictionary
  Nom := TPdfName.CreateName('Type1');
  Fontes.FObjet.AddElement('Subtype', Nom);
  // add encoding element to font dictionary
  Nom := TPdfName.CreateName('WinAnsiEncoding');
  Fontes.FObjet.AddElement('Encoding', Nom);
  // add firstchar element to font dictionary
  Nom := TPdfName.CreateName('32');
  //Nom:= TPdfName.CreateName('0');
  Fontes.FObjet.AddElement('FirstChar', Nom);
  // add lastchar element to font dictionary
  Nom := TPdfName.CreateName('255');
  Fontes.FObjet.AddElement('LastChar', Nom);
  // add basefont element to font dictionary
  Nom := TPdfName.CreateName(NomFonte);
  Fontes.FObjet.AddElement('BaseFont', Nom);
  // add name element to font dictionary
  Nom := TPdfName.CreateName('F' + IntToStr(NumFonte));
  Fontes.FObjet.AddElement('Name', Nom);
  // add font reference to all page dictionary
  for Cpt := 1 to Pred(FXRefObjets.Count) do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[Cpt]).FObjet);
    if Dictionaire.FElement.Count > 0 then
      if TPdfName(TPdfDicElement(Dictionaire.FElement[0]).FValue).FValue = 'Page' then
      begin
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Resources')]).FValue);
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Font')]).FValue);
        XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        Dictionaire.AddElement(TPdfName(Nom).FValue, XRefObjets);
      end;
  end;
  SetLength(FontFiles, Succ(Length(FontFiles)));
  FontFiles[NumFonte] := '';
end;

function TPdfDocument.LoadFont(NomFonte: string): string;
var
  FileTxt: TextFile;
  Ligne: WideString;
begin
  if fpgFileExists(FontDirectory + NomFonte + '.fnt') then
  begin
    AssignFile(FileTxt, FontDirectory + NomFonte + '.fnt');
    Reset(FileTxt);
    while not EOF(FileTxt) do
    begin
      Readln(FileTxt, Ligne);
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'FontType' then
        FontDef.FType         := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'FontName' then
        FontDef.FName         := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'Ascent' then
        FontDef.FAscent       := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'Descent' then
        FontDef.FDescent      := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'CapHeight' then
        FontDef.FCapHeight    := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'Flags' then
        FontDef.FFlags        := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'FontBBox' then
        FontDef.FFontBBox     := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'ItalicAngle' then
        FontDef.FItalicAngle  := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'StemV' then
        FontDef.FStemV        := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'MissingWidth' then
        FontDef.FMissingWidth := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'Encoding' then
        FontDef.FEncoding     := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'FontFile' then
        FontDef.FFile         := FontDirectory + Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'OriginalSize' then
        FontDef.FOriginalSize := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'Diffs' then
        FontDef.FDiffs        := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
      if Copy(Ligne, 1, Pred(Pos('=', Ligne))) = 'CharWidth' then
        FontDef.FCharWidth    := Copy(Ligne, Succ(Pos('=', Ligne)), Length(Ligne) - Pos('=', Ligne));
    end;
    Result := FontDef.FType;
  end
  else
    ShowMessage(Format(rsErrReportFontFileMissing, [NomFonte]));
end;

procedure TPdfDocument.CreateTtfFont(const NumFonte: integer);
var
  Fontes: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Value: TPdfInteger;
  Cpt: integer;
begin
  // add xref entry
  Fontes := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Fontes);
  // add type element to font dictionary
  Nom    := TPdfName.CreateName('Font');
  Fontes.FObjet.AddElement('Type', Nom);
  // add subtype element to font dictionary
  Nom    := TPdfName.CreateName(FontDef.FType);
  Fontes.FObjet.AddElement('Subtype', Nom);
  // add encoding element to font dictionary
  Nom    := TPdfName.CreateName('WinAnsiEncoding');
  Fontes.FObjet.AddElement('Encoding', Nom);
  // add firstchar element to font dictionary
  Value  := TPdfInteger.CreateInteger(32);
  Fontes.FObjet.AddElement('FirstChar', Value);
  // add lastchar element to font dictionary
  Value  := TPdfInteger.CreateInteger(255);
  Fontes.FObjet.AddElement('LastChar', Value);
  // add basefont element to font dictionary
  Nom    := TPdfName.CreateName(FontDef.FName);
  Fontes.FObjet.AddElement('BaseFont', Nom);
  // add name element to font dictionary
  Nom    := TPdfName.CreateName('F' + IntToStr(NumFonte));
  Fontes.FObjet.AddElement('Name', Nom);
  // add font reference to all page dictionary
  for Cpt := 1 to Pred(FXRefObjets.Count) do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[Cpt]).FObjet);
    if Dictionaire.FElement.Count > 0 then
      if TPdfName(TPdfDicElement(Dictionaire.FElement[0]).FValue).FValue = 'Page' then
      begin
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Resources')]).FValue);
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Font')]).FValue);
        XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        Dictionaire.AddElement(TPdfName(Nom).FValue, XRefObjets);
      end;
  end;
  CreateFontDescriptor(NumFonte);
  // add fontdescriptor reference to font dictionary
  XRefObjets          := TPdfReference.CreateReference(FXRefObjets.Count - 2);
  Fontes.FObjet.AddElement('FontDescriptor', XRefObjets);
  CreateFontWidth;
  // add fontwidth reference to font dictionary
  XRefObjets          := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  Fontes.FObjet.AddElement('Widths', XRefObjets);
  SetLength(FontFiles, Succ(Length(FontFiles)));
  FontFiles[NumFonte] := FontDef.FFile;
end;

procedure TPdfDocument.CreateTp1Font(const NumFonte: integer);
begin

end;

procedure TPdfDocument.CreateFontDescriptor(const NumFonte: integer);
var
  FtDesc: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Value: TPdfInteger;
  Table: TPdfArray;
  Dictionaire: TPdfDictionary;
begin
  // add xref entry
  FtDesc := TPdfXRef.CreateXRef;
  FXRefObjets.Add(FtDesc);
  // add type element to fontdescriptor dictionary
  Nom    := TPdfName.CreateName('FontDescriptor');
  FtDesc.FObjet.AddElement('Type', Nom);
  // add fontname element to fontdescriptor dictionary
  Nom    := TPdfName.CreateName(FontDef.FName);
  FtDesc.FObjet.AddElement('FontName', Nom);
  // add ascent element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(FontDef.FAscent));
  FtDesc.FObjet.AddElement('Ascent', Value);
  // add descent element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(FontDef.FDescent));
  FtDesc.FObjet.AddElement('Descent', Value);
  // add capheight element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(FontDef.FCapHeight));
  FtDesc.FObjet.AddElement('CapHeight', Value);
  // add flags element to fontdescriptor dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(FontDef.FFlags));
  FtDesc.FObjet.AddElement('Flags', Value);
  // add fontbbox element to fontdescriptor dictionary
  Table  := TPdfArray.CreateArray;
  FtDesc.FObjet.AddElement('FontBBox', Table);
  // add coordinates in page fontbbox
  while Pos(' ', FontDef.FFontBBox) > 0 do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FtDesc).FObjet);
    Value       := TPdfInteger.CreateInteger(StrToInt(Copy(FontDef.FFontBBox, 1, Pred(Pos(' ', FontDef.FFontBBox)))));
    TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('FontBBox')]).FValue).AddItem(Value);
    FontDef.FFontBBox := Copy(FontDef.FFontBBox, Succ(Pos(' ', FontDef.FFontBBox)), Length(FontDef.FFontBBox) - Pos(' ', FontDef.FFontBBox));
  end;
  // add italicangle element to fontdescriptor dictionary
  Value      := TPdfInteger.CreateInteger(StrToInt(FontDef.FItalicAngle));
  FtDesc.FObjet.AddElement('ItalicAngle', Value);
  // add stemv element to fontdescriptor dictionary
  Value      := TPdfInteger.CreateInteger(StrToInt(FontDef.FStemV));
  FtDesc.FObjet.AddElement('StemV', Value);
  // add missingwidth element to fontdescriptor dictionary
  Value      := TPdfInteger.CreateInteger(StrToInt(FontDef.FMissingWidth));
  FtDesc.FObjet.AddElement('MissingWidth', Value);
  CreateFontFile(NumFonte);
  // add fontfilereference to fontdescriptor dictionary
  XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  FtDesc.FObjet.AddElement('FontFile2', XRefObjets);
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
  FXRefObjets.Add(FtDesc);
  // add element to fontwidth dictionary
  Table  := TPdfArray.CreateArray;
  FtDesc.FObjet.AddElement('', Table);
  // add width values in fontwidth array
  while Pos(' ', FontDef.FCharWidth) > 0 do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FtDesc).FObjet);
    Value       := TPdfInteger.CreateInteger(StrToInt(Copy(FontDef.FCharWidth, 1, Pred(Pos(' ', FontDef.FCharWidth)))));
    TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('')]).FValue).AddItem(Value);
    FontDef.FCharWidth := Copy(FontDef.FCharWidth, Succ(Pos(' ', FontDef.FCharWidth)), Length(FontDef.FCharWidth) - Pos(' ', FontDef.FCharWidth));
  end;
end;

procedure TPdfDocument.CreateFontFile(const NumFonte: integer);
var
  FtDesc: TPdfXRef;
  Nom: TPdfName;
  Value: TPdfInteger;
begin
  // add xref entry
  FtDesc := TPdfXRef.CreateXRef;
  FXRefObjets.Add(FtDesc);
  // add filter element to fontfile dictionary
  Nom    := TPdfName.CreateName('FlateDecode');
  FtDesc.FObjet.AddElement('Filter', Nom);
  // add length1 element to fontfile dictionary
  Value  := TPdfInteger.CreateInteger(StrToInt(FontDef.FOriginalSize));
  FtDesc.FObjet.AddElement('Length1 ' + IntToStr(NumFonte), Value);
end;

procedure TPdfDocument.CreateImage(ImgWidth, ImgHeight, NumImg: integer);
var
  Images: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Long: TPdfInteger;
  Cpt: integer;
begin
  // add xref entry
  Images := TPdfXRef.CreateXRef;
  FXRefObjets.Add(Images);
  // add type element to image dictionary
  Nom    := TPdfName.CreateName('XObject');
  Images.FObjet.AddElement('Type', Nom);
  // add subtype element to image dictionary
  Nom    := TPdfName.CreateName('Image');
  Images.FObjet.AddElement('Subtype', Nom);
  // add width element to image dictionary
  Long   := TPdfInteger.CreateInteger(ImgWidth);
  Images.FObjet.AddElement('Width', Long);
  // add height element to image dictionary
  Long   := TPdfInteger.CreateInteger(ImgHeight);
  Images.FObjet.AddElement('Height', Long);
  // add color space element to image dictionary
  Nom    := TPdfName.CreateName('DeviceRGB');
  Images.FObjet.AddElement('ColorSpace', Nom);
  // add bits per component element to image dictionary
  Long   := TPdfInteger.CreateInteger(8);
  Images.FObjet.AddElement('BitsPerComponent', Long);
  // add name element to image dictionary
  Nom    := TPdfName.CreateName('I' + IntToStr(NumImg));
  Images.FObjet.AddElement('Name', Nom);
  // add image reference to page dictionary
  for Cpt := 1 to Pred(FXRefObjets.Count) do
  begin
    Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[Cpt]).FObjet);
    if Dictionaire.FElement.Count > 0 then
    begin
      if TPdfName(TPdfDicElement(Dictionaire.FElement[0]).FValue).FValue = 'Page' then
      begin
        Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Resources')]).FValue);
        if Dictionaire.ElementParCle('XObject') > -1 then
        begin
          Dictionaire := TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('XObject')]).FValue);
          XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
          Dictionaire.AddElement(TPdfName(Nom).FValue, XRefObjets);
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
  FXRefObjets.Add(Contents);
  Stream     := TPdfStream.CreateStream;
  TPdfXRef(FXRefObjets[Pred(FXRefObjets.Count)]).FStream := Stream;
  // add contents reference to page dictionary
  XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  TPdfDictionary(TPdfXRef(FXRefObjets[Pred(Pred(FXRefObjets.Count))]).FObjet).AddElement('Contents', XRefObjets);
  Result     := Pred(FXRefObjets.Count);
end;

procedure TPdfDocument.CreateStream(NumeroPage, PageNum: integer);
var
  Cpt: integer;
  Txt: TPdfText;
  Clr: TPdfColor;
  Fnt: TPdfFonte;
  Rct: TPdfRectangle;
  Lin: TPdfLigne;
  Srf: TPdfSurface;
  Sty: TPdfLineStyle;
  Img: TPdfImage;
begin
  for Cpt := 0 to Pred(PdfPage.Count) do
  begin
    if TPdfElement(PdfPage[Cpt]) is TPdfTexte then
    begin
      if TPdfTexte(PdfPage[Cpt]).PageId = NumeroPage then
      begin
        with TPdfTexte(PdfPage[Cpt]) do
        begin
          if FontName > -1 then
          begin
            Fnt          := TPdfFonte.CreateFonte(FontName, FontSize);
            // adjust font size to display device
            Fnt.FTxtSize := IntToStr(Round((StrToInt(FontSize) * fpgApplication.Screen_dpi_y) div 72));
            TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Fnt);
            Clr          := TPdfColor.CreateColor(True, Couleur);
            TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
          end;
          Txt := TPdfText.CreateText(TextPosX, TextPosY, Writting);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Txt);
        end;
      end;
    end;
    if TPdfElement(PdfPage[Cpt]) is TPdfRect then
    begin
      if TPdfRect(PdfPage[Cpt]).PageId = NumeroPage then
      begin
        with TPdfRect(PdfPage[Cpt]) do
        begin
          Clr := TPdfColor.CreateColor(True, RectColor);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
          if RectStroke then
          begin
            Sty := TPdfLineStyle.CreateLineStyle(RectLineStyle, 0);
            TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Sty);
          end;
          Rct := TPdfRectangle.CreateRectangle(RectThickness, RectLeft, RectBottom, RectWidth, RectHeight, RectFill, RectStroke);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Rct);
        end;
      end;
    end;
    if TPdfElement(PdfPage[Cpt]) is TPdfLine then
    begin
      if TPdfLine(PdfPage[Cpt]).PageId = NumeroPage then
      begin
        with TPdfLine(PdfPage[Cpt]) do
        begin
          Clr := TPdfColor.CreateColor(False, LineColor);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
          Sty := TPdfLineStyle.CreateLineStyle(LineStyle, 0);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Sty);
          Lin := TPdfLigne.CreateLigne(LineThikness, LineBeginX, LineBeginY, LineEndX, LineEndY);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Lin);
        end;
      end;
    end;
    if TPdfElement(PdfPage[Cpt]) is TPdfSurf then
    begin
      if TPdfSurf(PdfPage[Cpt]).PageId = NumeroPage then
      begin
        with TPdfSurf(PdfPage[Cpt]) do
        begin
          Clr := TPdfColor.CreateColor(True, SurfColor);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
          Srf := TPdfSurface.CreateSurface(Points);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Srf);
        end;
      end;
    end;
    if TPdfElement(PdfPage[Cpt]) is TPdfImg then
    begin
      if TPdfImg(PdfPage[Cpt]).PageId = NumeroPage then
      begin
        with TPdfImg(PdfPage[Cpt]) do
        begin
          Img := TPdfImage.CreateImage(ImgLeft, ImgBottom, ImgWidth, ImgHeight, ImgNumber);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Img);
        end;
      end;
    end;
  end; { for Cpt... }
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
  Catalogue     := CreateCatalog;
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
      XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
      TPdfDictionary(TPdfXRef(FXRefObjets[Catalogue]).FObjet).AddElement('Outlines', XRefObjets);
      // add useoutline element to catalog dictionary
      Nom         := TPdfName.CreateName('UseOutlines');
      TPdfDictionary(TPdfXRef(FXRefObjets[Catalogue]).FObjet).AddElement('PageMode', Nom);
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
        Dictionaire   := TPdfDictionary(TPdfXRef(FXRefObjets[OutlineRoot]).FObjet);
        TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
        if CptSect = 0 then
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
          TPdfDictionary(TPdfXRef(FXRefObjets[OutlineRoot]).FObjet).AddElement('First', XRefObjets);
          NextSect   := ParentOutline;
          PrevSect   := Pred(FXRefObjets.Count);
        end
        else
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
          TPdfDictionary(TPdfXRef(FXRefObjets[NextSect]).FObjet).AddElement('Next', XRefObjets);
          XRefObjets := TPdfReference.CreateReference(PrevSect);
          TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet).AddElement('Prev', XRefObjets);
          NextSect   := ParentOutline;
          if CptSect < Pred(Sections.Count) then
            PrevSect := Pred(FXRefObjets.Count);
        end;
        if CptSect = Pred(Sections.Count) then
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
          TPdfDictionary(TPdfXRef(FXRefObjets[OutlineRoot]).FObjet).AddElement('Last', XRefObjets);
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
        XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[ElementParNom('Catalog')]).FObjet);
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
        Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet);
        TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
        // add page reference to outline destination
        Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[PageOutline]).FObjet);
        XRefObjets  := TPdfReference.CreateReference(NewPage);
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(XRefObjets);
        // add display control name to outline destination
        Nom         := TPdfName.CreateName('Fit');
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(Nom);
        if CptPage = 0 then
        begin
          XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
          TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet).AddElement('First', XRefObjets);
          NextOutline := PageOutline;
          PrevOutline := Pred(FXRefObjets.Count);
          // add page reference to parent outline destination
          Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet);
          XRefObjets  := TPdfReference.CreateReference(NewPage);
          TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(XRefObjets);
          // add display control name to outline destination
          Nom         := TPdfName.CreateName('Fit');
          TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(Nom);
        end
        else
        begin
          XRefObjets  := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
          TPdfDictionary(TPdfXRef(FXRefObjets[NextOutline]).FObjet).AddElement('Next', XRefObjets);
          XRefObjets  := TPdfReference.CreateReference(PrevOutline);
          TPdfDictionary(TPdfXRef(FXRefObjets[PageOutline]).FObjet).AddElement('Prev', XRefObjets);
          NextOutline := PageOutline;
          if CptPage < Pred(T_Section(Sections[CptSect]).Pages.Count) then
            PrevOutline := Pred(FXRefObjets.Count);
        end;
        if CptPage = Pred(T_Section(Sections[CptSect]).Pages.Count) then
        begin
          XRefObjets := TPdfReference.CreateReference(Pred(FXRefObjets.Count));
          TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet).AddElement('Last', XRefObjets);
        end;
      end;
    end;
  end;
  if Sections.Count > 1 then
  begin
    // update count in root parent pages dictionary
    Dictionaire := TPdfDictionary(TPdfXRef(FXRefObjets[TreeRoot]).FObjet);
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
  TPdfInteger(TPdfDicElement(Trailer.FElement[Trailer.ElementParCle('Size')]).FValue).FValue := FXRefObjets.Count;
end;

destructor TPdfDocument.Destroy;
var
  Cpt: integer;
begin
  Trailer.Free;
  if FXRefObjets.Count > 0 then
    for Cpt := 0 to Pred(FXRefObjets.Count) do
      TPdfXRef(FXRefObjets[Cpt]).Free;
  FXRefObjets.Free;
  inherited;
end;

procedure TPdfDocument.WriteDocument(const AFlux: TStream);
var
  Cpt, XRefPos: integer;
begin
  AFlux.Position := 0;
  WriteChaine(PDF_VERSION + CRLF, AFlux);
  // write numbered indirect objects
  for Cpt := 1 to Pred(FXRefObjets.Count) do
  begin
    XRefPos := AFlux.Position;
    WriteObjet(Cpt, AFlux);
    TPdfXRef(FXRefObjets[Cpt]).Offset := XRefPos;
  end;
  XRefPos := AFlux.Position;
  // write xref table
  WriteChaine('xref' + CRLF + '0 ' + IntToStr(FXRefObjets.Count) + CRLF, AFlux);
  with TPdfXRef(FXRefObjets[0]) do
    WriteChaine(IntToChaine(Offset, 10) + ' ' + IntToChaine(PDF_MAX_GEN_NUM, 5) + ' f' + CRLF, AFlux);
  WriteXRefTable(AFlux);
  // write trailer
  WriteChaine('trailer' + CRLF, AFlux);
  Trailer.WriteDictionary(-1, AFlux);
  // write offset of last xref table
  WriteChaine(CRLF + 'startxref' + CRLF + IntToStr(XRefPos) + CRLF, AFlux);
  WriteChaine(PDF_FILE_END, AFlux);
end;

end.

