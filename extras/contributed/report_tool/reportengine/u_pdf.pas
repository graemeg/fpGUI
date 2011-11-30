{
    << Impressions >>  U_Pdf.pas

    Copyright (C) 2010 - JM.Levecque - <jmarc.levecque@jmlesite.fr>

   This library is a free software coming as a add-on to fpGUI toolkit
   See the copyright included in the fpGUI distribution for details about redistribution

   This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit produces the pdf file
}

unit U_Pdf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main, fpg_base;

type
  TPdfObjet = class(TObject)
    private
    protected
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
      FValue: Integer;
    protected
      procedure WriteInteger(const AFlux: TStream);
      procedure IncrementeInteger;
      property Value: Integer read FValue write FValue;
    public
      constructor CreateInteger(const AValue: Integer);
      destructor Destroy; override;
    end;

  TPdfReference = class(TPdfObjet)
    private
      FValue: Integer;
    protected
      procedure WriteReference(const AFlux: TStream);
    public
      constructor CreateReference(const AValue: Integer);
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
      FTxtFont: Integer;
      FTxtSize: string;
    protected
      procedure WriteFonte(const AFlux: TStream);
    public
      constructor CreateFonte(const AFont: Integer; const ASize: string);
      destructor Destroy; override;
    end;

  TPdfText = class(TPdfObjet)
    private
      FTxtPosX: Single;
      FTxtPosY: Single;
      FTxtText: TPdfString;
    protected
      procedure WriteText(const AFlux: TStream);
    public
      constructor CreateText(const APosX,APosY: Single; const AText: string);
      destructor Destroy; override;
    end;

  TPdfLigne = class(TPdfObjet)
    private
      FEpais: Single;
      FStaX: Single;
      FStaY: Single;
      FEndX: Single;
      FEndY: Single;
    protected
      procedure WriteLigne(const AFlux: TStream);
    public
      constructor CreateLigne(const AEpais,AStaX,AStaY,AEndX,AEndY: Single);
      destructor Destroy; override;
    end;

  TPdfRectangle = class(TPdfObjet)
    private
      FEpais: Single;
      FRecX: Single;
      FRecY: Single;
      FRecW: Single;
      FRecH: Single;
      FFill: Boolean;
      FStroke: Boolean;
    protected
      procedure WriteRectangle(const AFlux: TStream);
    public
      constructor CreateRectangle(const AEpais,APosX,APosY,AWidth,AHeight: Single; const AFill,AStroke: Boolean);
      destructor Destroy; override;
    end;

  TRefPos= record
    X: Single;
    Y: Single;
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

  TPdfLineStyle = class(TPdfObjet)
    private
      FDash: TfpgLineStyle;
      FPhase: Integer;
    protected
      procedure WriteLineStyle(const AFlux: TStream);
    public
      constructor CreateLineStyle(ADash: TfpgLineStyle; APhase: Integer);
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
      constructor CreateColor(const AStroke: Boolean; Couleur: LongInt);
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
      function ElementParCle(const AValue: string): Integer;
      procedure WriteDictionary(AFlux: TStream);
    public
      constructor CreateDictionary;
      destructor Destroy; override;
    end;

  TPdfXRef = class(TObject)
    private
      FOffset: Integer;
      FObjet: TPdfDictionary;
      FStream: TPdfStream;
    protected
      procedure WriteXRef(const AFlux: TStream);
    public
      constructor CreateXRef;
      destructor Destroy; override;
      property Offset: Integer read FOffset write FOffset;
    end;

  TPdfDocument = class(TObject)
    private
      FXRefObjets: TList; // list of TPdfXRef
    protected
      function ElementParNom(const AValue: string): Integer;
      procedure WriteXRefTable(const AFlux: TStream);
      procedure WriteObjet(const AObjet: Integer; const AFlux: TStream);
      procedure CreateRefTable;
      procedure CreateTrailer;
      function CreateCatalog: Integer;
      procedure CreateInfo;
      procedure CreatePreferences;
      function CreatePages(Parent: Integer): Integer;
      function CreatePage(Parent,Haut,Larg: Integer): Integer;
      function CreateOutlines: Integer;
      function CreateOutline(Parent,SectNo,PageNo: Integer; SectTitre: string): Integer;
      procedure CreateFont(NomFonte: string; NumFonte: Integer);
      function CreateContents: Integer;
      procedure CreateStream(NumeroPage,PageNum: Integer);
    public
      constructor CreateDocument;
      destructor Destroy; override;
      procedure WriteDocument(const AFlux: TStream);
    end;

const
  CRLF= #13#10;
  PDF_VERSION= '%PDF-1.3';
  PDF_FILE_END= '%%EOF';
  PDF_MAX_GEN_NUM= 65535;
  PDF_UNICODE_HEADER = 'FEFF001B%s001B';
  PDF_LANG_STRING = 'fr';

var
  Document: TPdfDocument;
  OldDecSeparator: Char;
  Outline: Boolean;

implementation

uses
  U_Report, U_Command;

var
  Trailer: TPdfDictionary;
  CurrentColor: string;
  CurrentWidth: string;
  Catalogue: Integer;

// utility functions

function InsertEscape(const AValue: string): string;
var
  Chaine: string;
begin
Result:= '';
Chaine:= AValue;
while Pos('\',Chaine)> 0 do
  begin
  Result:= Result+Copy(Chaine,1,Pred(Pos('\',Chaine)))+'\\';
  Chaine:= Copy(Chaine,Succ(Pos('\',Chaine)),Length(Chaine)-Pos('\',Chaine));
  end;
Chaine:= Result+Chaine;
Result:= '';
while Pos('(',Chaine)> 0 do
  begin
  Result:= Result+Copy(Chaine,1,Pred(Pos('(',Chaine)))+'\(';
  Chaine:= Copy(Chaine,Succ(Pos('(',Chaine)),Length(Chaine)-Pos('(',Chaine));
  end;
Chaine:= Result+Chaine;
Result:= '';
while Pos(')',Chaine)> 0 do
  begin
  Result:= Result+Copy(Chaine,1,Pred(Pos(')',Chaine)))+'\)';
  Chaine:= Copy(Chaine,Succ(Pos(')',Chaine)),Length(Chaine)-Pos(')',Chaine));
  end;
Result:= Result+Chaine;
end;

procedure WriteChaine(const Valeur: string; AFlux: TStream);
begin
AFlux.Write(PChar(Valeur)^,Length(Valeur));
end;

function IntToChaine(const Valeur: Integer; const Long: Integer): string;
var
  Chaine: string;
  Cpt: Integer;
begin
Result:= '';
Chaine:= IntToStr(Valeur);
if Length(Chaine)< Long
then
  for Cpt:= Succ(Length(Chaine)) to Long do
    Result:= Result+'0';
Result:= Result+Chaine;
end;

function DateToPdfDate(const ADate: TDateTime): string;
begin
Result:= FormatDateTime('"D:"yyyymmddhhnnss',ADate);
end;

function ExtractBaseFontName(const AValue: string): string;
var
  FontName,Chaine1,Chaine2: string;
begin
FontName:= Uppercase(AValue[1])+Copy(AValue,2,Pos('-',AValue)-2);
if Pos(':',AValue)> 0
then
  begin
  Chaine1:= Copy(AValue,Succ(Pos(':',AValue)),Length(AValue)-Pos(':',AValue));
  Chaine1:= Uppercase(Chaine1[1])+Copy(Chaine1,2,Pred(Length(Chaine1)));
  if Pos(':',Chaine1)> 0
  then
    begin
    Chaine2:= Copy(Chaine1,Succ(Pos(':',Chaine1)),Length(Chaine1)-Pos(':',Chaine1));
    Chaine2:= Uppercase(Chaine2[1])+Copy(Chaine2,2,Pred(Length(Chaine2)));
    if (FontName= 'Helvetica') or (FontName= 'Courier')
    then
      if Chaine2= 'Italic'
      then
        Chaine2:= 'Oblique';
    Chaine1:= Copy(Chaine1,1,Pred(Pos(':',Chaine1)));
    Chaine1:= Uppercase(Chaine1[1])+Copy(Chaine1,2,Pred(Length(Chaine1)));
    if (FontName= 'Helvetica') or (FontName= 'Courier')
    then
      if Chaine1= 'Italic'
      then
        Chaine1:= 'Oblique';
    Chaine1:= Chaine1+Chaine2;
    end
  else
    if (FontName= 'Helvetica') or (FontName= 'Courier')
    then
      if Chaine1= 'Italic'
      then
        Chaine1:= 'Oblique';
  Chaine1:= '-'+Chaine1;
  end;
Result:= FontName+Chaine1;
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
if FValue
then
  WriteChaine('true',AFlux)
else
  WriteChaine('false',AFlux);
end;

constructor TPdfBoolean.CreateBoolean(const AValue: Boolean);
begin
inherited Create;
FValue:= AValue;
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
FValue:= FValue+1;
end;

constructor TPdfInteger.CreateInteger(const AValue: Integer);
begin
inherited Create;
FValue:= AValue;
end;

destructor TPdfInteger.Destroy;
begin
inherited;
end;

procedure TPdfReference.WriteReference(const AFlux: TStream);
begin
WriteChaine(IntToStr(FValue)+' 0 R',AFlux);
end;

constructor TPdfReference.CreateReference(const AValue: Integer);
begin
inherited Create;
FValue:= AValue;
end;

destructor TPdfReference.Destroy;
begin
inherited;
end;

procedure TPdfName.WriteName(const AFlux: TStream);
begin
if FValue<> ''
then
  WriteChaine('/'+FValue,AFlux);
end;

constructor TPdfName.CreateName(const AValue: string);
begin
inherited Create;
FValue:= AValue;
end;

destructor TPdfName.Destroy;
begin
inherited;
end;

procedure TPdfString.WriteString(const AFlux: TStream);
begin
WriteChaine('('+Utf8ToAnsi(FValue)+')',AFlux);
end;

constructor TPdfString.CreateString(const AValue: string);
begin
inherited Create;
FValue:= AValue;
if (Pos('(',FValue)> 0) or (Pos(')',FValue)> 0) or (Pos('\',FValue)> 0)
then
  FValue:= InsertEscape(FValue);
end;

destructor TPdfString.Destroy;
begin
inherited;
end;

procedure TPdfArray.WriteArray(const AFlux: TStream);
var
  Cpt: Integer;
begin
WriteChaine('[',AFlux);
for Cpt:= 0 to Pred(FArray.Count) do
  begin
  if Cpt> 0
  then
    WriteChaine(' ',AFlux);
  if TPdfObjet(FArray[Cpt]) is TPdfInteger
  then
    TPdfInteger(FArray[Cpt]).WriteInteger(AFlux);
  if TPdfObjet(FArray[Cpt]) is TPdfReference
  then
    TPdfReference(FArray[Cpt]).WriteReference(AFlux);
  if TPdfObjet(FArray[Cpt]) is TPdfName
  then
    TPdfName(FArray[Cpt]).WriteName(AFlux);
  end;
WriteChaine(']',AFlux);
end;

procedure TPdfArray.AddItem(const AValue: TPdfObjet);
begin
FArray.Add(AValue);
end;

constructor TPdfArray.CreateArray;
begin
inherited Create;
FArray:= TList.Create;
end;

destructor TPdfArray.Destroy;
var
  Cpt: Integer;
begin
if FArray.Count> 0
then
  for Cpt:= 0 to Pred(FArray.Count) do
    if TPdfObjet(FArray[Cpt]) is TPdfInteger
    then
      TPdfInteger(FArray[Cpt]).Free
    else
      if TPdfObjet(FArray[Cpt]) is TPdfReference
      then
        TPdfReference(FArray[Cpt]).Free
      else
        if TPdfObjet(FArray[Cpt]) is TPdfName
        then
          TPdfName(FArray[Cpt]).Free;
FArray.Free;
inherited;
end;

procedure TPdfStream.WriteStream(const AFlux: TStream);
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(FStream.Count) do
  begin
  if TPdfObjet(FStream[Cpt]) is TPdfFonte
  then
    TPdfFonte(FStream[Cpt]).WriteFonte(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfColor
  then
    TPdfColor(FStream[Cpt]).WriteColor(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfText
  then
    TPdfText(FStream[Cpt]).WriteText(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfRectangle
  then
    TPdfRectangle(FStream[Cpt]).WriteRectangle(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfLigne
  then
    TPdfLigne(FStream[Cpt]).WriteLigne(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfLineStyle
  then
    TPdfLineStyle(FStream[Cpt]).WriteLineStyle(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfSurface
  then
    TPdfSurface(FStream[Cpt]).WriteSurface(AFlux);
  end;
end;

procedure TPdfStream.AddItem(const AValue: TPdfObjet);
begin
FStream.Add(AValue);
end;

constructor TPdfStream.CreateStream;
begin
inherited Create;
FStream:= TList.Create;
end;

destructor TPdfStream.Destroy;
var
  Cpt: Integer;
begin
if FStream.Count> 0
then
  for Cpt:= 0 to Pred(FStream.Count) do
    if TPdfObjet(FStream[Cpt]) is TPdfFonte
    then
      TPdfFonte(FStream[Cpt]).Free
    else
      if TPdfObjet(FStream[Cpt]) is TPdfColor
      then
        TPdfColor(FStream[Cpt]).Free
      else
        if TPdfObjet(FStream[Cpt]) is TPdfText
        then
          TPdfText(FStream[Cpt]).Free
        else
          if TPdfObjet(FStream[Cpt]) is TPdfRectangle
          then
            TPdfRectangle(FStream[Cpt]).Free
          else
            if TPdfObjet(FStream[Cpt]) is TPdfLigne
            then
              TPdfLigne(FStream[Cpt]).Free
            else
              if TPdfObjet(FStream[Cpt]) is TPdfLineStyle
              then
                TPdfLineStyle(FStream[Cpt]).Free
              else
                if TPdfObjet(FStream[Cpt]) is TPdfSurface
                then
                  TPdfSurface(FStream[Cpt]).Free;
FStream.Free;
inherited;
end;

procedure TPdfFonte.WriteFonte(const AFlux: TStream);
begin
WriteChaine('/F'+IntToStr(FTxtFont)+' '+FTxtSize+' Tf'+CRLF,AFlux);
end;

constructor TPdfFonte.CreateFonte(const AFont: Integer; const ASize: string);
begin
inherited Create;
FTxtFont:= AFont;
FTxtSize:= ASize;
end;

destructor TPdfFonte.Destroy;
begin
inherited;
end;

procedure TPdfText.WriteText(const AFlux: TStream);
begin
WriteChaine('BT'+CRLF,AFlux);
WriteChaine(FormatFloat('0.##',FTxtPosX)+' '+FormatFloat('0.##',FTxtPosY)+' Td'+CRLF,AFlux);
TPdfString(FTxtText).WriteString(AFlux);
WriteChaine(' Tj'+CRLF,AFlux);
WriteChaine('ET'+CRLF,AFlux);
end;

constructor TPdfText.CreateText(const APosX,APosY: Single; const AText: string);
begin
inherited Create;
FTxtPosX:= APosX;
FTxtPosY:= APosY;
FTxtText:= TPdfString.CreateString(AText);
end;

destructor TPdfText.Destroy;
begin
FTxtText.Free;
inherited;
end;

procedure TPdfLigne.WriteLigne(const AFlux: TStream);
begin
if (FormatFloat('0.##',FEpais)+' w')<> CurrentWidth
then
  begin
  WriteChaine('1 J'+CRLF,AFlux);
  WriteChaine(FormatFloat('0.##',FEpais)+' w'+CRLF,AFlux);
  CurrentWidth:= FormatFloat('0.##',FEpais)+' w';
  end;
WriteChaine(FormatFloat('0.##',FStaX)+' '+FormatFloat('0.##',FStaY)+' m'+CRLF,AFlux);
WriteChaine(FormatFloat('0.##',FEndX)+' '+FormatFloat('0.##',FEndY)+' l'+CRLF,AFlux);
WriteChaine('S'+CRLF,AFlux);
end;

constructor TPdfLigne.CreateLigne(const AEpais,AStaX,AStaY,AEndX,AEndY: Single);
begin
inherited Create;
FEpais:= AEpais;
FStaX:= AStaX;
FStaY:= AStaY;
FEndX:= AEndX;
FEndY:= AEndY;
end;

destructor TPdfLigne.Destroy;
begin
inherited;
end;

procedure TPdfRectangle.WriteRectangle(const AFlux: TStream);
begin
if FStroke
then
  if (FormatFloat('0.##',FEpais)+' w')<> CurrentWidth
  then
    begin
    WriteChaine('1 J'+CRLF,AFlux);
    WriteChaine(FormatFloat('0.##',FEpais)+' w'+CRLF,AFlux);
    CurrentWidth:= FormatFloat('0.##',FEpais)+' w';
    end;
WriteChaine(FormatFloat('0.##',FRecX)+' '+FormatFloat('0.##',FRecY)+' '+FormatFloat('0.##',FRecW)+' '+FormatFloat('0.##',FRecH)+' re'+CRLF,AFlux);
if FStroke
then
  WriteChaine('S'+CRLF,AFlux);
if FFill
then
  WriteChaine('f'+CRLF,AFlux);
end;

constructor TPdfRectangle.CreateRectangle(const AEpais,APosX,APosY,AWidth,AHeight: Single; const AFill,AStroke: Boolean);
begin
inherited Create;
FEpais:= AEpais;
FRecX:= APosX;
FRecY:= APosY;
FRecW:= AWidth;
FRecH:= AHeight;
FFill:= AFill;
FStroke:= AStroke;
end;

destructor TPdfRectangle.Destroy;
begin
inherited;
end;

procedure TPdfSurface.WriteSurface(const AFlux: TStream);
var
  Cpt: Integer;
begin
WriteChaine(FormatFloat('0.##',FPoints[0].X)+' '+FormatFloat('0.##',FPoints[0].Y)+' m'+CRLF,AFlux);
for Cpt:= 1 to Pred(Length(FPoints)) do
  WriteChaine(FormatFloat('0.##',FPoints[Cpt].X)+' '+FormatFloat('0.##',FPoints[Cpt].Y)+' l'+CRLF,AFlux);
WriteChaine('h'+CRLF,AFlux);
WriteChaine('f'+CRLF,AFlux);
end;

constructor TPdfSurface.CreateSurface(const APoints: T_Points);
begin
inherited Create;
FPoints:= APoints;
end;

destructor TPdfSurface.Destroy;
begin
inherited;
end;

procedure TPdfLineStyle.WriteLineStyle(const AFlux: TStream);
begin
WriteChaine('[',AFlux);
case FDash of
  lsDash:
    WriteChaine('5 5',AFlux);
  lsDot:
    WriteChaine('2 2',AFlux);
  lsDashDot:
    WriteChaine('5 2 2 2',AFlux);
  lsDashDotDot:
    WriteChaine('5 2 2 2 2 2',AFlux);
  end;
WriteChaine('] '+IntToStr(FPhase)+' d'+CRLF,AFlux);
end;

constructor TPdfLineStyle.CreateLineStyle(ADash: TfpgLineStyle; APhase: Integer);
begin
inherited Create;
FDash:= ADash;
FPhase:= APhase;
end;

destructor TPdfLineStyle.Destroy;
begin
inherited;
end;

procedure TPdfColor.WriteColor(const AFlux: TStream);
begin
if FStroke
then
  begin
  if (FRed+' '+FGreen+' '+FBlue+' rg')<> CurrentColor
  then
    begin
    WriteChaine(FRed+' '+FGreen+' '+FBlue+' rg'+CRLF,AFlux);
    CurrentColor:= FRed+' '+FGreen+' '+FBlue+' rg';
    end;
  end
else
  if (FRed+' '+FGreen+' '+FBlue+' RG')<> CurrentColor
  then
    begin
    WriteChaine(FRed+' '+FGreen+' '+FBlue+' RG'+CRLF,AFlux);
    CurrentColor:= FRed+' '+FGreen+' '+FBlue+' RG';
    end;
end;

constructor TPdfColor.CreateColor(const AStroke: Boolean; Couleur: Longint);
begin
inherited Create;
FBlue:= FormatFloat('0.##',Couleur mod 256/256);
Couleur:= Couleur div 256;
FGreen:= FormatFloat('0.##',Couleur mod 256/256);
FRed:= FormatFloat('0.##',Couleur div 256/256);
FStroke:= AStroke;
end;

destructor TPdfColor.Destroy;
begin
inherited
end;

procedure TPdfDicElement.WriteDicElement(const AFlux: TStream);
begin
FKey.WriteName(AFlux);
WriteChaine(' ',AFlux);
if FValue is TPdfBoolean
then
  TPdfBoolean(FValue).WriteBoolean(AFlux);
if FValue is TPdfInteger
then
  TPdfInteger(FValue).WriteInteger(AFlux);
if FValue is TPdfReference
then
  TPdfReference(FValue).WriteReference(AFlux);
if FValue is TPdfName
then
  TPdfName(FValue).WriteName(AFlux);
if FValue is TPdfString
then
  TPdfString(FValue).WriteString(AFlux);
if FValue is TPdfArray
then
  TPdfArray(FValue).WriteArray(AFlux);
if FValue is TPdfDictionary
then
  TPdfDictionary(FValue).WriteDictionary(AFlux);
WriteChaine(CRLF,AFlux);
end;

constructor TPdfDicElement.CreateDicElement(const AKey: string; const AValue: TPdfObjet);
begin
inherited Create;
FKey:= TPdfName.CreateName(AKey);
FValue:= AValue;
end;

destructor TPdfDicElement.Destroy;
begin
FKey.Free;
if FValue is TPdfBoolean
then
  TPdfBoolean(FValue).Free
else
  if FValue is TPdfDictionary
  then
    TPdfDictionary(FValue).Free
  else
    if FValue is TPdfInteger
    then
      TPdfInteger(FValue).Free
    else
      if FValue is TPdfName
      then
        TPdfName(FValue).Free
      else
        if FValue is TPdfReference
        then
          TPdfReference(FValue).Free
        else
          if FValue is TPdfString
          then
            TPdfString(FValue).Free
          else
            if FValue is TPdfArray
            then
              TPdfArray(FValue).Free;
inherited;
end;

procedure TPdfDictionary.AddElement(const AKey: string; const AValue: TPdfObjet);
var
  DicElement: TPdfDicElement;
begin
DicElement:= TPdfDicElement.CreateDicElement(AKey,AValue);
FElement.Add(DicElement);
end;

function TPdfDictionary.ElementParCle(const AValue: string): Integer;
var
  Cpt: Integer;
begin
Result:= -1;
for Cpt:= 0 to Pred(FElement.Count) do
  if TPdfDicElement(FElement[Cpt]).FKey.FValue= AValue
  then
    begin
    Result:= Cpt;
    Exit;
    end;
end;

procedure TPdfDictionary.WriteDictionary(AFlux: TStream);
var
  Cpt: Integer;
begin
WriteChaine('<<'+CRLF,AFlux);
for Cpt:= 0 to Pred(FElement.Count) do
  TPdfDicElement(FElement[Cpt]).WriteDicElement(AFlux);
WriteChaine('>>',AFlux);
end;

constructor TPdfDictionary.CreateDictionary;
begin
inherited Create;
FElement:= TList.Create;
end;

destructor TPdfDictionary.Destroy;
var
  Cpt: integer;
begin
if FElement.Count> 0
then
  for Cpt:= 0 to Pred(FElement.Count) do
    TPdfDicElement(FElement[Cpt]).Free;
FElement.Free;
inherited;
end;

procedure TPdfXRef.WriteXRef(const AFlux: TStream);
begin
WriteChaine(IntToChaine(FOffset,10)+' '+IntToChaine(0,5)+' n'+CRLF,AFlux);
end;

constructor TPdfXRef.CreateXRef;
begin
inherited Create;
FOffset:= 0;
FObjet:= TpdfDictionary.CreateDictionary;
FStream:= nil;
end;

destructor TPdfXRef.Destroy;
begin
FObjet.Free;
FStream.Free;
inherited;
end;

function TPdfDocument.ElementParNom(const AValue: string): Integer;
var
  Cpt: Integer;
begin
for Cpt:= 1 to Pred(FXRefObjets.Count) do
  if TPdfName(TPdfDicElement(TPdfDictionary(TPdfXRef(FXRefObjets[Cpt]).FObjet).FElement[0]).FValue).FValue= AValue
  then
    Result:= Cpt;
end;

procedure TPdfDocument.WriteXRefTable(const AFlux: TStream);
var
  Cpt: Integer;
begin
if FXRefObjets.Count> 1
then
  for Cpt:= 1 to Pred(FXRefObjets.Count) do
    TPdfXRef(FXRefObjets[Cpt]).WriteXRef(AFlux);
end;

procedure TPdfDocument.WriteObjet(const AObjet: Integer; const AFlux: TStream);
var
  Long: TPdfInteger;
  Flux: TMemoryStream;
begin
WriteChaine(IntToStr(AObjet)+' 0 obj'+CRLF,AFlux);
if TPdfXRef(FXRefObjets[AObjet]).FStream= nil
then
  TPdfDictionary(TPdfXRef(FXRefObjets[AObjet]).FObjet).WriteDictionary(AFlux)
else
  begin
  Flux:= TMemoryStream.Create;
  Flux.Position:= 0;
  CurrentColor:= '';
  CurrentWidth:= '';
  TPdfXRef(FXRefObjets[AObjet]).FStream.WriteStream(Flux);
// write stream length element in contents dictionary
  Long:= TPdfInteger.CreateInteger(Flux.Size);
  TPdfDictionary(TPdfXRef(FXRefObjets[AObjet]).FObjet).AddElement('Length',Long);
  Flux.Free;
  TPdfXRef(FXRefObjets[AObjet]).FObjet.WriteDictionary(AFlux);
// write stream in contents dictionary
  CurrentColor:= '';
  CurrentWidth:= '';
  WriteChaine(CRLF+'stream'+CRLF,AFlux);
  TPdfXRef(FXRefObjets[AObjet]).FStream.WriteStream(AFlux);
  WriteChaine('endstream',AFlux);
  end;
WriteChaine(CRLF+'endobj'+CRLF,AFlux);
end;

procedure TPdfDocument.CreateRefTable;
var
  XRefObjet: TPdfXRef;
begin
FXRefObjets:= TList.Create;
// add first xref entry
XRefObjet:= TPdfXRef.CreateXRef;
FXRefObjets.Add(XRefObjet);
end;

procedure TPdfDocument.CreateTrailer;
var
  XRefObjets: TPdfInteger;
begin
Trailer:= TPdfDictionary.CreateDictionary;
// add size trailer element
XRefObjets:= TPdfInteger.CreateInteger(FXRefObjets.Count);
Trailer.AddElement('Size',XRefObjets);
end;

function TPdfDocument.CreateCatalog: Integer;
var
  Catalog: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
begin
// add xref entry
Catalog:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Catalog);
// add root trailer element
XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
Trailer.AddElement('Root',XRefObjets);
// add type element to catalog dictionary
Nom:= TPdfName.CreateName('Catalog');
Catalog.FObjet.AddElement('Type',Nom);
Result:= Pred(FXRefObjets.Count);
end;

procedure TPdfDocument.CreateInfo;
var
  Info: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfString;
begin
// add xref entry
Info:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Info);
// add info trailer element
XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
Trailer.AddElement('Info',XRefObjets);
TPdfInteger(TPdfDicElement(Trailer.FElement[Trailer.ElementParCle('Size')]).FValue).FValue:= FXRefObjets.Count;
// add title element to info dictionary
Nom:= TPdfString.CreateString(Infos.Titre);
Info.FObjet.AddElement('Title',Nom);
// add author element to info dictionary
Nom:= TPdfString.CreateString(Infos.Auteur);
Info.FObjet.AddElement('Author',Nom);
// add creator element to info dictionary
Nom:= TPdfString.CreateString('fpGUI/FPC');
Info.FObjet.AddElement('Creator',Nom);
// add producer element to info dictionary
Nom:= TPdfString.CreateString('fpGUI/FPC');
Info.FObjet.AddElement('Producer',Nom);
// add creationdate element to info dictionary
Nom:= TPdfString.CreateString(DateToPdfDate(Now));
Info.FObjet.AddElement('CreationDate',Nom);
end;

procedure TPdfDocument.CreatePreferences;
var
  Viewer: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Preference: TPdfBoolean;
begin
// add xref entry
Viewer:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Viewer);
// add type element to preferences dictionary
Nom:= TPdfName.CreateName('ViewerPreferences');
Viewer.FObjet.AddElement('Type',Nom);
// add preference element to preferences dictionary
Preference:= TPdfBoolean.CreateBoolean(True);
Viewer.FObjet.AddElement('FitWindow',Preference);
// add preferences reference to catalog dictionary
XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
TPdfDictionary(TPdfXRef(FXRefObjets[ElementParNom('Catalog')]).FObjet).AddElement('ViewerPreferences',XRefObjets)
end;

function TPdfDocument.CreatePages(Parent: Integer): Integer;
var
  Pages: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Table: TPdfArray;
  Count: TPdfInteger;
begin
// add xref entry
Pages:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Pages);
// add type element to pages dictionary
Nom:= TPdfName.CreateName('Pages');
Pages.FObjet.AddElement('Type',Nom);
// add parent reference to pages dictionary if pages is not the root of the page tree
if Parent> 0
then
  begin
  XRefObjets:= TPdfReference.CreateReference(Parent);
  Pages.FObjet.AddElement('Parent',XRefObjets);
  // increment count in parent pages dictionary
  Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[Parent]).FObjet);
  TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
  // add kid reference in parent pages dictionary
  XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Kids')]).FValue).AddItem(XRefObjets);
  end
else
  begin
  // add pages reference to catalog dictionary
  XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
  TPdfDictionary(TPdfXRef(FXRefObjets[ElementParNom('Catalog')]).FObjet).AddElement('Pages',XRefObjets)
  end;
// add kids element to pages dictionary
Table:= TPdfArray.CreateArray;
Pages.FObjet.AddElement('Kids',Table);
// add count element to pages dictionary
Count:= TPdfInteger.CreateInteger(0);
Pages.FObjet.AddElement('Count',Count);
Result:= Pred(FXRefObjets.Count);
end;

function TPdfDocument.CreatePage(Parent,Haut,Larg: Integer): Integer;
var
  Page: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Table: TPdfArray;
  Coord: TPdfInteger;
begin
// add xref entry
Page:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Page);
// add type element to page dictionary
Nom:= TPdfName.CreateName('Page');
Page.FObjet.AddElement('Type',Nom);
// add parent reference to page dictionary
XRefObjets:= TPdfReference.CreateReference(Parent);
Page.FObjet.AddElement('Parent',XRefObjets);
// increment count in parent pages dictionary
Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[Parent]).FObjet);
TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
// add kid reference in parent pages dictionary
XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Kids')]).FValue).AddItem(XRefObjets);
// add mediabox element to page dictionary
Table:= TPdfArray.CreateArray;
Page.FObjet.AddElement('MediaBox',Table);
// add coordinates in page mediabox
Dictionaire:= TPdfDictionary(TPdfXRef(Page).FObjet);
Coord:= TPdfInteger.CreateInteger(0);
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
Coord:= TPdfInteger.CreateInteger(0);
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
Coord:= TPdfInteger.CreateInteger(Larg);
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
Coord:= TPdfInteger.CreateInteger(Haut);
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
// add resources element to page dictionary
Dictionaire:= TPdfDictionary.CreateDictionary;
Page.FObjet.AddElement('Resources',Dictionaire);
// add procset element in resources element to page dictionary
Table:= TPdfArray.CreateArray;
TPdfDictionary(TPdfDicElement(Page.FObjet.FElement[Pred(Page.FObjet.FElement.Count)]).FValue).AddElement('ProcSet',Table);
// add font element in resources element to page dictionary
Dictionaire:= TPdfDictionary.CreateDictionary;
TPdfDictionary(TPdfDicElement(Page.FObjet.FElement[Pred(Page.FObjet.FElement.Count)]).FValue).AddElement('Font',Dictionaire);
// add pdf element in procset array to page dictionary
Dictionaire:= TPdfDictionary(TPdfDicElement(Page.FObjet.FElement[Pred(Page.FObjet.FElement.Count)]).FValue);
Nom:= TPdfName.CreateName('PDF');
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(Nom);
// add text element in procset array to page dictionary
Nom:= TPdfName.CreateName('Text');
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('ProcSet')]).FValue).AddItem(Nom);
Result:= Pred(FXRefObjets.Count);
end;

function TPdfDocument.CreateOutlines: Integer;
var
  Outlines: TPdfXRef;
  Nom: TPdfName;
  Count: TPdfInteger;
begin
// add xref entry
Outlines:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Outlines);
// add type element to outlines dictionary
Nom:= TPdfName.CreateName('Outlines');
Outlines.FObjet.AddElement('Type',Nom);
// add count element to outlines dictionary
Count:= TPdfInteger.CreateInteger(0);
Outlines.FObjet.AddElement('Count',Count);
Result:= Pred(FXRefObjets.Count);
end;

function TPdfDocument.CreateOutline(Parent,SectNo,PageNo: Integer; SectTitre: string): Integer;
var
  Outline: TPdfXRef;
  XRefObjets: TPdfReference;
  Titre: TPdfString;
  Count: TPdfInteger;
  Table: TPdfArray;
begin
// add xref entry
Outline:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Outline);
// add title element to outline dictionary
if PageNo> -1
then
  if SectTitre<> ''
  then
    Titre:= TPdfString.CreateString(SectTitre+' Page '+IntToStr(PageNo))
  else
    Titre:= TPdfString.CreateString('Section '+IntToStr(SectNo)+' Page '+IntToStr(PageNo))
else
  if SectTitre<> ''
  then
    Titre:= TPdfString.CreateString(SectTitre)
  else
    Titre:= TPdfString.CreateString('Section '+IntToStr(SectNo));
Outline.FObjet.AddElement('Title',Titre);
// add parent reference to outline dictionary
XRefObjets:= TPdfReference.CreateReference(Parent);
Outline.FObjet.AddElement('Parent',XRefObjets);
// add count element to outline dictionary
Count:= TPdfInteger.CreateInteger(0);
Outline.FObjet.AddElement('Count',Count);
// add dest element to outline dictionary
Table:= TPdfArray.CreateArray;
Outline.FObjet.AddElement('Dest',Table);
Result:= Pred(FXRefObjets.Count);
end;

procedure TPdfDocument.CreateFont(NomFonte: string; NumFonte: Integer);
var
  Fontes: TPdfXRef;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  Dictionaire: TPdfDictionary;
  Cpt: Integer;
begin
// add xref entry
Fontes:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Fontes);
// add type element to font dictionary
Nom:= TPdfName.CreateName('Font');
Fontes.FObjet.AddElement('Type',Nom);
// add subtype element to font dictionary
Nom:= TPdfName.CreateName('Type1');
Fontes.FObjet.AddElement('Subtype',Nom);
// add encoding element to font dictionary
Nom:= TPdfName.CreateName('WinAnsiEncoding');
Fontes.FObjet.AddElement('Encoding',Nom);
// add firstchar element to font dictionary
Nom:= TPdfName.CreateName('32');
Fontes.FObjet.AddElement('FirstChar',Nom);
// add lastchar element to font dictionary
Nom:= TPdfName.CreateName('255');
Fontes.FObjet.AddElement('LastChar',Nom);
// add basefont element to font dictionary
Nom:= TPdfName.CreateName(NomFonte);
Fontes.FObjet.AddElement('BaseFont',Nom);
// add name element to font dictionary
Nom:= TPdfName.CreateName('F'+IntToStr(NumFonte));
Fontes.FObjet.AddElement('Name',Nom);
// add font reference to all page dictionary
for Cpt:= 1 to Pred(FXRefObjets.Count) do
  begin
  Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[Cpt]).FObjet);
  if Dictionaire.FElement.Count> 0
  then
    if TPdfName(TPdfDicElement(Dictionaire.FElement[0]).FValue).FValue= 'Page'
    then
      begin
      Dictionaire:= TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Resources')]).FValue);
      Dictionaire:= TPdfDictionary(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Font')]).FValue);
      XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
      Dictionaire.AddElement(TPdfName(Nom).FValue,XRefObjets);
      end;
  end;
end;

function TPdfDocument.CreateContents: Integer;
var
  Contents: TPdfXRef;
  XRefObjets: TPdfReference;
  Stream: TPdfStream;
begin
// add xref entry
Contents:= TPdfXRef.CreateXRef;
FXRefObjets.Add(Contents);
Stream:= TPdfStream.CreateStream;
TPdfXRef(FXRefObjets[Pred(FXRefObjets.Count)]).FStream:= Stream;
// add contents reference to page dictionary
XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
TPdfDictionary(TPdfXRef(FXRefObjets[Pred(Pred(FXRefObjets.Count))]).FObjet).AddElement('Contents',XRefObjets);
Result:= Pred(FXRefObjets.Count);
end;

procedure TPdfDocument.CreateStream(NumeroPage,PageNum: Integer);
var
  Cpt: Integer;
  Txt: TPdfText;
  Clr: TPdfColor;
  Fnt: TPdfFonte;
  Rct: TPdfRectangle;
  Lin: TPdfLigne;
  Srf: TPdfSurface;
  Sty: TpdfLineStyle;
begin
for Cpt:= 0 to Pred(PdfPage.Count) do
  begin
  if TPdfElement(PdfPage[Cpt]) is TPdfTexte
  then
    if TPdfTexte(PdfPage[Cpt]).PageId= NumeroPage
    then
      with TPdfTexte(PdfPage[Cpt]) do
        begin
        if FontName> -1
        then
          begin
          Fnt:= TPdfFonte.CreateFonte(FontName,FontSize);
// adjust font size to display device
          Fnt.FTxtSize:= IntToStr(Round((StrToInt(FontSize)*fpgApplication.Screen_dpi_y) div 72));
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Fnt);
          if Couleur> -1
          then
            begin
            Clr:= TPdfColor.CreateColor(True,Couleur);
            TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
            end;
          end;
        Txt:= TPdfText.CreateText(TextPosX,TextPosY,Writting);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Txt);
        end;
  if TPdfElement(PdfPage[Cpt]) is TPdfRect
  then
    if TPdfRect(PdfPage[Cpt]).PageId= NumeroPage
    then
      with TPdfRect(PdfPage[Cpt]) do
        begin
        if RectColor> -1
        then
          begin
          Clr:= TPdfColor.CreateColor(True,RectColor);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
          end;
        if RectStroke
        then
          begin
          Sty:= TPdfLineStyle.CreateLineStyle(RectLineStyle,0);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Sty);
          end;
        Rct:= TPdfRectangle.CreateRectangle(RectThickness,RectLeft,RectBottom,RectWidth,RectHeight,RectFill,RectStroke);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Rct);
        end;
  if TPdfElement(PdfPage[Cpt]) is TPdfLine
  then
    if TPdfLine(PdfPage[Cpt]).PageId= NumeroPage
    then
      with TPdfLine(PdfPage[Cpt]) do
        begin
        if LineColor> -1
        then
          begin
          Clr:= TPdfColor.CreateColor(False,LineColor);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
          end;
        Sty:= TPdfLineStyle.CreateLineStyle(LineStyle,0);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Sty);
        Lin:= TPdfLigne.CreateLigne(LineThikness,LineBeginX,LineBeginY,LineEndX,LineEndY);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Lin);
        end;
  if TPdfElement(PdfPage[Cpt]) is TPdfSurf
  then
    if TPdfSurf(PdfPage[Cpt]).PageId= NumeroPage
    then
      with TPdfSurf(PdfPage[Cpt]) do
        begin
        Clr:= TPdfColor.CreateColor(True,SurfColor);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
        Srf:= TPdfSurface.CreateSurface(Points);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Srf);
        end;
  end;
end;

constructor TPdfDocument.CreateDocument;
var
  Cpt,CptSect,CptPage,NumFont,TreeRoot,ParentPage,PageNum,NumPage: Integer;
  OutlineRoot,ParentOutline,PageOutline,NextOutline,NextSect,NewPage,PrevOutline,PrevSect: Integer;
  Dictionaire: TPdfDictionary;
  XRefObjets: TPdfReference;
  Nom: TPdfName;
  FontName: string;
begin
inherited Create;
CreateRefTable;
CreateTrailer;
Catalogue:= CreateCatalog;
CreateInfo;
CreatePreferences;
ParentPage:= 0;
ParentOutline:= 0;
if Sections.Count> 1
then
  begin
  if Outline
  then
    begin
    OutlineRoot:= CreateOutlines;
    // add outline reference to catalog dictionary
    XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
    TPdfDictionary(TPdfXRef(FXRefObjets[Catalogue]).FObjet).AddElement('Outlines',XRefObjets);
    // add useoutline element to catalog dictionary
    Nom:= TPdfName.CreateName('UseOutlines');
    TPdfDictionary(TPdfXRef(FXRefObjets[Catalogue]).FObjet).AddElement('PageMode',Nom);
    end;
  TreeRoot:= CreatePages(ParentPage);
  end;
NumPage:= 0; // numéro de page identique à celui de l'appel à PrintPage
for CptSect:= 0 to Pred(Sections.Count) do
  begin
  if Sections.Count> 1
  then
    begin
    if Outline
    then
      begin
      ParentOutline:= CreateOutline(OutlineRoot,Succ(CptSect),-1,T_Section(Sections[CptSect]).Title);
      Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[OutlineRoot]).FObjet);
      TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
      if CptSect= 0
      then
        begin
        XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        TPdfDictionary(TPdfXRef(FXRefObjets[OutlineRoot]).FObjet).AddElement('First',XRefObjets);
        NextSect:= ParentOutline;
        PrevSect:= Pred(FXRefObjets.Count);
        end
      else
        begin
        XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        TPdfDictionary(TPdfXRef(FXRefObjets[NextSect]).FObjet).AddElement('Next',XRefObjets);
        XRefObjets:= TPdfReference.CreateReference(PrevSect);
        TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet).AddElement('Prev',XRefObjets);
        NextSect:= ParentOutline;
        if CptSect< Pred(Sections.Count)
        then
          PrevSect:= Pred(FXRefObjets.Count);
        end;
      if CptSect= Pred(Sections.Count)
      then
        begin
        XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        TPdfDictionary(TPdfXRef(FXRefObjets[OutlineRoot]).FObjet).AddElement('Last',XRefObjets);
        end;
      end;
    ParentPage:= CreatePages(TreeRoot);
    end
  else
    ParentPage:= CreatePages(ParentPage);
  for CptPage:= 0 to Pred(T_Section(Sections[CptSect]).Pages.Count) do
    begin
    with T_Section(Sections[CptSect]) do
      NewPage:= CreatePage(ParentPage,Paper.H,Paper.W);
    Inc(NumPage);
    PageNum:= CreateContents; // pagenum = object number in the pdf file
    CreateStream(NumPage,PageNum);
    if (Sections.Count> 1) and Outline
    then
      begin
      PageOutline:= CreateOutline(ParentOutline,Succ(CptSect),Succ(Cptpage),T_Section(Sections[CptSect]).Title);
      Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet);
      TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).IncrementeInteger;
      // add page reference to outline destination
      Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[PageOutline]).FObjet);
      XRefObjets:= TPdfReference.CreateReference(NewPage);
      TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(XRefObjets);
      // add display control name to outline destination
      Nom:= TPdfName.CreateName('Fit');
      TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(Nom);
      if CptPage= 0
      then
        begin
        XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet).AddElement('First',XRefObjets);
        NextOutline:= PageOutline;
        PrevOutline:= Pred(FXRefObjets.Count);
        // add page reference to parent outline destination
        Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet);
        XRefObjets:= TPdfReference.CreateReference(NewPage);
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(XRefObjets);
        // add display control name to outline destination
        Nom:= TPdfName.CreateName('Fit');
        TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Dest')]).FValue).AddItem(Nom);
        end
      else
        begin
        XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        TPdfDictionary(TPdfXRef(FXRefObjets[NextOutline]).FObjet).AddElement('Next',XRefObjets);
        XRefObjets:= TPdfReference.CreateReference(PrevOutline);
        TPdfDictionary(TPdfXRef(FXRefObjets[PageOutline]).FObjet).AddElement('Prev',XRefObjets);
        NextOutline:= PageOutline;
        if CptPage< Pred(T_Section(Sections[CptSect]).Pages.Count)
        then
          PrevOutline:= Pred(FXRefObjets.Count);
        end;
      if CptPage= Pred(T_Section(Sections[CptSect]).Pages.Count)
      then
        begin
        XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
        TPdfDictionary(TPdfXRef(FXRefObjets[ParentOutline]).FObjet).AddElement('Last',XRefObjets);
        end;
      end;
    end;
  end;
if Sections.Count> 1
then
  begin
  // update count in root parent pages dictionary
  Dictionaire:= TPdfDictionary(TPdfXRef(FXRefObjets[TreeRoot]).FObjet);
  TPdfInteger(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('Count')]).FValue).Value:= T_Section(Sections[CptSect]).TotPages;
  end;
NumFont:= 0;
for Cpt:= 0 to Pred(Fonts.Count) do
  begin
  FontName:= ExtractBaseFontName(T_Font(Fonts[Cpt]).GetFont.FontDesc);
  CreateFont(FontName,NumFont);
  Inc(NumFont);
  end;
TPdfInteger(TPdfDicElement(Trailer.FElement[Trailer.ElementParCle('Size')]).FValue).FValue:= FXRefObjets.Count;
end;

destructor TPdfDocument.Destroy;
var
  Cpt: Integer;
begin
Trailer.Free;
if FXRefObjets.Count> 0
then
  for Cpt:= 0 to Pred(FXRefObjets.Count) do
    TPdfXRef(FXRefObjets[Cpt]).Free;
FXRefObjets.Free;
inherited;
end;

procedure TPdfDocument.WriteDocument(const AFlux: TStream);
var
  Cpt,XRefPos: Integer;
begin
AFlux.Position:= 0;
WriteChaine(PDF_VERSION+CRLF,AFlux);
// write numbered indirect objects
for Cpt:= 1 to Pred(FXRefObjets.Count) do
  begin
  XRefPos:= AFlux.Position;
  WriteObjet(Cpt,AFlux);
  TPdfXRef(FXRefObjets[Cpt]).Offset:= XRefPos;
  end;
XRefPos:= AFlux.Position;
// write xref table
WriteChaine('xref'+CRLF+'0 '+IntToStr(FXRefObjets.Count)+CRLF,AFlux);
with TPdfXRef(FXRefObjets[0]) do
  WriteChaine(IntToChaine(Offset,10)+' '+IntToChaine(PDF_MAX_GEN_NUM,5)+' f'+CRLF,AFlux);
WriteXRefTable(AFlux);
// write trailer
WriteChaine('trailer'+CRLF,AFlux);
Trailer.WriteDictionary(AFlux);
// write offset of last xref table
WriteChaine(CRLF+'startxref'+CRLF+IntToStr(XRefPos)+CRLF,AFlux);
WriteChaine(PDF_FILE_END,AFlux);
end;

end.

