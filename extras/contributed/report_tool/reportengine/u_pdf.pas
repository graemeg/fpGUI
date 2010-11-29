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
      procedure EcritBoolean(const AFlux: TStream);
    public
      constructor CreateBoolean(const AValue: Boolean);
      destructor Destroy; override;
    end;

  TPdfInteger = class(TPdfObjet)
    private
      FValue: Integer;
    protected
      procedure EcritInteger(const AFlux: TStream);
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
      procedure EcritReference(const AFlux: TStream);
    public
      constructor CreateReference(const AValue: Integer);
      destructor Destroy; override;
    end;

  TPdfName = class(TPdfObjet)
    private
      FValue: string;
    protected
      procedure EcritName(const AFlux: TStream);
    public
      constructor CreateName(const AValue: string);
      destructor Destroy; override;
    end;

  TPdfString = class(TPdfObjet)
    private
      FValue: string;
    protected
      procedure EcritString(const AFlux: TStream);
    public
      constructor CreateString(const AValue: string);
      destructor Destroy; override;
    end;

  TPdfArray = class(TPdfObjet)
    private
      FArray: TList;
    protected
      procedure EcritArray(const AFlux: TStream);
      procedure AddItem(const AValue: TPdfObjet);
    public
      constructor CreateArray;
      destructor Destroy; override;
    end;

  TPdfStream = class(TPdfObjet)
    private
      FStream: TList;
    protected
      procedure EcritStream(const AFlux: TStream);
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
      procedure EcritFonte(const AFlux: TStream);
    public
      constructor CreateFonte(const AFont: Integer; const ASize: string);
      destructor Destroy; override;
    end;

  TPdfText = class(TPdfObjet)
    private
      FTxtPosX: Integer;
      FTxtPosY: Integer;
      FTxtText: TPdfString;
    protected
      procedure EcritText(const AFlux: TStream);
    public
      constructor CreateText(const APosX,APosY: Integer; const AText: string);
      destructor Destroy; override;
    end;

  TPdfLigne = class(TPdfObjet)
    private
      FEpais: Integer;
      FStaX: Integer;
      FStaY: Integer;
      FEndX: Integer;
      FEndY: Integer;
    protected
      procedure EcritLigne(const AFlux: TStream);
    public
      constructor CreateLigne(const AEpais,AStaX,AStaY,AEndX,AEndY: Integer);
      destructor Destroy; override;
    end;

  TPdfRectangle = class(TPdfObjet)
    private
      FEpais: Integer;
      FRecX: Integer;
      FRecY: Integer;
      FRecW: Integer;
      FRecH: Integer;
      FFill: Boolean;
      FStroke: Boolean;
    protected
      procedure EcritRectangle(const AFlux: TStream);
    public
      constructor CreateRectangle(const AEpais,APosX,APosY,AWidth,AHeight: Integer; const AFill,AStroke: Boolean);
      destructor Destroy; override;
    end;

  TPdfLineStyle = class(TPdfObjet)
    private
      FDash: TfpgLineStyle;
      FPhase: Integer;
    protected
      procedure EcritLineStyle(const AFlux: TStream);
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
      procedure EcritColor(const AFlux: TStream);
    public
      constructor CreateColor(const AStroke: Boolean; Couleur: LongInt);
      destructor Destroy; override;
    end;

  TPdfDicElement = class(TObject)
    private
      FKey: TPdfName;
      FValue: TPdfObjet;
    protected
      procedure EcritDicElement(const AFlux: TStream);
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
      procedure EcritDictionary(AFlux: TStream);
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
      procedure EcritXRef(const AFlux: TStream);
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
      procedure EcritXRefTable(const AFlux: TStream);
      procedure EcritObjet(const AObjet: Integer; const AFlux: TStream);
      procedure CreateRefTable;
      procedure CreateTrailer;
      procedure CreateCatalog;
      procedure CreateInfo;
      procedure CreatePreferences;
      function CreatePages(Parent: Integer): Integer;
      procedure CreatePage(Parent: Integer);
      procedure CreateFont(NomFonte: string; NumFonte: Integer);
      function CreateContents: Integer;
      procedure CreateStream(NumeroPage,PageNum: Integer);
    public
      constructor CreateDocument;
      destructor Destroy; override;
      procedure EcritDocument(const AFlux: TStream);
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

implementation

uses
  U_Imprime, U_Commande;

var
  Trailer: TPdfDictionary;
  CurrentColor: string;
  CurrentWidth: string;

// utility functions

procedure EcritChaine(const Valeur: string; AFlux: TStream);
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

function ColorToString(Couleur: Integer): string;
var
  Red,Green,Blue: Integer;
begin
Red:= Couleur div 65535;
Couleur:= Couleur mod 65535;
Green:= Couleur div 255;
Blue:= Couleur mod 255;
end;

// object methods

constructor TPdfObjet.Create;
begin
  // implementation dans les descendants
end;

destructor TPdfObjet.Destroy;
begin
inherited;
end;

procedure TPdfBoolean.EcritBoolean(const AFlux: TStream);
begin
if FValue
then
  EcritChaine('true',AFlux)
else
  EcritChaine('false',AFlux);
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

procedure TPdfInteger.EcritInteger(const AFlux: TStream);
begin
EcritChaine(IntToStr(FValue), AFlux);
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

procedure TPdfReference.EcritReference(const AFlux: TStream);
begin
EcritChaine(IntToStr(FValue)+' 0 R',AFlux);
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

procedure TPdfName.EcritName(const AFlux: TStream);
begin
if FValue<> ''
then
  EcritChaine('/'+FValue,AFlux);
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

procedure TPdfString.EcritString(const AFlux: TStream);
begin
EcritChaine('('+Utf8ToAnsi(FValue)+')',AFlux);
end;

constructor TPdfString.CreateString(const AValue: string);
begin
inherited Create;
FValue:= AValue;
end;

destructor TPdfString.Destroy;
begin
inherited;
end;

procedure TPdfArray.EcritArray(const AFlux: TStream);
var
  Cpt: Integer;
begin
EcritChaine('[',AFlux);
for Cpt:= 0 to Pred(FArray.Count) do
  begin
  if Cpt> 0
  then
    EcritChaine(' ',AFlux);
  if TPdfObjet(FArray[Cpt]) is TPdfInteger
  then
    TPdfInteger(FArray[Cpt]).EcritInteger(AFlux);
  if TPdfObjet(FArray[Cpt]) is TPdfReference
  then
    TPdfReference(FArray[Cpt]).EcritReference(AFlux);
  if TPdfObjet(FArray[Cpt]) is TPdfName
  then
    TPdfName(FArray[Cpt]).EcritName(AFlux);
  end;
EcritChaine(']',AFlux);
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
begin
FArray.Free;
inherited;
end;

procedure TPdfStream.EcritStream(const AFlux: TStream);
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(FStream.Count) do
  begin
  if TPdfObjet(FStream[Cpt]) is TPdfFonte
  then
    TPdfFonte(FStream[Cpt]).EcritFonte(AFlux);
  if TPdfColor(FStream[Cpt]) is TPdfColor
  then
    TPdfColor(FStream[Cpt]).EcritColor(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfText
  then
    TPdfText(FStream[Cpt]).EcritText(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfRectangle
  then
    TPdfRectangle(FStream[Cpt]).EcritRectangle(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfLigne
  then
    TPdfLigne(FStream[Cpt]).EcritLigne(AFlux);
  if TPdfObjet(FStream[Cpt]) is TPdfLineStyle
  then
    TPdfLineStyle(FStream[Cpt]).EcritLineStyle(AFlux);
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
begin
FStream.Free;
inherited;
end;

procedure TPdfFonte.EcritFonte(const AFlux: TStream);
begin
EcritChaine('/F'+IntToStr(FTxtFont)+' '+FTxtSize+' Tf'+CRLF,AFlux);
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

procedure TPdfText.EcritText(const AFlux: TStream);
begin
EcritChaine('BT'+CRLF,AFlux);
EcritChaine(IntToStr(FTxtPosX)+' '+IntToStr(FTxtPosY)+' Td'+CRLF,AFlux);
TPdfString(FTxtText).EcritString(AFlux);
EcritChaine(' Tj'+CRLF,AFlux);
EcritChaine('ET'+CRLF,AFlux);
end;

constructor TPdfText.CreateText(const APosX,APosY: Integer; const AText: string);
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

procedure TPdfLigne.EcritLigne(const AFlux: TStream);
begin
if (IntToStr(FEpais)+' w')<> CurrentWidth
then
  begin
  EcritChaine('1 J'+CRLF,AFlux);
  EcritChaine(IntToStr(FEpais)+' w'+CRLF,AFlux);
  CurrentWidth:= IntToStr(FEpais)+' w';
  end;
EcritChaine(IntToStr(FStaX)+' '+IntToStr(FStaY)+' m'+CRLF,AFlux);
EcritChaine(IntToStr(FEndX)+' '+IntToStr(FEndY)+' l'+CRLF,AFlux);
EcritChaine('S'+CRLF,AFlux);
end;

constructor TPdfLigne.CreateLigne(const AEpais,AStaX,AStaY,AEndX,AEndY: Integer);
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

procedure TPdfRectangle.EcritRectangle(const AFlux: TStream);
begin
if FStroke
then
  if (IntToStr(FEpais)+' w')<> CurrentWidth
  then
    begin
    EcritChaine('1 J'+CRLF,AFlux);
    EcritChaine(IntToStr(FEpais)+' w'+CRLF,AFlux);
    CurrentWidth:= IntToStr(FEpais)+' w';
    end;
EcritChaine(IntToStr(FRecX)+' '+IntToStr(FRecY)+' '+IntToStr(FRecW)+' '+IntToStr(FRecH)+' re'+CRLF,AFlux);
if FStroke
then
  EcritChaine('S'+CRLF,AFlux);
if FFill
then
  EcritChaine('f'+CRLF,AFlux);
end;

constructor TPdfRectangle.CreateRectangle(const AEpais,APosX,APosY,AWidth,AHeight: Integer; const AFill,AStroke: Boolean);
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

procedure TPdfLineStyle.EcritLineStyle(const AFlux: TStream);
begin
EcritChaine('[',AFlux);
case FDash of
  lsDash:
    EcritChaine('5 5',AFlux);
  lsDot:
    EcritChaine('2 2',AFlux);
  lsDashDot:
    EcritChaine('5 2 2 2',AFlux);
  lsDashDotDot:
    EcritChaine('5 2 2 2 2 2',AFlux);
  end;
EcritChaine('] '+IntToStr(FPhase)+' d'+CRLF,AFlux);
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

procedure TPdfColor.EcritColor(const AFlux: TStream);
begin
if FStroke
then
  begin
  if (FRed+' '+FGreen+' '+FBlue+' rg')<> CurrentColor
  then
    begin
    EcritChaine(FRed+' '+FGreen+' '+FBlue+' rg'+CRLF,AFlux);
    CurrentColor:= FRed+' '+FGreen+' '+FBlue+' rg';
    end;
  end
else
  if (FRed+' '+FGreen+' '+FBlue+' RG')<> CurrentColor
  then
    begin
    EcritChaine(FRed+' '+FGreen+' '+FBlue+' RG'+CRLF,AFlux);
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

procedure TPdfDicElement.EcritDicElement(const AFlux: TStream);
begin
FKey.EcritName(AFlux);
EcritChaine(' ',AFlux);
if FValue is TPdfBoolean
then
  TPdfBoolean(FValue).EcritBoolean(AFlux);
if FValue is TPdfInteger
then
  TPdfInteger(FValue).EcritInteger(AFlux);
if FValue is TPdfReference
then
  TPdfReference(FValue).EcritReference(AFlux);
if FValue is TPdfName
then
  TPdfName(FValue).EcritName(AFlux);
if FValue is TPdfString
then
  TPdfString(FValue).EcritString(AFlux);
if FValue is TPdfArray
then
  TPdfArray(FValue).EcritArray(AFlux);
if FValue is TPdfDictionary
then
  TPdfDictionary(FValue).EcritDictionary(AFlux);
EcritChaine(CRLF,AFlux);
end;

constructor TPdfDicElement.CreateDicElement(const AKey: string; const AValue: TPdfObjet);
begin
inherited Create;
FKey:= TPdfName.CreateName(AKey);
FValue:= AValue;
end;

destructor TPdfDicElement.Destroy;
begin
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

procedure TPdfDictionary.EcritDictionary(AFlux: TStream);
var
  Cpt: Integer;
begin
EcritChaine('<<'+CRLF,AFlux);
for Cpt:= 0 to Pred(FElement.Count) do
  TPdfDicElement(FElement[Cpt]).EcritDicElement(AFlux);
EcritChaine('>>',AFlux);
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

procedure TPdfXRef.EcritXRef(const AFlux: TStream);
begin
EcritChaine(IntToChaine(FOffset,10)+' '+IntToChaine(0,5)+' n'+CRLF,AFlux);
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

procedure TPdfDocument.EcritXRefTable(const AFlux: TStream);
var
  Cpt: Integer;
begin
if FXRefObjets.Count> 1
then
  for Cpt:= 1 to Pred(FXRefObjets.Count) do
    TPdfXRef(FXRefObjets[Cpt]).EcritXRef(AFlux);
end;

procedure TPdfDocument.EcritObjet(const AObjet: Integer; const AFlux: TStream);
var
  Dictionaire: TPdfDictionary;
  Long: TPdfInteger;
  Fin: Integer;
  Flux: TMemoryStream;
begin
EcritChaine(IntToStr(AObjet)+' 0 obj'+CRLF,AFlux);
if TPdfXRef(FXRefObjets[AObjet]).FStream= nil
then
  TPdfDictionary(TPdfXRef(FXRefObjets[AObjet]).FObjet).EcritDictionary(AFlux)
else
  begin
   Flux:= TMemoryStream.Create;
  Flux.Position:= 0;
  CurrentColor:= '';
  CurrentWidth:= '';
  TPdfXRef(FXRefObjets[AObjet]).FStream.EcritStream(Flux);
// write stream length element in contents dictionary
  Long:= TPdfInteger.CreateInteger(Flux.Size);
  TPdfDictionary(TPdfXRef(FXRefObjets[AObjet]).FObjet).AddElement('Length',Long);
  Flux.Free;
  TPdfXRef(FXRefObjets[AObjet]).FObjet.EcritDictionary(AFlux);
// write stream in contents dictionary
  CurrentColor:= '';
  CurrentWidth:= '';
  EcritChaine(CRLF+'stream'+CRLF,AFlux);
  TPdfXRef(FXRefObjets[AObjet]).FStream.EcritStream(AFlux);
  EcritChaine('endstream',AFlux);
  end;
EcritChaine(CRLF+'endobj'+CRLF,AFlux);
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

procedure TPdfDocument.CreateCatalog;
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
Nom:= TPdfString.CreateString('FpGUI/FPC');
Info.FObjet.AddElement('Creator',Nom);
// add producer element to info dictionary
Nom:= TPdfString.CreateString('FpGUI/FPC');
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

procedure TPdfDocument.CreatePage(Parent: Integer);
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
Coord:= TPdfInteger.CreateInteger(Imprime.LargeurPapier);
TPdfArray(TPdfDicElement(Dictionaire.FElement[Dictionaire.ElementParCle('MediaBox')]).FValue).AddItem(Coord);
Coord:= TPdfInteger.CreateInteger(Imprime.HauteurPapier);
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
XRefObjets:= TPdfReference.CreateReference(Pred(FXRefObjets.Count));
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
        Txt:= TPdfText.CreateText(TextPosX,TextPosY,Ecriture);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Txt);
        end;
  if TPdfElement(PdfPage[Cpt]) is TPdfRect
  then
    if TPdfRect(PdfPage[Cpt]).PageId= NumeroPage
    then
      with TPdfRect(PdfPage[Cpt]) do
        begin
        if RectCouleur> -1
        then
          begin
          Clr:= TPdfColor.CreateColor(True,RectCouleur);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Clr);
          end;
        if RectTrace
        then
          begin
          Sty:= TPdfLineStyle.CreateLineStyle(RectLineStyle,0);
          TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Sty);
          end;
        Rct:= TPdfRectangle.CreateRectangle(RectEpais,RectGauche,RectBas,RectLarg,RectHaut,RectEmplit,RectTrace);
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
        Lin:= TPdfLigne.CreateLigne(LineEpais,LineStartX,LineStartY,LineEndX,LineEndY);
        TPdfStream(TPdfXRef(FXRefObjets[PageNum]).FStream).AddItem(Lin);
        end;
  end;
end;

constructor TPdfDocument.CreateDocument;
var
  Cpt,CptSect,CptPage,CptFont,NumFont,TreeRoot,Parent,PageNum,NumPage: Integer;
  Trouve: Boolean;
  Dictionaire: TPdfDictionary;
  FontName: string;
begin
inherited Create;
CreateRefTable;
CreateTrailer;
CreateCatalog;
CreateInfo;
CreatePreferences;
Parent:= 0;
if Sections.Count> 1
then
  TreeRoot:= CreatePages(Parent);
NumPage:= 0; // numéro de page identique à celui de l'appel à ImprimePage
for CptSect:= 0 to Pred(Sections.Count) do
  begin
  if Sections.Count> 1
  then
    Parent:= CreatePages(TreeRoot)
  else
    Parent:= CreatePages(Parent);
  for CptPage:= 0 to Pred(T_Section(Sections[CptSect]).Pages.Count) do
    begin
    CreatePage(Parent);
    Inc(NumPage);
    PageNum:= CreateContents; // pagenum = numéro d'objet dans le fichier PDF
    CreateStream(NumPage,PageNum);
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
for Cpt:= 0 to Pred(Fontes.Count) do
  begin
  Trouve:= False;
  FontName:= ExtractBaseFontName(T_Fonte(Fontes[Cpt]).GetFonte.FontDesc);
  CreateFont(FontName,NumFont);
  Inc(NumFont);
  end;
TPdfInteger(TPdfDicElement(Trailer.FElement[Trailer.ElementParCle('Size')]).FValue).FValue:= FXRefObjets.Count;
if PdfPage.Count> 0
then
  for CptPage:= 0 to Pred(PdfPage.Count) do
    TPdfElement(PdfPage[CptPage]).Free;
PdfPage.Free;
end;

destructor TPdfDocument.Destroy;
var
  Cpt: Integer;
begin
Trailer.Free;
for Cpt:= 0 to Pred(FXRefObjets.Count) do
  TPdfXRef(FXRefObjets[Cpt]).Free;
FXRefObjets.Free;
inherited;
end;

procedure TPdfDocument.EcritDocument(const AFlux: TStream);
var
  Cpt,XRefPos: Integer;
begin
AFlux.Position:= 0;
EcritChaine(PDF_VERSION+CRLF,AFlux);
// write numbered indirect objects
for Cpt:= 1 to Pred(FXRefObjets.Count) do
  begin
  XRefPos:= AFlux.Position;
  EcritObjet(Cpt,AFlux);
  TPdfXRef(FXRefObjets[Cpt]).Offset:= XRefPos;
  end;
XRefPos:= AFlux.Position;
// write xref table
EcritChaine('xref'+CRLF+'0 '+IntToStr(FXRefObjets.Count)+CRLF,AFlux);
with TPdfXRef(FXRefObjets[0]) do
  EcritChaine(IntToChaine(Offset,10)+' '+IntToChaine(PDF_MAX_GEN_NUM,5)+' f'+CRLF,AFlux);
EcritXRefTable(AFlux);
// write trailer
EcritChaine('trailer'+CRLF,AFlux);
Trailer.EcritDictionary(AFlux);
// write offset of last xref table
EcritChaine(CRLF+'startxref'+CRLF+IntToStr(XRefPos)+CRLF,AFlux);
EcritChaine(PDF_FILE_END,AFlux);
end;

end.

