{
    << Impressions >>  U_Commande.pas

    Copyright (C) 2010 - JM.Levecque - <jmarc.levecque@jmlesite.fr>

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
  fpg_base, fpg_main, U_Pdf;

type
  TZone = (zEnTete,zPied,zPage,zMarges);
  TSectPageNum = (PageNum,SectNum,PSectNum);
  TFBordFlags= set of (bcGauche,bcDroite,bcHaut,bcBas);

  TDimensions= record
    T: Single;
    L: Single;
    R: Single;
    B: Single;
    end;

  TPapier= record
    H: Integer;
    W: Integer;
    Imprimable: TDimensions;
    end;

  // document classes

  T_Section = class
    private
      FNumSect: Integer;
      FNbPages: Integer;
      FPaper: TPapier;
      FMarges: TDimensions;
      FBasEnTete: Single;
      FHautPied: Single;
      FPages: TList;
      FEnTete: TList;
      FPied: TList;
      FCadres: TList;
      FColonnes: TList;
      FTitre: string;
      function GetFirstPage: Integer;
      function GetTotalPages: Integer;
    public
      constructor Create(APaper: TPapier; AMarges: TDimensions; ANum: Integer); virtual;
      destructor Destroy; override;
      procedure LoadPage(APageNum: Integer);
      procedure LoadCmdEnTete;
      procedure LoadCmdPage;
      procedure LoadCmdPied;
      procedure LoadCmdGroupe;
      procedure LoadCmdGroupeToPage;
      procedure LoadEspaceEnTete(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer);
      procedure LoadEspacePage(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer);
      procedure LoadEspacePied(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer);
      procedure LoadEspaceGroupe(AHeight: Single);
      procedure LoadCadre(AStyle: Integer; AZone: TZone);
      procedure LoadTrait(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single; AStyle: Integer);
      procedure LoadTraitHorizEnTete(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single; AStyle: Integer);
      procedure LoadTraitHorizPage(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single; AStyle: Integer);
      procedure LoadTraitHorizPied(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single; AStyle: Integer);
      procedure LoadTraitHorizGroupe(AHeight: Single);
      procedure LoadSurf(APos: T_Points; AColor: TfpgColor);
      function GetCmdPage(NumPage: Integer): TList;
      property CmdEnTete: TList read FEntete;
      property CmdPied: TList read FPied;
      property NbPages: Integer read FNbPages;
      property FirstPage: Integer read GetFirstPage;
      property Pages: TList read FPages;
      property TotPages: Integer read GetTotalPages;
      property Paper: TPapier read FPaper;
      property Marges: TDimensions read FMarges;
      property CmdCadres: TList read FCadres;
      property Colonnes: TList read FColonnes;
      property Titre: string read FTitre write FTitre;
    end;

  T_Page = class
    private
      FNumPageTot: Integer;
      FNumPageSect: Integer;
      FCommandes: TList;
    public
      constructor Create(ANumSec,ANumTot: Integer); virtual;
      destructor Destroy; override;
      property Commandes: TList read FCommandes write FCommandes;
      property PagesTot: Integer read FNumPageTot;
      property PagesSect: Integer read FNumPageSect;
    end;

  T_Groupe = class
    private
      FLineHeight: Single;
      FGroupeHeight: Single;
      FCommandes: TList;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      property Commandes: TList read FCommandes write FCommandes;
      property LineHeight: Single read FLineHeight;
      property GroupeHeight: Single read FGroupeHeight;
    end;

  T_Ligne = class
    private
      FHeight: Integer;
      FCommandes: TList;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure LoadTexte(APosX,APosY: Single; AColonne,ATexte,AFonte,AHeight,AFond,ABord,AInterL: Integer;
                ACurFont: Boolean; AFlags: TfpgTextFlags);
      procedure LoadNumero(APosX,APosY: Single; AColonne,ATexteNum,ATexteTot,AFonte,AHeight,AFond,ABord,AInterL: Integer;
                ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
      property Commandes: TList read FCommandes;
      property LineHeight: Integer read FHeight;
    end;

  // command classes

  T_Commande = class
    end;

  PSection = ^T_Section;
  PPage = ^T_Page;
  PLigne = ^T_Ligne;
  PCommande = ^T_Commande;
  PFont = ^TfpgFont;

  T_EcritTexte = class(T_Commande)
    private
      FPosX: Single;
      FPosY: Single;
      FColonne: Integer;
      FTexte: Integer;
      FFonte: Integer;
      FFond: Integer;
      FBord: Integer;
      FInterL: Integer;
      FCurFont: Boolean;
      FFlags: TfpgTextFlags;
    public
      constructor Create(APosX,APosY: Single; AColonne,ATexte,AFonte,AFond,ABord,AInterL: Integer; ACurFont: Boolean; AFlags: TfpgTextFlags); virtual;
      procedure SetPosY(const AValue: Single);
      property GetPosX: Single read FPosX;
      property GetPosY: Single read FPosY;
      property GetColonne: Integer read FColonne;
      property GetTexte: Integer read FTexte;
      property GetFonte: Integer read FFonte;
      property GetFond: Integer read FFond;
      property GetBord: Integer read FBord;
      property GetInterL: Integer read FInterL;
      property GetCurFont: Boolean read FCurFont;
      property GetFlags: TfpgTextFlags read FFlags;
    end;

  T_Numero = class(T_Commande)
    private
      FPosX: Single;
      FPosY: Single;
      FColonne: Integer;
      FTexteNum: Integer;
      FTexteTot: Integer;
      FFonte: Integer;
      FFond: Integer;
      FBord: Integer;
      FInterL: Integer;
      FCurFont: Boolean;
      FFlags: TfpgTextFlags;
      FTotal: Boolean;
      FAlpha: Boolean;
      FTypeNum: TSectPageNum;
    public
      constructor Create(APosX,APosY: Single; AColonne,ATexteNum,ATexteTot,AFonte,AFond,ABord,AInterL: Integer;
                  ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum); virtual;
      procedure SetPosY(const AValue: Single);
      property GetPosX: Single read FPosX;
      property GetPosY: Single read FPosY;
      property GetColonne: Integer read FColonne;
      property GetTexteNum: Integer read FTexteNum;
      property GetTexteTot: Integer read FTexteTot;
      property GetFonte: Integer read FFonte;
      property GetFond: Integer read FFond;
      property GetBord: Integer read FBord;
      property GetInterL: Integer read FInterL;
      property GetCurFont: Boolean read FCurFont;
      property GetFlags: TfpgTextFlags read FFlags;
      property GetTotal: Boolean read FTotal;
      property GetAlpha: Boolean read FAlpha;
      property GetTypeNum: TSectPageNum read FTypeNum;
    end;

  T_Trait = class(T_Commande)
    private
      FPosX: Single;
      FPosY: Single;
      FColonne: Integer;
      FStyle: Integer;
      FEndX: Single;
      FEndY: Single;
    public
      constructor Create(APosX,APosY: Single; AColonne,AStyle: Integer; AEndX,AEndY: Single); virtual;
      property GetPosX: Single read FPosX;
      property GetPosY: Single read FPosY;
      property GetColonne: Integer read FColonne;
      property GetStyle: Integer read FStyle;
      property GetEndX: Single read FEndX;
      property GetEndY: Single read FEndY;
    end;

  T_Colonne = class(T_Commande)
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

  T_Fonte = class(T_Commande)
    private
      FFonte: TfpgFont;
      FColor: TfpgColor;
      FSize: string;
    public
      constructor Create(AFonte: string; AColor: TfpgColor); virtual;
      destructor Destroy; override;
      function GetHeight: Integer;
      property GetFonte: TfpgFont read FFonte;
      property GetColor: TfpgColor read FColor;
      property GetSize: string read FSize;
    end;

  T_Interligne = class(T_Commande)
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

  T_Espace = class(T_Commande)
    private
      FPosY: Single;
      FColonne: Integer;
      FHeight: Single;
      FFond: Integer;
    public
      constructor Create(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer); virtual;
      procedure SetPosY(const AValue: Single);
      property GetPosY: Single read FPosY;
      property GetColonne: Integer read FColonne;
      property GetHeight: Single read FHeight;
      property GetFond: Integer read FFond;
    end;

  T_Fond = class(T_Commande)
    private
      FColor: TfpgColor;
    public
      constructor Create(AColor: TfpgColor); virtual;
      property GetColor: TfpgColor read FColor;
    end;

  T_TraitStyle = class(T_Commande)
    private
      FEpais: Single;
      FColor: TfpgColor;
      FStyle: TfpgLineStyle;
    public
      constructor Create(AEpais: Single; AColor: Tfpgcolor; AStyle: TfpgLineStyle); virtual;
      property GetEpais: Single read FEpais;
      property GetColor: TfpgColor read FColor;
      property GetStyle: TfpgLineStyle read FStyle;
    end;

  T_Bord = class(T_Commande)
    private
      FFlags: TFBordFlags;
      FStyle: Integer;
    public
      constructor Create(AFlags: TFBordFlags; AStyle: Integer);
      property GetFlags: TFBordFlags read FFlags;
      property GetStyle: Integer read FStyle;
    end;

  T_Cadre = class(T_Commande)
    private
      FStyle: Integer;
      FZone: TZone;
    public
      constructor Create(AStyle: Integer; AZone: TZone);
      property GetStyle: Integer read FStyle;
      property GetZone: TZone read FZone;
    end;

  T_Surface = class(T_Commande)
    private
      FPoints: T_Points;
      FColor: TfpgColor;
    public
      constructor Create(APoints: array of TRefPos; AColor: TfpgColor);
      property GetPoints: T_Points read FPoints;
      property GetColor: TfpgColor read FColor;
    end;

var
  Sections: TList;
//  Colonnes: TList;
  Textes: TStringList;
  Fontes: TList;
  Interlignes: TList;
  Fonds: TList;
  TraitStyles: TList;
  Bords: TList;
  ASection: T_Section;
  APage: T_Page;
  AGroupe: T_Groupe;
  ALigne: T_Ligne;
  ACommande: T_Commande;
  AColonne: T_Colonne;
  AFond: T_Fond;
  AFonte: T_Fonte;
  AInterligne: T_Interligne;
  ATraitStyle: T_TraitStyle;
  ABord: T_Bord;

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

constructor T_Section.Create(APaper: TPapier; AMarges: TDimensions; ANum: Integer);
begin
FNumSect:= ANum;
FNbPages:= 0;
FPaper:= APaper;
FMarges:= AMarges;
FBasEnTete:= FMarges.T;
FHautPied:= FMarges.B;
FPages:= TList.Create;
FEnTete:= TList.Create;
FPied:= TList.Create;
FCadres:= TList.Create;
FColonnes:= TList.Create;
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
if FEntete.Count> 0
then
  for Cpt:= 0 to Pred(FEntete.Count) do
    T_Commande(FEntete[Cpt]).Free;
FEnTete.Free;
if FPied.Count> 0
then
  for Cpt:= 0 to Pred(FPied.Count) do
    T_Commande(FPied[Cpt]).Free;
FPied.Free;
if FCadres.Count> 0
then
  for Cpt:= 0 to Pred(FCadres.Count) do
    T_Commande(FCadres[Cpt]).Free;
FCadres.Free;
if FColonnes.Count> 0
then
  for Cpt:= 0 to Pred(FColonnes.Count) do
    T_Commande(FColonnes[Cpt]).Free;
FColonnes.Free;
inherited Destroy;
end;

procedure T_Section.LoadPage(APageNum: Integer);
begin
Inc(FNbPages);
APage:= T_Page.Create(FNbPages,APageNum);
FPages.Add(APage);
end;

procedure T_Section.LoadCmdEnTete;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(ALigne.Commandes.Count) do
  FEnTete.Add(ALigne.Commandes.Items[Cpt]);
ALigne.FHeight:= 0;
ALigne.Commandes.Clear;
end;

procedure T_Section.LoadCmdPage;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(ALigne.Commandes.Count) do
  T_Page(Pages[Pred(FPages.Count)]).Commandes.Add(ALigne.Commandes.Items[Cpt]);
ALigne.FHeight:= 0;
ALigne.Commandes.Clear;
end;

procedure T_Section.LoadCmdPied;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(ALigne.Commandes.Count) do
  FPied.Add(ALigne.Commandes.Items[Cpt]);
ALigne.FHeight:= 0;
ALigne.Commandes.Clear;
end;

procedure T_Section.LoadCmdGroupe;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(ALigne.Commandes.Count) do
  AGroupe.Commandes.Add(ALigne.Commandes.Items[Cpt]);
with AGroupe do
  begin
  FLineHeight:= ALigne.FHeight;
  FGroupeHeight:= FGroupeHeight+FLineHeight;
  end;
ALigne.FHeight:= 0;
ALigne.Commandes.Clear;
end;

procedure T_Section.LoadCmdGroupeToPage;
var
  Cpt: Integer;
begin
for Cpt:= 0 to Pred(AGroupe.Commandes.Count) do
  T_Page(Pages[Pred(FPages.Count)]).Commandes.Add(AGroupe.Commandes.Items[Cpt]);
AGroupe.FGroupeHeight:= 0;
AGroupe.Commandes.Clear;
end;

procedure T_Section.LoadEspaceEnTete(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer);
begin
ACommande:= T_Espace.Create(APosY,AColonne,AHeight,AFond);
FEnTete.Add(ACommande);
end;

procedure T_Section.LoadEspacePage(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer);
begin
ACommande:= T_Espace.Create(APosY,AColonne,AHeight,AFond);
T_Page(Pages[Pred(FPages.Count)]).Commandes.Add(ACommande);
end;

procedure T_Section.LoadEspacePied(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer);
begin
ACommande:= T_Espace.Create(APosY,AColonne,AHeight,AFond);
FPied.Add(ACommande);
end;

procedure T_Section.LoadEspaceGroupe(AHeight: Single);
begin
AGroupe.FGroupeHeight:= AGroupe.FGroupeHeight+AHeight;
end;

procedure T_Section.LoadCadre(AStyle: Integer; AZone: TZone);
begin
ACommande:= T_Cadre.Create(AStyle,AZone);
FCadres.Add(ACommande);
end;

procedure T_Section.LoadTrait(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single; AStyle: Integer);
begin
ACommande:= T_Trait.Create(APosXDeb,APosYDeb,AColonne,AStyle,APosXFin,APosYFin);
T_Page(Pages[Pred(FPages.Count)]).Commandes.Add(ACommande);
end;

procedure T_Section.LoadTraitHorizEnTete(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single;
          AStyle: Integer);
begin
ACommande:= T_Trait.Create(APosXDeb,APosYDeb,AColonne,AStyle,APosXFin,APosYFin);
FEnTete.Add(ACommande);
end;

procedure T_Section.LoadTraitHorizPage(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single; AStyle: Integer);
begin
ACommande:= T_Trait.Create(APosXDeb,APosYDeb,AColonne,AStyle,APosXFin,APosYFin);
T_Page(Pages[Pred(FPages.Count)]).Commandes.Add(ACommande);
end;

procedure T_Section.LoadTraitHorizPied(APosXDeb,APosYDeb: Single; AColonne: Integer; APosXFin,APosYFin: Single; AStyle: Integer);
begin
ACommande:= T_Trait.Create(APosXDeb,APosYDeb,AColonne,AStyle,APosXFin,APosYFin);
FPied.Add(ACommande);
end;

procedure T_Section.LoadTraitHorizGroupe(AHeight: Single);
begin
AGroupe.FGroupeHeight:= AGroupe.FGroupeHeight+AHeight;
end;

procedure T_Section.LoadSurf(APos: T_Points; AColor: TfpgColor);
begin
Acommande:= T_Surface.Create(APos,AColor);
T_Page(Pages[Pred(FPages.Count)]).Commandes.Add(ACommande);
end;

function T_Section.GetCmdPage(NumPage: Integer): TList;
begin
Result:= T_Page(Pages[Pred(NumPage)]).Commandes;
end;

constructor T_Page.Create(ANumSec,ANumTot: Integer);
begin
FNumPageTot:= ANumTot;
FNumPageSect:= ANumSec;
FCommandes:= TList.Create;
end;

destructor T_Page.Destroy;
var
  Cpt: Integer;
begin
if FCommandes.Count> 0
then
  for Cpt:= 0 to Pred(FCommandes.Count) do
    T_Commande(FCommandes[Cpt]).Free;
FCommandes.Free;
inherited Destroy;
end;

constructor T_Groupe.Create;
begin
FLineHeight:= 0;
FGroupeHeight:= 0;
FCommandes:= TList.Create;
end;

destructor T_Groupe.Destroy;
var
  Cpt: Integer;
begin
if FCommandes.Count> 0
then
  for Cpt:= 0 to Pred(FCommandes.Count) do
    T_Commande(FCommandes[Cpt]).Free;
FCommandes.Free;
inherited Destroy;
end;

constructor T_Ligne.Create;
begin
FHeight:= 0;
FCommandes:= TList.Create;
end;

destructor T_Ligne.Destroy;
var
  Cpt: Integer;
begin
if FCommandes.Count> 0
then
  for Cpt:= 0 to Pred(FCommandes.Count) do
    T_Commande(FCommandes[Cpt]).Free;
FCommandes.Free;
inherited Destroy;
end;

procedure T_Ligne.LoadTexte(APosX,APosY: Single; AColonne,ATexte,AFonte,AHeight,AFond,ABord,AInterL: Integer;
          ACurFont: Boolean; AFlags: TfpgTextFlags);
begin
if FHeight< AHeight
then
  FHeight:= AHeight;
ACommande:= T_EcritTexte.Create(APosX,APosY,AColonne,ATexte,AFonte,AFond,ABord,AInterL,ACurFont,AFlags);
Commandes.Add(ACommande);
end;

procedure T_Ligne.LoadNumero(APosX,APosY: Single; AColonne,ATexteNum,ATexteTot,AFonte,AHeight,AFond,ABord,AInterL: Integer;
          ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
begin
if FHeight< AHeight
then
  FHeight:= AHeight;
ACommande:= T_Numero.Create(APosX,APosY,AColonne,ATexteNum,ATexteTot,AFonte,AFond,ABord,AInterL,ACurFont,AFlags,ATotal,AAlpha,ATypeNum);
Commandes.Add(ACommande);
end;

// command class methods

procedure T_EcritTexte.SetPosY(const AValue: Single);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_EcritTexte.Create(APosX,APosY: Single; AColonne,ATexte,AFonte,AFond,ABord,AInterL: Integer; ACurFont: Boolean; AFlags: TfpgTextFlags);
begin
inherited Create;
FPosX:= APosX;
FPosY:= APosY;
FColonne:= AColonne;
FTexte:= ATexte;
FFonte:= AFonte;
FFond:= AFond;
FBord:= ABord;
FInterL:= AInterL;
FCurFont:= ACurFont;
FFlags:= AFlags;
end;

procedure T_Numero.SetPosY(const AValue: Single);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_Numero.Create(APosX,APosY: Single; AColonne,ATexteNum,ATexteTot,AFonte,AFond,ABord,AInterL: Integer;
            ACurFont: Boolean; AFlags: TfpgTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
begin
inherited Create;
FPosX:= APosX;
FPosY:= APosY;
FColonne:= AColonne;
FTexteNum:= ATexteNum;
FTexteTot:= ATexteTot;
FFonte:= AFonte;
FFond:= AFond;
FBord:= ABord;
FInterL:= AInterL;
FCurFont:= ACurFont;
FFlags:= AFlags;
FTotal:= ATotal;
FAlpha:= AAlpha;
FTypeNum:= ATypeNum;
end;

constructor T_Trait.Create(APosX,APosY: Single; AColonne,AStyle: Integer; AEndX,AEndY: Single);
begin
FPosX:= APosX;
FPosY:= APosY;
FColonne:= AColonne;
FStyle:= AStyle;
FEndX:= AEndX;
FEndY:= AEndY;
end;

constructor T_Colonne.Create(APos,AWidth,AMargin: Single; AColor: TfpgColor);
begin
inherited Create;
FPos:= APos;
FWidth:= AWidth;
FMargin:= AMargin;
FColor:= AColor;
end;

function T_Colonne.GetTextPos: Single;
begin
Result:= FPos+FMargin;
end;

function T_Colonne.GetTextWidth: Single;
begin
Result:= FWidth-(FMargin*2);
end;

procedure T_Colonne.SetColColor(AColor: TfpgColor);
begin
if FColor<> AColor
then
  FColor:= AColor;
end;

constructor T_Fonte.Create(AFonte: string; AColor: TfpgColor);
begin
inherited Create;
FFonte:= fpgApplication.GetFont(AFonte);
FColor:= AColor;
FSize:= ExtractFontSize(AFonte);
end;

destructor T_Fonte.Destroy;
begin
  FFonte.Free;
  inherited Destroy;
end;

function T_Fonte.GetHeight: Integer;
begin
Result:= TfpgFont(FFonte).Height;
end;

constructor T_Interligne.Create(ASup,AInt,AInf: Single);
begin
inherited Create;
FSup:= ASup;
FInt:= AInt;
FInf:= AInf;
end;

constructor T_Espace.Create(APosY: Single; AColonne: Integer; AHeight: Single; AFond: Integer);
begin
inherited Create;
FPosY:= APosY;
FColonne:= AColonne;
FHeight:= AHeight;
FFond:= AFond;
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

procedure T_Espace.SetPosY(const AValue: Single);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_Fond.Create(AColor: TfpgColor);
begin
FColor:= AColor;
end;

constructor T_TraitStyle.Create(AEpais: Single; AColor: Tfpgcolor; AStyle: TfpgLineStyle);
begin
inherited Create;
FEpais:= AEpais;
FColor:= AColor;
FStyle:= AStyle;
end;

constructor T_Bord.Create(AFlags: TFBordFlags; AStyle: Integer);
begin
FFlags:= AFlags;
FStyle:= AStyle;
end;

constructor T_Cadre.Create(AStyle: Integer; AZone: TZone);
begin
FStyle:= AStyle;
FZone:= AZone;
end;

end.

