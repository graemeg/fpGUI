{
    << Impressions >>  U_Pdf.pas

    Copyright (C) 2010 - JM.Levecque - <jmarc.levecque@jmlesite.fr>

   This library is a free software coming as a add-on to fpGUI toolkit
   See the copyright included in the fpGUI distribution for details about redistribution

   This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit builds the objects in memory to produce either the preview or pdf file
}

unit U_Commande;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main;

type
  TZone = (zEnTete,zPied,zPage,zMarges);
  TSectPageNum = (PageNum,SectNum,PSectNum);
  TFBordFlags= set of (bcGauche,bcDroite,bcHaut,bcBas);

  TDimensions= record
    T: Integer;
    L: Integer;
    R: Integer;
    B: Integer;
    end;

  TPapier= record
    H: Integer;
    W: Integer;
    Imprimable: TDimensions;
    end;

  T_Section = class
    private
      FNumSect: Integer;
      FNbPages: Integer;
      FMarges: TDimensions;
      FBasEnTete: Integer;
      FHautPied: Integer;
      FPages: TList;
      FEnTete: TList;
      FPied: TList;
      FCadres: TList;
      function FirstPage: Integer;
      function TotalPages: Integer;
    public
      constructor Create(AMarges: TDimensions; ANum: Integer); virtual;
      destructor Destroy; override;
      procedure LoadPage(APageNum: Integer);
      procedure LoadCmdEnTete;
      procedure LoadCmdPage;
      procedure LoadCmdPied;
      procedure LoadCmdGroupe;
      procedure LoadCmdGroupeToPage;
      procedure LoadEspaceEnTete(APosY,AColonne,AHeight,AFond: Integer);
      procedure LoadEspacePage(APosY,AColonne,AHeight,AFond: Integer);
      procedure LoadEspacePied(APosY,AColonne,AHeight,AFond: Integer);
      procedure LoadCadre(AStyle: Integer; AZone: TZone);
      procedure LoadTrait(APosXDeb,APosYDeb,AColonne,APosXFin,APosYFin,AStyle: Integer);
      function GetCmdPage(NumPage: Integer): TList;
      property GetCmdEnTete: TList read FEntete;
      property GetCmdPied: TList read FPied;
      property GetNbPages: Integer read FNbPages;
      property GetFirstPage: Integer read FirstPage;
      property Pages: TList read FPages;
      property TotPages: Integer read TotalPages;
      property GetMarges: TDimensions read FMarges;
      property GetCmdCadres: TList read FCadres;
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
      FLineHeight: Integer;
      FGroupeHeight: Integer;
      FCommandes: TList;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      property Commandes: TList read FCommandes write FCommandes;
      property GetLineHeight: Integer read FLineHeight;
      property GetGroupeHeight: Integer read FGroupeHeight;
    end;

  T_Ligne = class
    private
      FHeight: Integer;
      FCommandes: TList;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure LoadTexte(APosX,APosY,AColonne,ATexte,AFonte,AHeight,AFond,ABord,AInterL: Integer;
                ACurFont: Boolean; AFlags: TFTextFlags);
      procedure LoadNumero(APosX,APosY,AColonne,ATexteNum,ATexteTot,AFonte,AHeight,AFond,ABord,AInterL: Integer;
                ACurFont: Boolean; AFlags: TFTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
      property Commandes: TList read FCommandes;
      property GetHeight: Integer read FHeight;
    end;

  T_Commande = class
    end;

  PSection = ^T_Section;
  PPage = ^T_Page;
  PLigne = ^T_Ligne;
  PCommande = ^T_Commande;
  PFont = ^TfpgFont;

  T_EcritTexte = class(T_Commande)
    private
      FPosX: Integer;
      FPosY: Integer;
      FColonne: Integer;
      FTexte: Integer;
      FFonte: Integer;
      FFond: Integer;
      FBord: Integer;
      FInterL: Integer;
      FCurFont: Boolean;
      FFlags: TFTextFlags;
    public
      constructor Create(APosX,APosY,AColonne,ATexte,AFonte,AFond,ABord,AInterL: Integer; ACurFont: Boolean; AFlags: TFTextFlags); virtual;
      procedure SetPosY(const AValue: Integer);
      property GetPosX: Integer read FPosX;
      property GetPosY: Integer read FPosY;
      property GetColonne: Integer read FColonne;
      property GetTexte: Integer read FTexte;
      property GetFonte: Integer read FFonte;
      property GetFond: Integer read FFond;
      property GetBord: Integer read FBord;
      property GetInterL: Integer read FInterL;
      property GetCurFont: Boolean read FCurFont;
      property GetFlags: TFTextFlags read FFlags;
    end;

  T_Numero = class(T_Commande)
    private
      FPosX: Integer;
      FPosY: Integer;
      FColonne: Integer;
      FTexteNum: Integer;
      FTexteTot: Integer;
      FFonte: Integer;
      FFond: Integer;
      FBord: Integer;
      FInterL: Integer;
      FCurFont: Boolean;
      FFlags: TFTextFlags;
      FTotal: Boolean;
      FAlpha: Boolean;
      FTypeNum: TSectPageNum;
    public
      constructor Create(APosX,APosY,AColonne,ATexteNum,ATexteTot,AFonte,AFond,ABord,AInterL: Integer;
                  ACurFont: Boolean; AFlags: TFTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum); virtual;
      procedure SetPosY(const AValue: Integer);
      property GetPosX: Integer read FPosX;
      property GetPosY: Integer read FPosY;
      property GetColonne: Integer read FColonne;
      property GetTexteNum: Integer read FTexteNum;
      property GetTexteTot: Integer read FTexteTot;
      property GetFonte: Integer read FFonte;
      property GetFond: Integer read FFond;
      property GetBord: Integer read FBord;
      property GetInterL: Integer read FInterL;
      property GetCurFont: Boolean read FCurFont;
      property GetFlags: TFTextFlags read FFlags;
      property GetTotal: Boolean read FTotal;
      property GetAlpha: Boolean read FAlpha;
      property GetTypeNum: TSectPageNum read FTypeNum;
    end;

  T_Trait = class(T_Commande)
    private
      FPosX: Integer;
      FPosY: Integer;
      FColonne: Integer;
      FStyle: Integer;
      FEndX: Integer;
      FEndY: Integer;
    public
      constructor Create(APosX,APosY,AColonne,AStyle,AEndX,AEndY: Integer); virtual;
      property GetPosX: Integer read FPosX;
      property GetPosY: Integer read FPosY;
      property GetColonne: Integer read FColonne;
      property GetStyle: Integer read FStyle;
      property GetEndX: Integer read FEndX;
      property GetEndY: Integer read FEndY;
    end;

  T_Colonne = class(T_Commande)
    private
      FPos: Integer;
      FWidth: Integer;
      FMargin: Integer;
      FColor: TfpgColor;
    public
      constructor Create(APos,AWidth,AMargin: Integer; AColor: TfpgColor); virtual;
      function GetTextPos: Integer;
      function GetTextWidth: Integer;
      procedure SetColColor(AColor: TfpgColor);
      property GetColPos: Integer read FPos;
      property GetColWidth: Integer read FWidth;
      property GetColMargin: Integer read FMargin;
      property GetColor: TfpgColor read FColor;
    end;

  T_Fonte = class(T_Commande)
    private
      FFonte: TfpgFont;
      FColor: TfpgColor;
      FSize: string;
    public
      constructor Create(AFonte: string; AColor: TfpgColor); virtual;
      function GetHeight: Integer;
      property GetFonte: TfpgFont read FFonte;
      property GetColor: TfpgColor read FColor;
      property GetSize: string read FSize;
    end;

  T_Interligne = class(T_Commande)
    private
      FSup: Integer;
      FInt: Integer;
      FInf: Integer;
    public
      constructor Create(ASup,AInt,AInf: Integer); virtual;
      property GetSup: Integer read FSup;
      property GetInt: Integer read FInt;
      property GetInf: Integer read FInf;
    end;

  T_Espace = class(T_Commande)
    private
      FPosY: Integer;
      FColonne: Integer;
      FHeight: Integer;
      FFond: Integer;
    public
      constructor Create(APosY,AColonne,AHeight,AFond: Integer); virtual;
      procedure SetPosY(const AValue: Integer);
      property GetPosY: Integer read FPosY;
      property GetColonne: Integer read FColonne;
      property GetHeight: Integer read FHeight;
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
      FEpais: Integer;
      FColor: TfpgColor;
      FStyle: TfpgLineStyle;
    public
      constructor Create(AEpais: Integer; AColor: Tfpgcolor; AStyle: TfpgLineStyle); virtual;
      property GetEpais: Integer read FEpais;
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

var
  Sections: TList;
  Colonnes: TList;
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

function ExtractFontSize(const AValue: string): string;
begin
if Pos(':',AValue)> 0
then
  Result:= Copy(AValue,Succ(Pos('-',AValue)),Pred(Pos(':',Avalue)-Pos('-',AValue)))
else
  Result:= Copy(AValue,Succ(Pos('-',AValue)),Length(AValue)-Pos('-',AValue));
end;

function T_Section.FirstPage: Integer;
begin
Result:= T_Page(Pages[0]).PagesTot;
end;

function T_Section.TotalPages: Integer;
begin
if Pages.Count> 0
then
  Result:= T_Page(Pages[Pred(Pages.Count)]).PagesTot
else
  Result:= 0;
end;

constructor T_Section.Create(AMarges: TDimensions; ANum: Integer);
begin
FNumSect:= ANum;
FNbPages:= 0;
FMarges:= AMarges;
FBasEnTete:= FMarges.T;
FHautPied:= FMarges.B;
FPages:= TList.Create;
FEnTete:= TList.Create;
FPied:= TList.Create;
FCadres:= TList.Create;
end;

destructor T_Section.Destroy;
begin
FPages.Free;
FEnTete.Free;
FPied.Free;
FCadres.Free;
inherited Destroy;
end;

procedure T_Section.LoadPage(APageNum: Integer);
begin
Inc(FNbPages);
APage:= T_Page.Create(FNbPages,APageNum);
Pages.Add(APage);
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
  T_Page(Pages[Pred(Pages.Count)]).Commandes.Add(ALigne.Commandes.Items[Cpt]);
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
  T_Page(Pages[Pred(Pages.Count)]).Commandes.Add(AGroupe.Commandes.Items[Cpt]);
AGroupe.FGroupeHeight:= 0;
AGroupe.Commandes.Clear;
end;

procedure T_Section.LoadEspaceEnTete(APosY,AColonne,AHeight,AFond: Integer);
begin
ACommande:= T_Espace.Create(APosY,AColonne,AHeight,AFond);
FEnTete.Add(ACommande);
end;

procedure T_Section.LoadEspacePage(APosY,AColonne,AHeight,AFond: Integer);
begin
ACommande:= T_Espace.Create(APosY,AColonne,AHeight,AFond);
T_Page(Pages[Pred(Pages.Count)]).Commandes.Add(ACommande);
end;

procedure T_Section.LoadEspacePied(APosY,AColonne,AHeight,AFond: Integer);
begin
ACommande:= T_Espace.Create(APosY,AColonne,AHeight,AFond);
FPied.Add(ACommande);
end;

procedure T_Section.LoadCadre(AStyle: Integer; AZone: TZone);
begin
ACommande:= T_Cadre.Create(AStyle,AZone);
FCadres.Add(ACommande);
end;

procedure T_Section.LoadTrait(APosXDeb,APosYDeb,AColonne,APosXFin,APosYFin,AStyle: Integer);
begin
ACommande:= T_Trait.Create(APosXDeb,APosYDeb,AColonne,AStyle,APosXFin,APosYFin);
T_Page(Pages[Pred(Pages.Count)]).Commandes.Add(ACommande);
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
begin
FCommandes.Free;
inherited Destroy;
end;

constructor T_Groupe.Create;
begin
FLineHeight:= 0;
FGroupeHeight:= 0;
Commandes:= TList.Create;
end;

destructor T_Groupe.Destroy;
begin
Commandes.Free;
inherited Destroy;
end;

constructor T_Ligne.Create;
begin
FHeight:= 0;
FCommandes:= TList.Create;
end;

destructor T_Ligne.Destroy;
begin
FCommandes.Free;
inherited Destroy;
end;

procedure T_Ligne.LoadTexte(APosX,APosY,AColonne,ATexte,AFonte,AHeight,AFond,ABord,AInterL: Integer;
          ACurFont: Boolean; AFlags: TFTextFlags);
begin
if FHeight< AHeight
then
  FHeight:= AHeight;
ACommande:= T_EcritTexte.Create(APosX,APosY,AColonne,ATexte,AFonte,AFond,ABord,AInterL,ACurFont,AFlags);
Commandes.Add(ACommande);
end;

procedure T_Ligne.LoadNumero(APosX,APosY,AColonne,ATexteNum,ATexteTot,AFonte,AHeight,AFond,ABord,AInterL: Integer;
          ACurFont: Boolean; AFlags: TFTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
begin
if FHeight< AHeight
then
  FHeight:= AHeight;
ACommande:= T_Numero.Create(APosX,APosY,AColonne,ATexteNum,ATexteTot,AFonte,AFond,ABord,AInterL,ACurFont,AFlags,ATotal,AAlpha,ATypeNum);
Commandes.Add(ACommande);
end;

procedure T_EcritTexte.SetPosY(const AValue: Integer);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_EcritTexte.Create(APosX,APosY,AColonne,ATexte,AFonte,AFond,ABord,AInterL: Integer; ACurFont: Boolean; AFlags: TFTextFlags);
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

procedure T_Numero.SetPosY(const AValue: Integer);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_Numero.Create(APosX,APosY,AColonne,ATexteNum,ATexteTot,AFonte,AFond,ABord,AInterL: Integer;
            ACurFont: Boolean; AFlags: TFTextFlags; ATotal,AAlpha: Boolean; ATypeNum: TSectPageNum);
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

constructor T_Trait.Create(APosX,APosY,AColonne,AStyle,AEndX,AEndY: Integer);
begin
FPosX:= APosX;
FPosY:= APosY;
FColonne:= AColonne;
FStyle:= AStyle;
FEndX:= AEndX;
FEndY:= AEndY;
end;

constructor T_Colonne.Create(APos,AWidth,AMargin: Integer; AColor: TfpgColor);
begin
inherited Create;
FPos:= APos;
FWidth:= AWidth;
FMargin:= AMargin;
FColor:= AColor;
end;

function T_Colonne.GetTextPos: Integer;
begin
Result:= FPos+FMargin;
end;

function T_Colonne.GetTextWidth: Integer;
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

function T_Fonte.GetHeight: Integer;
begin
Result:= TfpgFont(FFonte).Height;
end;

constructor T_Interligne.Create(ASup,AInt,AInf: Integer);
begin
inherited Create;
FSup:= ASup;
FInt:= AInt;
FInf:= AInf;
end;

constructor T_Espace.Create(APosY,AColonne,AHeight,AFond: Integer);
begin
inherited Create;
FPosY:= APosY;
FColonne:= AColonne;
FHeight:= AHeight;
FFond:= AFond;
end;

procedure T_Espace.SetPosY(const AValue: Integer);
begin
if FPosY<> AValue
then
  FPosY:= AValue;
end;

constructor T_Fond.Create(AColor: TfpgColor);
begin
FColor:= AColor;
end;

constructor T_TraitStyle.Create(AEpais: Integer; AColor: Tfpgcolor; AStyle: TfpgLineStyle);
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

