{
    fpGUI  -  Free Pascal GUI Library
    
    Style Manager as a Singleton
    
    Copyright (C) 2000 - 2006 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit stylemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,fpGUI
  ;
  
type
  TStyleManager = class(TObject)
  private
    FDefaultStyle: TStyle;
    FUserStyle: TStyle;
    function    GetDefaultStyle: TStyle;
  public
    constructor Create;
    destructor  Destroy;override;
    property    DefaultStyle: TStyle read GetDefaultStyle;
    procedure   SetStyle(pNewStyle: TStyle);
  end;


// lazy mans singleton
function gStyleManager: TStyleManager;


implementation
uses
  fpGFX
  ;

var
  uStyleManager: TStyleManager;


{ Creation is deferred to the first request }
function gStyleManager: TStyleManager;
begin
  if uStyleManager = nil then
    uStyleManager := TStyleManager.Create;
  result := uStyleManager;
end;

{ TStyleManager }

function TStyleManager.GetDefaultStyle: TStyle;
begin
  if Assigned(FUserStyle) then
    Result := FUserStyle
  else
  begin
    if not Assigned(FDefaultStyle) then
      FDefaultStyle := TDefaultStyle.Create(GFApplication);
    Result := FDefaultStyle;
  end;
end;

constructor TStyleManager.Create;
begin
  FUserStyle := nil;
  FDefaultStyle := nil;
end;

destructor TStyleManager.Destroy;
begin
  if FUserStyle <> nil then
    FUserStyle.Free;
  if FDefaultStyle <> nil then
    FDefaultStyle.Free;
  inherited Destroy;
end;

procedure TStyleManager.SetStyle(pNewStyle: TStyle);
begin
  if Assigned(FUserStyle) then
    FUserStyle.Free;
  FUserStyle := pNewStyle;
end;


initialization
  uStyleManager := nil;

finalization
  uStyleManager.Free;

end.

