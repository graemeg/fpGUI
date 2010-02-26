{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A hyperlink label component. When the user clicks the label, a
      web browser is opened with the URL specified.
}


unit fpg_hyperlink;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Sysutils,
  fpg_base,
  fpg_main,
  fpg_label;

type

  TfpgHyperlink = class(TfpgCustomLabel)
  private
    fHotTrackColor: TfpgColor;
    fOldColor: TfpgColor;
    fOldFont: TfpgString;
    fHTFont: TfpgString;
    fUrl: TfpgString;
    procedure   SetHotTrackColor(const AValue: TfpgColor);
    procedure   SetHotTrackFont(const AValue: TfpgString);
    procedure   SetURL(const Value: TfpgString);
  protected
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   GoHyperLink;
  published
    property    Autosize;
    property    FontDesc;
    property    Hint;
    property    HotTrackColor: TfpgColor read fHotTrackColor write SetHotTrackColor;
    property    HotTrackFont: TfpgString read fHTFont write SetHotTrackFont;
    property    Text;
    property    TextColor;
    property    ShowHint;
    property    URL: TfpgString read FUrl write SetURL;
    property    OnClick;
end;



implementation

uses
  fpg_utils;


{ TfpgHyperlink }

constructor TfpgHyperlink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHotTrackColor  := clBlue;
  TextColor       := clBlue;
  fUrl            := 'http://opensoft.homeip.net/fpgui/';
  Text            := 'fpGUI website';
  fHTFont         := 'Arial-8:antialias=true:underline:bold';
  FontDesc        := 'Arial-8:antialias=true:underline';
  AutoSize        := True;
end;

procedure TfpgHyperlink.SetURL(const Value: TfpgString);
begin
  if fUrl <> Value then
    fUrl := Value;
end;

procedure TfpgHyperlink.SetHotTrackFont(const AValue: TfpgString);
begin
  if fHTFont = AValue then
    Exit;
  fHTFont := AValue;
end;

procedure TfpgHyperlink.SetHotTrackColor(const AValue: TfpgColor);
begin
  if fHotTrackColor = AValue then
    Exit;
  fHotTrackColor := AValue;
end;

procedure TfpgHyperlink.GoHyperLink;
begin
  if URL <> '' then
    fpgOpenURL(URL);
end;

procedure TfpgHyperlink.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  fOldColor   := TextColor;
  TextColor   := fHotTrackColor;
  fOldFont    := FontDesc;
  FontDesc    := fHTFont;
  MouseCursor := mcHand;
end;

procedure TfpgHyperlink.HandleMouseExit;
begin
  inherited HandleMouseExit;
  TextColor   := fOldColor;
  MouseCursor := mcDefault;
  FontDesc    := fOldFont;
end;

procedure TfpgHyperlink.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if not Assigned(OnClick) then
    GoHyperlink;
end;


end.

