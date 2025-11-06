(*
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2025 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


  A very quick and basic style implementation. It took all of 10 minutes.
  To apply this style, follow these instructions:

    1) (optional) Check if a style was specified via a command line parameter
    2) If (1) was false, set the new default which will instantiate the new
       style class and automatically free the old one.
    3) Assign our new style instance to the fpgStyle variable


  Example:

    procedure MainProc;
    var
      frm: TMainForm;
      cmd: ICmdLineParams;
    begin
      fpgApplication.Initialize;

      { Set our new style as the default (before we create any forms), unless
        a the end-user specified a different style via the command line. }
      if Supports(fpgApplication, ICmdLineParams, cmd) and not cmd.HasOption('style') then
        if fpgStyleManager.SetStyle('Demo Style') then
          fpgStyle := fpgStyleManager.Style;

      frm := TMainForm.Create(nil);
      try
        frm.Show;
        fpgApplication.Run;
      finally
        frm.Free;
      end;
    end;

*)
unit mystyle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_main, fpg_base, fpg_cmdlineparams;

type

  TMyStyle = class(TfpgStyle)
  public
    constructor Create; override;
    { General }
    procedure   DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord); override;
    { Buttons }
    procedure   DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags); override;
    { Menus }
    procedure   DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags); override;
    procedure   DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor); override;
    { listbox }
    procedure DrawListBox(ACanvas: TfpgCanvas; const r: TfpgRect; const IsEnabled: Boolean;
      const IsReadOnly: Boolean; const ABackgroundColor: TfpgColor); override;
    procedure DrawListBoxItem(ACanvas: TfpgCanvas; r: TfpgRect; const IsFocusedItem: Boolean;
      const HasFocus: Boolean); override;
  end;


implementation

uses
  fpg_stylemanager
  ;

{ TMyStyle }

constructor TMyStyle.Create;
begin
  inherited Create;
  fpgSetNamedColor(clWindowBackground, fpgColor(Byte($ee), Byte($ee), Byte($ec)));
end;

procedure TMyStyle.DrawControlFrame(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow1);
  ACanvas.Clear(clYellow);
  ACanvas.DrawRectangle(r);
end;

procedure TMyStyle.DrawButtonFace(ACanvas: TfpgCanvas; x, y, w, h: TfpgCoord; AFlags: TfpgButtonFlags);
var
  r: TfpgRect;
begin
  r.SetRect(x, y, w, h);

  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(fpgColor(Byte($7b), Byte($7b), Byte($7b)));
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    r.InflateRect(-1, -1);
    Exclude(AFlags, btfIsDefault);
    fpgStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  // Clear the canvas
  ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);

  if (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
    Exit; // no need to go further

  // outer rectangle
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.SetColor(fpgColor(Byte($a6), Byte($a6), Byte($a6)));
  ACanvas.DrawRectangle(r);

  // so we don't paint over the border
  r.InflateRect(-1, -1);
  // now paint the face of the button
  if (btfIsPressed in AFlags) then
  begin
    ACanvas.GradientFill(r, fpgColor(Byte($cc), Byte($cc), Byte($cc)), fpgColor(Byte($e4), Byte($e4), Byte($e4)), gdVertical);
  end
  else
  begin
    ACanvas.GradientFill(r, fpgColor(Byte($fa), Byte($fa), Byte($fa)), fpgColor(Byte($e2), Byte($e2), Byte($e2)), gdVertical);
    ACanvas.SetColor(fpgColor(Byte($cc), Byte($cc), Byte($cc)));
    ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
    ACanvas.DrawLine(r.Right, r.Bottom, r.Left, r.Bottom);   // bottom
  end;
end;

procedure TMyStyle.DrawMenuRow(ACanvas: TfpgCanvas; r: TfpgRect; AFlags: TfpgMenuItemFlags);
begin
  inherited DrawMenuRow(ACanvas, r, AFlags);
  if (mifSelected in AFlags) and not (mifSeparator in AFlags) then
    ACanvas.GradientFill(r, fpgColor(Byte($fe), Byte($c4), Byte($75)), fpgColor(Byte($fb), Byte($9d), Byte($24)), gdVertical);
end;

procedure TMyStyle.DrawMenuBar(ACanvas: TfpgCanvas; r: TfpgRect; ABackgroundColor: TfpgColor);
var
  FLightColor: TfpgColor;
  FDarkColor: TfpgColor;
begin
  // a possible future theme option
  FLightColor := fpgColor(Byte($f0), Byte($ec), Byte($e3));  // color at top of menu bar
  FDarkColor  := fpgColor(Byte($be), Byte($b8), Byte($a4));  // color at bottom of menu bar
  ACanvas.GradientFill(r, FLightColor, FDarkColor, gdVertical);

  // inner bottom line
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom-1, r.Right+1, r.Bottom-1);   // bottom
  // outer bottom line
  ACanvas.SetColor(clWhite);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right+1, r.Bottom);   // bottom
end;

procedure TMyStyle.DrawListBox(ACanvas: TfpgCanvas; const r: TfpgRect; const IsEnabled: Boolean;
  const IsReadOnly: Boolean; const ABackgroundColor: TfpgColor);
var
  c1, c2: TfpgColor;
begin
  if IsEnabled and not IsReadOnly then
  begin
    c1 := fpgDarker(ABackgroundColor);
    c2 := fpgLighter(ABackgroundColor);
  end
  else
  begin
    c1 := fpgDarker(fpgColorToRGB(clWindowBackground));
    c2 := fpgLighter(fpgColorToRGB(clWindowBackground));
  end;
  ACanvas.GradientFill(r, c1, c2, gdHorizontal);
end;

procedure TMyStyle.DrawListBoxItem(ACanvas: TfpgCanvas; r: TfpgRect; const IsFocusedItem: Boolean;
  const HasFocus: Boolean);
var
  c1, c2: TfpgColor;
begin
  if IsFocusedItem then
  begin
    if HasFocus then
    begin
      c1 := clSelection;
      c2 := fpgLighter(fpgColorToRGB(clSelection));
      ACanvas.SetTextColor(clSelectionText);
    end
    else
    begin
      c1 := clInactiveSel;
      c2 := fpgLighter(fpgColorToRGB(clInactiveSel));
      ACanvas.SetColor(clInactiveSel);
      ACanvas.SetTextColor(clInactiveSelText);
    end;
    ACanvas.GradientFill(r, c2, c1, gdHorizontal);
  end;
end;


initialization
  fpgStyleManager.RegisterClass('Demo Style', TMyStyle);

end.

