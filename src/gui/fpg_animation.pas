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
      It's a simple little component that animates an image that contains
      multiple frames (in a horizontal direction). See the Animation
      demo for image examples.
}

unit fpg_animation;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpg_base,
  fpg_main,
  fpg_widget;

type

  TfpgBaseImgAnim = class(TfpgWidget)
  private
    FFrameCount: integer;
    FImageFilename: TfpgString;
    FImage: TfpgImage;
    FInterval: integer;
    FTimer: TfpgTimer;
    FPos: integer;
    FImageWidth: TfpgCoord;
    FTransparent: Boolean;
    procedure   InternalTimerFired(Sender: TObject);
    procedure   SetAnimPosition(const AValue: integer);
    procedure   SetInterval(const AValue: integer);
    procedure   RecalcImageWidth;
  protected
    procedure   HandlePaint; override;
    procedure   SetEnabled(const AValue: boolean); override;
    procedure   SetImageFilename(const AValue: TfpgString); virtual;
    //
    property    Interval: integer read FInterval write SetInterval default 50;
    property    ImageFileName: TfpgString read FImageFilename write SetImageFilename;
    property    IsTransparent: Boolean read FTransparent write FTransparent default True;
    property    FrameCount: integer read FFrameCount write FFrameCount default 4;
    property    Position: integer read FPos write SetAnimPosition default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  end;


  TfpgImgAnim = class(TfpgBaseImgAnim)
  public
    property    Position;
  published
    property    Align;
    property    Enabled;
    property    Interval;
    property    ImageFileName;
    property    IsTransparent;
    property    FrameCount;
    property    OnShowHint;
  end;


implementation

uses
  SysUtils,
  fpg_imgfmt_bmp,
  fpg_utils;


{ TfpgBaseImgAnim }

procedure TfpgBaseImgAnim.InternalTimerFired(Sender: TObject);
begin
  Repaint;
  inc(FPos);
  if FPos > FrameCount-1 then
    FPos := 0;
end;

procedure TfpgBaseImgAnim.SetAnimPosition(const AValue: integer);
begin
  if FTimer.Enabled then
    Exit; // ignore position because animation is running
  if AValue < 0 then
    FPos := 0
  else
    FPos := AValue;
  Repaint;
end;

procedure TfpgBaseImgAnim.SetInterval(const AValue: integer);
begin
  if FInterval = AValue then
    Exit; //==>
  FInterval := AValue;
  FTimer.Interval := FInterval;
  RecalcImageWidth;
end;

procedure TfpgBaseImgAnim.RecalcImageWidth;
begin
  FImageWidth := FImage.Width div FrameCount;
  FPos := 0;
end;

procedure TfpgBaseImgAnim.HandlePaint;
begin
  if (FImageFilename = '') or (FImage = nil) then
    Exit; //==>
  Canvas.BeginDraw;
  Canvas.Clear(clWindowBackground);
  Canvas.DrawImagePart(0, 0, FImage, (FImageWidth * FPos), 0, FImageWidth, FImage.Height);
  Canvas.EndDraw;
end;

procedure TfpgBaseImgAnim.SetEnabled(const AValue: boolean);
begin
  inherited SetEnabled(AValue);
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := FEnabled;
end;

procedure TfpgBaseImgAnim.SetImageFilename(const AValue: TfpgString);
begin
  if FImageFilename = AValue then
    Exit; //==>

  if Trim(AValue) = '' then
    Exit; //==>

  if not fpgFileExists(AValue) then
    raise Exception.CreateFmt('The file <%s> does not exist.', [AValue])
  else
    FImageFilename := AValue;

  FTimer.Enabled := False;
  FImage.Free;
  FImage := LoadImage_BMP(FImageFilename);
  if FTransparent then
  begin
    FImage.CreateMaskFromSample(0, 0);
    FImage.UpdateImage;
  end;
  RecalcImageWidth;
  Repaint;
end;

constructor TfpgBaseImgAnim.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPos          := 0;
  FFrameCount   := 4;
  FInterval     := 50;
  FImage        := nil;
  FEnabled      := False;
  FTransparent  := True;

  FTimer := TfpgTimer.Create(FInterval);
  FTimer.OnTimer := @InternalTimerFired;
end;

destructor TfpgBaseImgAnim.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  FImage.Free;
  inherited Destroy;
end;


end.

