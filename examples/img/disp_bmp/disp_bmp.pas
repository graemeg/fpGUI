{
    fpGUI  -  Free Pascal GUI Library

    Example: Display BMP file using the fpImg - Free Pascal Imaging Library

    Copyright (C) 2000 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program Disp_BMP;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

uses
  Classes
  ,fpgfx
  ,gfxbase, fpgfxpackage
  ,fpImg
  ,BMPReader, fpimgpackage
  ;

type
  TMainForm = class(TFWindow)
  private
    Image: TFBitmap;
  public
    constructor Create; overload;
    destructor  Destroy; override;
    procedure   Paint(Sender: TObject; const Rect: TRect);
  end;

constructor TMainForm.Create;
begin
  inherited Create(nil, [woWindow]);
  Title   := 'fpImg Bitmap Test';
  OnPaint := @Paint;
  Image   := CreateImageFromFile(GFScreen, TBMPReader, ParamStr(1));
  SetClientSize(Size(Image.Width, Image.Height));
end;

destructor TMainForm.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

procedure TMainForm.Paint(Sender: TObject; const Rect: TRect);
begin
  // debug only
  Canvas.SetColor(colRed);
  Canvas.FillRect(Rect);
  Canvas.SetColor(colYellow);
  // paint image
  Canvas.DrawImage(Image, Point(0, 0));
end;

var
  MainForm: TMainForm;
begin
  if ParamCount <> 1 then
  begin
    WriteLn(StdErr, 'Please give the name of a BMP file as argument');
    Halt(2);
  end;

  GFApplication.Initialize;
  MainForm := TMainForm.Create;
  MainForm.Show;
  GFApplication.Run;
end.

