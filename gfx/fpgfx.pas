{
    fpGUI  -  Free Pascal GUI Library
    
    fpGFX  -  Main unit for the core drawing engine of fpGUI
    
    Copyright (C) 2000 - 2006 See the file AUTHORS, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit fpgfx;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  gfxinterface, gfxbase;

type

  { TFFont }

  TFFont = class(TDefFont)
  end;

  { TFCanvas }

  TFCanvas = class(TDefCanvas)
  end;

  { TFImage }

  TFImage = class(TDefImage)
  end;

  { TFScreen }

  TFScreen = class(TDefScreen)
  end;

  { TFWindow }

  TFWindow = class(TDefWindow)
  end;

  { TFApplication }

  TFApplication = class(TDefApplication)
  private
    FDisplayName: String;
  protected
    FTitle: String;
    procedure   SetTitle(const ATitle: String);
  public
    constructor Create; override;
    destructor  Destroy; override;
//    procedure   CreateForm(InstanceClass: TComponentClass; var Reference);
    property    Title: String read FTitle write SetTitle;
  end;

{ Using the singleton pattern to hide instance variables and
  only instantiate them when they are referred to for the first time. }
function gScreen: TFScreen;
function gApplication: TFApplication;


implementation


var
  uScreen: TFScreen;
  uApplication: TFApplication;


function gScreen: TFScreen;
begin
  if uScreen = nil then
    uScreen := TFScreen.Create;
  result := uScreen;
end;

function gApplication: TFApplication;
begin
  if uApplication = nil then
    uApplication := TFApplication.Create;
  result := uApplication;
end;



{ TFApplication }

constructor TFApplication.Create;
begin
  inherited Create;

end;


destructor TFApplication.Destroy;
begin

  inherited Destroy;
end;

{procedure TFApplication.CreateForm(AForm: TCustomForm);
var
  form: PForm;
  Filename: String;
  TextStream, BinStream: TStream;
begin
  form := @Reference;
  form^ := TCustomForm(InstanceClass.Create(Self));

  Filename := LowerCase(Copy(InstanceClass.ClassName, 2, 255)) + '.frm';

  TextStream := TFileStream.Create(Filename, fmOpenRead);
  BinStream := TMemoryStream.Create;
  ObjectTextToBinary(TextStream, BinStream);
  TextStream.Free;

  BinStream.Position := 0;
  BinStream.ReadComponent(Form^);
  BinStream.Free;

  Form^.Show;
end;}


procedure TFApplication.SetTitle(const ATitle: String);
begin
  if ATitle <> FTitle then FTitle := ATitle;
end;

{*******************************************************************
*  Initialization section
*
*  DESCRIPTION:    Upon startup FApplication and FScreen objects are created
*                  and memory for them allocated.
*
*******************************************************************}

initialization
  uScreen := nil;
  uApplication := nil;

{*******************************************************************
*  Finalization section
*
*  DESCRIPTION:    Free memory allocated on the initialization section
*
*******************************************************************}

finalization
  uApplication.Free;
  uScreen.Free;

end.

