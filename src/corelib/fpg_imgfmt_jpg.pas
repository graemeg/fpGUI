{
    This unit is part of the fpGUI Toolkit project.

    Copyright (c) 2006 - 2016 by Graeme Geldenhuys.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      JPEG image loading using the Free Pascal library: fcl-image
}

unit fpg_imgfmt_jpg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpg_main,
  fpg_base,
  FPImage,
  FPReadJPEG;
  

function  LoadImage_JPG(const AImageData: Pointer; const AImageDataSize: LongWord; const AScale: TJPEGScale = jsFullSize): TfpgImage;
function  LoadImage_JPG(AStream: TStream; const AScale: TJPEGScale = jsFullSize): TfpgImage; overload;
function  LoadImage_JPG(const AFileName: TfpgString; const AScale: TJPEGScale = jsFullSize): TfpgImage;
  
implementation

uses
  fpg_utils;


function FPImageToFPG(ASource: TFPCustomImage): TfpgImage;
var
  i, j: integer;
  colorA: TFPColor;   // struct Alpha, Red, Green, Blue: word
  colorB: TfpgColor;  // ONE long 32-bit-word
  xlocal, ylocal: integer;
begin
  Result := nil;
  if ASource=nil then Exit;

  xlocal := ASource.Width;
  ylocal := ASource.Height;
  Result := TfpgImage.Create;
  Result.AllocateImage(32, xlocal, ylocal); // 32=colordepth
  for i := 0 to ylocal - 1 do
    for j := 0 to xlocal - 1 do
    begin
      colorA := ASource.Colors[j, i];
      colorB := ((colorA.Blue and $FF00) shr 8) or (colorA.Green and $FF00) or ((colorA.Red and $FF00) shl 8);// or ((colorA.Alpha and $FF) shl 16); Alpha not working
      Result.Colors[j, i] := colorB;
    end;
  Result.UpdateImage;
end;

function LoadImage_JPG(const AImageData: Pointer; const AImageDataSize: LongWord;
    const AScale: TJPEGScale): TfpgImage;
var
  s: TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    s.Write(AImageData^, AImageDataSize);
    Result := LoadImage_JPG(s, AScale);
  finally
    s.Free;
  end;
end;

function LoadImage_JPG(AStream: TStream; const AScale: TJPEGScale): TfpgImage;
var
  imga: TFPMemoryImage;
  reader: TFPReaderJPEG;
begin
  Result := nil;
  if AStream = nil then
    Exit;

  imga := TFPMemoryImage.Create(0, 0);
  try
    try
      AStream.Position := 0;
      reader := TFPReaderJPEG.Create;
      reader.Scale := AScale;
      imga.LoadFromStream(AStream, reader); // auto size image
      Result := FPImageToFPG(imga);
    except
      on e: Exception do
      begin
        Result := nil;
        raise; // so we can report PNG errors (eg: missing resource name)
      end;
    end;
  finally
    reader.Free;
    imga.Free;
  end;
end;

function LoadImage_JPG(const AFileName: TfpgString; const AScale: TJPEGScale): TfpgImage;
var
  imga: TFPCustomImage;
  ImgReader: TFPReaderJPEG;
begin
  Result := nil;
  if not fpgFileExists(AFileName) then
    Exit; //==>

  imga := TFPMemoryImage.Create(0, 0);
  try
    ImgReader := TFPReaderJPEG.Create;
    ImgReader.Scale := AScale;
    try
      imga.LoadFromFile(fpgToOSEncoding(AFileName), ImgReader); // auto size image
    finally
      ImgReader.Free;
    end;
  except
    imga := nil;
  end;
  if imga <> nil then
  begin
    Result := FPImageToFPG(imga);
    imga.Free;
  end;
end;


end.

