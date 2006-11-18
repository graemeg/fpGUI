{
    fpImg  -  Free Pascal Imaging Library
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Image I/O interface declarations

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


unit ImageIO;

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

interface

uses SysUtils, Classes, GFXBase;

resourcestring
  SImgOutOfData = 'No more data available for image';
  SImgUnsupportedPixelFormat = 'Unsupported pixel format in image';

type

  EImgError = class(Exception);

  EImgOutOfData = class(EImgError)
  public
    constructor Create;
  end;

  EImgUnsupportedPixelFormat = class(EImgError)
  public
    constructor Create;
  end;


// Image reading

  TImageReaderClass = class of TImageReader;

  TImageReaderState = (irsStart, irsInHeader, irsHeaderRead,
    irsInImage, irsFinished);

  TSegmentEvent = procedure(Sender: TObject; StartY, Height: Integer) of object;

  TImageReader = class
  private
    FState: TImageReaderState;

    { Needed for reading of image data. These values must be initialized by
      the user via SetImageSegmentBuffer }
    FSegmentData: Pointer;
    FSegmentStride: LongWord;
    FSegmentHeight: Integer;
    FSegmentSize: LongWord;		// Size in bytes

    FOnHeader: TNotifyEvent;
    FOnSegment: TSegmentEvent;
    FOnImage: TNotifyEvent;
  protected
    // Image properties, only available after OnHeaderRead event
    FWidth, FHeight: Integer;
    FPixelFormat: TGfxPixelFormat;
    FPaletteSize: Integer;
    FPalette: PGfxColor;

    procedure HeaderFinished;
    procedure SegmentFinished(AStartY, AHeight: Integer);
    procedure ImageFinished;

    procedure DoProcessHeaderData(AStream: TStream); virtual; abstract;
    function DoGetImageSegmentStartY(ASegmentHeight: Integer): Integer;
      virtual; abstract;
    procedure InitImageReading; virtual;
    procedure DoProcessImageData(AStream: TStream); virtual; abstract;

  public
    constructor Create; virtual;
    procedure ProcessHeaderData(AStream: TStream);
    function GetImageSegmentStartY(ASegmentHeight: Integer): Integer;
    procedure SetImageSegmentBuffer(AData: Pointer; AStride: LongWord;
      ASegmentHeight: Integer);
    procedure ProcessImageData(AStream: TStream);

    property State: TImageReaderState read FState;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property PixelFormat: TGfxPixelFormat read FPixelFormat;
    property PaletteSize: Integer read FPaletteSize;
    property Palette: PGfxColor read FPalette;
    property SegmentData: Pointer read FSegmentData;
    property SegmentStride: LongWord read FSegmentStride;
    property SegmentHeight: Integer read FSegmentHeight;
    property SegmentSize: LongWord read FSegmentSize;

    property OnHeader: TNotifyEvent read FOnHeader write FOnHeader;
    property OnSegment: TSegmentEvent read FOnSegment write FOnSegment;
    property OnImage: TNotifyEvent read FOnImage write FOnImage;
  end;


implementation

constructor EImgOutOfData.Create;
begin
  inherited Create(SImgOutOfData);
end;

constructor EImgUnsupportedPixelFormat.Create;
begin
  inherited Create(SImgUnsupportedPixelFormat);
end;


// TImageReader

constructor TImageReader.Create;
begin
  inherited Create;
end;

procedure TImageReader.ProcessHeaderData(AStream: TStream);
begin
  ASSERT(FState in [irsStart, irsInHeader]);
  if State = irsStart then
    FState := irsInHeader;
  DoProcessHeaderData(AStream);
end;

function TImageReader.GetImageSegmentStartY(ASegmentHeight: Integer): Integer;
begin
  ASSERT(State in [irsHeaderRead, irsInImage]);
  Result := DoGetImageSegmentStartY(ASegmentHeight);
end;

procedure TImageReader.SetImageSegmentBuffer(AData: Pointer; AStride: LongWord;
  ASegmentHeight: Integer);
begin
  ASSERT(State in [irsHeaderRead, irsInImage]);
  FSegmentData := AData;
  FSegmentStride := AStride;
  FSegmentHeight := ASegmentHeight;
  FSegmentSize := SegmentStride * SegmentHeight;
end;

procedure TImageReader.ProcessImageData(AStream: TStream);
begin
  if State = irsFinished then
    exit;
  ASSERT(State in [irsHeaderRead, irsInImage]);

  if State = irsHeaderRead then
  begin
    FState := irsInImage;
    InitImageReading;
  end;

  DoProcessImageData(AStream);
end;

procedure TImageReader.HeaderFinished;
begin
  ASSERT(FState = irsInHeader);
  FState := irsHeaderRead;
  if Assigned(OnHeader) then
    OnHeader(Self);
end;

procedure TImageReader.SegmentFinished(AStartY, AHeight: Integer);
begin
  ASSERT(FState = irsInImage);
  if Assigned(OnSegment) then
    OnSegment(Self, AStartY, AHeight);
end;

procedure TImageReader.ImageFinished;
begin
  ASSERT(FState = irsInImage);
  FState := irsFinished;
  if Assigned(OnImage) then
    OnImage(Self);
end;

procedure TImageReader.InitImageReading;
begin
  // Do nothing
end;


end.
