{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    QR Code report element.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportqrcode;

{$MODE objfpc}
{$H+}

interface

uses
  Classes, fpimage, fpexprpars, fpimgqrcode, fpqrcodegen, fpreport, fpreportstreamer;

Type

  { TFPReportQRCode }

  TFPReportQRCode = Class(TFPReportElement)
  private
    FExpression: String;
    FPixelSize: Integer;
    FValue: String;
    FExprValue : String;
    FMask : TQRMask;
    FECL : TQRErrorLevelCorrection;
    FCenter : Boolean;
  Protected
    procedure BeforePrint;  override;
    procedure RecalcLayout; override;
    Procedure DoWriteLocalProperties(AWriter: TFPReportStreamer; AOriginal: TFPReportElement=nil); override;
  Public
    procedure Assign(Source: TPersistent); override;
    // Will calculate the value to display. Either Value or evaluated expression.
    Function QRCodeValue : String;
    Function QRPixelSize(aWidth,aHeight,aQRSize : Integer)  : Integer;
    Procedure ReadElement(AReader: TFPReportStreamer); override;
  Published
    // If zero or less, it will be calculated from width/height, truncated, after calculating the QR size.
    Property PixelSize : Integer Read FPixelSize Write FPixelSize;
    // Expression takes precedence
    Property Value : String Read FValue Write FValue;
    Property Expression : String Read FExpression Write FExpression;
    Property Mask : TQRMask Read FMask Write FMask;
    Property ErrorCorrectionLevel : TQRErrorLevelCorrection Read FECL Write FECL;
    Property Center : Boolean Read FCenter Write FCenter;
  end;

Procedure RegisterReportQRCode;
Procedure UnRegisterReportQRCode;
  
implementation

uses typinfo, strutils;


{ TFPReportQRCode }

procedure TFPReportQRCode.RecalcLayout;
begin
  // Do nothing for the moment.
  // We may consider adding a Boolean property FitWidth and calculating width based on value/expression when it is set to true
end;

procedure TFPReportQRCode.DoWriteLocalProperties(AWriter: TFPReportStreamer; AOriginal: TFPReportElement);


begin
  inherited DoWriteLocalProperties(AWriter, AOriginal);
  AWriter.WriteInteger('PixelSize',PixelSize);
  AWriter.WriteString('Value',Value);
  AWriter.WriteString('Expression',Expression);
  AWriter.WriteString('Mask',GetEnumName(TypeInfo(TQRMask),Ord(Mask)));
  AWriter.WriteString('ErrorCorrectionLevel',GetEnumName(TypeInfo(TQRErrorLevelCorrection),Ord(ErrorCorrectionLevel)));
  AWriter.WriteBoolean('Center',Center);
end;

procedure TFPReportQRCode.Assign(Source: TPersistent);

Var
  QRC : TFPReportQRCode;

begin
  if (Source is TFPReportQRCode) then
    begin
    QRC:=TFPReportQRCode(Source);
    FValue:=QRC.FValue;
    FExpression:=QRC.FExpression;
    FPixelSize:=QRC.FPixelSize;
    FMask:=QRC.FMask;
    FECl:=QRC.FECL;
    FCenter:=QRC.Center;
    end;
  inherited Assign(Source);
end;


procedure TFPReportQRCode.BeforePrint;

begin
  Inherited;
  if (FExpression<>'') then
  FExprValue:=EvaluateExpressionAsText(FExpression)
end;

function TFPReportQRCode.QRCodeValue: String;

begin
  if (FExpression<>'') then
    Result:=FExprValue // Calculated in beforeprint
  else
    Result:=FValue;
end;

function TFPReportQRCode.QRPixelSize (aWidth,aHeight,aQRSize : Integer) : Integer;

Var
  PS2 : Integer;

begin
  Result:=FPixelSize;
  if (Result<=0) and (aQRSize>0) then
    begin
    Result:=aWidth div aQRSize;
    PS2:=aHeight div aQRSize;
    if PS2<Result then
      Result:=PS2;
    end;
  if Result<1 then
    Result:=1;  
end;

procedure TFPReportQRCode.ReadElement(AReader: TFPReportStreamer);

Var
  S : String;
  I : Integer;

begin
  inherited ReadElement(AReader);
  PixelSize:=AReader.ReadInteger('UnitWidth',PixelSize);
  Value:=AReader.ReadString('Value',Value);
  Expression:=AReader.ReadString('Expression',Expression);
  I:=GetEnumValue(TypeInfo(TQRMask), Areader.ReadString('Mask',''));
  if I<>-1 then
    FMask:=TQRMask(I);
  I:=GetEnumValue(TypeInfo(TQRErrorLevelCorrection),AReader.ReadString('ErrorCorrectionLevel',''));
  if I<>-1 then
    FECL:=TQRErrorLevelCorrection(I);
  Center:=AReader.ReadBoolean('Center',Center);
end;

procedure RenderQRCode(aElement: TFPReportElement; aImage: TFPCustomImage);

Var
  D : TImageQRCodeGenerator;
  Q : TFPReportQRCode;
  DD,PX,PY : Integer;
  

begin
  Q:=TFPReportQRCode(aElement);
  D:=TImageQRCodeGenerator.Create;
  try
    D.MinVersion:=QRVERSIONMIN;
    D.MaxVersion:=QRVERSIONMAX;
    D.ErrorCorrectionLevel:=Q.ErrorCorrectionLevel;
    D.Mask:=Q.Mask;
    D.Generate(Q.QRCodeValue);
    D.PixelSize:=Q.QRPixelSize(aImage.Width,aImage.height,D.Size);
    PX:=0;
    PY:=0;
    if Q.Center then
     begin
     DD:=aImage.Width-(D.PixelSize*D.Size);
     if DD>0 then
       PX:=DD div 2;
     DD:=aImage.Height-(D.PixelSize*D.Size);
     if DD>0 then
       PY:=DD div 2;
     end; 
    D.Draw(aImage, PX, PY);
  finally
    D.Free;
  end;
end;


Procedure RegisterReportQRCode;

begin
  gElementFactory.RegisterClass('QRCode',TFPReportQRCode);
  // Fallback renderer
  gElementFactory.RegisterImageRenderer(TFPReportQRCode,@RenderQRCode);
end;

Procedure UnRegisterReportQRCode;

begin
  gElementFactory.RemoveClass('QRCode');
end;

initialization
  RegisterReportQRcode;
end.  
