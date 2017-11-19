{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Barcode report element.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportbarcode;

{$mode objfpc}
{$H+}

interface

uses
  Classes, fpimage, fpexprpars, fpimgbarcode, fpbarcode, fpreport, fpreportstreamer;

Type

  { TFPReportBarcode }

  TFPReportBarcode = Class(TFPReportElement)
  private
    FEncoding: TBarcodeEncoding;
    FExpression: String;
    FPadLength: Integer;
    FUnitWidth: Integer;
    FValue: String;
    FExprValue : String;
    FWeight: Double;
  Protected
    procedure BeforePrint;  override;
    procedure RecalcLayout; override;
    Procedure DoWriteLocalProperties(AWriter: TFPReportStreamer; AOriginal: TFPReportElement=nil); override;
  Public
    procedure   Assign(Source: TPersistent); override;
    Constructor Create(AOwner: TComponent); override;
    // Will calculate the value to display. Either Value or evaluated expression.
    Function BarcodeValue : String;
    Procedure ReadElement(AReader: TFPReportStreamer); override;
  Published
    Property Encoding : TBarcodeEncoding Read FEncoding Write FEncoding;
    Property UnitWidth : Integer Read FUnitWidth Write FUnitWidth;
    Property Weight : Double Read FWeight Write FWeight;
    Property PadLength : Integer Read FPadLength Write FPadLength;
    // Expression takes precedence
    Property Value : String Read FValue Write FValue;
    Property Expression : String Read FExpression Write FExpression;
  end;

Procedure RegisterReportBarcode;
Procedure UnRegisterReportBarcode;
  
implementation

uses typinfo, strutils;


{ TFPReportBarcode }

procedure TFPReportBarcode.RecalcLayout;
begin
  // Do nothing for the moment.
  // We may consider adding a Boolean property FitWidth and calculating width based on value/expression when it is set to true
end;

procedure TFPReportBarcode.DoWriteLocalProperties(AWriter: TFPReportStreamer; AOriginal: TFPReportElement);


begin
  inherited DoWriteLocalProperties(AWriter, AOriginal);
  AWriter.WriteString('Encoding',GetEnumName(TypeInfo(TBarcodeEncoding),Ord(FEncoding)));
  AWriter.WriteInteger('UnitWidth',UnitWidth);
  AWriter.WriteInteger('PadLength',PadLength);
  AWriter.WriteFloat('Weight',Weight);
  AWriter.WriteString('Value',Value);
  AWriter.WriteString('Expression',Expression);
end;

procedure TFPReportBarcode.Assign(Source: TPersistent);

Var
  BC : TFPReportBarcode;

begin
  if (Source is TFPReportBarcode) then
    begin
    BC:=TFPReportBarcode(Source);
    FValue:=BC.Value;
    FPadlength:=BC.PadLength;
    FExpression:=BC.Expression;
    FWeight:=BC.Weight;
    FUnitWidth:=BC.UnitWidth;
    FEncoding:=BC.Encoding;
    end;
  inherited Assign(Source);
end;

constructor TFPReportBarcode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEncoding:=be128A;
  FUnitWidth:=1;
  FWeight:=2.0;
end;

procedure TFPReportBarcode.BeforePrint;

begin
  Inherited;
  if (FExpression<>'') then
  FExprValue:=EvaluateExpressionAsText(FExpression)
end;

function TFPReportBarcode.BarcodeValue: String;

begin
  if (FExpression<>'') then
    Result:=FExprValue // Calculated in beforeprint
  else
    Result:=FValue;
  Result:=AddChar('0',Result,PadLength);
end;

procedure TFPReportBarcode.ReadElement(AReader: TFPReportStreamer);

Var
  S : String;
  I : Integer;

begin
  inherited ReadElement(AReader);
  S:=AReader.ReadString('Encoding','beEan8');
  I:=GetEnumValue(TypeInfo(TBarcodeEncoding),S);
  if I<>-1 then
    FEncoding:=TBarcodeEncoding(I);
  UnitWidth:=AReader.ReadInteger('UnitWidth',UnitWidth);
  PadLength:=AReader.ReadInteger('UnitWidth',PadLength);
  Weight:=AReader.ReadFloat('Weight',Weight);
  Value:=AReader.ReadString('Value',Value);
  Expression:=AReader.ReadString('Expression',Expression);
end;

procedure RenderBarcode(aElement: TFPReportElement; aImage: TFPCustomImage);

Var
  D : TFPDrawBarcode;
  B : TFPReportBarcode;

begin
  B:=TFPReportBarcode(aElement);
  D:=TFPDrawBarcode.Create;
  try
    D.Image:=aImage;
    D.Weight:=B.Weight;
    D.UnitWidth:=B.UnitWidth;
    D.Rect:=Rect(0,0,aImage.Width-1,aImage.Height-1);
    D.Text:=B.BarcodeValue;
    // Writeln('Weight: ',D.Weight,' unitwidth:',D.UnitWidth,' ',aImage.Width-1,'x',aImage.Height-1,' Text: ',D.Text);
    D.Encoding:=B.Encoding;
    D.Clipping:=True;
    D.Draw;
  finally
    D.Free;
  end;
end;


Procedure RegisterReportBarcode;

begin
  gElementFactory.RegisterClass('Barcode',TFPReportBarcode);
  // Fallback renderer
  gElementFactory.RegisterImageRenderer(TFPReportBarcode,@RenderBarcode);
end;

Procedure UnRegisterReportBarcode;

begin
  gElementFactory.RemoveClass('Barcode');
end;

initialization
  RegisterReportBarcode;
end.  
