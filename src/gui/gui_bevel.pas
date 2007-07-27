unit gui_bevel;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpgfx,
  gfxbase,
  gfx_widget;
  
type

  TBevelShape = (bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine, bsRightLine, bsSpacer);

  TBevelStyle = (bsLowered, bsRaised);
  

  TfpgBevel = class(TfpgWidget)
  private
    FBevelShape: TBevelShape;
    FBevelStyle: TBevelStyle;
    procedure   SetBevelShape(const AValue: TBevelShape);
    procedure   SetBevelStyle(const AValue: TBevelStyle);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Shape: TBevelShape read FBevelShape write SetBevelShape default bsBox;
    property    Style: TBevelStyle read FBevelStyle write SetBevelStyle default bsRaised;
  end;


function CreateBevel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TfpgCoord;
           AShape: TBevelShape; AStyle: TBevelStyle): TfpgBevel;


implementation


function CreateBevel(AOwner: TComponent; ALeft, ATop, AWidth,
  AHeight: TfpgCoord; AShape: TBevelShape; AStyle: TBevelStyle): TfpgBevel;
begin
  Result := TfpgBevel.Create(AOwner);
  Result.Left     := ALeft;
  Result.Top      := ATop;
  Result.Width    := AWidth;
  Result.Height   := AHeight;
  Result.Shape    := AShape;
  Result.Style    := AStyle;
end;

{ TfpgBevel }

procedure TfpgBevel.SetBevelShape(const AValue: TBevelShape);
begin
  if FBevelShape = AValue then
    Exit; //==>
  FBevelShape := AValue;
  Repaint;
end;

procedure TfpgBevel.SetBevelStyle(const AValue: TBevelStyle);
begin
  if FBevelStyle = AValue then
    Exit; //==>
  FBevelStyle := AValue;
  Repaint;
end;

procedure TfpgBevel.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(clWindowBackground);
  
//  Canvas.SetLineStyle(2, lsSolid);
//  Canvas.SetColor(clWindowBackground);
//  Canvas.DrawRectangle(1, 1, Width - 1, Height - 1);
  Canvas.SetLineStyle(1, lsSolid);

  if Style = bsRaised then
    Canvas.SetColor(clHilite2)
  else
    Canvas.SetColor(clShadow2);

  if Shape in [bsBox, bsFrame, bsTopLine] then
    Canvas.DrawLine(0, 0, Width - 1, 0);
  if Shape in [bsBox, bsFrame, bsLeftLine] then
    Canvas.DrawLine(0, 1, 0, Height - 1);
  if Shape in [bsFrame, bsRightLine] then
    Canvas.DrawLine(Width - 2, 1, Width - 2, Height - 1);
  if Shape in [bsFrame, bsBottomLine] then
    Canvas.DrawLine(1, Height - 2, Width - 1, Height - 2);

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  if Shape in [bsFrame, bsTopLine] then
    Canvas.DrawLine(1, 1, Width - 2, 1);
  if Shape in [bsFrame, bsLeftLine] then
    Canvas.DrawLine(1, 2, 1, Height - 2);
  if Shape in [bsBox, bsFrame, bsRightLine] then
    Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1);
  if Shape in [bsBox, bsFrame, bsBottomLine] then
    Canvas.DrawLine(0, Height - 1, Width, Height - 1);

  Canvas.EndDraw;
end;

constructor TfpgBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBevelShape   := bsBox;
  FBevelStyle   := bsRaised;
  FWidth        := 80;
  FHeight       := 80;
end;

end.

