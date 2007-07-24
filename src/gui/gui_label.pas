unit gui_label;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfxbase,
  fpgfx,
  gfx_widget;

type

  TfpgLabel = class(TfpgWidget)
  private
    FAutoSize: boolean;
    FBackgroundColor: TfpgColor;
    FColor: TfpgColor;
    function    GetFontDesc: string;
    procedure   SetAutoSize(const AValue: boolean);
    procedure   SetBackgroundColor(const AValue: TfpgColor);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetColor(const AValue: TfpgColor);
    procedure   SetText(const AValue: string);
    procedure   ResizeLabel;
  protected
    FText: string;
    FFont: TfpgFont;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TfpgFont read FFont;
  published
    property    AutoSize: boolean read FAutoSize write SetAutoSize default True;
    property    Text: string read FText write SetText;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Color: TfpgColor read FColor write SetColor;
    property    BackgroundColor: TfpgColor read FBackgroundColor write SetBackgroundColor;
    property    OnMouseMove;
  end;

  TLabelClass = class of TfpgLabel;

function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgLabel;

implementation

function CreateLabel(AOwner: TComponent; x, y: TfpgCoord; AText: string): TfpgLabel;
begin
  Result       := TfpgLabel.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  Result.Text  := AText;
  Result.Width := Result.Font.TextWidth(Result.Text);
end;

{ TfpgLabel }

procedure TfpgLabel.SetColor(const AValue: TfpgColor);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  RePaint;
end;

function TfpgLabel.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TfpgLabel.SetAutoSize(const AValue: boolean);
begin
  if FAutoSize = AValue then
    Exit; //==>
  FAutoSize := AValue;
  if FAutoSize then
  begin
    ResizeLabel;
    RePaint;
  end;
end;

procedure TfpgLabel.SetBackgroundColor(const AValue: TfpgColor);
begin
  if FBackgroundColor = AValue then
    Exit;
  FBackgroundColor := AValue;
  RePaint;
end;

procedure TfpgLabel.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  if FAutoSize then
    ResizeLabel;
  RePaint;
end;

procedure TfpgLabel.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  if FAutoSize then
    ResizeLabel;
  RePaint;
end;

procedure TfpgLabel.ResizeLabel;
begin
  Width   := FFont.TextWidth(FText);
  Height  := FFont.Height;
  UpdateWindowPosition;
end;

constructor TfpgLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText   := 'Label';
  FFont   := fpgGetFont('#Label1');
  FHeight := FFont.Height;
  FWidth  := 80;
  FColor  := clText1;
  FBackgroundColor := clWindowBackground;
  FAutoSize := True;
end;

destructor TfpgLabel.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TfpgLabel.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited;
  Canvas.Clear(FBackgroundColor);
  Canvas.SetFont(Font);
  Canvas.SetTextColor(FColor);
  Canvas.DrawString(0, 0, FText);
  Canvas.EndDraw;
end;

end.

