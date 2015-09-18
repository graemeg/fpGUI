unit customedits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_edit, fpg_base, fpg_main;

type

  { TCustomColorEdit }

  TCustomColorEdit = class(TfpgEdit)
  protected
    procedure DrawPlaceholderText(constref ARect: TfpgRect); override;
  end;

  { TCustomAlignmentEdit }

  TCustomAlignmentEdit = class(TfpgEdit)
  protected
    procedure DrawPlaceholderText(constref ARect: TfpgRect); override;
  end;

  { TCustomFontEdit }

  TCustomFontEdit = class(TfpgEdit)
  protected
    FExtraHintFont: TfpgFont;
    procedure DrawPlaceholderText(constref ARect: TfpgRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TCustomFontEdit }

procedure TCustomFontEdit.DrawPlaceholderText(constref ARect: TfpgRect);
begin
  //inherited DrawPlaceholderText(ARect);
  Canvas.SetFont(FExtraHintFont);
  fpgStyle.DrawPlaceholderText(Canvas, ARect, ExtraHint);
  Canvas.SetFont(Font); // Restore font
end;

constructor TCustomFontEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FExtraHintFont := fpgGetFont('DejaVu Sans-10:bold:italic:antialias=true:underline');
end;

destructor TCustomFontEdit.Destroy;
begin
  FExtraHintFont.Free;
  inherited Destroy;
end;

{ TCustomAlignmentEdit }

procedure TCustomAlignmentEdit.DrawPlaceholderText(constref ARect: TfpgRect);
begin
  //inherited DrawPlaceholderText(ARect);
  Canvas.SetTextColor(clPlaceholderText);
  Canvas.DrawText(ARect, ExtraHint, [txtHCenter, txtVCenter]);
end;

{ TCustomColorEdit }

procedure TCustomColorEdit.DrawPlaceholderText(constref ARect: TfpgRect);
begin
  //inherited DrawPlaceholderText(ARect);
  Canvas.SetTextColor(clRed);
  Canvas.DrawText(ARect, ExtraHint);
end;

end.

