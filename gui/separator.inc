{
    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther
    Copyright (C) 2006 by Graeme Geldenhuys 
      member of the fpGUI development team.

    Separator class declarations

    See the file COPYING.fpGUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{%mainunit fpgui.pp}

{$IFDEF read_interface}

  TCustomSeparator = class(TWidget)
  private
    procedure SetOrientation(AOrientation: TOrientation);
    procedure SetSpacing(ASpacing: Integer);
  protected
    FOrientation: TOrientation;
    FSpacing: Integer;
    procedure   Paint(Canvas: TFCanvas); override;
    procedure   CalcSizes; override;
    property    Orientation: TOrientation read FOrientation write SetOrientation default Horizontal;
    property    Spacing: Integer read FSpacing write SetSpacing default 4;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TSeparator = class(TCustomSeparator)
  published
    property    Enabled;
    property    Orientation;
    property    Spacing;
  end;

{$ENDIF read_interface}



{$IFDEF read_implementation}



// ===================================================================
//   TCustomSeparator
// ===================================================================

constructor TCustomSeparator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanExpandWidth := True;
  FSpacing := 4;
end;

procedure TCustomSeparator.Paint(Canvas: TFCanvas);
begin
  Style.DrawSeparator(Canvas, Rect(0, 0, Width, Height), Orientation);
end;

procedure TCustomSeparator.CalcSizes;
begin
  if Orientation = Horizontal then
  begin
    FCanExpandWidth := True;
    FCanExpandHeight := False;
    FMinSize.cy := Style.GetSeparatorSize + 2 * Spacing
  end else
  begin
    FCanExpandWidth := False;
    FCanExpandHeight := True;
    FMinSize.cx := Style.GetSeparatorSize + 2 * Spacing;
  end;
end;

procedure TCustomSeparator.SetOrientation(AOrientation: TOrientation);
begin
  if AOrientation <> Orientation then
  begin
    FOrientation := AOrientation;
    Update;
  end;
end;

procedure TCustomSeparator.SetSpacing(ASpacing: Integer);
begin
  if ASpacing <> Spacing then
  begin
    FSpacing := ASpacing;
    Update;
  end;
end;


{$ENDIF read_implementation}

