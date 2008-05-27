unit gui_miglayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_panel, gfxbase;
  
type

  { TfpgLayoutPanel }

  TfpgLayoutPanel = class(TfpgBevel)
  protected
    procedure   HandleResize(awidth, aheight: TfpgCoord); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Add(AComponent: TComponent; AConstraint: string);
  end;

implementation

uses
  fpgfx;

{ TfpgLayoutPanel }

procedure TfpgLayoutPanel.HandleResize(awidth, aheight: TfpgCoord);
begin
  writeln('HandleResize');
  inherited HandleResize(awidth, aheight);
end;

constructor TfpgLayoutPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  Shape := bsSpacer;
end;

procedure TfpgLayoutPanel.Add(AComponent: TComponent; AConstraint: string);
begin
  //
end;

end.

