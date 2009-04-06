unit gui_miglayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_panel, fpg_base;
  
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
  fpg_main;

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

