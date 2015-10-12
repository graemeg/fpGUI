unit fpg_dnd_window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpg_main,
  fpg_base,
  fpg_window;

type

  { TfpgDNDWindow }

  TfpgDNDWindow = class(TfpgWindow)
  private
    FDrag: TfpgDrag;
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent; ADrag: TfpgDrag);
    procedure   Show(ASize: TfpgSize);
  end;

implementation

{ TfpgDNDWindow }

type
  TfpgDragAccess = class(TfpgDrag);

procedure TfpgDNDWindow.HandlePaint;
begin
  if Assigned(FDrag) then
    TfpgDragAccess(FDrag).DoOnPaintPreview(Canvas);
end;

constructor TfpgDNDWindow.Create(AOwner: TComponent; ADrag: TfpgDrag);
begin
  inherited Create(AOwner);
  WindowType:=wtPopup;
  WindowOpacity:=0.4;
  Focusable:=False;
  FDrag := ADrag;
end;

procedure TfpgDNDWindow.Show(ASize: TfpgSize);
begin
  FVisible:=True;

  HandleShow;
  SetPosition(Left, Top, ASize.W, ASize.H);
end;

end.

