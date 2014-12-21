unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpg_base, fpg_form, fpg_toggle;

type

  { TfrmMain }

  TfrmMain = class(TfpgForm)
  private
    FToggle: TfpgToggle;
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

{ TfrmMain }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle:='Yay a toggle!';
  SetWidth(300);
  SetHeight(200);

  FToggle := TfpgToggle.Create(Self);
  FToggle.SetPosition(10, 10, 200, 20);
//  FToggle.Width:=200;

  //FToggle.ToggleSide:=tsLeft;
  //FToggle.ToggleWidth:=100;
  //FToggle.UseAnimation:=False;

end;

end.

