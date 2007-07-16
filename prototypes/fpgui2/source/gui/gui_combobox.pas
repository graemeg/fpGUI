unit gui_combobox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  gfx_widget,
  gui_form,
  gfxbase,
  gui_button;

type

  { TfpgCustomComboBox }

  TfpgCustomComboBox = class(TfpgWidget)
  private
    FDropDownCount: integer;
    FDropDown: TfpgForm;
    FBackgroundColor: TfpgColor;
    FInternalBtn: TfpgButton;
    procedure   SetDropDownCount(const AValue: integer);
    procedure   DoDropDown;
    procedure   InternalBtnClick(Sender: TObject);
  protected
    property    DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterConstruction; override;
  end;


  TfpgComboBox = class(TfpgCustomComboBox)
  published
    property    DropDownCount;
  end;
  

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList): TfpgComboBox;


implementation

uses
  fpgfx,
  Math;

type
  // This is so we can access protected methods
  TPrivateWidget = class(TfpgWidget)
  end;
  
  { TDropDownWindow }

  TDropDownWindow = class(TfpgForm)
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TDropDownWindow }

procedure TDropDownWindow.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  Canvas.Clear(clWhite);
  Canvas.SetColor(clYellow);
  Canvas.SetLineStyle(2, lsSolid);
  Canvas.DrawRectangle(1, 1, Width-1, Height-1);
  Canvas.EndDraw;
end;

constructor TDropDownWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowType := wtPopup;
  WindowAttributes := [];
  WindowPosition := wpUser;
end;
  

function CreateComboBox(AOwner: TComponent; x, y, w: TfpgCoord; AList: TStringList): TfpgComboBox;
begin
  Result           := TfpgComboBox.Create(AOwner);
  Result.Left      := x;
  Result.Top       := y;
  Result.Width     := w;
  Result.Focusable := True;

  Result.Height := 23;  // replace this with font height + margins
  {$Note We still need to handle the AList param as well.}
end;

{ TfpgCustomComboBox }

procedure TfpgCustomComboBox.SetDropDownCount(const AValue: integer);
begin
  if FDropDownCount = AValue then
    Exit;
  FDropDownCount := AValue;
end;

procedure TfpgCustomComboBox.DoDropDown;
var
  pt: TPoint;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    pt := WindowToScreen(Parent, Point(Left, Top+Height));
    FDropDown          := TDropDownWindow.Create(nil);
    FDropDown.Left     := pt.X;
    FDropDown.Top      := pt.Y;
    FDropDown.Width    := Width;
    FDropDown.Height   := (DropDownCount * (Height-4));
    FDropDown.Show;
  end
  else
  begin
    FDropDown.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TfpgCustomComboBox.InternalBtnClick(Sender: TObject);
begin
  DoDropDown;
end;

procedure TfpgCustomComboBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  DoDropDown;
end;

procedure TfpgCustomComboBox.HandlePaint;
var
  r: TfpgRect;
begin
  inherited HandlePaint;
  Canvas.BeginDraw;
  Canvas.ClearClipRect;
  Canvas.DrawControlFrame(0, 0, Width, Height);

  // internal background rectangle (without frame)
  r.Left   := 2;
  r.Top    := 2;
  r.Width  := Width - 4;
  r.Height := Height - 4;
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);
  // we already set the clip rectangle so we can paint full size
  Canvas.FillRectAngle(2, 2, Width - 4, Height - 4);
  //  Canvas.FillRectAngle(0,0,Width,Height);

//  fpgStyle.DrawButtonFace(canvas, width - min(height, 20)-3, 2, height-4, height-4, [btnIsEmbedded]);
//  fpgStyle.DrawDirectionArrow(canvas, width - height + 1, 1, height-2, height-2, 1);

  Canvas.EndDraw;
end;

constructor TfpgCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := clBoxColor;
  FDropDownCount   := 8;
  FWidth := 120;
  Height := 23;
end;

procedure TfpgCustomComboBox.AfterConstruction;
begin
  inherited AfterConstruction;

  if not Assigned(FInternalBtn) then
  begin
    FInternalBtn           := CreateButton(self, (Width-19), 2, 18, '', @InternalBtnClick);
    FInternalBtn.Height    := 19;
    FInternalBtn.Embedded  := True;
    FInternalBtn.Parent    := self;
    FInternalBtn.ImageName := 'sys.sb.down';
    FInternalBtn.ShowImage := True;
  end;
end;

end.

