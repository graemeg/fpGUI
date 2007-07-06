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
    procedure   HandleLMouseDown(x, y: integer; shiftstate: word); override;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
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
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FDropDown          := TfpgForm.Create(nil);
    FDropDown.WindowType := wtPopup;
    FDropDown.Sizeable := False;
    FDropDown.Left     := Left;
    FDropDown.Top      := Top + Height;
    FDropDown.Width    := Width;
    FDropDown.Height   := DropDownCount * Height;
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
  TPrivateWidget(FInternalBtn).MoveAndResize(Width - min(Height, 20) - 3, 2, Height - 4, Height - 4);
  DoDropDown;
end;

procedure TfpgCustomComboBox.HandleLMouseDown(x, y: integer; shiftstate: word);
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

  //  pgfStyle.DrawButtonFace(canvas, width - min(height, 20)-3, 2, height-4, height-4, [btnIsEmbedded]);
  //  pgfStyle.DrawDirectionArrow(canvas, width - height + 1, 1, height-2, height-2, 1);

  Canvas.EndDraw;
end;

constructor TfpgCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := clBoxColor;
  FDropDownCount   := 8;
  if not Assigned(FInternalBtn) then
  begin
    FInternalBtn           := CreateButton(self, Width - min(Height, 20) - 3, 2, Height - 4, '', @InternalBtnClick);
    FInternalBtn.Embedded  := True;
    FInternalBtn.Parent    := self;
    FInternalBtn.ImageName := 'sys.sb.down';
    FInternalBtn.ShowImage := True;
  end;
end;

end.

