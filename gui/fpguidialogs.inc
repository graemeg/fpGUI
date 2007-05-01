{
    fpGUI  -  Free Pascal GUI Library
    
    Dialogs class declarations
    
    Copyright (C) 2000 - 2007 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{%mainunit fpgui.pas}


{$IFDEF read_interface}

  { TFCustomStandardDialog }

  TFCustomStandardDialog = class(TFCustomForm)
  private
    function    GetMessage: string;
    procedure   SetMessage(const AValue: string);
    procedure   StdBtnClicked(Sender: TObject);
  protected
    FButtons: TMsgDlgButtons;
    MainLayout, BtnLayout: TFBoxLayout;
    Separator: TSeparator;
    FMessage: TFLabel;
    function    ProcessEvent(Event: TEventObj): Boolean; override;
    function    DistributeEvent(Event: TEventObj): Boolean; override;
    procedure   CalcSizes; override;
    procedure   Resized; override;
    procedure   SetButtons(AButtons: TMsgDlgButtons);
    property    Buttons: TMsgDlgButtons read FButtons write SetButtons default [mbOk, mbCancel];
  public
    constructor Create(AOwner: TComponent); override;
    property    Message: string read GetMessage write SetMessage;
  end;


  TFStandardDialog = class(TFCustomStandardDialog)
  published
    property Text;
    property OnCreate;
    property Buttons;
  end;
  
  

{$ENDIF read_interface}



{$IFDEF read_implementation}



// ===================================================================
//   TFCustomStandardDialog
// ===================================================================

// public methods

constructor TFCustomStandardDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtons := [mbOk, mbCancel];
  FBorderWidth := 4;

  MainLayout := TFBoxLayout.Create(Self);
  MainLayout.Orientation := Vertical;
  Child := MainLayout;

  FMessage := TFLabel.Create(self);
  FMessage.CanExpandWidth := True;
  MainLayout.InsertChild(FMessage);
  
  Separator := TSeparator.Create(Self);
  MainLayout.InsertChild(Separator);

  BtnLayout := TFBoxLayout.Create(Self);
  BtnLayout.Orientation := Horizontal;
  BtnLayout.HorzAlign := horzRight;
  BtnLayout.VertAlign := vertCenter;
  BtnLayout.CanExpandHeight := False;
  MainLayout.InsertChild(BtnLayout);

  SetButtons(FButtons);
end;


// protected methods

function TFCustomStandardDialog.ProcessEvent(Event: TEventObj): Boolean;
begin
  Result := MainLayout.ProcessEvent(Event) or inherited ProcessEvent(Event);
end;

function TFCustomStandardDialog.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(MainLayout) or inherited DistributeEvent(Event);
end;

procedure TFCustomStandardDialog.CalcSizes;
begin
  if Assigned(Child) then
  begin
    FMinSize := Child.MinSize + 2 * BorderWidth;
    FDefSize := Child.DefSize + 2 * BorderWidth;
    FMaxSize.cx := Min(InfiniteSize, Child.MaxSize.cx + 2 * BorderWidth);
    FMaxSize.cy := Min(InfiniteSize, Child.MaxSize.cy + 2 * BorderWidth);
  end;

  FMinSize.cx := Max(MinSize.cx, MainLayout.MinSize.cx + 2 * BorderWidth);
  Inc(FMinSize.cy, MainLayout.DefSize.cy + BorderWidth);
  FDefSize.cx := Max(DefSize.cx, MainLayout.DefSize.cx + 2 * BorderWidth);
  FDefSize.cy := Min(InfiniteSize, DefSize.cy);
  FMaxSize.cx := Min(MaxSize.cx, MainLayout.MaxSize.cx + 2 * BorderWidth);
  FMaxSize.cy := Min(InfiniteSize, MaxSize.cy + MainLayout.DefSize.cy);
end;

procedure TFCustomStandardDialog.Resized;
begin
  if Assigned(Child) then
    Child.SetBounds(Point(BorderWidth, BorderWidth),
      Size(Width - 2 * BorderWidth,
      Height - MainLayout.DefSize.cy - 2 * BorderWidth));
  MainLayout.SetBounds(
    Point(BorderWidth, Height - MainLayout.DefSize.cy - BorderWidth),
    Size(Width - 2 * BorderWidth, MainLayout.DefSize.cy - BorderWidth));
end;

procedure TFCustomStandardDialog.SetButtons(AButtons: TMsgDlgButtons);

  function AddBtn(const AText: String; ADefault: Boolean): TFButton;
  begin
    Result := TFButton.Create(Self);
    Result.Text := AText;
    // Result.Default := ADefault;
    Result.OnClick := @StdBtnClicked;
    Result.Parent := BtnLayout;
  end;

var
  i: integer;
  b: TFButton;
begin
  // remove and free all previous buttons
  for i := ComponentCount - 1 downto 0 do
  begin
    if Components[i] is TFButton then
    begin
      b := TFButton(Components[i]);
      if BtnLayout.ContainsChild(b) then
        BtnLayout.RemoveChild(b);
      b.Free;
    end;
  end;

  FButtons := AButtons;
  
  if mbYes      in FButtons then AddBtn(mbText_Yes, False);
  if mbNo       in FButtons then AddBtn(mbText_No, False);
  if mbOk       in FButtons then AddBtn(mbText_OK, True);
  if mbCancel   in FButtons then AddBtn(mbText_Cancel, False);
  if mbApply    in FButtons then AddBtn(mbText_Apply, False);
  if mbAbort    in FButtons then AddBtn(mbText_Abort, False);
  if mbRetry    in FButtons then AddBtn(mbText_Retry, False);
  if mbIgnore   in FButtons then AddBtn(mbText_Ignore, False);
  if mbAll      in FButtons then AddBtn(mbText_All, False);
  if mbNoToAll  in FButtons then AddBtn(mbText_NoToAll, False);
  if mbYesToAll in FButtons then AddBtn(mbText_YesToAll, False);
  if mbHelp     in FButtons then AddBtn(mbText_Help, False);
end;

function TFCustomStandardDialog.GetMessage: string;
begin
  Result := FMessage.Text;
end;

procedure TFCustomStandardDialog.SetMessage(const AValue: string);
begin
  if FMessage.Text <> AValue then
    FMessage.Text := AValue;
end;

procedure TFCustomStandardDialog.StdBtnClicked(Sender: TObject);
begin
  Close;
end;


{$ENDIF read_implementation}

