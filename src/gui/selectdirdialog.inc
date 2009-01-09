{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}


  { TfpgSelectDirDialog }

  TfpgSelectDirDialog = class(TfpgBaseDialog)
  private
    lblTitle: TfpgLabel;
    edDirectory: TfpgEdit;
    tv: TfpgTreeView;
    function    GetDirectory: TfpgString;
  public
    constructor Create(AOwner: TComponent); override;
    property    Directory: TfpgString read GetDirectory;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}

function TfpgSelectDirDialog.GetDirectory: TfpgString;
begin
  //
end;

constructor TfpgSelectDirDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  lblTitle := CreateLabel(self, 8, 8, rsEnterNewDirectory);
  edDirectory := CreateEdit(self, 8, 28, 270, 0);
  edDirectory.Anchors := [anLeft, anTop, anRight];

  tv := TfpgTreeView.Create(self);
  with tv do
  begin
    Name := 'tv';
    SetPosition(8, 28, 270, 250);
  end;

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;

  // now reset tab order
  edDirectory.TabOrder  := 1;
  btnOK.TabOrder        := 2;
  btnCancel.TabOrder    := 3;
end;

{$ENDIF read_implementation}


