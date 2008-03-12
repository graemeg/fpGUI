{%mainunit gui_dialogs.pas}

{$IFDEF read_interface}


{ TfpgPromptUserDialog }

TfpgPromptUserDialog = class(TfpgBaseDialog)
private
  lblTitle: TfpgLabel;
  lblTitlePw: TfpgLabel;
  edUserID: TfpgEdit;
  edPassword: TfpgEdit;
  function    GetUserID: TfpgString;
  function    GetUserPassword: TfpgString;
public
  constructor Create(AOwner: TComponent); override;
  property    UserID: TfpgString read GetUserID;
  property    Password: TfpgString read GetUserPassword;
end;


{$ENDIF read_interface}



{$IFDEF read_implementation}


{ TfpgPromptUserDialog }

function TfpgPromptUserDialog.GetUserID: TfpgString;
begin
  Result := edUserID.Text;
end;

function TfpgPromptUserDialog.GetUserPassword: TfpgString;
begin
  Result := edPassword.Text;
end;

constructor TfpgPromptUserDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // reset dimensions
  Width     := 200;
  Height    := 140;
  MinWidth  := 200;
  MinHeight := 140;
  
  lblTitle := CreateLabel(self, 8, 8, rsUserName);
  edUserID := CreateEdit(self, 8, 28, 184, 0);
  edUserID.Anchors := [anLeft, anTop, anRight];

  lblTitlePw := CreateLabel(self, 8, 55, rsPassword);
  edPassword := CreateEdit(self, 8, 73, 184, 0);
  edPassword.Anchors := [anLeft, anTop, anRight];
  edPassword.PasswordMode := True;

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;
  
  // now reset tab order
  edUserID.TabOrder     := 1;
  edPassword.TabOrder   := 2;
  btnOK.TabOrder        := 3;
  btnCancel.TabOrder    := 4;
end;


{$ENDIF read_implementation}

