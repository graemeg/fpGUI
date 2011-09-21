{%mainunit fpg_dialogs.pas}

{$IFDEF read_interface}


  TfpgPromptUserDialog = class(TfpgBaseDialog)
  private
    lblTitle: TfpgLabel;
    lblTitlePw: TfpgLabel;
    function    GetUserID: TfpgString;
    function    GetUserPassword: TfpgString;
    procedure SetUserID(const AValue: TfpgString);
  protected
    edUserID: TfpgEdit;
    edPassword: TfpgEdit;
    procedure   btnOKClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    // Gof defines this Authenticate method a Hook operation in Template methods pattern.
    // Subclass will have to override and implement it. By default it returns True.
    function    Authenticate: boolean; virtual;
    procedure   Wiggle(const Seconds: integer = 1; const MovingPixels: integer = 5; const aSleep: integer = 75);
    property    UserID: TfpgString read GetUserID write SetUserID;
    property    Password: TfpgString read GetUserPassword;
  end;


  TfpgPromptUserDbDialog = class(TfpgPromptUserDialog)
  private
    lblDatabases: TfpgLabel;
  protected
    aStringList: TStringList;
    cbDatabases: TfpgComboBox;
    function    GetSelectedDatabase: TfpgString; virtual;
    procedure   PopulateComboDb; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Database: TfpgString read GetSelectedDatabase;
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

procedure TfpgPromptUserDialog.SetUserID(const AValue: TfpgString);
begin
  edUserID.Text := AValue;
end;

procedure TfpgPromptUserDialog.btnOKClick(Sender: TObject);
begin
  if Authenticate then
    inherited btnOKClick(Sender);
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

function TfpgPromptUserDialog.Authenticate: boolean;
begin
  Result := True;
end;

procedure TfpgPromptUserDialog.Wiggle(const Seconds: integer; const MovingPixels: integer; const aSleep: integer);
var
  MyTime: TDateTime;
begin
  MyTime := Now;
  repeat
    MoveWindow(Left - MovingPixels, Top);
    fpgApplication.ProcessMessages;
    Sleep(aSleep);
    MoveWindow(Left + MovingPixels * 2, Top);
    fpgApplication.ProcessMessages;
    Sleep(aSleep);
    MoveWindow(Left - MovingPixels, Top);
    fpgApplication.ProcessMessages;
  until SecondsBetween(Now, MyTime) >= Seconds;
end;

{ TfpgPromptUserDbDialog }

function TfpgPromptUserDbDialog.GetSelectedDatabase: TfpgString;
begin
  Result := cbDatabases.Text;
end;

constructor TfpgPromptUserDbDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // reset dimensions
  Width     := 200;
  Height    := 185;
  MinWidth  := 200;
  MinHeight := 185;

  aStringList := TStringList.Create;

  lblDatabases := CreateLabel(self, 8, 101, rsDatabase);
  cbDatabases := CreateComboBox(self, 8, 119, 184, aStringList);
  cbDatabases.Anchors := [anLeft, anTop, anRight];
  cbDatabases.Height := edUserID.Height;

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;

  // now reset tab order
  edUserID.TabOrder     := 1;
  edPassword.TabOrder   := 2;
  cbDatabases.TabOrder  := 3;
  btnOK.TabOrder        := 5;
  btnCancel.TabOrder    := 6;
  
  PopulateComboDb;
end;

destructor TfpgPromptUserDbDialog.Destroy;
begin
  aStringList.Free;
  inherited Destroy;
end;

procedure TfpgPromptUserDbDialog.PopulateComboDb;
begin
  // do nothing. Derived classe will implement it
end;

{$ENDIF read_implementation}

