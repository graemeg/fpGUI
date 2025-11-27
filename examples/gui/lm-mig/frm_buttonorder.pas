unit frm_buttonorder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_radiobutton,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc, fpg_mig_platformdefaults;

type

  TButtonOrderForm = class(TfpgForm)
  private
    FDebug: boolean;
    lblTitle: TfpgLabel;
    lblPlatform: TfpgLabel;
    rbWindows: TfpgRadioButton;
    rbMacOSX: TfpgRadioButton;
    rbGnome: TfpgRadioButton;
    btnHelp: TfpgButton;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    procedure rbPlatformChanged(Sender: TObject);
    procedure RecreateButtonLayout;
  public
    constructor Create(AEnableDebug: Boolean); reintroduce;
    procedure AfterCreate; override;
  end;

implementation

constructor TButtonOrderForm.Create(AEnableDebug: Boolean);
begin
  inherited Create(nil);
  FDebug := AEnableDebug;
end;

procedure TButtonOrderForm.rbPlatformChanged(Sender: TObject);
begin
  // Update platform defaults based on selected radio button
  if rbWindows.Checked then
    TfpgMigPlatformDefaults.SetPlatform(PLATFORM_WINDOWS)
  else if rbMacOSX.Checked then
    TfpgMigPlatformDefaults.SetPlatform(PLATFORM_MAC_OSX)
  else if rbGnome.Checked then
    TfpgMigPlatformDefaults.SetPlatform(PLATFORM_GNOME);

  // Recreate layout to apply new button order
  RecreateButtonLayout;
end;

procedure TButtonOrderForm.RecreateButtonLayout;
var
  mig: TfpgMigLayoutManager;
begin
  // Destroy existing buttons
  if Assigned(btnHelp) then
    FreeAndNil(btnHelp);
  if Assigned(btnOK) then
    FreeAndNil(btnOK);
  if Assigned(btnCancel) then
    FreeAndNil(btnCancel);

  // Create new MigLayout manager
  mig := TfpgMigLayoutManager.Create;
  if FDebug then
    mig.LC.Debug(500);
  LayoutManager := mig;

  // Re-add title label
  mig.AddLayoutComponent(lblTitle, TfpgMigCC.Create().SpanX().Wrap());

  // Re-add platform selection label and radio buttons
  mig.AddLayoutComponent(lblPlatform, TfpgMigCC.Create().SpanX().Wrap());
  mig.AddLayoutComponent(rbWindows, TfpgMigCC.Create());
  mig.AddLayoutComponent(rbMacOSX, TfpgMigCC.Create());
  mig.AddLayoutComponent(rbGnome, TfpgMigCC.Create().Wrap());

  // Create buttons with tags for platform-specific ordering
  btnHelp := TfpgButton.Create(Self);
  btnHelp.Name := 'btnHelp';
  btnHelp.Text := 'Help';
  btnHelp.PreferredSize := fpgSize(80, 24);
  mig.AddLayoutComponent(btnHelp, TfpgMigCC.Create().SpanX().Split(3).Tag('help'));

  btnOK := TfpgButton.Create(Self);
  btnOK.Name := 'btnOK';
  btnOK.Text := 'OK';
  btnOK.PreferredSize := fpgSize(80, 24);
  btnOK.ModalResult := mrOK;
  mig.AddLayoutComponent(btnOK, TfpgMigCC.Create().Tag('ok'));

  btnCancel := TfpgButton.Create(Self);
  btnCancel.Name := 'btnCancel';
  btnCancel.Text := 'Cancel';
  btnCancel.PreferredSize := fpgSize(80, 24);
  btnCancel.ModalResult := mrCancel;
  mig.AddLayoutComponent(btnCancel, TfpgMigCC.Create().Tag('cancel'));

  // Force layout update
  Realign;
end;

procedure TButtonOrderForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  currentPlatform: Integer;
begin
  inherited AfterCreate;
  Name := 'ButtonOrderForm';
  Left := 300;
  Top := 200;
  Width := 500;
  Height := 250;
  WindowTitle := 'MigLayout - Platform Button Ordering';

  // Get current platform
  currentPlatform := TfpgMigPlatformDefaults.GetCurrentPlatform;

  // Create MigLayout manager
  mig := TfpgMigLayoutManager.Create;
  if FDebug then
    mig.LC.Debug(500);
  LayoutManager := mig;

  // Title label
  lblTitle := TfpgLabel.Create(Self);
  lblTitle.Name := 'lblTitle';
  lblTitle.Text := 'Select a platform to see different button ordering:';
  mig.AddLayoutComponent(lblTitle, TfpgMigCC.Create().SpanX().Wrap());

  // Platform selection label
  lblPlatform := TfpgLabel.Create(Self);
  lblPlatform.Name := 'lblPlatform';
  lblPlatform.Text := 'Platform:';
  mig.AddLayoutComponent(lblPlatform, TfpgMigCC.Create().SpanX().Wrap());

  // Radio buttons for platform selection
  rbWindows := TfpgRadioButton.Create(Self);
  rbWindows.Name := 'rbWindows';
  rbWindows.Text := 'Windows';
  rbWindows.PreferredSize := fpgSize(130, 24);
  rbWindows.GroupIndex := 1;
  rbWindows.Checked := (currentPlatform = PLATFORM_WINDOWS);
  rbWindows.OnChange := @rbPlatformChanged;
  mig.AddLayoutComponent(rbWindows, TfpgMigCC.Create());

  rbMacOSX := TfpgRadioButton.Create(Self);
  rbMacOSX.Name := 'rbMacOSX';
  rbMacOSX.Text := 'Mac OSX';
  rbMacOSX.PreferredSize := fpgSize(130, 24);
  rbMacOSX.GroupIndex := 1;
  rbMacOSX.Checked := (currentPlatform = PLATFORM_MAC_OSX);
  rbMacOSX.OnChange := @rbPlatformChanged;
  mig.AddLayoutComponent(rbMacOSX, TfpgMigCC.Create());

  rbGnome := TfpgRadioButton.Create(Self);
  rbGnome.Name := 'rbGnome';
  rbGnome.Text := 'GNOME/Linux';
  rbGnome.PreferredSize := fpgSize(130, 24);
  rbGnome.GroupIndex := 1;
  rbGnome.Checked := (currentPlatform = PLATFORM_GNOME);
  rbGnome.OnChange := @rbPlatformChanged;
  mig.AddLayoutComponent(rbGnome, TfpgMigCC.Create().Wrap());

  // Create buttons with tags for platform-specific ordering
  btnHelp := TfpgButton.Create(Self);
  btnHelp.Name := 'btnHelp';
  btnHelp.Text := 'Help';
  btnHelp.PreferredSize := fpgSize(80, 24);
  mig.AddLayoutComponent(btnHelp, TfpgMigCC.Create().SpanX().Split(3).Tag('help'));

  btnOK := TfpgButton.Create(Self);
  btnOK.Name := 'btnOK';
  btnOK.Text := 'OK';
  btnOK.PreferredSize := fpgSize(80, 24);
  btnOK.ModalResult := mrOK;
  mig.AddLayoutComponent(btnOK, TfpgMigCC.Create().Tag('ok'));

  btnCancel := TfpgButton.Create(Self);
  btnCancel.Name := 'btnCancel';
  btnCancel.Text := 'Cancel';
  btnCancel.PreferredSize := fpgSize(80, 24);
  btnCancel.ModalResult := mrCancel;
  mig.AddLayoutComponent(btnCancel, TfpgMigCC.Create().Tag('cancel'));
end;

end.
