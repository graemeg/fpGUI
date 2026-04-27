unit frm_buttonorder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_radiobutton,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc, fpg_mig_platformdefaults,
  fpg_mig_unitvalue;

type

  { TButtonOrderForm }

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
    procedure btnHelpClicked(Sender: TObject);
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

  // Invalidate layout - Grid will be recreated with new button order and sizes
  Realign;
end;

procedure TButtonOrderForm.btnHelpClicked(Sender: TObject);
begin
  PrintRect(btnHelp.GetBoundsRect);
  PrintRect(btnOK.GetBoundsRect);
  PrintRect(btnCancel.GetBoundsRect);
end;

procedure TButtonOrderForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
  currentPlatform: Integer;
  minBtnWidth: TfpgMigUnitValue;
  btnWidth, btnHeight: Integer;
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
    mig.LC.Debug();
  LayoutManager := mig;

  // Title label
  lblTitle := TfpgLabel.Create(Self);
  lblTitle.Name := 'lblTitle';
  lblTitle.Text := 'Select a platform to see different button ordering:';
  lblTitle.FontDesc := '#Label2';
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
  rbMacOSX.Text := 'MacOS';
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

  // Get platform-specific minimum button width with DPI scaling
  minBtnWidth := TfpgMigPlatformDefaults.GetMinimumButtonWidth;
  btnWidth := Round(minBtnWidth.GetPixels(0, Self, nil));
  // Button height uses natural DPI-aware calculation
  btnHeight := Font.GetHeight + 8;  // Same as TfpgButton's default

  // Create buttons with tags for platform-specific ordering
  btnHelp := TfpgButton.Create(Self);
  btnHelp.Name := 'btnHelp';
  btnHelp.Text := 'Help';
  btnHelp.ImageName := 'stdimg.help';
  btnHelp.PreferredSize := fpgSize(btnWidth, btnHeight);
  btnHelp.OnClick := @btnHelpClicked;
  mig.AddLayoutComponent(btnHelp, TfpgMigCC.Create().SpanX().Split(3).Tag('help'));

  btnOK := TfpgButton.Create(Self);
  btnOK.Name := 'btnOK';
  btnOK.Text := 'OK';
  btnOK.PreferredSize := fpgSize(btnWidth, btnHeight);
  btnOK.ModalResult := mrOK;
  mig.AddLayoutComponent(btnOK, TfpgMigCC.Create().Tag('ok'));

  btnCancel := TfpgButton.Create(Self);
  btnCancel.Name := 'btnCancel';
  btnCancel.Text := 'Cancel';
  btnCancel.PreferredSize := fpgSize(btnWidth, btnHeight);
  btnCancel.ModalResult := mrCancel;
  mig.AddLayoutComponent(btnCancel, TfpgMigCC.Create().Tag('cancel'));
end;

end.
