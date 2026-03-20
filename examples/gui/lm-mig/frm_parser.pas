unit frm_parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_edit, fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TParserMigForm = class(TfpgForm)
  private
    FDebug: boolean;
  public
    lblTitle: TfpgLabel;

    // Section 1: Unit types demonstration
    lblUnits: TfpgLabel;
    lblPx: TfpgLabel;
    edtPx: TfpgEdit;
    lblPercent: TfpgLabel;
    edtPercent: TfpgEdit;
    lblMM: TfpgLabel;
    edtMM: TfpgEdit;

    // Section 2: BoundSize demonstration
    lblBounds: TfpgLabel;
    lblMinPrefMax: TfpgLabel;
    edtBounds: TfpgEdit;

    // Section 3: Alignment keywords
    lblAlign: TfpgLabel;
    btnLeft: TfpgButton;
    btnCenter: TfpgButton;
    btnRight: TfpgButton;

    // Section 4: Operations
    lblOps: TfpgLabel;
    lblOpExample: TfpgLabel;
    edtOperation: TfpgEdit;

    // Bottom
    memoInfo: TfpgMemo;
    btnClose: TfpgButton;

    constructor Create(AEnableDebug: Boolean); reintroduce;
    procedure AfterCreate; override;
    procedure HandleShow; override;
    procedure btnCloseClicked(Sender: TObject);
  end;

implementation

constructor TParserMigForm.Create(AEnableDebug: Boolean);
begin
  inherited Create(nil);
  FDebug := AEnableDebug;
end;

procedure TParserMigForm.btnCloseClicked(Sender: TObject);
begin
  Close;
end;

procedure TParserMigForm.HandleShow;
begin
  inherited HandleShow;
  // Debug: Show actual widget widths after layout
  WriteLn(Format('DEBUG Widget widths after layout: edtPx=%d edtMM=%d edtPercent=%d',
    [edtPx.Width, edtMM.Width, edtPercent.Width]));
end;

procedure TParserMigForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
begin
  inherited AfterCreate;

  // The following lines are a temporary workaround for systems where the
  // screen DPI is not correctly configured. This functionality is now handled
  // by the global fpgui.ini configuration file.
  // See TfpgMigPlatformDefaults.Initialize for more details.
  //
  // TfpgMigPlatformDefaults.SetHorizontalScaleFactor(158.75 / 96.0);
  // TfpgMigPlatformDefaults.SetVerticalScaleFactor(158.75 / 96.0);

  Name := 'ParserMigForm';
  Left := 250;
  Top := 150;
  Width := 650;
  Height := 700;
  WindowTitle := 'MigLayout - String Parser Examples';
  ShowHint := true;

  // Debug: Show DPI information
  WriteLn(Format('DEBUG Screen DPI: X=%d Y=%d', [fpgApplication.Screen_dpi_x, fpgApplication.Screen_dpi_y]));

  // Create MigLayout with 2-column grid
  mig := TfpgMigLayoutManager.Create;
  mig.LC.WrapAfter(2);  // Wrap after 2 components
  mig.LC.Fill;             // Fill both horizontally and vertically
  mig.LC.InsetsAll('10lpx'); // Using string insets - 10 logical pixels all sides
  if FDebug then
    mig.LC.Debug;       // Enable debug visualization
  LayoutManager := mig;

  // Title - spanning 2 columns
  lblTitle := TfpgLabel.Create(Self);
  lblTitle.Name := 'lblTitle';
  lblTitle.Text := 'ConstraintParser String Syntax Examples';
  lblTitle.FontDesc := '#Label2';
  lblTitle.Alignment := taCenter;
  lblTitle.AutoSize := True;
  mig.AddLayoutComponent(lblTitle,
    TfpgMigCC.Create()
      .SpanX(2)        // Span 2 columns
      .AlignX('center') // Using string alignment
      .GapBottom('15px')); // Using string gap

  // ==== Section 1: Unit Types ====
  lblUnits := TfpgLabel.Create(Self);
  lblUnits.Name := 'lblUnits';
  lblUnits.Text := '1. Unit Types (px, %, mm, lpx):';
  lblUnits.FontDesc := '#Label2';
  lblUnits.AutoSize := True;
  mig.AddLayoutComponent(lblUnits,
    TfpgMigCC.Create()
      .SpanX(2)
      .GapTop('10px'));

  // Pixels example
  lblPx := TfpgLabel.Create(Self);
  lblPx.Name := 'lblPx';
  lblPx.Text := 'Pixels (200px width):';
  lblPx.AutoSize := True;
  mig.AddLayoutComponent(lblPx, TfpgMigCC.Create());

  edtPx := TfpgEdit.Create(Self);
  edtPx.Name := 'edtPx';
  edtPx.Text := 'Fixed 200px wide';
  edtPx.Height := 30;
  // Using string constraint: width="200px" instead of PreferredSize
  mig.AddLayoutComponent(edtPx,
    TfpgMigCC.Create()
      .Width('200px'));  // String-based width constraint

  // Percentage example
  lblPercent := TfpgLabel.Create(Self);
  lblPercent.Name := 'lblPercent';
  lblPercent.Text := 'Percent (50% width):';
  lblPercent.AutoSize := True;
  mig.AddLayoutComponent(lblPercent, TfpgMigCC.Create());

  edtPercent := TfpgEdit.Create(Self);
  edtPercent.Name := 'edtPercent';
  edtPercent.Text := '50% of available space';
  edtPercent.Height := 30;
  mig.AddLayoutComponent(edtPercent,
    TfpgMigCC.Create()
      .Width('50%'));    // 50% of available width

  // Millimeters example
  lblMM := TfpgLabel.Create(Self);
  lblMM.Name := 'lblMM';
  lblMM.Text := 'Millimeters (50mm):';
  lblMM.AutoSize := True;
  mig.AddLayoutComponent(lblMM, TfpgMigCC.Create());

  edtMM := TfpgEdit.Create(Self);
  edtMM.Name := 'edtMM';
  edtMM.Text := 'Physical 50mm wide';
  edtMM.Height := 30;
  mig.AddLayoutComponent(edtMM,
    TfpgMigCC.Create()
      .Width('50mm'));   // Physical millimeters

  // ==== Section 2: BoundSize (min:pref:max) ====
  lblBounds := TfpgLabel.Create(Self);
  lblBounds.Name := 'lblBounds';
  lblBounds.Text := '2. BoundSize (min:pref:max):';
  lblBounds.FontDesc := '#Label2';
  lblBounds.AutoSize := True;
  mig.AddLayoutComponent(lblBounds,
    TfpgMigCC.Create()
      .SpanX(2)
      .GapTop('15px'));

  lblMinPrefMax := TfpgLabel.Create(Self);
  lblMinPrefMax.Name := 'lblMinPrefMax';
  lblMinPrefMax.Text := 'Min:Pref:Max (100:200:300):';
  lblMinPrefMax.AutoSize := True;
  mig.AddLayoutComponent(lblMinPrefMax, TfpgMigCC.Create());

  edtBounds := TfpgEdit.Create(Self);
  edtBounds.Name := 'edtBounds';
  edtBounds.Text := 'Resize to see min/max constraints';
  edtBounds.Height := 30;
  // Width constraint: min=100px, preferred=200px, max=300px
  mig.AddLayoutComponent(edtBounds,
    TfpgMigCC.Create()
      .Width('100px:200px:300px')  // String BoundSize
      .GrowX());                    // Allow growth within bounds

  // ==== Section 3: Alignment Keywords ====
  lblAlign := TfpgLabel.Create(Self);
  lblAlign.Name := 'lblAlign';
  lblAlign.Text := '3. Alignment Keywords (left, center, right):';
  lblAlign.FontDesc := '#Label2';
  lblAlign.AutoSize := True;
  mig.AddLayoutComponent(lblAlign,
    TfpgMigCC.Create()
      .SpanX(2)
      .GapTop('15px'));

  btnLeft := TfpgButton.Create(Self);
  btnLeft.Name := 'btnLeft';
  btnLeft.Text := 'Left';
  btnLeft.PreferredSize := fpgSize(80, 30);
  mig.AddLayoutComponent(btnLeft,
    TfpgMigCC.Create()
      .AlignX('left')    // String alignment keyword
      .SpanX(2));

  btnCenter := TfpgButton.Create(Self);
  btnCenter.Name := 'btnCenter';
  btnCenter.Text := 'Center';
  btnCenter.PreferredSize := fpgSize(80, 30);
  mig.AddLayoutComponent(btnCenter,
    TfpgMigCC.Create()
      .AlignX('center')  // String alignment keyword
      .SpanX(2));

  btnRight := TfpgButton.Create(Self);
  btnRight.Name := 'btnRight';
  btnRight.Text := 'Right';
  btnRight.PreferredSize := fpgSize(80, 30);
  mig.AddLayoutComponent(btnRight,
    TfpgMigCC.Create()
      .AlignX('right')   // String alignment keyword
      .SpanX(2));

  // ==== Section 4: Operations ====
  lblOps := TfpgLabel.Create(Self);
  lblOps.Name := 'lblOps';
  lblOps.Text := '4. Operations (min, max, +, -):';
  lblOps.FontDesc := '#Label2';
  lblOps.AutoSize := True;
  mig.AddLayoutComponent(lblOps,
    TfpgMigCC.Create()
      .SpanX(2)
      .GapTop('15px'));

  lblOpExample := TfpgLabel.Create(Self);
  lblOpExample.Name := 'lblOpExample';
  lblOpExample.Text := 'Max operation (max(150px,30%)):';
  lblOpExample.AutoSize := True;
  mig.AddLayoutComponent(lblOpExample, TfpgMigCC.Create());

  edtOperation := TfpgEdit.Create(Self);
  edtOperation.Name := 'edtOperation';
  edtOperation.Text := 'MaxWidth = max(150px, 30%) - resize to see!';
  edtOperation.Hint := edtOperation.Text;
  edtOperation.Height := 30;
  // MaxWidth uses max() operation: limits growth to larger of 150px or 30%
  // When narrow (< 500px): maxes out at 150px
  // When wide (> 500px): maxes out at 30% of available width
  mig.AddLayoutComponent(edtOperation,
    TfpgMigCC.Create()
      .MaxWidth('max(150px,30%)')   // Max operation limits maximum width
      .GrowX());                     // Grows up to the max limit

  // ==== Info memo ====
  memoInfo := TfpgMemo.Create(Self);
  memoInfo.Name := 'memoInfo';
  memoInfo.Lines.Add('String-Based Constraint Features:');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('• Units: px, %, mm, cm, in, pt, lpx, lpy');
  memoInfo.Lines.Add('• BoundSize: "min:pref:max" syntax');
  memoInfo.Lines.Add('• Alignment: left, right, center, top, bottom');
  memoInfo.Lines.Add('• Operations: min(), max(), mid(), +, -, *, /');
  memoInfo.Lines.Add('• Special: pref, null, push, inf');
  memoInfo.Lines.Add('');
  memoInfo.Lines.Add('Examples:');
  memoInfo.Lines.Add('  .Width("100px:200px:300px") - min, preferred, and max widths');
  memoInfo.Lines.Add('  .MaxWidth("max(150px,30%)") - limit to larger of 150px or 30%');
  memoInfo.Lines.Add('  .GapLeft("min(10px,5%)") - use smaller of 10px or 5%');
  mig.AddLayoutComponent(memoInfo,
    TfpgMigCC.Create()
      .SpanX(2)
      .GrowX()
      .GrowY()
      .Height('120px:150px:')    // Min 120px, preferred 150px, no max (grows)
      .GapTop('45px'));

  // ==== Close button ====
  btnClose := TfpgButton.Create(Self);
  btnClose.Name := 'btnClose';
  btnClose.Text := 'Close';
  btnClose.PreferredSize := fpgSize(80, 30);
  btnClose.OnClick := @btnCloseClicked;
  mig.AddLayoutComponent(btnClose,
    TfpgMigCC.Create()
      .SpanX(2)
      .AlignX('right')   // Right-align the close button
      .GapTop('10px'));
end;

end.
