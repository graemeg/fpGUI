unit frm_parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_button, fpg_label, fpg_edit, fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc;

type

  TParserMigForm = class(TfpgForm)
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

    procedure AfterCreate; override;
    procedure HandleShow; override;
    procedure btnCloseClicked(Sender: TObject);
  end;

implementation

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
  Name := 'ParserMigForm';
  Left := 250;
  Top := 150;
  Width := 650;
  Height := 550;
  WindowTitle := 'MigLayout - String Parser Examples';

  // Debug: Show DPI information
  WriteLn(Format('DEBUG Screen DPI: X=%d Y=%d', [fpgApplication.Screen_dpi_x, fpgApplication.Screen_dpi_y]));

  // Create MigLayout with 2-column grid
  mig := TfpgMigLayoutManager.Create;
  mig.LC.SetWrapAfter(2);  // Wrap after 2 components
  mig.LC.Fill;             // Fill both horizontally and vertically
  mig.LC.InsetsAll('10lpx'); // Using string insets - 10 logical pixels all sides
  mig.LC.Debug(500);       // Enable debug visualization
  LayoutManager := mig;

  // Title - spanning 2 columns
  lblTitle := TfpgLabel.Create(Self);
  lblTitle.Name := 'lblTitle';
  lblTitle.Text := 'ConstraintParser String Syntax Examples';
  lblTitle.FontDesc := '#Label1:bold';
  lblTitle.Alignment := taCenter;
  mig.AddLayoutComponent(lblTitle,
    TfpgMigCC.Create()
      .SpanX(2)        // Span 2 columns
      .AlignX('center') // Using string alignment
      .GapBottom('15px')); // Using string gap

  // ==== Section 1: Unit Types ====
  lblUnits := TfpgLabel.Create(Self);
  lblUnits.Name := 'lblUnits';
  lblUnits.Text := '1. Unit Types (px, %, mm, lpx):';
  lblUnits.FontDesc := '#Label1:bold';
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
  mig.AddLayoutComponent(edtMM,
    TfpgMigCC.Create()
      .Width('50mm'));   // Physical millimeters

  // ==== Section 2: BoundSize (min:pref:max) ====
  lblBounds := TfpgLabel.Create(Self);
  lblBounds.Name := 'lblBounds';
  lblBounds.Text := '2. BoundSize (min:pref:max):';
  lblBounds.FontDesc := '#Label1:bold';
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
  // Width constraint: min=100px, preferred=200px, max=300px
  mig.AddLayoutComponent(edtBounds,
    TfpgMigCC.Create()
      .Width('100px:200px:300px')  // String BoundSize
      .GrowX());                    // Allow growth within bounds

  // ==== Section 3: Alignment Keywords ====
  lblAlign := TfpgLabel.Create(Self);
  lblAlign.Name := 'lblAlign';
  lblAlign.Text := '3. Alignment Keywords (left, center, right):';
  lblAlign.FontDesc := '#Label1:bold';
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
  lblOps.FontDesc := '#Label1:bold';
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
  edtOperation.Text := 'Width = max(150px, 30%)';
  // Width uses max() operation: larger of 150px or 30% of available space
  mig.AddLayoutComponent(edtOperation,
    TfpgMigCC.Create()
      .Width('max(150px,30%)')   // Operation in string constraint
      .GrowX());

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
  memoInfo.Lines.Add('Example: .Width("100px:200px:300px") sets min, preferred, and max widths.');
  memoInfo.Lines.Add('Example: .GapLeft("max(10px,5%)") uses the larger of 10px or 5%.');
  mig.AddLayoutComponent(memoInfo,
    TfpgMigCC.Create()
      .SpanX(2)
      .GrowX()
      .GrowY()
      .Height('120px:150px:')    // Min 120px, preferred 150px, no max (grows)
      .GapTop('15px'));

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
