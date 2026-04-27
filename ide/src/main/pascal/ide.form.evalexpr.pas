{
    fpGUI IDE - Maximus

    Copyright (C) 2012 - 2026 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Evaluate Expression dialog (Ctrl+Alt+F8).

      Shows while the debugger is paused. The user types an expression,
      clicks Evaluate, and the result is displayed. Evaluation is routed
      through the worker thread (EvaluateExpressionLive) so all ptrace
      calls remain on the ptrace-owner thread.
}

unit ide.form.evalexpr;

{$mode objfpc}{$H+}
{$I ide.debug.config.inc}

interface

uses
  Classes, SysUtils,
  fpg_base, fpg_main, fpg_form,
  fpg_label, fpg_edit, fpg_button, fpg_memo,
  fpg_miglayout, fpg_mig_lc, fpg_mig_cc,
  {$IFDEF HAS_OPDF_DEBUG}
  ide.debug.adapter, pdr_ports
  {$ELSE}
  ide.debug.adapter.stub
  {$ENDIF}
  ;

type

  TEvalExprForm = class(TfpgForm)
  private
    FAdapter:    TIDEDebugAdapter;
    lblExpr:     TfpgLabel;
    edtExpr:     TfpgEdit;
    lblResult:   TfpgLabel;
    memoResult:  TfpgMemo;
    btnEvaluate: TfpgButton;
    btnClose:    TfpgButton;
    procedure btnEvaluateClicked(Sender: TObject);
    procedure btnCloseClicked(Sender: TObject);
    procedure edtExprKeyPressed(Sender: TObject; var KeyCode: Word;
      var ShiftState: TShiftState; var Consumed: Boolean);
    procedure OnEvalDone(Sender: TObject);
  public
    procedure AfterCreate; override;
    property Adapter: TIDEDebugAdapter read FAdapter write FAdapter;
  end;

procedure ShowEvalExprDialog(AAdapter: TIDEDebugAdapter);

implementation

procedure ShowEvalExprDialog(AAdapter: TIDEDebugAdapter);
var
  frm: TEvalExprForm;
begin
  frm := TEvalExprForm.Create(nil);
  try
    frm.Adapter := AAdapter;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TEvalExprForm.AfterCreate;
var
  mig: TfpgMigLayoutManager;
begin
  Name           := 'EvalExprForm';
  WindowTitle    := 'Evaluate Expression';
  SetPosition(0, 0, 520, 300);
  WindowPosition := wpMainFormCenter;
  Sizeable       := True;

  mig := TfpgMigLayoutManager.Create;
  mig.LC.WrapAfter(2);
  mig.LC.Fill;
  mig.LC.InsetsAll('8');
  LayoutManager := mig;

  { Expression label — spans both columns }
  lblExpr := TfpgLabel.Create(Self);
  lblExpr.Text := 'Expression:';
  lblExpr.Width  := 100;
  lblExpr.Height := 20;
  mig.AddLayoutComponent(lblExpr, TfpgMigCC.Create().SpanX(2));

  { Expression input — spans both columns, grows horizontally }
  edtExpr := TfpgEdit.Create(Self);
  edtExpr.Width      := 400;
  edtExpr.Height     := 24;
  edtExpr.OnKeyPress := @edtExprKeyPressed;
  mig.AddLayoutComponent(edtExpr, TfpgMigCC.Create().SpanX(2).GrowX());

  { Result label — spans both columns }
  lblResult := TfpgLabel.Create(Self);
  lblResult.Text   := 'Result:';
  lblResult.Width  := 100;
  lblResult.Height := 20;
  mig.AddLayoutComponent(lblResult, TfpgMigCC.Create().SpanX(2));

  { Result memo — spans both columns, grows in both directions }
  memoResult        := TfpgMemo.Create(Self);
  memoResult.Width  := 400;
  memoResult.Height := 120;
  mig.AddLayoutComponent(memoResult, TfpgMigCC.Create().SpanX(2).GrowX().GrowY());

  { Button row — split into two cells within the span }
  btnEvaluate         := TfpgButton.Create(Self);
  btnEvaluate.Text    := 'Evaluate';
  btnEvaluate.Width   := 90;
  btnEvaluate.Height  := 28;
  btnEvaluate.OnClick := @btnEvaluateClicked;
  mig.AddLayoutComponent(btnEvaluate,
      TfpgMigCC.Create().SpanX(2).Split(2).AlignX('right').Tag('ok'));

  btnClose         := TfpgButton.Create(Self);
  btnClose.Text    := 'Close';
  btnClose.Width   := 90;
  btnClose.Height  := 28;
  btnClose.OnClick := @btnCloseClicked;
  mig.AddLayoutComponent(btnClose, TfpgMigCC.Create().Tag('cancel'));
end;

procedure TEvalExprForm.btnEvaluateClicked(Sender: TObject);
var
  Expr: String;
begin
  Expr := Trim(edtExpr.Text);
  if Expr = '' then
    Exit;
  if (FAdapter = nil) or (FAdapter.State <> idsPaused) then
  begin
    memoResult.Lines.Text := '<debugger not paused>';
    Exit;
  end;
  btnEvaluate.Enabled   := False;
  memoResult.Lines.Text := 'Evaluating...';
  FAdapter.EvaluateExpressionLive(Expr, @OnEvalDone);
end;

procedure TEvalExprForm.btnCloseClicked(Sender: TObject);
begin
  Close;
end;

procedure TEvalExprForm.edtExprKeyPressed(Sender: TObject; var KeyCode: Word;
  var ShiftState: TShiftState; var Consumed: Boolean);
begin
  if KeyCode = keyReturn then
  begin
    Consumed := True;
    btnEvaluateClicked(Sender);
  end
  else if KeyCode = keyEscape then
  begin
    Consumed := True;
    Close;
  end;
end;

procedure TEvalExprForm.OnEvalDone(Sender: TObject);
var
  R:   TVariableValue;
  Txt: String;
begin
  btnEvaluate.Enabled := True;
  if FAdapter = nil then
    Exit;
  R := FAdapter.LastEvalResult;
  if R.IsValid then
  begin
    Txt := R.Name + ' = ' + R.Value;
    if R.TypeName <> '' then
      Txt := Txt + ' : ' + R.TypeName;
  end
  else
    Txt := R.Name + ' = <not in scope>';
  memoResult.Lines.Text := Txt;
  edtExpr.SetFocus;
end;

end.
