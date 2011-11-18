unit frm_child;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_edit,
  fpg_checkbox, fpg_radiobutton, fpg_gauge, fpg_mdi, fpg_panel, fpg_trackbar;

type

	TChildForm = class(TfpgFrame)
	private
		{@VFD_HEAD_BEGIN: ChildForm}
		btnClose: TfpgButton;
		CheckBox1: TfpgCheckBox;
		CheckBox2: TfpgCheckBox;
		RadioButton1: TfpgRadioButton;
		RadioButton2: TfpgRadioButton;
		Edit1: TfpgEdit;
		Gauge1: TfpgGauge;
		TrackBar1: TfpgTrackBar;
		{@VFD_HEAD_END: ChildForm}
		FWindowTitle: TfpgString;
		procedure btnCloseClicked(Sender: TObject);
		procedure TrackBarChanged(Sender: TObject; APosition: integer);
		procedure SetWindowTitle(AValue: TfpgString);
	public
		procedure AfterCreate; override;
		property WindowTitle: TfpgString read FWindowTitle write SetWindowTitle;
	end;

{@VFD_NEWFORM_DECL}

var
  ChildForm: TChildForm;

implementation


{@VFD_NEWFORM_IMPL}

procedure TChildForm.TrackBarChanged(Sender: TObject; APosition: integer);
begin
	Gauge1.Progress := APosition;
end;

procedure TChildForm.SetWindowTitle(AValue: TfpgString);
begin
	if FWindowTitle = AValue then
		Exit;
	FWindowTitle := AValue;
	TfpgMDIChildForm(Owner.Owner).WindowTitle := FWindowTitle;
end;

procedure TChildForm.btnCloseClicked(Sender: TObject);
begin
	TfpgMDIChildForm(Owner).Close;
end;

procedure TChildForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ChildForm}
	Name := 'ChildForm';
	SetPosition(391, 210, 271, 150);
//	WindowTitle := 'ChildForm';
	Hint := '';

	btnClose := TfpgButton.Create(self);
	with btnClose do
	begin
		Name := 'btnClose';
		SetPosition(180, 116, 80, 24);
		Text := 'Close';
		FontDesc := '#Label1';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		OnClick := @btnCloseClicked;
	end;

	CheckBox1 := TfpgCheckBox.Create(self);
	with CheckBox1 do
	begin
		Name := 'CheckBox1';
		SetPosition(164, 16, 120, 20);
		FontDesc := '#Label1';
		Hint := '';
		TabOrder := 2;
		Text := 'CheckBox';
	end;

	CheckBox2 := TfpgCheckBox.Create(self);
	with CheckBox2 do
	begin
		Name := 'CheckBox2';
		SetPosition(164, 36, 120, 20);
		FontDesc := '#Label1';
		Hint := '';
		TabOrder := 3;
		Text := 'CheckBox';
	end;

	RadioButton1 := TfpgRadioButton.Create(self);
	with RadioButton1 do
	begin
		Name := 'RadioButton1';
		SetPosition(164, 60, 120, 20);
		FontDesc := '#Label1';
		GroupIndex := 0;
		Hint := '';
		TabOrder := 4;
		Text := 'RadioButton';
	end;

	RadioButton2 := TfpgRadioButton.Create(self);
	with RadioButton2 do
	begin
		Name := 'RadioButton2';
		SetPosition(164, 80, 120, 20);
		FontDesc := '#Label1';
		GroupIndex := 0;
		Hint := '';
		TabOrder := 5;
		Text := 'RadioButton';
	end;

	Edit1 := TfpgEdit.Create(self);
	with Edit1 do
	begin
		Name := 'Edit1';
		SetPosition(8, 8, 120, 24);
		ExtraHint := '';
		FontDesc := '#Edit1';
		Hint := '';
		TabOrder := 6;
		Text := '';
	end;

	Gauge1 := TfpgGauge.Create(self);
	with Gauge1 do
	begin
		Name := 'Gauge1';
		SetPosition(12, 44, 116, 25);
		Color := TfpgColor($C4C4C4);
		Hint := '';
		Progress := 65;
	end;

	TrackBar1 := TfpgTrackBar.Create(self);
	with TrackBar1 do
	begin
		Name := 'TrackBar1';
		SetPosition(12, 84, 116, 30);
		Hint := '';
		TabOrder := 8;
		Position := 65;
		OnChange  := @TrackBarChanged;
	end;

	{@VFD_BODY_END: ChildForm}
  {%endregion}
	Name := 'ChildForm' + IntToStr(Random(MaxInt));

end;


end.
