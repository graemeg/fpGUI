{ **************************************
	* oeu oe uoe uoeu	*
	* oeu oe uoe uoeu	*
	* oeu oe uoe uoeu	*
	************************************** }
procedure TMainForm.miOpenClick(Sender: TObject);
var
	dlg: TfpgFileDialog;
begin
	dlg := TfpgFileDialog.Create(nil);
	try
		if dlg.RunOpenFile then
		begin
			memEditor.Lines.LoadFromFile(dlg.FileName);
		end;
	finally
		dlg.Free;
	end;
end;
