{ This will draw a red filled arc, starting at the 6 o'clock position, and
  drawing 45 degrees of the arc, ending at the half-past-four position. }
procedure TMainForm.FormPaint;  // the form's OnPaint event handler. 
begin
  Canvas.Color := clRed;
  Canvas.FillArc(5, 5, 100, 100, 270, 45);
end;

