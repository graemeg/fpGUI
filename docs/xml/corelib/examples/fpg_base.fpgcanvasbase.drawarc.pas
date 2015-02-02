{ This will draw a red arc, starting at the 6 o'clock position, and
  drawing 90 degrees of the arc, ending at the 3 o'clock position. }
procedure TMainForm.FormPaint;  // the forms OnPaint event handler
begin
  Canvas.Color := clRed;
  Canvas.DrawArc(5, 5, 100, 100, 270, 90);
end;
