{ The following example shows how we can set up a TGroupBox with a layout }
  grpOne := TGroupBox.Create('Group Box 1', self);
  grpOne.CanExpandWidth := True;

  VBox1 := TFBoxLayout.Create(self);
  VBox1.Orientation := Vertical;
  grpOne.InsertChild(VBox1);
  
  Radio1 := TRadioButton.Create('Radio button 1', self);
  Radio2 := TRadioButton.Create('Radio button 2', self);
  Radio3 := TRadioButton.Create('Radio button 3', self);

  Radio1.Checked := True;
  Radio1.CanExpandWidth := True;
  Radio2.CanExpandWidth := True;
  Radio3.CanExpandWidth := True;

  VBox1.InsertChild(Radio1);
  VBox1.InsertChild(Radio2);
  VBox1.InsertChild(Radio3);
