{ This example shows how we can setup a static TComboBox }  
  cbStyle := TComboBox.Create(self);
  cbStyle.CanExpandWidth := True;
  cbStyle.Items.Add('Windows');           // insert items into the combobox
  cbStyle.Items.Add('WindowsXP');
  cbStyle.Items.Add('Motif');
  cbStyle.Items.Add('ClearLooks');
  cbStyle.Items.Add('OpenSoft');
  cbStyle.OnChange := @cbStyleChanged;    // setup an event handler
  cbStyle.ItemIndex := 0;                 // select the first item
