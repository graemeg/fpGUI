object CheckBoxForm: TCheckBoxForm
  BorderWidth = 8
  Text = 'Check box test'
  object Box: TFBoxLayout
    Orientation = Vertical
    object GrayCheckBox: TFCheckBox
      Text = 'Gray other check boxes'
      OnClick = GrayCheckBoxClick
    end
    object CheckBox1: TFCheckBox
      Text = 'First other check box'
    end
    object CheckBox2: TFCheckBox
      Text = 'Second other check box'
    end
  end
end
