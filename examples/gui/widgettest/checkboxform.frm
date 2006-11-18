object CheckBoxForm: TCheckBoxForm
  BorderWidth = 8
  Text = 'Check box test'
  object Box: TBoxLayout
    Orientation = Vertical
    object GrayCheckBox: TCheckBox
      Text = 'Gray other check boxes'
      OnClick = GrayCheckBoxClick
    end
    object CheckBox1: TCheckBox
      Text = 'First other check box'
    end
    object CheckBox2: TCheckBox
      Text = 'Second other check box'
    end
  end
end
