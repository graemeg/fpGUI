object ComboBoxForm: TComboBoxForm
  BorderWidth = 8
  Text = 'Combo box test'
  OnCreate = FormCreate
  object VertBox: TFBoxLayout
    Orientation = Vertical
    object GrayCheckBox: TCheckBox
      Text = 'Gray combo boxes'
      OnClick = GrayCheckBoxClick
    end
    object BetaLabel: TFLabel
      Text = '(the drop-down lists are work in progress)'
    end
    object ComboBox1: TComboBox
    end
    object ComboBox2: TComboBox
    end
  end
end
