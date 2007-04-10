object ComboBoxForm: TComboBoxForm
  BorderWidth = 8
  Text = 'Combo box test'
  OnCreate = FormCreate
  object VertBox: TFBoxLayout
    Orientation = Vertical
    object GrayCheckBox: TFCheckBox
      Text = 'Gray combo boxes'
      OnClick = GrayCheckBoxClick
    end
    object BetaLabel: TFLabel
      Text = '(the drop-down lists are work in progress)'
    end
    object ComboBox1: TFComboBox
    end
    object ComboBox2: TFComboBox
    end
  end
end
