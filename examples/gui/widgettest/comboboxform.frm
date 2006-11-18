object ComboBoxForm: TComboBoxForm
  BorderWidth = 8
  Text = 'Combo box test'
  OnCreate = FormCreate
  object VertBox: TBoxLayout
    Orientation = Vertical
    object GrayCheckBox: TCheckBox
      Text = 'Gray combo boxes'
      OnClick = GrayCheckBoxClick
    end
    object BetaLabel: TLabel
      Text = '(the drop-down lists are work in progress)'
    end
    object ComboBox1: TComboBox
    end
    object ComboBox2: TComboBox
    end
  end
end
