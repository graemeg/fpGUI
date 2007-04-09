object RadioButtonForm: TRadioButtonForm
  BorderWidth = 8
  Text = 'Radio button test'
  object Box: TFBoxLayout
    Orientation = Vertical
    object GrayCheckbox: TCheckbox
      Text = 'Gray radio buttons'
      OnClick = GrayCheckboxClick
    end
    object HorzBox: TFBoxLayout
      object ButtonBox1: TFBoxLayout
        Orientation = Vertical
        object Radio1a: TRadioButton
	        Checked = True
          Text = 'Button 1 a'
        end
        object Radio1b: TRadioButton
          Text = 'Button 1 b'
        end
      end
      object ButtonBox2: TFBoxLayout
        Orientation = Vertical
        object Radio2a: TRadioButton
          Text = 'Button 2 a'
        end
        object Radio2b: TRadioButton
	        Checked = True
          Text = 'Button 2 b'
        end
      end
    end
  end
end
