object GroupBoxForm: TGroupBoxForm
  BorderWidth = 8
  Text = 'Group box test'
  object HorzBox: TFBoxLayout
    VertAlign = vertTop
    object GroupBox1: TFGroupBox
      Text = 'Group box #1'
      object VertBox1: TFBoxLayout
        Orientation = Vertical
        object GrayCheckBox: TFCheckBox
          Text = 'Gray other group box'
          OnClick = GrayCheckBoxClick
        end
        object Button: TFButton
          Enabled = False
          Text = 'Reset radio buttons'
          OnClick = ButtonClick
        end
      end
    end
    object GroupBox2: TFGroupBox
      Text = 'Group box #2'
      object VertBox2: TFBoxLayout
        Orientation = Vertical
        object Radio1: TFRadioButton
          Checked = True
          Text = 'Option 1'
          OnClick = RadioButtonClick
        end
        object Radio2: TFRadioButton
          Text = 'Option 2'
          OnClick = RadioButtonClick
        end
        object Radio3: TFRadioButton
          Text = 'Option 3'
          OnClick = RadioButtonClick
        end
        object Radio4: TFRadioButton
          Text = 'Option 4'
          OnClick = RadioButtonClick
        end
        object Radio5: TFRadioButton
          Text = 'Option 5'
          OnClick = RadioButtonClick
        end
      end
    end
  end
end
