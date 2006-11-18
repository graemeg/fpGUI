object GroupBoxForm: TGroupBoxForm
  BorderWidth = 8
  Text = 'Group box test'
  object HorzBox: TBoxLayout
    VertAlign = vertTop
    object GroupBox1: TGroupBox
      Text = 'Group box #1'
      object VertBox1: TBoxLayout
        Orientation = Vertical
        object GrayCheckBox: TCheckBox
          Text = 'Gray other group box'
	  OnClick = GrayCheckBoxClick
        end
        object Button: TButton
	  Enabled = False
          Text = 'Reset radio buttons'
	  OnClick = ButtonClick
	end
      end
    end
    object GroupBox2: TGroupBox
      Text = 'Group box #2'
      object VertBox2: TBoxLayout
        Orientation = Vertical
	object Radio1: TRadioButton
	  Checked = True
	  Text = 'Option 1'
	  OnClick = RadioButtonClick
	end
	object Radio2: TRadioButton
	  Text = 'Option 2'
	  OnClick = RadioButtonClick
	end
	object Radio3: TRadioButton
	  Text = 'Option 3'
	  OnClick = RadioButtonClick
	end
	object Radio4: TRadioButton
	  Text = 'Option 4'
	  OnClick = RadioButtonClick
	end
	object Radio5: TRadioButton
	  Text = 'Option 5'
	  OnClick = RadioButtonClick
	end
      end
    end
  end
end
