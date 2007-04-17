object EditForm: TEditForm
  BorderWidth = 8
  Text = 'Edit field test'
  object Grid: TFGridLayout
    ColCount = 3
    RowCount = 5
    GridPositions = <
      item
	      Widget = Label1
      end
      item
	      x = 1
	      Widget = Edit1
      end
      item
	      x = 2
	      Widget = GrayCheckBox1
      end
      item
        y = 1
	      width = 3
	      Widget = Separator
      end
      item
        y = 2
	      Widget = Label2
      end
      item
	      x = 1
        y = 2
	      Widget = Edit2
      end
      item
	      x = 2
        y = 2
	      Widget = GrayCheckBox2
      end
      item
        x = 1
	      y = 3
	      Widget = PasswordDisplay
      end
      item
	      Widget = cbBorderStyle
        x = 0
	      y = 4
	      Width = 3
	      Height = 1
      end>
    object Label1: TFLabel
      Text = 'Normal edit field:'
      CanExpandWidth = False
    end
    object Edit1: TFEdit
      Text = 'Edit1'
    end
    object GrayCheckBox1: TFCheckBox
      Text = 'Disabled'
      OnClick = GrayCheckBox1Click
    end
    object Separator: TSeparator
    end
    object Label2: TFLabel
      Text = 'Password edit field:'
      CanExpandWidth = False
    end
    object Edit2: TFEdit
      PasswordChar = '*'
      Text = 'Edit2'
      OnChange = Edit2Change
    end
    object GrayCheckBox2: TFCheckBox
      Text = 'Disabled'
      OnClick = GrayCheckBox2Click
    end
    object PasswordDisplay: TFLabel
      Text = '(Password field)'
      CanExpandWidth = False
    end
    object cbBorderStyle: TFButton
      Text = 'Alternate Border Style'
      OnClick = cbBorderStyleClick
    end
  end
end