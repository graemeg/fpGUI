object MainForm: TMainForm
  Text = 'Widget tests'
  BorderWidth = 8
  WindowOptions = [woWindow]
  object Box: TBoxLayout
    CanExpandWidth = False
    Spacing = 8
    Orientation = Vertical
    object Label: TLabel
      CanExpandWidth = True
      Text = 'Choose a test form:'
    end
    object CheckboxBtn: TButton
      CanExpandWidth = True
      Text = 'Check boxes'
      OnClick = CheckboxBtnClick
    end
    object RadioButtonBtn: TButton
      CanExpandWidth = True
      Text = 'Radio buttons'
      OnClick = RadioButtonBtnClick
    end
    object GroupBoxBtn: TButton
      CanExpandWidth = True
      Text = 'Group boxes'
      OnClick = GroupBoxBtnClick
    end
    object EditBtn: TButton
      CanExpandWidth = True
      Text = 'Edit fields'
      OnClick = EditBtnClick
    end
    object ScrollBarBtn: TButton
      CanExpandWidth = True
      Text = 'Scroll bars'
      OnClick = ScrollBarBtnClick
    end
    object ScrollBoxBtn: TButton
      CanExpandWidth = True
      Text = 'Scroll boxes'
      OnClick = ScrollBoxBtnClick
    end
    object ListBoxBtn: TButton
      CanExpandWidth = True
      Text = 'List boxes'
      OnClick = ListBoxBtnClick
    end
    object ComboBoxBtn: TButton
      CanExpandWidth = True
      Text = 'Combo boxes'
      OnClick = ComboBoxBtnClick
    end
    object GridBtn: TButton
      CanExpandWidth = True
      Text = 'Grids'
      OnClick = GridBtnClick
    end
    object MenuBtn: TButton
      CanExpandWidth = True
      Text = 'Menus'
      OnClick = MenuBtnClick
    end
    object PanelBtn: TButton
      CanExpandWidth = True
      Text = 'Panel'
      OnClick = PanelBtnClick
    end
    object ProgressBarBtn: TButton
      CanExpandWidth = True
      Text = 'Progress Bar'
      OnClick = ProgressBarBtnClick
    end
    object StdDialogBtn: TButton
      CanExpandWidth = True
      Text = 'Std Dialog'
      OnClick = StdDialogBtnClick
      Enabled = False
    end
    object Separator: TSeparator
    end
    object ExitBtn: TButton
      CanExpandWidth = True
      Text = 'Exit'
      OnClick = ExitBtnClick
    end
  end
end
