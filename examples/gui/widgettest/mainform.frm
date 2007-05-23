object MainForm: TMainForm
  Text = 'Widget tests'
  BorderWidth = 8
  WindowOptions = [woWindow]
  object Box: TFBoxLayout
    CanExpandWidth = False
    Spacing = 8
    Orientation = Vertical
    object Label: TFLabel
      CanExpandWidth = True
      Text = 'Choose a test form:'
    end
    object CheckboxBtn: TFButton
      CanExpandWidth = True
      Text = 'Check boxes'
      OnClick = CheckboxBtnClick
    end
    object RadioButtonBtn: TFButton
      CanExpandWidth = True
      Text = 'Radio buttons'
      OnClick = RadioButtonBtnClick
    end
    object GroupBoxBtn: TFButton
      CanExpandWidth = True
      Text = 'Group boxes'
      OnClick = GroupBoxBtnClick
    end
    object EditBtn: TFButton
      CanExpandWidth = True
      Text = 'Edit fields'
      OnClick = EditBtnClick
    end
    object ScrollBarBtn: TFButton
      CanExpandWidth = True
      Text = 'Scroll bars'
      OnClick = ScrollBarBtnClick
    end
    object ScrollBoxBtn: TFButton
      CanExpandWidth = True
      Text = 'Scroll boxes'
      OnClick = ScrollBoxBtnClick
    end
    object ListBoxBtn: TFButton
      CanExpandWidth = True
      Text = 'List boxes'
      OnClick = ListBoxBtnClick
    end
    object ComboBoxBtn: TFButton
      CanExpandWidth = True
      Text = 'Combo boxes'
      OnClick = ComboBoxBtnClick
    end
    object GridBtn: TFButton
      CanExpandWidth = True
      Text = 'Grids'
      OnClick = GridBtnClick
    end
    object MenuBtn: TFButton
      CanExpandWidth = True
      Text = 'Menus'
      OnClick = MenuBtnClick
    end
    object PanelBtn: TFButton
      CanExpandWidth = True
      Text = 'Panel'
      OnClick = PanelBtnClick
    end
    object ProgressBarBtn: TFButton
      CanExpandWidth = True
      Text = 'Progress Bar'
      OnClick = ProgressBarBtnClick
    end
    object MemoBtn: TFButton
      CanExpandWidth = True
      Text = 'Memo widget'
      OnClick = MemoBtnClick
    end
    object ShowMessageBtn: TFButton
      CanExpandWidth = True
      Text = 'ShowMessage()'
      OnClick = ShowMessageBtnClick
    end
    object Separator: TSeparator
    end
    object ExitBtn: TFButton
      CanExpandWidth = True
      Text = 'Exit'
      OnClick = ExitBtnClick
    end
  end
end
