object MainWindow: TMainWindow
  BorderWidth = 8
  object Box: TBoxLayout
    Spacing = 8
    Orientation = Vertical
    object Label: TLabel
      Text = 'Choose a test Window:'
    end
    object SimpleBtn: TButton
      Text = 'Simple layout'
      OnClick = SimpleBtnClicked
    end
    object FixedBtn: TButton
      Text = 'Fixed layout'
    end
    object BoxBtn: TButton
      Text = 'Box layout'
      OnClick = BoxBtnClicked
    end
    object GridBtn: TButton
      Text = 'Grid layout'
      OnClick = GridBtnClicked
    end
    object DockingBtn: TButton
      Text = 'Docking layout'
      OnClick = DockingBtnClicked
    end
    object ExitBtn: TButton
      Text = 'Exit'
      OnClick = ExitBtnClicked
    end
  end
end
