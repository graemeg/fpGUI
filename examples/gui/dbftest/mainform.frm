object MainForm: TMainForm
  Text = 'DBF test'
  BorderWidth = 8
  OnCreate = FormCreate
  object Box: TBoxLayout
    Spacing = 8
    Orientation = Vertical
    object ListBox: TListBox
    end
    object CurDatasetLabel: TLabel
      Text = 'Current dataset:'
    end
    object CurNameText: TDBText
      Text = '<name>'
    end
    object CurEMailText: TDBText
      Text = '<e-mail>'
    end
    object Navi: TBoxLayout
      object FirstDataset: TButton
        Text = 'First'
	OnClick = FirstDatasetClick
      end
      object PrevDataset: TButton
        Text = 'Previous'
	OnClick = PrevDatasetClick
      end
      object NextDataset: TButton
        Text = 'Next'
	OnClick = NextDatasetClick
      end
      object LastDataset: TButton
        Text = 'Last'
	OnClick = LastDatasetClick
      end
    end
  end
end
