object MainForm: TMainForm
  Text = 'DBF test'
  BorderWidth = 8
  OnCreate = FormCreate
  object Box: TFBoxLayout
    Spacing = 8
    Orientation = Vertical
    object ListBox: TFListBox
    end
    object CurDataseTFLabel: TFLabel
      Text = 'Current dataset:'
    end
    object CurNameText: TDBText
      Text = '<name>'
    end
    object CurEMailText: TDBText
      Text = '<e-mail>'
    end
    object CurEmailEdit: TDBEdit
    end
    object Navi: TFBoxLayout
      object FirstDataset: TFButton
        Text = 'First'
	OnClick = FirstDatasetClick
      end
      object PrevDataset: TFButton
        Text = 'Previous'
	OnClick = PrevDatasetClick
      end
      object NextDataset: TFButton
        Text = 'Next'
	OnClick = NextDatasetClick
      end
      object LastDataset: TFButton
        Text = 'Last'
	OnClick = LastDatasetClick
      end
    end
  end
end
