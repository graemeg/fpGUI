object MainForm: TMainForm
  Text = 'Interbase test'
  BorderWidth = 8
  OnCreate = FormCreate
  object Database: TIBDatabase
    DatabaseName = 'test.gdb'
    UserName = 'sysdba'
    Password = 'masterkey'
  end
  object Transaction: TIBTransaction
  end
  object DataSource: TDataSource
  end
  object Box: TFBoxLayout
    Spacing = 8
    Orientation = Vertical
    object ConnectionBox: TFBoxLayout
      object ConnectionLabel: TFLabel
        Text = 'Connected to database? '
      end
      object ConnectionStateLabel: TFLabel
        Text = 'unknown'
      end
    end
    object ListBox: TListBox
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
