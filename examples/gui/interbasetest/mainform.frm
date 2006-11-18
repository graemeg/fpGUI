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
  object Box: TBoxLayout
    Spacing = 8
    Orientation = Vertical
    object ConnectionBox: TBoxLayout
      object ConnectionLabel: TLabel
        Text = 'Connected to database? '
      end
      object ConnectionStateLabel: TLabel
        Text = 'unknown'
      end
    end
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
