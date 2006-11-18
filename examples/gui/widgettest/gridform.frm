object GridForm: TGridForm
  BorderWidth = 8
  Text = 'Grid test'
  OnCreate = FormCreate
  object StringGrid: TStringGrid
    ColCount = 10
    RowCount = 15
  end
end
