object ScrollBoxForm: TScrollBoxForm
  BorderWidth = 8
  Text = 'Scroll box test'
  WindowOptions = [woWindow]
  object VertLayout: TBoxLayout
    Orientation = Vertical
    object Label1: TLabel
      CanExpandWidth = True
      Text = 'ScrollBox should be transparent in this demo.'
    end
    object ScrollBox: TScrollBox
    end
  end
end
