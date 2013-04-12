object ScrollBoxForm: TScrollBoxForm
  BorderWidth = 8
  Text = 'Scroll box test'
  WindowOptions = [woWindow]
  object VertLayout: TFBoxLayout
    Orientation = Vertical
    object Label1: TFLabel
      CanExpandWidth = True
      Text = 'ScrollBox should be transparent in this demo.'
    end
    object ScrollBox: TFScrollBox
    end
  end
end
