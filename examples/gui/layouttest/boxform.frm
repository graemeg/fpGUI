object BoxWindow: TBoxWindow
  Text = 'Box layout'
  BorderWidth = 8
  object Layout: TFBoxLayout
    Spacing = 8
    Orientation = Vertical
    object BoxLayout: TFBoxLayout
      Spacing = 4
      object Button1: TFButton
        Text = 'Button 1'
      end
      object Button2: TFButton
        Text = 'Button 2'
      end
      object Button3: TFButton
        Text = 'Button 3'
      end
    end
    object FlipButton: TFButton
      Text = 'Switch to vertical'
      OnClick = FlipOrientation
    end
  end
end
