object BoxWindow: TBoxWindow
  Text = 'Box layout'
  BorderWidth = 8
  object Layout: TBoxLayout
    Spacing = 8
    Orientation = Vertical
    object BoxLayout: TBoxLayout
      Spacing = 4
      object Button1: TButton
        Text = 'Button 1'
      end
      object Button2: TButton
        Text = 'Button 2'
      end
      object Button3: TButton
        Text = 'Button 3'
      end
    end
    object FlipButton: TButton
      Text = 'Switch to vertical'
      OnClick = FlipOrientation
    end
  end
end
