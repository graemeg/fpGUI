object ScrollBarForm: TScrollBarForm
  BorderWidth = 8
  Text = 'Scroll bar test'
  object VertBox: TBoxLayout
    Orientation = Vertical
    object GrayCheckBox: TCheckBox
      Text = 'Gray everything'
      OnClick = GrayCheckBoxClick
    end
    object HorzBox: TBoxLayout
      object HorzGrid: TGridLayout
        ColCount = 3
        RowCount = 6
        ColSpacing = 8
        GridPositions = <
          item
	    x = 2
	    Widget = Col3Label
	  end
	  item
	    y = 1
	    Widget = Label1
	  end
	  item
	    x = 1
	    y = 1
	    Widget = ScrollBar1
	  end
	  item
	    x = 2
	    y = 1
	    Widget = PosLabel1
	  end
	  item
	    y = 2
	    Widget = Label2
	  end
	  item
	    x = 1
	    y = 2
	    Widget = ScrollBar2
	  end
	  item
	    x = 2
	    y = 2
	    Widget = PosLabel2
	  end
	  item
	    y = 3
	    Widget = Label3
	  end
	  item
	    x = 1
	    y = 3
	    Widget = ScrollBar3
	  end
	  item
	    x = 2
	    y = 3
	    Widget = PosLabel3
	  end
	  item
	    y = 4
	    Widget = Label4
	  end
	  item
	    x = 1
	    y = 4
	    Widget = ScrollBar4
	  end
	  item
	    x = 2
	    y = 4
	    Widget = PosLabel4
	  end
	  item
	    y = 5
	    Widget = Label5
	  end
	  item
	    x = 1
	    y = 5
	    Widget = ScrollBar5
	  end
	  item
	    x = 2
	    y = 5
	    Widget = PosLabel5
	  end>
        object Col3Label: TLabel
	  Alignment = taCenter
          Text = 'Position'
        end
        object Label1: TLabel
	  Alignment = taRightJustify
          Text = '0..1, PageSize=0:'
        end
        object ScrollBar1: TScrollBar
          Min = 0
          Max = 1
	  OnChange = ScrollBar1Change
        end
        object PosLabel1: TLabel
	  Alignment = taCenter
          Text = '---'
        end
        object Label2: TLabel
	  Alignment = taRightJustify
          Text = '0..1, PageSize=1:'
        end
        object ScrollBar2: TScrollBar
          Max = 1
          PageSize = 1
	  OnChange = ScrollBar2Change
        end
        object PosLabel2: TLabel
	  Alignment = taCenter
          Text = '---'
        end
        object Label3: TLabel
	  Alignment = taRightJustify
          Text = '-2..3, PageSize=0:'
        end
        object ScrollBar3: TScrollBar
          Min = -2
          Max = 3
	  OnChange = ScrollBar3Change
        end
        object PosLabel3: TLabel
	  Alignment = taCenter
          Text = '---'
        end
        object Label4: TLabel
	  Alignment = taRightJustify
          Text = '-5..9, PageSize=4:'
        end
        object ScrollBar4: TScrollBar
          Min = -5
          Max = 9
          PageSize = 4
	  OnChange = ScrollBar4Change
        end
        object PosLabel4: TLabel
	  Alignment = taCenter
          Text = '---'
        end
        object Label5: TLabel
	  Alignment = taRightJustify
          Text = '-100..200, PageSize=7:'
        end
        object ScrollBar5: TScrollBar
          Min = -100
          Max = 200
          PageSize = 7
	  OnChange = ScrollBar5Change
        end
        object PosLabel5: TLabel
	  Alignment = taCenter
          Text = '---'
	      end
      end
      object VertBar: TSeparator
        Orientation = Vertical
      end
      object VertLabel: TLabel
        Text = 'Vertical:'
      end
      object VertScrollBar: TScrollBar
        Orientation = Vertical
        Min = -2
	Max = 3
      end
    end
  end
end
