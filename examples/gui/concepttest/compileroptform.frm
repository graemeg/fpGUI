object CompilerOptForm: TCompilerOptForm
  Text = 'Compiler Options - Code'
  BorderWidth = 5
  object Box1: TBoxLayout
    Spacing = 8
    Orientation = Vertical
    object Box2: TBoxLayout
      Orientation = Vertical
      Spacing = 4
      object Box4: TBoxLayout
        object grpBox1: TGroupBox
          Text = 'Unit Style:'
          object grpBox1VBox1: TBoxLayout
            Orientation = Vertical
          	object cbSmartLink: TCheckbox
          	  Checked = True
          	  Text = 'Smart Linkable (-CX)'
          	end
          end
        end
        object grpBox2: TGroupBox
          Text = 'Checks:'
          object grpBox2VBox1: TBoxLayout
            Orientation = Vertical
          	object rbIO: TRadioButton
          	  Checked = True
          	  Text = 'I/O (-Ci)'
          	end
          	object rbOverflow: TRadioButton
          	  Checked = True
          	  Text = 'Overflow (-Co)'
          	end
          	object rbRange: TRadioButton
          	  Checked = True
          	  Text = 'Range (-Cr)'
          	end
          	object rbStack: TRadioButton
          	  Checked = True
          	  Text = 'Stack (-Ct)'
          	end
          end
        end
        object grpBox3: TGroupBox
          Text = 'Heap Size (-Ch):'
          object grpBox3VBox1: TBoxLayout
            Orientation = Vertical
          	object edHeapSize: TEdit
          	  Text = '0'
              CanExpandWidth = False
          	end
          end
        end
      end
      object Box5: TBoxLayout
        object grpBox4: TGroupBox
          Text = 'Generate:'
          object grpBox4VBox1: TBoxLayout
            Orientation = Vertical
          	object rbNormal: TRadioButton
          	  Text = 'Normal Code (none)'
          	end
          	object rbFaster: TRadioButton
          	  Checked = True
          	  Text = 'Faster Code (-OG)'
          	end
          	object rbSmaller: TRadioButton
          	  Text = 'Smaller Code (-Og)'
          	end
          end
        end
        object grpBox5: TGroupBox
          Text = 'Target Platform:'
          CanExpandHeight = True
          object grpBox5VBox1: TBoxLayout
            Orientation = Vertical
            VertAlign = vertTop
          	object lblTarget1: TLabel
          	  Text = 'Target OS (-T)'
          	end
          	object lblTarget2: TLabel
          	  Text = 'Target CPU (-P)'
          	end
          	object lblTarget3: TLabel
          	  Text = 'Target i386'
          	end
          end
        end
      end
      object Box6: TBoxLayout
        object grpBox6: TGroupBox
          Text = 'Optimizations:'
          object grpBox6VBox1: TBoxLayout
            Orientation = Vertical
            CanExpandWidth = True
          	object rbLevel0: TRadioButton
          	  Text = 'Level 0 (no extra Optimizations) (none)'
          	end
          	object rbLevel1: TRadioButton
          	  Checked = True
          	  Text = 'Level 1 (Quick Optimizations) (-O1)'
          	end
          	object rbLevel2: TRadioButton
          	  Text = 'Level 2 (Level 1 + Slower Optimizations) (-O2)'
          	end
          	object rbLevel3: TRadioButton
          	  Text = 'Level 3 (Level 2 + Uncertain) (-O3)'
          	end
          	object rbKeepVarReg: TCheckbox
          	  Text = 'Keep certain variables in registers (-Or)'
          	end
          	object rbUncOpt: TCheckbox
          	  Text = 'Uncertain Optimizations (-Ou)'
          	end
          end
        end
      end
    end
    object Box3: TBoxLayout
      object btnOK: TButton
        Text = 'OK'
        OnClick = btnCloseClick
      end
      object btnCancel: TButton
        Text = 'Cancel'
      end
      object btnShowOpt: TButton
        Text = 'Show Options'
      end
      object btnTest: TButton
        Text = 'Test'
      end
      object btnLoadSave: TButton
        Text = 'Load/Save'
      end
    end
  end
end
