object Form2: TForm2
  Left = 341
  Top = 207
  Width = 627
  Height = 516
  Caption = 'Memory Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 49
    Top = 138
    Width = 31
    Height = 16
    Caption = 'Time'
  end
  object MemoryManagerClass1: TMemoryManagerClass
    Left = 49
    Top = 79
    Width = 94
    Height = 16
    Active = True
    MemoryShow = msAllocMemCount
    Interval = 1000
  end
  object ComboBox1: TComboBox
    Left = 217
    Top = 69
    Width = 178
    Height = 24
    ItemHeight = 16
    TabOrder = 0
    Text = 'msMemCount'
    OnChange = ComboBox1Change
    Items.Strings = (
      'msMemCount'
      'msFreeMemCount'
      'msReallocMemCount'
      'msAllocMemCount'
      'msAllocMemSize'
      'msPendingCall')
  end
  object Edit1: TEdit
    Left = 49
    Top = 158
    Width = 129
    Height = 24
    TabOrder = 1
    Text = '0'
  end
  object Button1: TButton
    Left = 207
    Top = 158
    Width = 60
    Height = 30
    Caption = 'Do'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 305
    Top = 158
    Width = 93
    Height = 30
    Caption = 'Reset'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 192
    Width = 75
    Height = 25
    Caption = 'GetMem '
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 96
    Top = 192
    Width = 169
    Height = 25
    Caption = 'Test Dll Mem'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 368
    Top = 120
    Width = 177
    Height = 25
    Caption = 'Test Virtual Allocation'
    TabOrder = 6
    OnClick = Button5Click
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 260
    Width = 619
    Height = 223
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 7
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 231
    Width = 619
    Height = 29
    Align = alBottom
    ButtonHeight = 24
    ButtonWidth = 98
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 8
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Caption = 'GetSystemInfo'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 98
      Top = 2
      Caption = 'ProtegPage'
      ImageIndex = 1
      OnClick = ToolButton2Click
    end
    object ToolButton3: TToolButton
      Left = 196
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 204
      Top = 2
      Caption = 'Save Persisten'
      ImageIndex = 2
      OnClick = ToolButton4Click
    end
  end
end
