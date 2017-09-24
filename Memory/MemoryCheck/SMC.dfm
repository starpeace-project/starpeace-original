object Form6: TForm6
  Left = 192
  Top = 131
  Width = 870
  Height = 640
  Caption = 'Form6'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 0
    Top = 365
    Width = 862
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Button1: TButton
    Left = 104
    Top = 72
    Width = 75
    Height = 25
    Caption = 'GetMem'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 112
    Width = 75
    Height = 25
    Caption = 'FreeMem'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 408
    Top = 72
    Width = 265
    Height = 25
    Caption = 'Reallocate Array More 100 elements'
    TabOrder = 2
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 408
    Top = 112
    Width = 265
    Height = 25
    Caption = 'Free Array'
    TabOrder = 3
    OnClick = Button5Click
  end
  object Button3: TButton
    Left = 104
    Top = 176
    Width = 241
    Height = 25
    Caption = 'p=Nil Size=0'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button6: TButton
    Left = 104
    Top = 216
    Width = 241
    Height = 25
    Caption = 'p=NIL  Size=100'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 104
    Top = 256
    Width = 241
    Height = 25
    Caption = 'p=kk  Size=0'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 104
    Top = 296
    Width = 241
    Height = 25
    Caption = 'p=kk Size = 4000'
    TabOrder = 7
    OnClick = Button8Click
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 368
    Width = 862
    Height = 239
    Align = alBottom
    TabOrder = 8
  end
  object Button9: TButton
    Left = 456
    Top = 176
    Width = 217
    Height = 25
    Caption = 'Button9'
    TabOrder = 9
    OnClick = Button9Click
  end
end
