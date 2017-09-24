object MainServerForm: TMainServerForm
  Left = 259
  Top = 225
  BorderStyle = bsDialog
  Caption = 'RDO SERVER'
  ClientHeight = 181
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 36
    Top = 73
    Width = 47
    Height = 13
    Caption = 'Allocated:'
  end
  object Label2: TLabel
    Left = 40
    Top = 103
    Width = 35
    Height = 13
    Caption = 'Blocks:'
  end
  object MemoryManagerClass1: TMemoryManagerClass
    Left = 92
    Top = 74
    Width = 66
    Height = 13
    Active = True
    MemoryShow = msAllocMemSize
    Interval = 1000
  end
  object MemoryManagerClass2: TMemoryManagerClass
    Left = 93
    Top = 105
    Width = 74
    Height = 13
    Active = True
    MemoryShow = msAllocMemCount
    Interval = 1000
  end
  object btnStart: TButton
    Left = 344
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
end
