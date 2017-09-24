object MainClientForm: TMainClientForm
  Left = 218
  Top = 208
  BorderStyle = bsDialog
  Caption = 'RDO CLIENT'
  ClientHeight = 167
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object MemoryManagerClass1: TMemoryManagerClass
    Left = 88
    Top = 72
    Width = 66
    Height = 13
    Active = True
    MemoryShow = msAllocMemSize
    Interval = 1000
  end
  object MemoryManagerClass2: TMemoryManagerClass
    Left = 88
    Top = 104
    Width = 74
    Height = 13
    Active = True
    MemoryShow = msAllocMemCount
    Interval = 1000
  end
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
  object btnStart: TButton
    Left = 328
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object tTicks: TTimer
    Enabled = False
    OnTimer = tTicksTimer
    Left = 384
    Top = 136
  end
end
