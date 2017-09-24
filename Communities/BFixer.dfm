object Form1: TForm1
  Left = 458
  Top = 524
  Width = 353
  Height = 334
  Caption = 'Forums Fixer'
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
    Left = 8
    Top = 18
    Width = 82
    Height = 13
    Caption = 'Forums root path:'
  end
  object Label2: TLabel
    Left = 8
    Top = 68
    Width = 37
    Height = 13
    Caption = 'Forums:'
  end
  object Path: TEdit
    Left = 9
    Top = 38
    Width = 246
    Height = 21
    TabOrder = 0
  end
  object Forums: TListBox
    Left = 8
    Top = 88
    Width = 329
    Height = 185
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
  object Search: TButton
    Left = 262
    Top = 38
    Width = 75
    Height = 22
    Caption = 'Search'
    TabOrder = 2
    OnClick = SearchClick
  end
  object Fix: TButton
    Left = 262
    Top = 278
    Width = 75
    Height = 22
    Caption = 'Fix'
    Enabled = False
    TabOrder = 3
    OnClick = FixClick
  end
end
