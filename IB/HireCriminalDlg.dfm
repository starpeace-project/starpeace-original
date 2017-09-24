object HireCriminal: THireCriminal
  Left = 456
  Top = 253
  Width = 290
  Height = 434
  Caption = 'Hire Criminal'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 74
    Height = 13
    Caption = 'Expert Criminals'
  end
  object Label2: TLabel
    Left = 144
    Top = 16
    Width = 78
    Height = 13
    Caption = 'Rookie Criminals'
  end
  object Label3: TLabel
    Left = 112
    Top = 248
    Width = 56
    Height = 13
    Caption = 'Criminal info'
  end
  object lbExpert: TListBox
    Left = 8
    Top = 32
    Width = 121
    Height = 209
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbExpertClick
  end
  object lbRookies: TListBox
    Left = 144
    Top = 32
    Width = 129
    Height = 209
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbRookiesClick
  end
  object lbProperties: TListBox
    Left = 8
    Top = 264
    Width = 265
    Height = 105
    ItemHeight = 13
    TabOrder = 2
  end
  object Button1: TButton
    Left = 198
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 118
    Top = 376
    Width = 75
    Height = 25
    Caption = 'Hire'
    TabOrder = 4
    OnClick = Button2Click
  end
end
