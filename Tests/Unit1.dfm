object AuthtestForm: TAuthtestForm
  Left = 323
  Top = 346
  BorderStyle = bsDialog
  Caption = 'Authtest'
  ClientHeight = 128
  ClientWidth = 258
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
    Left = 16
    Top = 8
    Width = 56
    Height = 13
    Caption = 'User Name:'
  end
  object Label2: TLabel
    Left = 16
    Top = 51
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label3: TLabel
    Left = 16
    Top = 104
    Width = 33
    Height = 13
    Caption = 'Result:'
  end
  object Label4: TLabel
    Left = 55
    Top = 103
    Width = 3
    Height = 16
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 16
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 16
    Top = 67
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 168
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Login'
    TabOrder = 2
  end
end
