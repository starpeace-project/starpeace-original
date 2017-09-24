object MainTestForm: TMainTestForm
  Left = 316
  Top = 245
  Width = 340
  Height = 139
  Caption = 'MainTestForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 40
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Create Client'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Create GM'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 40
    Width = 153
    Height = 21
    TabOrder = 2
    Text = 'aSDad'
  end
end
