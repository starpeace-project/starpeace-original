object SubForm: TSubForm
  Left = 189
  Top = 102
  Width = 414
  Height = 331
  Caption = 'SubForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 0
    Top = 180
    Width = 406
    Height = 124
    Align = alBottom
    TabOrder = 0
    Tabs.Strings = (
      'Tab1'
      'Tab2'
      'Tab3'
      'Tab4'
      'Tab5'
      'Tab6'
      'Tab7')
    TabIndex = 0
  end
  object WebBrowser: TWebBrowser
    Left = 206
    Top = 0
    Width = 200
    Height = 180
    Align = alRight
    TabOrder = 1
    ControlData = {
      4C000000AC1400009B1200000100000005000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Button1: TButton
    Left = 48
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
end
