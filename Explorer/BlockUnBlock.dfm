object FBlockUnblock: TFBlockUnblock
  Left = 438
  Top = 157
  BorderStyle = bsToolWindow
  Caption = 'Block/Unblock User'
  ClientHeight = 97
  ClientWidth = 412
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
    Top = 19
    Width = 56
    Height = 13
    Caption = 'User Name:'
  end
  object Button1: TButton
    Left = 200
    Top = 64
    Width = 81
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 320
    Top = 64
    Width = 81
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 136
    Top = 16
    Width = 265
    Height = 21
    TabOrder = 2
  end
  object block: TRadioButton
    Left = 136
    Top = 40
    Width = 129
    Height = 17
    Caption = 'Block User'
    Checked = True
    TabOrder = 3
    TabStop = True
  end
  object unblock: TRadioButton
    Left = 272
    Top = 40
    Width = 129
    Height = 17
    Caption = 'Unblock User'
    TabOrder = 4
  end
end
