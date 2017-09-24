object Form1: TForm1
  Left = 263
  Top = 289
  BorderStyle = bsDialog
  Caption = 'Dear Jerome'
  ClientHeight = 53
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 112
    Top = 16
    Width = 201
    Height = 25
    Caption = 'Donde están las tablas?'
    TabOrder = 0
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'tln'
    Filter = 'TLN|*.tln|All|*.*'
    FilterIndex = 0
    Left = 376
    Top = 16
  end
end
