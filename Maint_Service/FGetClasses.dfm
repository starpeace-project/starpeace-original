object Form1: TForm1
  Left = 264
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Get Windows Classes'
  ClientHeight = 416
  ClientWidth = 688
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
    Left = 248
    Top = 360
    Width = 201
    Height = 49
    Caption = '&Do it'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 688
    Height = 345
    ItemHeight = 13
    TabOrder = 1
  end
end
