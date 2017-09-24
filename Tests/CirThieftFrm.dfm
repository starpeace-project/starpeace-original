object Form1: TForm1
  Left = 150
  Top = 214
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 189
  ClientWidth = 574
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
    Top = 16
    Width = 22
    Height = 13
    Caption = 'Path'
  end
  object ePath: TEdit
    Left = 16
    Top = 32
    Width = 433
    Height = 21
    TabOrder = 0
  end
  object eStartMark: TEdit
    Left = 16
    Top = 83
    Width = 433
    Height = 21
    TabOrder = 1
  end
  object eEndMark: TEdit
    Left = 16
    Top = 135
    Width = 433
    Height = 21
    TabOrder = 2
  end
  object btnGO: TButton
    Left = 480
    Top = 32
    Width = 75
    Height = 25
    Caption = 'GO!'
    TabOrder = 3
    OnClick = btnGOClick
  end
end
