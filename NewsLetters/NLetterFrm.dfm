object NLMainForm: TNLMainForm
  Left = 315
  Top = 279
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'NewsLetter Daemon'
  ClientHeight = 433
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 7
    Width = 76
    Height = 13
    Caption = 'NewsLetter File:'
  end
  object ePath: TEdit
    Left = 8
    Top = 24
    Width = 345
    Height = 21
    TabOrder = 0
    Text = 's:\temp\NewsLetter2.htm'
  end
  object Button1: TButton
    Left = 368
    Top = 24
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object pbProgress: TProgressBar
    Left = 8
    Top = 56
    Width = 345
    Height = 11
    Min = 0
    Max = 100
    TabOrder = 2
  end
  object Memo1: TMemo
    Left = 8
    Top = 80
    Width = 385
    Height = 321
    TabOrder = 3
  end
  object Button2: TButton
    Left = 312
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 5
    OnClick = Button3Click
  end
  object MainMenu1: TMainMenu
    Left = 368
    Top = 48
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open'
        ShortCut = 16463
        OnClick = Button1Click
      end
      object Send1: TMenuItem
        Caption = '&Send'
        ShortCut = 16467
        OnClick = Send1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'NewsLetter|*.html|All Files|*.*'
    FilterIndex = 0
    Left = 320
  end
end
