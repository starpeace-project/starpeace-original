object MainForm: TMainForm
  Left = 206
  Top = 252
  BorderStyle = bsSingle
  Caption = 'Create Account Tool...'
  ClientHeight = 474
  ClientWidth = 599
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
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Directory Server:'
  end
  object Label2: TLabel
    Left = 355
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object mUsers: TMemo
    Left = 8
    Top = 32
    Width = 489
    Height = 257
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
  end
  object brnLoad: TButton
    Left = 512
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 1
    OnClick = brnLoadClick
  end
  object btnRun: TButton
    Left = 512
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 2
    OnClick = btnRunClick
  end
  object eServer: TEdit
    Left = 91
    Top = 4
    Width = 239
    Height = 21
    TabOrder = 3
    Text = '10.10.15.101'
  end
  object ePort: TEdit
    Left = 383
    Top = 4
    Width = 114
    Height = 21
    TabOrder = 4
    Text = '2222'
  end
  object mLogs: TMemo
    Left = 8
    Top = 296
    Width = 489
    Height = 170
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object cbNewSerial: TCheckBox
    Left = 512
    Top = 96
    Width = 77
    Height = 17
    Caption = 'New Serial'
    TabOrder = 6
  end
end
