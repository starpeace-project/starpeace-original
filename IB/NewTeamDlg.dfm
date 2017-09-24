object NewTeam: TNewTeam
  Left = 384
  Top = 285
  Width = 278
  Height = 122
  Caption = 'Create new team'
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
    Top = 32
    Width = 56
    Height = 13
    Caption = 'Team name'
  end
  object edTeamName: TEdit
    Left = 81
    Top = 29
    Width = 184
    Height = 21
    TabOrder = 0
    Text = 'buddies'
  end
  object Button1: TButton
    Left = 192
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 112
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = Button2Click
  end
end
