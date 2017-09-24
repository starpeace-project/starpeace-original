object Form1: TForm1
  Left = 458
  Top = 217
  Width = 393
  Height = 478
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label12: TLabel
    Left = 24
    Top = 80
    Width = 67
    Height = 13
    Caption = 'Criminal  Alias '
  end
  object nbMain: TNotebook
    Left = 0
    Top = 0
    Width = 385
    Height = 451
    Align = alClient
    PageIndex = 4
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'Default'
      object Label1: TLabel
        Left = 24
        Top = 56
        Width = 67
        Height = 13
        Caption = 'Criminal  Alias '
      end
      object edTycoonName: TEdit
        Left = 94
        Top = 54
        Width = 148
        Height = 21
        TabOrder = 0
        Text = 'ajax'
      end
      object Button1: TButton
        Left = 248
        Top = 52
        Width = 75
        Height = 25
        Caption = 'Log In'
        TabOrder = 1
        OnClick = Button1Click
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'CreateAccount'
      object Label2: TLabel
        Left = 24
        Top = 56
        Width = 45
        Height = 13
        Caption = 'SP  Alias '
      end
      object Label14: TLabel
        Left = 24
        Top = 80
        Width = 67
        Height = 13
        Caption = 'Criminal  Alias '
      end
      object edSPAlias: TEdit
        Left = 94
        Top = 54
        Width = 148
        Height = 21
        TabOrder = 0
        Text = 'ajax'
      end
      object Button2: TButton
        Left = 249
        Top = 77
        Width = 75
        Height = 25
        Caption = 'Create'
        TabOrder = 1
        OnClick = Button2Click
      end
      object edCrimAlias: TEdit
        Left = 95
        Top = 78
        Width = 145
        Height = 21
        TabOrder = 2
        Text = 'esc'
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'TeamInfo'
      object Label3: TLabel
        Left = 16
        Top = 46
        Width = 32
        Height = 13
        Caption = 'Teams'
      end
      object Label4: TLabel
        Left = 16
        Top = 68
        Width = 43
        Height = 13
        Caption = 'Members'
      end
      object Label5: TLabel
        Left = 196
        Top = 68
        Width = 24
        Height = 13
        Caption = 'Skills'
      end
      object Label6: TLabel
        Left = 208
        Top = 46
        Width = 30
        Height = 13
        Caption = 'Status'
      end
      object lbTeamStatus: TLabel
        Left = 245
        Top = 25
        Width = 3
        Height = 13
      end
      object lbCriminalName: TLabel
        Left = 19
        Top = 16
        Width = 3
        Height = 13
      end
      object cbTeams: TComboBox
        Left = 54
        Top = 42
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 0
        OnChange = cbTeamsChange
      end
      object lbCriminals: TListBox
        Left = 16
        Top = 84
        Width = 169
        Height = 292
        ItemHeight = 13
        TabOrder = 1
        OnClick = lbCriminalsClick
      end
      object lbCriminalInfo: TListBox
        Left = 192
        Top = 84
        Width = 185
        Height = 292
        ItemHeight = 13
        TabOrder = 2
      end
      object btnHQ: TButton
        Left = 206
        Top = 420
        Width = 89
        Height = 25
        Caption = 'Set Headquarter'
        TabOrder = 3
        OnClick = btnHQClick
      end
      object btnHire: TButton
        Left = 96
        Top = 384
        Width = 75
        Height = 26
        Caption = 'Hire'
        TabOrder = 4
        OnClick = btnHireClick
      end
      object btnFire: TButton
        Left = 176
        Top = 384
        Width = 75
        Height = 26
        Caption = 'Fire'
        TabOrder = 5
        OnClick = btnFireClick
      end
      object Train: TButton
        Left = 256
        Top = 384
        Width = 75
        Height = 26
        Caption = 'Train'
        TabOrder = 6
      end
      object btnMission: TButton
        Left = 304
        Top = 420
        Width = 75
        Height = 25
        Caption = 'Mission >'
        TabOrder = 7
        OnClick = btnMissionClick
      end
      object Button7: TButton
        Left = 17
        Top = 385
        Width = 75
        Height = 25
        Caption = 'Create Team'
        TabOrder = 8
        OnClick = Button7Click
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'MissionSetup'
      object Missions: TLabel
        Left = 16
        Top = 32
        Width = 40
        Height = 13
        Caption = 'Missions'
      end
      object Label7: TLabel
        Left = 20
        Top = 106
        Width = 72
        Height = 13
        Caption = 'Roles to assign'
      end
      object lbRole1: TLabel
        Left = 20
        Top = 130
        Width = 36
        Height = 13
        Caption = 'lbRole1'
      end
      object lbRole2: TLabel
        Left = 20
        Top = 155
        Width = 36
        Height = 13
        Caption = 'lbRole2'
      end
      object lbRole3: TLabel
        Left = 20
        Top = 180
        Width = 36
        Height = 13
        Caption = 'lbRole3'
      end
      object lbRole4: TLabel
        Left = 20
        Top = 204
        Width = 36
        Height = 13
        Caption = 'lbRole4'
      end
      object lbRole5: TLabel
        Left = 20
        Top = 229
        Width = 36
        Height = 13
        Caption = 'lbRole5'
      end
      object lbRole6: TLabel
        Left = 20
        Top = 254
        Width = 36
        Height = 13
        Caption = 'lbRole6'
      end
      object lbRole7: TLabel
        Left = 20
        Top = 279
        Width = 36
        Height = 13
        Caption = 'lbRole7'
      end
      object lbRole8: TLabel
        Left = 20
        Top = 302
        Width = 36
        Height = 13
        Caption = 'lbRole8'
      end
      object Label8: TLabel
        Left = 116
        Top = 106
        Width = 36
        Height = 13
        Caption = 'Criminal'
      end
      object Label9: TLabel
        Left = 18
        Top = 8
        Width = 85
        Height = 13
        Caption = 'Select the mission'
      end
      object cbMissions: TComboBox
        Left = 62
        Top = 30
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 0
        OnChange = cbMissionsChange
      end
      object cbCriminal1: TComboBox
        Left = 116
        Top = 130
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 1
        Text = 'cbCriminal1'
      end
      object cbCriminal3: TComboBox
        Left = 116
        Top = 178
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 2
        Text = 'cbCriminal3'
      end
      object cbCriminal2: TComboBox
        Left = 116
        Top = 154
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 3
        Text = 'cbCriminal2'
      end
      object cbCriminal4: TComboBox
        Left = 116
        Top = 202
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 4
        Text = 'cbCriminal4'
      end
      object cbCriminal5: TComboBox
        Left = 116
        Top = 226
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 5
        Text = 'cbCriminal5'
      end
      object cbCriminal6: TComboBox
        Left = 116
        Top = 250
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 6
        Text = 'cbCriminal6'
      end
      object cbCriminal7: TComboBox
        Left = 116
        Top = 274
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 7
        Text = 'cbCriminal7'
      end
      object cbCriminal8: TComboBox
        Left = 116
        Top = 298
        Width = 145
        Height = 21
        ItemHeight = 0
        TabOrder = 8
        Text = 'cbCriminal8'
      end
      object Button3: TButton
        Left = 301
        Top = 417
        Width = 75
        Height = 25
        Caption = 'ACTION'
        TabOrder = 9
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 219
        Top = 416
        Width = 75
        Height = 25
        Caption = 'Objective'
        TabOrder = 10
      end
      object Button6: TButton
        Left = 136
        Top = 416
        Width = 75
        Height = 25
        Caption = '< Back'
        TabOrder = 11
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'MissionReport'
      object Label10: TLabel
        Left = 16
        Top = 24
        Width = 68
        Height = 13
        Caption = 'Mission output'
      end
      object lbOutput: TListBox
        Left = 16
        Top = 48
        Width = 353
        Height = 361
        ItemHeight = 13
        TabOrder = 0
      end
      object Button5: TButton
        Left = 294
        Top = 418
        Width = 75
        Height = 25
        Caption = '< Back'
        TabOrder = 1
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 272
    Top = 48
  end
end
