object ProxyConfig: TProxyConfig
  Left = 0
  Top = 0
  Width = 501
  Height = 193
  Anchors = []
  Color = clBtnFace
  ParentColor = False
  TabOrder = 0
  object Label2: TLabel
    Left = 16
    Top = 103
    Width = 25
    Height = 13
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '&Host:'
    FocusControl = Edit1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 311
    Top = 103
    Width = 22
    Height = 13
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '&Port:'
    FocusControl = Edit2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 152
    Top = 53
    Width = 48
    Height = 13
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Username'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 311
    Top = 53
    Width = 46
    Height = 13
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Password'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 13
    Width = 122
    Height = 84
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Proxy Select'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Sock 4'
      'Sock 5')
    ParentFont = False
    TabOrder = 0
    OnClick = CheckBox1Click
    OnEnter = CheckBox1Click
  end
  object Edit1: TEdit
    Left = 16
    Top = 119
    Width = 290
    Height = 21
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 308
    Top = 119
    Width = 91
    Height = 21
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    Text = '1080'
  end
  object Edit3: TEdit
    Left = 149
    Top = 69
    Width = 157
    Height = 21
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object CheckBox1: TCheckBox
    Left = 152
    Top = 29
    Width = 210
    Height = 20
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Authentication'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object Edit4: TEdit
    Left = 308
    Top = 69
    Width = 91
    Height = 21
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PasswordChar = '*'
    TabOrder = 5
  end
  object hostname: TCheckBox
    Left = 16
    Top = 151
    Width = 250
    Height = 20
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Use proxy to resolve hostnames'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object Button1: TButton
    Left = 408
    Top = 116
    Width = 92
    Height = 28
    Anchors = [akLeft, akTop, akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 7
  end
  object Button2: TButton
    Left = 408
    Top = 69
    Width = 92
    Height = 28
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 8
    OnClick = SpeedButton2Click
  end
  object Button3: TButton
    Left = 328
    Top = 16
    Width = 161
    Height = 25
    Caption = 'Test Starpeace Conection'
    TabOrder = 9
  end
end
