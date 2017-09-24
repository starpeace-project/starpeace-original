object ProxySettingsMainForm: TProxySettingsMainForm
  Left = 234
  Top = 259
  BorderStyle = bsDialog
  Caption = 'Star Peace Proxy Settings'
  ClientHeight = 193
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inline TProxyConfig1: TProxyConfig
    Width = 489
    Align = alClient
    inherited Label2: TLabel
      Width = 34
      Height = 14
    end
    inherited Label3: TLabel
      Width = 31
      Height = 14
    end
    inherited Label4: TLabel
      Left = 132
      Top = 57
      Width = 60
      Height = 14
      Caption = 'Username:'
    end
    inherited Label5: TLabel
      Left = 279
      Top = 57
      Width = 58
      Height = 14
      AutoSize = False
      Caption = 'Password:'
    end
    inherited RadioGroup1: TRadioGroup
      Width = 110
    end
    inherited Edit1: TEdit
      Width = 278
      Height = 22
    end
    inherited Edit2: TEdit
      Width = 79
      Height = 22
    end
    inherited Edit3: TEdit
      Left = 129
      Top = 73
      Width = 145
      Height = 22
    end
    inherited CheckBox1: TCheckBox
      Left = 132
      Top = 33
      Width = 198
    end
    inherited Edit4: TEdit
      Left = 278
      Top = 73
      Width = 109
      Height = 22
    end
    inherited hostname: TCheckBox
      Width = 238
    end
    inherited Button1: TButton
      Left = 400
      Top = 124
      Width = 80
      Height = 25
      OnClick = TProxyConfig1Button1Click
    end
    inherited Button2: TButton
      Top = 85
      Width = 80
      Height = 25
      OnClick = TProxyConfig1Button2Click
    end
    inherited Button3: TButton
      Left = 320
    end
  end
end
