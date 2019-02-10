object Form1: TForm1
  Left = 241
  Top = 327
  Width = 697
  Height = 415
  Caption = 'Star Peace Stats'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Chart1: TChart
    Left = 0
    Top = 0
    Width = 689
    Height = 335
    AnimatedZoom = True
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    Chart3DPercent = 10
    View3D = False
    Align = alClient
    TabOrder = 0
    AutoSize = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 335
    Width = 689
    Height = 53
    Align = alBottom
    BorderWidth = 4
    TabOrder = 1
    object Label1: TLabel
      Left = 26
      Top = 13
      Width = 40
      Height = 13
      Caption = 'DSAddr:'
    end
    object Label2: TLabel
      Left = 218
      Top = 13
      Width = 37
      Height = 13
      Caption = 'DSPort:'
    end
    object Edit1: TEdit
      Left = 72
      Top = 10
      Width = 129
      Height = 21
      TabOrder = 0
      Text = 'dir.starpeace.net'
    end
    object Edit2: TEdit
      Left = 264
      Top = 10
      Width = 41
      Height = 21
      TabOrder = 1
      Text = '2222'
    end
    object Button1: TButton
      Left = 421
      Top = 9
      Width = 75
      Height = 22
      Caption = 'Get Data'
      TabOrder = 2
      OnClick = Button1Click
    end
    object ProgressBar1: TProgressBar
      Left = 5
      Top = 39
      Width = 679
      Height = 9
      Align = alBottom
      Min = 0
      Max = 100
      TabOrder = 3
    end
    object CheckBox1: TCheckBox
      Left = 313
      Top = 12
      Width = 97
      Height = 17
      Caption = 'Plot Variation'
      TabOrder = 4
    end
  end
end
