object StatsForm: TStatsForm
  Left = 184
  Top = 196
  Width = 719
  Height = 415
  BorderStyle = bsSizeToolWin
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
    Width = 529
    Height = 281
    AnimatedZoom = True
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    Chart3DPercent = 10
    View3D = False
    TabOrder = 0
    AutoSize = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 335
    Width = 711
    Height = 53
    Align = alBottom
    BorderWidth = 4
    TabOrder = 1
    object Button1: TButton
      Left = 581
      Top = 9
      Width = 75
      Height = 22
      Caption = 'Get Data'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ProgressBar1: TProgressBar
      Left = 5
      Top = 39
      Width = 701
      Height = 9
      Align = alBottom
      Min = 0
      Max = 100
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 25
      Top = 12
      Width = 97
      Height = 17
      Caption = 'Plot Variation'
      TabOrder = 2
    end
  end
end
