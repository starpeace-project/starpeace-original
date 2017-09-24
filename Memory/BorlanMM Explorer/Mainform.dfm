object Form3: TForm3
  Left = 283
  Top = 275
  Width = 764
  Height = 481
  Caption = 'BorlandMM Explorer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 756
    Height = 29
    Caption = 'ToolBar1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Hint = 'Explorer'
      Caption = 'ToolButton1'
      ImageIndex = 0
    end
    object ToolButton3: TToolButton
      Left = 23
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 31
      Top = 2
      Hint = 'GetMem'
      Caption = 'ToolButton4'
      ImageIndex = 2
      OnClick = ToolButton4Click
    end
    object ToolButton2: TToolButton
      Left = 54
      Top = 2
      Hint = 'SetLength'
      Caption = 'ToolButton2'
      ImageIndex = 1
      OnClick = ToolButton2Click
    end
    object ToolButton5: TToolButton
      Left = 77
      Top = 2
      Hint = 'Create Object'
      Caption = 'ToolButton5'
      ImageIndex = 2
      OnClick = ToolButton5Click
    end
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 29
    Width = 297
    Height = 400
    Align = alLeft
    Indent = 19
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 429
    Width = 756
    Height = 19
    Panels = <>
    SimplePanel = False
  end
end
